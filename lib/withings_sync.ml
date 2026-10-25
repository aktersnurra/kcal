module Domain_error = Error
open Base
module Error = Domain_error

let log_src =
  Logs.Src.create "kcal.withings.sync"
    ~doc:"Withings measurement synchronization"

module Log = (val Logs.src_log log_src : Logs.LOG)

type t = {
  store : Store_sqlite.t;
  client : (module Withings.S);
  token_key : Secret.Key.t;
  now : unit -> Ptime.t;
}

let make ~store ~client ~token_key ~now = { store; client; token_key; now }

let kg measurement =
  let value =
    Float.(
      Int64.to_float measurement.Withings.value
      * (10. ** of_int measurement.unit_))
  in
  if (not (Float.is_finite value)) || Float.(value <= 0.0) then None
  else Some value

let imports account measurements =
  let rec collect values = function
    | [] ->
        Ok
          (List.dedup_and_sort
             ~compare:(fun left right ->
               String.compare (Weigh_in.Import.to_raw left).external_id
                 (Weigh_in.Import.to_raw right).external_id)
             values)
    | measurement :: rest when measurement.Withings.measure_type <> 1 ->
        collect values rest
    | measurement :: rest -> (
        match
          ( kg measurement,
            Ptime.of_float_s (Int64.to_float measurement.measured_at) )
        with
        | Some weight_kg, Some measured_at
          when not (String.is_empty measurement.group_id) -> (
            let withings_user_id =
              Withings_connection.Account.to_string account
            in
            match
              Weigh_in.Import.of_raw
                Weigh_in.
                  {
                    external_id = withings_user_id ^ ":" ^ measurement.group_id;
                    measured_at;
                    weight_kg;
                  }
            with
            | Ok row -> collect (row :: values) rest
            | Error _ as error -> error)
        | _ -> Error (Error.Withings_failure Malformed_response))
  in
  collect [] measurements

let ( let* ) result f = Result.bind result ~f

let load_credentials t ~user ~connection ~user_id =
  match
    Store_sqlite.withings_credentials t.store ~user ~connection ~key:t.token_key
  with
  | Ok credentials -> Ok credentials
  | Error error as result ->
      Log.err (fun m ->
          m "user=%s could not load stored Withings credentials: %s" user_id
            (Error.to_string error));
      result

let require_reauthorization t ~user ~connection ~user_id =
  Log.warn (fun m ->
      m
        "user=%s Withings refresh token rejected, marking connection for \
         reauthorization"
        user_id);
  match
    Store_sqlite.mark_withings_reauthorization t.store ~user ~connection
  with
  | Ok () -> Error (Error.Invalid_input "Withings reauthorization required")
  | Error error as result ->
      Log.err (fun m ->
          m "user=%s failed to mark Withings connection for reauthorization: %s"
            user_id (Error.to_string error));
      result

let refresh_credentials t ~user ~connection ~user_id credentials =
  if Ptime.compare credentials.Withings.expires_at (t.now ()) > 0 then
    Ok (credentials, connection)
  else
    let module Client = (val t.client : Withings.S) in
    match Client.refresh ~refresh_token:credentials.refresh_token with
    | Ok credentials ->
        Log.info (fun m -> m "user=%s refreshed Withings access token" user_id);
        Result.map
          (Store_sqlite.refresh_withings_credentials t.store ~user ~connection
             ~key:t.token_key credentials) ~f:(fun connection ->
            (credentials, connection))
    | Error (Error.Withings_failure (Credential_rejection _)) ->
        require_reauthorization t ~user ~connection ~user_id
    | Error error as result ->
        Log.err (fun m ->
            m "user=%s Withings token refresh failed: %s" user_id
              (Error.to_string error));
        result

let fetch_measurements t ~connection ~user_id credentials =
  let module Client = (val t.client : Withings.S) in
  match
    Client.get_measurements ~access_token:credentials.Withings.access_token
      ~lastupdate:connection.Withings_connection.sync_cursor
  with
  | Ok batch -> Ok batch
  | Error error as result ->
      Log.err (fun m ->
          m "user=%s fetching Withings measurements failed: %s" user_id
            (Error.to_string error));
      result

let normalize_measurements ~account ~user_id batch =
  match imports account batch.Withings.measurements with
  | Ok rows -> Ok rows
  | Error error as result ->
      Log.err (fun m ->
          m "user=%s could not parse Withings measurement batch: %s" user_id
            (Error.to_string error));
      result

let persist_imports t ~user ~connection ~user_id ~cursor rows =
  match
    Store_sqlite.persist_withings_import t.store ~user ~connection ~rows
      ~expected_cursor:connection.Withings_connection.sync_cursor ~cursor
  with
  | Ok () as result ->
      Log.info (fun m ->
          m "user=%s synced %d Withings measurement(s)" user_id
            (List.length rows));
      result
  | Error error as result ->
      Log.err (fun m ->
          m "user=%s failed to persist Withings import: %s" user_id
            (Error.to_string error));
      result

let sync t ~user ~connection =
  let user_id = User_id.to_string user.User.id in
  let* credentials = load_credentials t ~user ~connection ~user_id in
  let* credentials, connection =
    refresh_credentials t ~user ~connection ~user_id credentials
  in
  let account = connection.account in
  let* batch = fetch_measurements t ~connection ~user_id credentials in
  let* rows = normalize_measurements ~account ~user_id batch in
  let cursor = batch.lastupdate in
  persist_imports t ~user ~connection ~user_id ~cursor rows
