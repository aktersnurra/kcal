let log_src = Logs.Src.create "kcal.withings.sync" ~doc:"Withings measurement synchronization"
module Log = (val Logs.src_log log_src : Logs.LOG)

type t = {
  store : Store_sqlite.t;
  client : (module Withings.S);
  token_key : bytes;
  now : unit -> Ptime.t;
}

let make ~store ~client ~token_key ~now = { store; client; token_key; now }

let kg measurement =
  let value = Int64.to_float measurement.Withings.value *. (10. ** float_of_int measurement.unit_) in
  if classify_float value = FP_nan || classify_float value = FP_infinite || value <= 0.0 then None else Some value

let imports connection measurements =
  let rec collect values = function
    | [] -> Ok (List.sort_uniq (fun (left : Weigh_in.import) right -> String.compare left.external_id right.external_id) values)
    | measurement :: rest when measurement.Withings.measure_type <> 1 -> collect values rest
    | measurement :: rest ->
        match kg measurement, Ptime.of_float_s (Int64.to_float measurement.measured_at) with
        | Some weight_kg, Some measured_at when measurement.group_id <> "" ->
            let withings_user_id = Option.value connection.Withings_connection.withings_user_id ~default:"unknown" in
            collect (Weigh_in.{ external_id = withings_user_id ^ ":" ^ measurement.group_id; measured_at; weight_kg } :: values) rest
        | _ -> Error (Error.Invalid_input "invalid Withings weight measurement")
  in collect [] measurements

let cursor previous received =
  match previous, received with
  | Some left, Some right -> Some (Int64.max left right)
  | Some value, None | None, Some value -> Some value
  | None, None -> None

let ( let* ) = Result.bind

let load_credentials t ~user ~connection ~user_id =
  match Store_sqlite.withings_credentials t.store ~user ~connection ~key:t.token_key with
  | Ok credentials -> Ok credentials
  | Error error as result ->
      Log.err (fun m -> m "user=%s could not load stored Withings credentials: %s" user_id (Error.to_string error));
      result

let require_reauthorization t ~user ~connection ~user_id =
  Log.warn (fun m -> m "user=%s Withings refresh token rejected, marking connection for reauthorization" user_id);
  match Store_sqlite.mark_withings_reauthorization t.store ~user ~connection with
  | Ok () -> Error (Error.Invalid_input "Withings reauthorization required")
  | Error error as result ->
      Log.err (fun m -> m "user=%s failed to mark Withings connection for reauthorization: %s" user_id (Error.to_string error));
      result

let refresh_credentials t ~user ~connection ~user_id credentials =
  if Ptime.compare credentials.Withings.expires_at (t.now ()) > 0 then Ok (credentials, connection)
  else
    let module Client = (val t.client : Withings.S) in
    match Client.refresh ~refresh_token:credentials.refresh_token with
    | Ok credentials ->
        Log.info (fun m -> m "user=%s refreshed Withings access token" user_id);
        Result.map (fun connection -> (credentials, connection))
          (Store_sqlite.save_withings_credentials t.store ~user ~key:t.token_key credentials)
    | Error error when Withings.is_permanent_refresh_error error ->
        require_reauthorization t ~user ~connection ~user_id
    | Error error as result ->
        Log.err (fun m -> m "user=%s Withings token refresh failed: %s" user_id (Error.to_string error));
        result

let fetch_measurements t ~connection ~user_id credentials =
  let module Client = (val t.client : Withings.S) in
  match Client.get_measurements ~access_token:credentials.Withings.access_token ~lastupdate:connection.Withings_connection.sync_cursor with
  | Ok batch -> Ok batch
  | Error error as result ->
      Log.err (fun m -> m "user=%s fetching Withings measurements failed: %s" user_id (Error.to_string error));
      result

let normalize_measurements ~connection ~user_id batch =
  match imports connection batch.Withings.measurements with
  | Ok rows -> Ok rows
  | Error error as result ->
      Log.err (fun m -> m "user=%s could not parse Withings measurement batch: %s" user_id (Error.to_string error));
      result

let persist_imports t ~user ~connection ~user_id ~cursor rows =
  match Store_sqlite.persist_withings_import t.store ~user ~connection ~rows ~cursor with
  | Ok () as result ->
      Log.info (fun m -> m "user=%s synced %d Withings measurement(s)" user_id (List.length rows));
      result
  | Error error as result ->
      Log.err (fun m -> m "user=%s failed to persist Withings import: %s" user_id (Error.to_string error));
      result

let sync t ~user ~connection =
  let user_id = User_id.to_string user.User.id in
  let* credentials = load_credentials t ~user ~connection ~user_id in
  let* credentials, connection = refresh_credentials t ~user ~connection ~user_id credentials in
  let* batch = fetch_measurements t ~connection ~user_id credentials in
  let* rows = normalize_measurements ~connection ~user_id batch in
  let cursor = cursor connection.sync_cursor batch.lastupdate in
  persist_imports t ~user ~connection ~user_id ~cursor rows
