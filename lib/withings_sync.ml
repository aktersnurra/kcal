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
  measurements
  |> List.filter (fun measurement -> measurement.Withings.measure_type = 1)
  |> List.filter_map (fun measurement ->
    match kg measurement, Ptime.of_float_s (Int64.to_float measurement.measured_at) with
    | Some weight_kg, Some measured_at ->
        let withings_user_id = Option.value connection.Withings_connection.withings_user_id ~default:"unknown" in
        Some Weigh_in.{ external_id = withings_user_id ^ ":" ^ measurement.group_id; measured_at; weight_kg }
    | _ -> None)
  |> List.sort_uniq (fun (left : Weigh_in.import) (right : Weigh_in.import) -> String.compare left.external_id right.external_id)

let cursor previous received =
  match previous, received with
  | Some left, Some right -> Some (Int64.max left right)
  | Some value, None | None, Some value -> Some value
  | None, None -> None

let sync t ~user ~connection =
  let module Client = (val t.client : Withings.S) in
  match Store_sqlite.withings_credentials t.store ~user ~connection ~key:t.token_key with
  | Error _ as error -> error
  | Ok credentials ->
      let refreshed =
        if Ptime.compare credentials.expires_at (t.now ()) > 0 then Ok (credentials, connection)
        else
          match Client.refresh ~refresh_token:credentials.refresh_token with
          | Ok credentials -> Result.map (fun connection -> (credentials, connection)) (Store_sqlite.save_withings_credentials t.store ~user ~key:t.token_key credentials)
          | Error _ ->
              ignore (Store_sqlite.mark_withings_reauthorization t.store ~user ~connection);
              Error (Error.Invalid_input "Withings reauthorization required")
      in
      match refreshed with
      | Error _ as error -> error
      | Ok (credentials, connection) ->
          match Client.get_measurements ~access_token:credentials.access_token ~lastupdate:connection.sync_cursor with
          | Error _ as error -> error
          | Ok batch ->
              Store_sqlite.persist_withings_import t.store ~user ~connection
                ~rows:(imports connection batch.measurements) ~cursor:(cursor connection.sync_cursor batch.lastupdate)
