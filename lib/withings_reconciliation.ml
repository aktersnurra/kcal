let sync_all ~store ~sync =
  match Store_sqlite.withings_connections store with
  | Error _ as error -> error
  | Ok connections ->
      let first_error = ref None in
      List.iter
        (fun (user, connection) ->
          match sync ~user ~connection with Ok () -> () | Error error -> if !first_error = None then first_error := Some error)
        connections;
      match !first_error with None -> Ok () | Some error -> Error error
