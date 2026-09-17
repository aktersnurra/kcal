let sync_all ~store ~sync =
  match Store_sqlite.withings_connections store with
  | Error _ as error -> error
  | Ok connections ->
      List.fold_left
        (fun result (user, connection) ->
          Result.bind result (fun () -> sync ~user ~connection))
        (Ok ()) connections
