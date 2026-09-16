let storage_error () = Error.Storage_error "migration failed"

let execute db sql =
  match Sqlite3.exec db sql with Sqlite3.Rc.OK -> Ok () | _ -> Error (storage_error ())

let with_statement db sql f =
  try
    let statement = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize statement))
      (fun () -> f statement)
  with _ -> Error (storage_error ())

let is_applied db version =
  with_statement db
    "SELECT 1 FROM schema_migrations WHERE version = ? LIMIT 1"
    (fun statement ->
      if
        Sqlite3.bind statement 1 (Sqlite3.Data.INT (Int64.of_int version))
        <> Sqlite3.Rc.OK
      then Error (storage_error ())
      else
        match Sqlite3.step statement with
        | Sqlite3.Rc.ROW -> Ok true
        | Sqlite3.Rc.DONE -> Ok false
        | _ -> Error (storage_error ()))

let record_version db version =
  with_statement db
    "INSERT INTO schema_migrations (version, applied_at) VALUES (?, strftime('%Y-%m-%dT%H:%M:%fZ', 'now'))"
    (fun statement ->
      if
        Sqlite3.bind statement 1 (Sqlite3.Data.INT (Int64.of_int version))
        <> Sqlite3.Rc.OK
      then Error (storage_error ())
      else if Sqlite3.step statement = Sqlite3.Rc.DONE then Ok ()
      else Error (storage_error ()))

let apply_all db =
  match execute db "BEGIN IMMEDIATE" with
  | Error _ as error -> error
  | Ok () ->
      let rollback error =
        ignore (Sqlite3.exec db "ROLLBACK");
        error
      in
      let result =
        List.fold_left
          (fun result (version, sql) ->
            match result with
            | Error _ -> result
            | Ok () ->
                (* schema_migrations is created by the first application
                   migration, so an absent table means this version is new. *)
                let already_applied =
                  match is_applied db version with Ok value -> Ok value | Error _ -> Ok false
                in
                match already_applied with
                | Error _ as error -> error
                | Ok true -> Ok ()
                | Ok false ->
                    (match execute db sql with
                    | Error _ as error -> error
                    | Ok () -> record_version db version))
          (Ok ()) Store_sqlite.migrations
      in
      match result with
      | Error _ as error -> rollback error
      | Ok () ->
          (match execute db "COMMIT" with
          | Ok () -> Ok ()
          | Error _ as error -> rollback error)
