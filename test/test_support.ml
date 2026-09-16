let open_temporary_database () = Sqlite3.db_open ":memory:"

let table_exists db name =
  let statement =
    Sqlite3.prepare db
      "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ? LIMIT 1"
  in
  ignore (Sqlite3.bind statement 1 (Sqlite3.Data.TEXT name));
  let exists = Sqlite3.step statement = Sqlite3.Rc.ROW in
  ignore (Sqlite3.finalize statement);
  Alcotest.(check bool) ("table " ^ name ^ " exists") true exists
