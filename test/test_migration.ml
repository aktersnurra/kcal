let test_creates_ledger_tables () =
  let db = Test_support.open_temporary_database () in
  Alcotest.(check bool) "migration succeeds" true (Result.is_ok (Migration.apply_all db));
  List.iter (Test_support.table_exists db) [ "users"; "meals"; "weigh_ins" ]

let test_repeat_is_a_no_op () =
  let db = Test_support.open_temporary_database () in
  Result.get_ok (Migration.apply_all db);
  Alcotest.(check bool) "second migration succeeds" true (Result.is_ok (Migration.apply_all db))

let test_failure_rolls_back_and_does_not_record_version () =
  let db = Test_support.open_temporary_database () in
  ignore (Sqlite3.exec db "CREATE TABLE users (id TEXT PRIMARY KEY)");
  Alcotest.(check bool) "migration fails" true (Result.is_error (Migration.apply_all db));
  Alcotest.(check bool) "migration bookkeeping rolled back" false
    (Test_support.table_exists_bool db "schema_migrations")

let () =
  Alcotest.run "migration"
    [
      ( "schema",
        [ Alcotest.test_case "creates ledger tables" `Quick test_creates_ledger_tables;
          Alcotest.test_case "repeat is a no-op" `Quick test_repeat_is_a_no_op;
          Alcotest.test_case "failure rolls back" `Quick test_failure_rolls_back_and_does_not_record_version ] );
    ]
