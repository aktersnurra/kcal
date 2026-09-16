let test_creates_ledger_tables () =
  let db = Test_support.open_temporary_database () in
  Alcotest.(check bool) "migration succeeds" true
    (Result.is_ok (Migration.apply_all db));
  List.iter (Test_support.table_exists db)
    [ "users"; "meals"; "weigh_ins" ]

let () = Alcotest.run "migration" [ ("schema", [ Alcotest.test_case "creates ledger tables" `Quick test_creates_ledger_tables ]) ]
