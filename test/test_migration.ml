let test_embedded_ranked_search_migration_matches_file () =
  let channel = open_in_bin "../migrations/004_ranked_search.sql" in
  let sql =
    Fun.protect
      ~finally:(fun () -> close_in channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  in
  Alcotest.(check string)
    "installed and standalone migration agree" (String.trim sql)
    (String.trim (List.assoc 4 Store_sqlite.migrations))

let test_creates_ledger_tables () =
  Test_support.with_database (fun db ->
      Alcotest.(check bool)
        "migration succeeds" true
        (Result.is_ok (Migration.apply_all db));
      List.iter
        (Test_support.table_exists db)
        [
          "users";
          "meals";
          "weigh_ins";
          "withings_connections";
          "withings_oauth_states";
          "products";
        ])

let test_repeat_is_a_no_op () =
  Test_support.with_database (fun db ->
      Result.get_ok (Migration.apply_all db);
      Alcotest.(check bool)
        "second migration succeeds" true
        (Result.is_ok (Migration.apply_all db)))

let test_failure_rolls_back_and_does_not_record_version () =
  Test_support.with_database (fun db ->
      ignore (Sqlite3.exec db "CREATE TABLE users (id TEXT PRIMARY KEY)");
      Alcotest.(check bool)
        "migration fails" true
        (Result.is_error (Migration.apply_all db));
      Alcotest.(check bool)
        "migration bookkeeping rolled back" false
        (Test_support.table_exists_bool db "schema_migrations"))

let test_upgrade_from_v2_preserves_meal () =
  Test_support.with_database (fun db ->
      List.iter
        (fun (version, sql) ->
          if version <= 2 then (
            Result.get_ok (Migration.execute db sql);
            Result.get_ok (Migration.record_version db version)))
        Store_sqlite.migrations;
      let user =
        Result.get_ok
          (Store_sqlite.resolve_user db ~issuer:"issuer" ~subject:"alice")
      in
      let meal = Test_support.create_meal db user in
      Result.get_ok (Migration.apply_all db);
      Test_support.table_exists db "products";
      Alcotest.check Test_support.meal "meal preserved" meal
        (Result.get_ok (Store_sqlite.get_meal db ~user meal.id)))

let () =
  Alcotest.run "migration"
    [
      ( "schema",
        [
          Alcotest.test_case "embedded ranked search migration matches file"
            `Quick test_embedded_ranked_search_migration_matches_file;
          Alcotest.test_case "upgrade from v2 preserves meal" `Quick
            test_upgrade_from_v2_preserves_meal;
          Alcotest.test_case "creates ledger tables" `Quick
            test_creates_ledger_tables;
          Alcotest.test_case "repeat is a no-op" `Quick test_repeat_is_a_no_op;
          Alcotest.test_case "failure rolls back" `Quick
            test_failure_rolls_back_and_does_not_record_version;
        ] );
    ]
