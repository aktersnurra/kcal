open Base

let test_embedded_migrations_match_files () =
  List.iter
    ~f:(fun (version, name) ->
      let channel = Stdlib.open_in_bin ("../migrations/" ^ name) in
      let sql =
        Stdlib.Fun.protect
          ~finally:(fun () -> Stdlib.close_in channel)
          (fun () ->
            Stdlib.really_input_string channel
              (Stdlib.in_channel_length channel))
      in
      Alcotest.(check string)
        "installed and standalone migrations agree" sql
        (List.Assoc.find_exn Migration.migrations ~equal:Int.equal version))
    [
      (1, "001_ledger.sql");
      (2, "002_withings.sql");
      (3, "003_products.sql");
      (4, "004_ranked_search.sql");
      (5, "005_connection_history.sql");
    ]

let test_creates_ledger_tables () =
  Test_support.with_database (fun db ->
      Alcotest.(check bool)
        "migration succeeds" true
        (Result.is_ok (Migration.apply_all db));
      List.iter
        ~f:(Test_support.table_exists db)
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
      Option.value_exn (Result.ok (Migration.apply_all db));
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
        ~f:(fun (version, sql) ->
          if version <= 2 then (
            Option.value_exn (Result.ok (Migration.For_testing.execute db sql));
            Option.value_exn
              (Result.ok (Migration.For_testing.record_version db version))))
        Migration.migrations;
      Option.value_exn
        (Result.ok
           (Migration.For_testing.execute db
              "INSERT INTO users VALUES ('u','i','s','2026-01-01T00:00:00Z'); \
               INSERT INTO meals VALUES \
               ('m','u','2026-01-01T00:00:00Z','Soup',250,12,NULL,NULL,NULL,NULL,NULL,'2026-01-01T00:00:00Z','2026-01-01T00:00:00Z',NULL)"));
      Option.value_exn (Result.ok (Migration.apply_all db));
      Test_support.table_exists db "products";
      let statement =
        Sqlite3.prepare db "SELECT description FROM meals WHERE id='m'"
      in
      Stdlib.Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize statement))
        (fun () ->
          assert (
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW -> true
            | _ -> false);
          Alcotest.(check string)
            "meal preserved" "Soup"
            (Sqlite3.column_text statement 0)))

let test_v4_dangling_history () =
  Test_support.with_database (fun db ->
      List.iter
        ~f:(fun (version, sql) ->
          if version <= 4 then (
            Option.value_exn (Result.ok (Migration.For_testing.execute db sql));
            Option.value_exn
              (Result.ok (Migration.For_testing.record_version db version))))
        Migration.migrations;
      Option.value_exn
        (Result.ok
           (Migration.For_testing.execute db
              "INSERT INTO users VALUES \
               ('u','issuer','subject','2026-01-01T00:00:00Z'); INSERT INTO \
               weigh_ins VALUES \
               ('observation','u','2026-01-01T01:00:00Z',81.5,'withings','account:group','2026-01-01T02:00:00Z','2026-01-01T03:00:00Z','2026-01-01T04:00:00Z','missing')"));
      Option.value_exn (Result.ok (Migration.apply_all db));
      let stmt =
        Sqlite3.prepare db
          "SELECT id, weight_kg, measured_at, external_id, created_at, \
           updated_at, deleted_at, withings_connection_id FROM weigh_ins"
      in
      Stdlib.Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          assert (
            match Sqlite3.step stmt with Sqlite3.Rc.ROW -> true | _ -> false);
          Alcotest.(check string)
            "id" "observation"
            (Sqlite3.column_text stmt 0);
          Alcotest.(check (float 0.))
            "value" 81.5
            (Sqlite3.column_double stmt 1);
          List.iteri
            ~f:(fun i expected ->
              Alcotest.(check string)
                "history preserved" expected
                (Sqlite3.column_text stmt (i + 2)))
            [
              "2026-01-01T01:00:00Z";
              "account:group";
              "2026-01-01T02:00:00Z";
              "2026-01-01T03:00:00Z";
              "2026-01-01T04:00:00Z";
            ];
          Alcotest.(check bool)
            "dangling linkage normalized" true
            (Sqlite3.column_is_null stmt 7));
      let stmt = Sqlite3.prepare db "PRAGMA foreign_key_check" in
      Stdlib.Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          Alcotest.(check bool)
            "foreign keys clean" true
            (match Sqlite3.step stmt with
            | Sqlite3.Rc.DONE -> true
            | _ -> false)))

let test_v4_history_disconnect () =
  let path = Stdlib.Filename.temp_file "kcal-v4" ".sqlite" in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.Sys.remove path)
    (fun () ->
      let raw = Sqlite3.db_open path in
      let history db =
        let statement =
          Sqlite3.prepare db
            "SELECT \
             id,user_id,measured_at,weight_kg,source,external_id,created_at,updated_at,deleted_at \
             FROM weigh_ins ORDER BY id"
        in
        Stdlib.Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize statement))
          (fun () ->
            let rec rows acc =
              match Sqlite3.step statement with
              | Sqlite3.Rc.ROW ->
                  rows (Array.to_list (Sqlite3.row_data statement) :: acc)
              | Sqlite3.Rc.DONE -> List.rev acc
              | _ -> Alcotest.fail "history query failed"
            in
            rows [])
      in
      let original =
        Stdlib.Fun.protect
          ~finally:(fun () -> Test_support.close_database raw)
          (fun () ->
            List.iter
              ~f:(fun (version, sql) ->
                if version <= 4 then (
                  Option.value_exn
                    (Result.ok (Migration.For_testing.execute raw sql));
                  Option.value_exn
                    (Result.ok
                       (Migration.For_testing.record_version raw version))))
              Migration.migrations;
            Option.value_exn
              (Result.ok
                 (Migration.For_testing.execute raw
                    "INSERT INTO users VALUES \
                     ('12345678-1234-4234-8234-123456789abc','issuer','alice','2026-01-01T00:00:00Z'); \
                     INSERT INTO withings_connections VALUES \
                     ('connection','12345678-1234-4234-8234-123456789abc','account','ciphertext','ciphertext','2026-01-02T00:00:00Z',42,0,'2026-01-01T00:00:00Z','2026-01-01T00:00:00Z'); \
                     INSERT INTO weigh_ins VALUES \
                     ('linked','12345678-1234-4234-8234-123456789abc','2026-01-01T01:00:00Z',80.5,'withings','account:linked','2026-01-01T02:00:00Z','2026-01-01T03:00:00Z',NULL,'connection'); \
                     INSERT INTO weigh_ins VALUES \
                     ('dangling','12345678-1234-4234-8234-123456789abc','2026-01-01T01:00:00Z',81.5,'withings','account:deleted','2026-01-01T02:00:00Z','2026-01-01T03:00:00Z','2026-01-01T04:00:00Z','missing'); \
                     INSERT INTO weigh_ins VALUES \
                     ('manual','12345678-1234-4234-8234-123456789abc','2026-01-01T01:00:00Z',82.5,'manual',NULL,'2026-01-01T02:00:00Z','2026-01-01T03:00:00Z',NULL,NULL)"));
            history raw)
      in
      let store =
        Option.value_exn (Result.ok (Store_sqlite.open_database ~path))
      in
      Stdlib.Fun.protect
        ~finally:(fun () ->
          Option.value_exn (Result.ok (Store_sqlite.close store)))
        (fun () ->
          let db = Store_sqlite.For_testing.raw_database store in
          let check_history () =
            let render rows =
              List.map ~f:(List.map ~f:Sqlite3.Data.to_string_debug) rows
            in
            Alcotest.(check (list (list string)))
              "all durable observation columns preserved" (render original)
              (render (history db));
            let statement = Sqlite3.prepare db "PRAGMA foreign_key_check" in
            Stdlib.Fun.protect
              ~finally:(fun () -> ignore (Sqlite3.finalize statement))
              (fun () ->
                Alcotest.(check bool)
                  "foreign_key_check is empty" true
                  (match Sqlite3.step statement with
                  | Sqlite3.Rc.DONE -> true
                  | _ -> false))
          in
          check_history ();
          let user =
            Option.value_exn
              (Result.ok
                 (Store_sqlite.resolve_user store ~issuer:"issuer"
                    ~subject:"alice"))
          in
          Option.value_exn
            (Result.ok
               (Service.disconnect_withings (Service.make ~store) ~user));
          check_history ();
          let statement =
            Sqlite3.prepare db
              "SELECT count(*) FROM weigh_ins WHERE withings_connection_id IS \
               NOT NULL"
          in
          Stdlib.Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize statement))
            (fun () ->
              assert (
                match Sqlite3.step statement with
                | Sqlite3.Rc.ROW -> true
                | _ -> false);
              Alcotest.(check int)
                "all historical linkage null after disconnect" 0
                (Sqlite3.column_int statement 0));
          Alcotest.(check bool)
            "external identity uniqueness retained" true
            (Result.is_error
               (Migration.For_testing.execute db
                  "INSERT INTO weigh_ins SELECT \
                   'duplicate',user_id,measured_at,weight_kg,source,external_id,created_at,updated_at,deleted_at,withings_connection_id \
                   FROM weigh_ins WHERE id='dangling'"))))

let test_invalid_bookkeeping_is_not_absence () =
  Test_support.with_database (fun db ->
      Option.value_exn
        (Result.ok
           (Migration.For_testing.execute db
              "CREATE TABLE schema_migrations (broken TEXT)"));
      Alcotest.(check bool)
        "bookkeeping query failure propagates" true
        (Result.is_error (Migration.apply_all db));
      Alcotest.(check bool)
        "no schema changes committed" false
        (Test_support.table_exists_bool db "users"))

let () =
  Alcotest.run "migration"
    [
      ( "schema",
        [
          Alcotest.test_case "invalid bookkeeping is not absence" `Quick
            test_invalid_bookkeeping_is_not_absence;
          Alcotest.test_case "v4 history and disconnect" `Quick
            test_v4_history_disconnect;
          Alcotest.test_case "v4 dangling history" `Quick
            test_v4_dangling_history;
          Alcotest.test_case "embedded migrations match files" `Quick
            test_embedded_migrations_match_files;
          Alcotest.test_case "upgrade from v2 preserves meal" `Quick
            test_upgrade_from_v2_preserves_meal;
          Alcotest.test_case "creates ledger tables" `Quick
            test_creates_ledger_tables;
          Alcotest.test_case "repeat is a no-op" `Quick test_repeat_is_a_no_op;
          Alcotest.test_case "failure rolls back" `Quick
            test_failure_rolls_back_and_does_not_record_version;
        ] );
    ]
