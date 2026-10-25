module Domain_error = Error
open Base
module Error = Domain_error

let now = Option.value_exn (Ptime.of_float_s 1_800_000_000.)
let key = Option.value_exn (Result.ok (Secret.Key.of_bytes (Bytes.make 32 'k')))

let measurement ?(value = 80000L) ?(date = 1_700_000_000L) group_id =
  Withings.{ group_id; measured_at = date; measure_type = 1; value; unit_ = -3 }

let with_setup measurements f =
  Test_support.with_store (fun store user ->
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.ensure_withings_connection store ~user
                 ~withings_user_id:"upstream-alice")));
      let credentials =
        Withings.
          {
            access_token = "access";
            refresh_token = "refresh";
            expires_at =
              Option.value_exn (Ptime.add_span now (Ptime.Span.of_int_s 3600));
            withings_user_id = "upstream-alice";
          }
      in
      let connection =
        Option.value_exn
          (Result.ok
             (Store_sqlite.replace_withings_connection store ~user ~key
                credentials))
      in
      let module Client = struct
        let exchange_code ~redirect_uri:_ ~code:_ =
          Error (Error.Invalid_input "unused")

        let refresh ~refresh_token:_ = Error (Error.Invalid_input "unused")

        let get_measurements ~access_token:_ ~lastupdate:_ =
          Ok Withings.{ measurements; lastupdate = Some 77L }

        let subscribe ~access_token:_ ~callback_url:_ = Ok ()
      end in
      f store user connection (module Client : Withings.S))

let sync_cursor store =
  let statement =
    Sqlite3.prepare
      (Store_sqlite.For_testing.raw_database store)
      "SELECT sync_cursor FROM withings_connections"
  in
  Stdlib.Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize statement))
    (fun () ->
      match Sqlite3.step statement with
      | Sqlite3.Rc.ROW ->
          if Sqlite3.column_is_null statement 0 then None
          else Some (Sqlite3.column_int64 statement 0)
      | _ -> None)

let test_normalizes_deduplicates_and_advances_cursor () =
  with_setup
    [ measurement "one"; measurement "one"; measurement ~value:75000L "two" ]
    (fun store user connection client ->
      Option.value_exn
        (Result.ok
           (Withings_sync.sync
              (Withings_sync.make ~store ~client ~token_key:key ~now:(fun () ->
                   now))
              ~user ~connection));
      let rows =
        Option.value_exn
          (Result.ok
             (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                ~limit:10))
      in
      Alcotest.(check int) "deduplicated" 2 (List.length rows);
      Alcotest.(check (float 0.0001))
        "kg normalization" 80.0 (List.hd_exn rows).weight_kg;
      Alcotest.(check string)
        "imported provenance" "withings"
        (Weigh_in.source_to_string (List.hd_exn rows).source);
      (match (List.hd_exn rows).source with
      | Weigh_in.Manual -> Alcotest.fail "expected imported identity"
      | Withings identity ->
          Alcotest.(check string)
            "imported account" "upstream-alice" identity.account_id;
          Alcotest.(check string) "imported group" "one" identity.group_id);
      Alcotest.(check (option int64))
        "cursor advanced" (Some 77L) (sync_cursor store))

let test_upstream_change_preserves_timestamp_and_tombstone () =
  with_setup
    [ measurement "one" ]
    (fun store user connection client ->
      let sync measurements =
        let module Client = (val client : Withings.S) in
        ignore Client.get_measurements;
        let module Changed = struct
          include Client

          let get_measurements ~access_token:_ ~lastupdate:_ =
            Ok Withings.{ measurements; lastupdate = Some 80L }
        end in
        Withings_sync.sync
          (Withings_sync.make ~store
             ~client:(module Changed)
             ~token_key:key
             ~now:(fun () -> now))
          ~user ~connection
      in
      Option.value_exn (Result.ok (sync [ measurement "one" ]));
      let original =
        List.hd_exn
          (Option.value_exn
             (Result.ok
                (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                   ~limit:10)))
      in
      Option.value_exn
        (Result.ok
           (sync [ measurement ~value:81000L ~date:1_700_000_100L "one" ]));
      let changed =
        List.hd_exn
          (Option.value_exn
             (Result.ok
                (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                   ~limit:10)))
      in
      Alcotest.(check (float 0.0001))
        "upstream value updated" 81.0 changed.weight_kg;
      Alcotest.(check string)
        "timestamp preserved"
        (Time.to_utc_string original.measured_at)
        (Time.to_utc_string changed.measured_at);
      Option.value_exn
        (Result.ok (Store_sqlite.delete_weigh_in store ~user changed.id));
      Option.value_exn (Result.ok (sync [ measurement ~value:82000L "one" ]));
      Alcotest.(check int)
        "tombstone retained" 0
        (List.length
           (Option.value_exn
              (Result.ok
                 (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                    ~limit:10)))))

let test_relink_different_account_resets_cursor () =
  with_setup [] (fun store user connection _ ->
      Option.value_exn
        (Result.ok
           (Test_support.persist_withings_import_raw store ~user ~connection
              ~rows:[] ~cursor:(Some 100L)));
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.ensure_withings_connection store ~user
                 ~withings_user_id:"upstream-alice")));
      Alcotest.(check (option int64))
        "same account retains cursor" (Some 100L) (sync_cursor store);
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.ensure_withings_connection store ~user
                 ~withings_user_id:"upstream-bob")));
      Alcotest.(check (option int64))
        "different account starts from beginning" None (sync_cursor store))

let test_reconnect_same_account_updates_import () =
  with_setup [] (fun store user connection _ ->
      let original =
        Weigh_in.
          {
            external_id = "upstream-alice:one";
            measured_at = now;
            weight_kg = 80.0;
          }
      in
      Option.value_exn
        (Result.ok
           (Test_support.persist_withings_import_raw store ~user ~connection
              ~rows:[ original ] ~cursor:(Some 100L)));
      Option.value_exn
        (Result.ok (Store_sqlite.delete_withings_connection store ~user));
      let replacement =
        Option.value_exn
          (Result.ok
             (Test_support.ensure_withings_connection store ~user
                ~withings_user_id:"upstream-alice"))
      in
      let corrected = Weigh_in.{ original with weight_kg = 81.0 } in
      Option.value_exn
        (Result.ok
           (Test_support.persist_withings_import_raw store ~user
              ~connection:replacement ~rows:[ corrected ] ~cursor:(Some 101L)));
      let rows =
        Option.value_exn
          (Result.ok
             (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                ~limit:10))
      in
      Alcotest.(check int) "no duplicate" 1 (List.length rows);
      Alcotest.(check (float 0.0001))
        "corrected weight" 81.0 (List.hd_exn rows).weight_kg;
      let statement =
        Sqlite3.prepare
          (Store_sqlite.For_testing.raw_database store)
          "SELECT withings_connection_id FROM weigh_ins WHERE external_id = \
           'upstream-alice:one'"
      in
      Stdlib.Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize statement))
        (fun () ->
          Alcotest.(check bool)
            "import exists" true
            (match Sqlite3.step statement with
            | Sqlite3.Rc.ROW -> true
            | _ -> false);
          Alcotest.(check string)
            "new connection owns import" replacement.id
            (Sqlite3.column_text statement 0)))

let test_failed_transaction_retains_cursor () =
  with_setup [] (fun store user connection _ ->
      ignore
        (Sqlite3.exec
           (Store_sqlite.For_testing.raw_database store)
           "CREATE TRIGGER reject_import BEFORE INSERT ON weigh_ins WHEN \
            NEW.source = 'withings' BEGIN SELECT RAISE(ABORT, 'reject'); END");
      Alcotest.(check bool)
        "transaction fails" true
        (Result.is_error
           (Test_support.persist_withings_import_raw store ~user ~connection
              ~rows:
                [
                  Weigh_in.
                    {
                      external_id = "upstream-alice:one";
                      measured_at = now;
                      weight_kg = 80.0;
                    };
                ]
              ~cursor:(Some 99L)));
      Alcotest.(check (option int64)) "cursor retained" None (sync_cursor store))

let test_stale_batch_cannot_regress_value_or_cursor () =
  with_setup [] (fun store user connection _ ->
      let current =
        Weigh_in.
          {
            external_id = "upstream-alice:one";
            measured_at = now;
            weight_kg = 80.0;
          }
      in
      Option.value_exn
        (Result.ok
           (Test_support.persist_withings_import_raw store ~user ~connection
              ~rows:[ current ] ~cursor:(Some 100L)));
      let stale = Weigh_in.{ current with weight_kg = 70.0 } in
      Option.value_exn
        (Result.ok
           (Test_support.persist_withings_import_raw store ~user ~connection
              ~rows:[ stale ] ~cursor:(Some 99L)));
      let weight =
        List.hd_exn
          (Option.value_exn
             (Result.ok
                (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                   ~limit:10)))
      in
      Alcotest.(check (float 0.0001))
        "value is not regressed" 80.0 weight.weight_kg;
      Alcotest.(check (option int64))
        "cursor is not regressed" (Some 100L) (sync_cursor store))

let test_refresh_failure_requires_reauthorization () =
  with_setup
    [ measurement "one" ]
    (fun store user connection client ->
      let module Fake = struct
        include (val client : Withings.S)

        let refresh ~refresh_token:_ =
          Error (Error.Withings_failure (Credential_rejection 401))
      end in
      ignore
        (Withings_sync.sync
           (Withings_sync.make ~store
              ~client:(module Fake)
              ~token_key:key
              ~now:(fun () ->
                Ptime.add_span now (Ptime.Span.of_int_s 7200)
                |> Option.value_exn))
           ~user ~connection);
      let status =
        Option.value_exn
          (Result.ok (Store_sqlite.get_withings_status store ~user))
      in
      Alcotest.(check bool)
        "reauthorization" true
        (match status with
        | Withings_connection.Reauthorization_required _ -> true
        | _ -> false))

let test_transient_refresh_failure_does_not_require_reauthorization () =
  with_setup
    [ measurement "one" ]
    (fun store user connection client ->
      let module Fake = struct
        include (val client : Withings.S)

        let refresh ~refresh_token:_ =
          Error (Error.Invalid_input "Withings request failed")
      end in
      Alcotest.(check bool)
        "transient refresh fails" true
        (Result.is_error
           (Withings_sync.sync
              (Withings_sync.make ~store
                 ~client:(module Fake)
                 ~token_key:key
                 ~now:(fun () ->
                   Ptime.add_span now (Ptime.Span.of_int_s 7200)
                   |> Option.value_exn))
              ~user ~connection));
      let status =
        Option.value_exn
          (Result.ok (Store_sqlite.get_withings_status store ~user))
      in
      Alcotest.(check bool)
        "transient failure retains authorization" false
        (match status with
        | Withings_connection.Reauthorization_required _ -> true
        | _ -> false))

let test_refresh_race_with_account_replacement () =
  with_setup [] (fun store user connection client ->
      let replacement =
        Withings.
          {
            withings_user_id = "replacement";
            access_token = "new";
            refresh_token = "new-refresh";
            expires_at = now;
          }
      in
      let module Fake = struct
        include (val client : Withings.S)

        let refresh ~refresh_token:_ =
          let raw = Store_sqlite.For_testing.raw_database store in
          (* A network boundary must be outside every database transaction. *)
          Option.value_exn
            (Result.ok
               (Migration.For_testing.execute raw "BEGIN IMMEDIATE; ROLLBACK"));
          ignore
            (Option.value_exn
               (Result.ok
                  (Service.connect_withings (Service.make ~store) ~user ~key
                     replacement)));
          Ok
            Withings.
              {
                replacement with
                withings_user_id = "upstream-alice";
                access_token = "stale";
              }
      end in
      let sync =
        Withings_sync.make ~store
          ~client:(module Fake)
          ~token_key:key
          ~now:(fun () ->
            Option.value_exn (Ptime.add_span now (Ptime.Span.of_int_s 7200)))
      in
      Alcotest.(check bool)
        "stale refresh fails through sync" true
        (Result.is_error (Withings_sync.sync sync ~user ~connection));
      let _, current =
        Option.value_exn
          (Result.ok
             (Store_sqlite.withings_connection_for_identity store
                ~withings_user_id:"replacement"))
      in
      let saved =
        Option.value_exn
          (Result.ok
             (Store_sqlite.withings_credentials store ~user ~connection:current
                ~key))
      in
      Alcotest.(check string)
        "replacement token preserved" "new" saved.access_token;
      Alcotest.(check string)
        "replacement refresh preserved" "new-refresh" saved.refresh_token)

let test_import_does_not_repair_corrupt_weight () =
  with_setup
    [ measurement "one" ]
    (fun store user connection client ->
      let sync =
        Withings_sync.make ~store ~client ~token_key:key ~now:(fun () -> now)
      in
      Option.value_exn (Result.ok (Withings_sync.sync sync ~user ~connection));
      Alcotest.(check bool)
        "inject Float.infinity" true
        (match
           Sqlite3.exec
             (Store_sqlite.For_testing.raw_database store)
             "UPDATE weigh_ins SET weight_kg = 1e309"
         with
        | Sqlite3.Rc.OK -> true
        | _ -> false);
      Alcotest.(check bool)
        "corrupt correction rejected" true
        (match Withings_sync.sync sync ~user ~connection with
        | Error (Error.Storage_decoding _) -> true
        | _ -> false);
      let weight = ref 0.0 in
      ignore
        (Sqlite3.exec
           (Store_sqlite.For_testing.raw_database store)
           ~cb:(fun row _ ->
             weight := Float.of_string (Option.value_exn row.(0)))
           "SELECT weight_kg FROM weigh_ins");
      Alcotest.(check bool)
        "original corrupt value untouched" true (Float.is_inf !weight);
      Alcotest.(check (option int64))
        "cursor unchanged" (Some 77L) (sync_cursor store))

let test_full_sync_rejects_stale_watermark () =
  with_setup [] (fun store user connection _ ->
      Option.value_exn
        (Result.ok
           (Test_support.persist_withings_import_raw store ~user ~connection
              ~rows:
                [
                  Weigh_in.
                    {
                      external_id = "upstream-alice:one";
                      measured_at = now;
                      weight_kg = 80.0;
                    };
                ]
              ~cursor:(Some 100L)));
      let _, connection =
        List.hd_exn
          (Option.value_exn
             (Result.ok (Store_sqlite.withings_connections store)))
      in
      let request =
        Withings.
          {
            post_form =
              (fun ~uri:_ ~fields ->
                Alcotest.(check (option string))
                  "fetch uses stored cursor" (Some "100")
                  (List.Assoc.find fields ~equal:String.equal "lastupdate");
                Ok
                  {|{"status":0,"body":{"lastupdate":99,"more":0,"measuregrps":[{"grpid":"one","date":1700000000,"measures":[{"type":1,"value":70000,"unit":-3}]}]}}|});
          }
      in
      let client =
        Withings.make
          ~now:(fun () -> now)
          ~request
          ~config:Withings.{ client_id = "id"; client_secret = "secret" }
      in
      Option.value_exn
        (Result.ok
           (Withings_sync.sync
              (Withings_sync.make ~store ~client ~token_key:key ~now:(fun () ->
                   now))
              ~user ~connection));
      let row =
        List.hd_exn
          (Option.value_exn
             (Result.ok
                (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                   ~limit:10)))
      in
      Alcotest.(check (float 0.0001))
        "stale full sync retains weight" 80.0 row.weight_kg;
      Alcotest.(check (option int64))
        "stale full sync retains cursor" (Some 100L) (sync_cursor store))

let batch_json ?(more = `Int 0) ?(offset = `Null) cursor measurements =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("status", `Int 0);
         ( "body",
           `Assoc
             [
               ("more", more);
               ("offset", offset);
               ( "lastupdate",
                 match cursor with
                 | None -> `Null
                 | Some value -> `Intlit (Int64.to_string value) );
               ( "measuregrps",
                 `List
                   (List.map
                      ~f:(fun (m : Withings.measurement) ->
                        `Assoc
                          [
                            ("grpid", `String m.group_id);
                            ("date", `Intlit (Int64.to_string m.measured_at));
                            ( "measures",
                              `List
                                [
                                  `Assoc
                                    [
                                      ("type", `Int m.measure_type);
                                      ( "value",
                                        `Intlit (Int64.to_string m.value) );
                                      ("unit", `Int m.unit_);
                                    ];
                                ] );
                          ])
                      measurements) );
             ] );
       ])

let sync_request store user connection request =
  let client =
    Withings.make
      ~now:(fun () -> now)
      ~request
      ~config:Withings.{ client_id = "id"; client_secret = "secret" }
  in
  Withings_sync.sync
    (Withings_sync.make ~store ~client ~token_key:key ~now:(fun () -> now))
    ~user ~connection

let store_batch store user connection cursor weight =
  Test_support.persist_withings_import_raw store ~user ~connection
    ~rows:
      [
        Weigh_in.
          {
            external_id = "upstream-alice:one";
            measured_at = now;
            weight_kg = weight;
          };
      ]
    ~cursor
  |> fun result -> Option.value_exn (Result.ok result)

let check_state store user cursor weight =
  let rows =
    Option.value_exn
      (Result.ok
         (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
            ~limit:10))
  in
  Alcotest.(check (float 0.0001))
    "stored weight" weight (List.hd_exn rows).weight_kg;
  Alcotest.(check (option int64)) "stored cursor" cursor (sync_cursor store)

let test_missing_watermark_race () =
  with_setup [] (fun store user connection _ ->
      store_batch store user connection (Some 100L) 80.0;
      let connection =
        Test_support.connection_with_cursor connection (Some 100L)
      in
      let request =
        Withings.
          {
            post_form =
              (fun ~uri:_ ~fields:_ ->
                store_batch store user connection (Some 101L) 81.0;
                Ok (batch_json None [ measurement ~value:70000L "one" ]));
          }
      in
      Option.value_exn (Result.ok (sync_request store user connection request));
      check_state store user (Some 101L) 81.0)

let test_missing_watermark_baseline_and_equal_correction () =
  with_setup [] (fun store user connection _ ->
      store_batch store user connection (Some 100L) 80.0;
      let connection =
        Test_support.connection_with_cursor connection (Some 100L)
      in
      List.iter
        ~f:(fun (cursor, value, weight) ->
          let request =
            Withings.
              {
                post_form =
                  (fun ~uri:_ ~fields:_ ->
                    Ok (batch_json cursor [ measurement ~value "one" ]));
              }
          in
          Option.value_exn
            (Result.ok (sync_request store user connection request));
          check_state store user (Some 100L) weight)
        [ (None, 81000L, 81.0); (Some 100L, 82000L, 82.0) ])

let test_page_two_failure_does_not_persist () =
  with_setup [] (fun store user connection _ ->
      store_batch store user connection (Some 100L) 80.0;
      let connection =
        Test_support.connection_with_cursor connection (Some 100L)
      in
      let calls = ref 0 in
      let request =
        Withings.
          {
            post_form =
              (fun ~uri:_ ~fields:_ ->
                Int.incr calls;
                if !calls = 1 then
                  Ok
                    (batch_json ~more:(`Int 1) ~offset:(`Int 42) (Some 101L)
                       [ measurement ~value:70000L "one" ])
                else Error (Error.Withings_failure (Http_status 502)));
          }
      in
      Alcotest.(check bool)
        "second page fails sync" true
        (Result.is_error (sync_request store user connection request));
      Alcotest.(check int) "requested page two" 2 !calls;
      check_state store user (Some 100L) 80.0)

let test_account_identity_checked_with_missing_watermark () =
  with_setup [] (fun store user connection _ ->
      store_batch store user connection (Some 100L) 80.0;
      let connection =
        Test_support.connection_with_cursor connection (Some 100L)
      in
      let request =
        Withings.
          {
            post_form =
              (fun ~uri:_ ~fields:_ ->
                ignore
                  (Option.value_exn
                     (Result.ok
                        (Test_support.ensure_withings_connection store ~user
                           ~withings_user_id:"replacement")));
                Ok (batch_json None [ measurement ~value:70000L "one" ]));
          }
      in
      Alcotest.(check bool)
        "changed account rejected" true
        (Result.is_error (sync_request store user connection request));
      check_state store user None 80.0)

let test_import_requires_account_and_group () =
  List.iter
    ~f:(fun external_id ->
      Alcotest.(check bool)
        "incomplete imported identity rejected" true
        (Result.is_error
           (Weigh_in.Import.of_raw
              Weigh_in.{ external_id; measured_at = now; weight_kg = 80.0 })))
    [ "orphan"; ":group"; "account:" ]

let draw_observations tc step =
  let count = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:8 ()) in
  List.init count ~f:(fun group ->
      let value =
        Int64.of_int (Hegel.draw tc (Hegel.integers ~min_value:1 ()))
      in
      measurement ~value
        ~date:(Int64.of_int (1_700_000_000 + (step * 100) + group))
        (Int.to_string group))
  |> List.concat_map ~f:(fun observation ->
      if Hegel.draw tc (Hegel.booleans ()) then [ observation; observation ]
      else [ observation ])

let draw_partition tc observations =
  let rec partition observations =
    match observations with
    | [] -> [ [] ]
    | _ ->
        let count =
          Hegel.draw tc
            (Hegel.integers ~min_value:1 ~max_value:(List.length observations)
               ())
        in
        let page, rest = List.split_n observations count in
        if List.is_empty rest then [ page ] else page :: partition rest
  in
  partition observations

let request_pages ~cursor ~baseline ?fail_at pages =
  let calls = ref 0 in
  let request =
    Withings.
      {
        post_form =
          (fun ~uri:_ ~fields ->
            let index =
              match List.Assoc.find fields ~equal:String.equal "offset" with
              | None -> 0
              | Some value -> Int.of_string value / 7
            in
            Alcotest.(check int) "continuation order" !calls index;
            Alcotest.(check (option string))
              "fetch baseline unchanged"
              (Option.map ~f:Int64.to_string baseline)
              (List.Assoc.find fields ~equal:String.equal "lastupdate");
            Int.incr calls;
            if Option.equal Int.equal fail_at (Some index) then
              Error (Error.Withings_failure (Http_status 502))
            else
              Ok
                (batch_json
                   ~more:(`Int (if index + 1 = List.length pages then 0 else 1))
                   ~offset:(`Int ((index + 1) * 7))
                   cursor (List.nth_exn pages index)));
      }
  in
  (request, calls)

let visible_map store user =
  Option.value_exn
    (Result.ok
       (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None ~limit:100))
  |> List.fold
       ~f:(fun rows (row : Weigh_in.t) ->
         Map.set rows
           ~key:(Option.value_exn row.external_id)
           ~data:(row.weight_kg, row.measured_at))
       ~init:(Map.empty (module String))

let%hegel_test page_partition_equivalence tc =
  let observations = draw_observations tc 0 in
  let pages = draw_partition tc observations in
  let cursor = Some (Int64.of_int (Hegel.draw tc (Hegel.integers ()))) in
  with_setup [] (fun store user connection _ ->
      let request, calls = request_pages ~cursor ~baseline:None pages in
      Option.value_exn (Result.ok (sync_request store user connection request));
      Alcotest.(check int)
        "all generated pages fetched" (List.length pages) !calls;
      let partitioned = visible_map store user in
      with_setup [] (fun whole_store whole_user whole_connection _ ->
          let request, _ =
            request_pages ~cursor ~baseline:None [ observations ]
          in
          Option.value_exn
            (Result.ok
               (sync_request whole_store whole_user whole_connection request));
          let whole = visible_map whole_store whole_user in
          assert (
            Map.equal
              (fun (a, at) (b, bt) -> Float.equal a b && Ptime.equal at bt)
              whole partitioned);
          assert (
            Option.equal Int64.equal (sync_cursor store)
              (sync_cursor whole_store))))

let%hegel_test paginated_failure_is_atomic tc =
  let observations = draw_observations tc 0 in
  let pages = draw_partition tc observations in
  let fail_at =
    Hegel.draw tc
      (Hegel.integers ~min_value:0 ~max_value:(List.length pages - 1) ())
  in
  with_setup [] (fun store user connection _ ->
      store_batch store user connection (Some 100L) 80.0;
      let connection =
        Test_support.connection_with_cursor connection (Some 100L)
      in
      let before = visible_map store user in
      let request, calls =
        request_pages ~cursor:(Some 101L) ~baseline:(Some 100L) ~fail_at pages
      in
      assert (Result.is_error (sync_request store user connection request));
      Alcotest.(check int) "requests stop at failing page" (fail_at + 1) !calls;
      assert (
        Map.equal
          (fun (a, at) (b, bt) -> Float.equal a b && Ptime.equal at bt)
          before (visible_map store user));
      Alcotest.(check (option int64))
        "cursor unchanged" (Some 100L) (sync_cursor store))

(* The model is an observation map plus revision (-1 means no watermark).
   Unversioned batches belong to a fetch snapshot; versioned batches are ordered
   by revision, with corrections permitted at the same revision. *)
let%hegel_test sync_state_matches_map_and_cursor_model tc =
  let steps = Hegel.draw tc (Hegel.integers ~min_value:1 ~max_value:20 ()) in
  with_setup [] (fun store user connection _ ->
      let model = ref (Map.empty (module Base.String)) in
      let revision = ref (-1) in
      for step = 0 to steps - 1 do
        let observations = draw_observations tc step in
        let baseline_revision =
          if Hegel.draw tc (Hegel.booleans ()) then !revision else -1
        in
        let baseline =
          if baseline_revision = -1 then None
          else Some (Int64.of_int baseline_revision)
        in
        let received =
          if Hegel.draw tc (Hegel.booleans ()) then None
          else
            Some (Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:10 ()))
        in
        let cursor = Option.map ~f:Int64.of_int received in
        let request, _ =
          request_pages ~cursor ~baseline (draw_partition tc observations)
        in
        Option.value_exn
          (Result.ok
             (sync_request store user
                (Test_support.connection_with_cursor connection baseline)
                request));
        let accepted =
          match received with
          | None -> baseline_revision = !revision
          | Some next_revision -> next_revision >= !revision
        in
        if accepted then (
          List.iter
            ~f:(fun (m : Withings.measurement) ->
              let id = "upstream-alice:" ^ m.group_id in
              let at =
                match Map.find !model id with
                | Some (_, at) -> at
                | None ->
                    Option.value_exn
                      (Ptime.of_float_s (Int64.to_float m.measured_at))
              in
              model :=
                Map.set !model ~key:id
                  ~data:(Int64.to_float m.value /. 1000., at))
            observations;
          Option.iter ~f:(fun next -> revision := next) received);
        let actual = visible_map store user in
        assert (
          Map.equal
            (fun (expected, at) (weight, bt) ->
              (* Independent division versus multiplication by 10^-3 can
                 round differently; allow four floating-point roundoffs. *)
              Float.(
                abs (expected -. weight) <= 4. *. epsilon_float *. abs expected)
              && Ptime.equal at bt)
            !model actual);
        let expected_cursor =
          if !revision = -1 then None else Some (Int64.of_int !revision)
        in
        assert (Option.equal Int64.equal expected_cursor (sync_cursor store))
      done)

type lifecycle_link = {
  identity : string;
  connection_id : string;
  access : string;
  refresh : string;
  cursor : int64 option;
}

type lifecycle_observation = {
  weight : float;
  deleted : bool;
  observation_id : string;
  measured : string;
  created : string;
  linkage : string option;
}

type lifecycle_model = {
  link : lifecycle_link option;
  observations :
    (string, lifecycle_observation, String.comparator_witness) Map.t;
  snapshots : Withings_connection.connected list;
  sequence : int;
}

let%hegel_test connection_import_lifecycle_model tc =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let raw = Store_sqlite.For_testing.raw_database store in
      let credentials identity access refresh =
        Withings.
          {
            withings_user_id = identity;
            access_token = access;
            refresh_token = refresh;
            expires_at = now;
          }
      in
      let current identity =
        snd
          (Option.value_exn
             (Result.ok
                (Store_sqlite.withings_connection_for_identity store
                   ~withings_user_id:identity)))
      in
      let history () =
        let statement =
          Sqlite3.prepare raw
            "SELECT \
             external_id,id,weight_kg,deleted_at,measured_at,created_at,withings_connection_id \
             FROM weigh_ins ORDER BY external_id"
        in
        Stdlib.Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize statement))
          (fun () ->
            let rec rows acc =
              match Sqlite3.step statement with
              | Sqlite3.Rc.DONE -> acc
              | Sqlite3.Rc.ROW ->
                  let external_id = Sqlite3.column_text statement 0 in
                  let row =
                    {
                      weight = Sqlite3.column_double statement 2;
                      deleted = not (Sqlite3.column_is_null statement 3);
                      observation_id = Sqlite3.column_text statement 1;
                      measured = Sqlite3.column_text statement 4;
                      created = Sqlite3.column_text statement 5;
                      linkage =
                        (if Sqlite3.column_is_null statement 6 then None
                         else Some (Sqlite3.column_text statement 6));
                    }
                  in
                  rows (Map.set acc ~key:external_id ~data:row)
              | _ -> failwith "history query failed"
            in
            rows (Map.empty (module Base.String)))
      in
      let invariant model =
        (match
           ( model.link,
             Option.value_exn
               (Result.ok (Service.withings_status service ~user)) )
         with
        | None, Withings_connection.Disconnected -> ()
        | Some expected, Connected connection ->
            assert (
              String.equal expected.identity
                (Withings_connection.Account.to_string connection.account));
            assert (String.equal expected.connection_id connection.id);
            assert (
              Option.equal Int64.equal expected.cursor connection.sync_cursor);
            let saved =
              Option.value_exn
                (Result.ok
                   (Store_sqlite.withings_credentials store ~user ~connection
                      ~key))
            in
            assert (String.equal saved.access_token expected.access);
            assert (String.equal saved.refresh_token expected.refresh)
        | _ -> failwith "connection differs from independent model");
        let actual = history () in
        assert (Map.length actual = Map.length model.observations);
        Map.iteri model.observations ~f:(fun ~key:external_id ~data:expected ->
            let found = Map.find_exn actual external_id in
            assert (Float.equal expected.weight found.weight);
            assert (Bool.equal expected.deleted found.deleted);
            assert (String.equal expected.observation_id found.observation_id);
            assert (String.equal expected.measured found.measured);
            assert (String.equal expected.created found.created);
            assert (Option.equal String.equal expected.linkage found.linkage));
        let statement = Sqlite3.prepare raw "PRAGMA foreign_key_check" in
        Stdlib.Fun.protect
          ~finally:(fun () -> ignore (Sqlite3.finalize statement))
          (fun () ->
            assert (
              match Sqlite3.step statement with
              | Sqlite3.Rc.DONE -> true
              | _ -> false))
      in
      let rule ~name ~step =
        Hegel.Stateful.Rule.create ~name
          ~step:(fun tc state -> state := step tc !state)
          ()
      in
      let connect =
        rule ~name:"connect_or_replace_account" ~step:(fun tc model ->
            let identity =
              if Hegel.draw tc (Hegel.booleans ()) then "a" else "b"
            in
            let access = "access-" ^ Int.to_string model.sequence in
            let refresh = "refresh-" ^ Int.to_string model.sequence in
            let connection =
              Option.value_exn
                (Result.ok
                   (Service.connect_withings service ~user ~key
                      (credentials identity access refresh)))
            in
            let cursor =
              match model.link with
              | Some old when String.equal old.identity identity -> old.cursor
              | _ -> None
            in
            {
              model with
              link =
                Some
                  {
                    identity;
                    access;
                    refresh;
                    cursor;
                    connection_id = connection.id;
                  };
              snapshots = connection :: model.snapshots;
              sequence = model.sequence + 1;
            })
      in
      let failed_relink =
        rule ~name:"failed_relink_is_atomic" ~step:(fun _tc model ->
            Option.value_exn
              (Result.ok
                 (Migration.For_testing.execute raw
                    "CREATE TRIGGER fail_relink BEFORE UPDATE ON \
                     withings_connections BEGIN SELECT \
                     RAISE(ABORT,'injected'); END; CREATE TRIGGER \
                     fail_first_link BEFORE INSERT ON withings_connections \
                     BEGIN SELECT RAISE(ABORT,'injected'); END"));
            Stdlib.Fun.protect
              ~finally:(fun () ->
                Option.value_exn
                  (Result.ok
                     (Migration.For_testing.execute raw
                        "DROP TRIGGER fail_relink; DROP TRIGGER fail_first_link")))
              (fun () ->
                assert (
                  Result.is_error
                    (Service.connect_withings service ~user ~key
                       (credentials "failed" "bad" "bad"))));
            model)
      in
      let disconnect =
        rule ~name:"disconnect_preserves_history" ~step:(fun _tc model ->
            Option.value_exn
              (Result.ok (Service.disconnect_withings service ~user));
            {
              model with
              link = None;
              observations =
                Map.map model.observations ~f:(fun row ->
                    { row with linkage = None });
            })
      in
      let import =
        rule ~name:"import_and_tombstone_corrections" ~step:(fun tc model ->
            Hegel.assume tc (Option.is_some model.link);
            let link = Option.value_exn model.link in
            let connection = current link.identity in
            let group =
              Int.to_string
                (Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:3 ()))
            in
            let external_id = link.identity ^ ":" ^ group in
            let weight =
              Float.of_int
                (Hegel.draw tc (Hegel.integers ~min_value:1 ~max_value:200 ()))
            in
            let cursor =
              Hegel.draw tc
                (Hegel.optional (Hegel.integers ~min_value:0 ~max_value:10 ()))
              |> Option.map ~f:Int64.of_int
            in
            Option.value_exn
              (Result.ok
                 (Test_support.persist_withings_import_raw store ~user
                    ~connection
                    ~rows:
                      [
                        Weigh_in.
                          { external_id; measured_at = now; weight_kg = weight };
                      ]
                    ~cursor));
            let accepted =
              match (cursor, link.cursor) with
              | Some received, Some stored -> Int64.compare received stored >= 0
              | _ -> true
            in
            if not accepted then model
            else
              let observations =
                match Map.find model.observations external_id with
                | Some old when old.deleted -> model.observations
                | Some old ->
                    Map.set model.observations ~key:external_id
                      ~data:
                        { old with weight; linkage = Some link.connection_id }
                | None ->
                    let observed = Map.find_exn (history ()) external_id in
                    (* IDs and creation time are generated by SQLite's caller; the model
                   captures them once, then requires stability across transitions. *)
                    Map.set model.observations ~key:external_id
                      ~data:
                        {
                          observed with
                          weight;
                          deleted = false;
                          measured = Time.to_utc_string now;
                          linkage = Some link.connection_id;
                        }
              in
              {
                model with
                observations;
                link =
                  Some
                    {
                      link with
                      cursor =
                        (match cursor with
                        | None -> link.cursor
                        | Some _ -> cursor);
                    };
              })
      in
      let delete =
        rule ~name:"delete_import_leaves_tombstone" ~step:(fun tc model ->
            let visible =
              Map.to_alist model.observations
              |> List.filter ~f:(fun (_, row) -> not row.deleted)
            in
            Hegel.assume tc (not (List.is_empty visible));
            let external_id, row =
              Hegel.draw_silent tc (Hegel.sampled_from visible)
            in
            let id =
              Option.value_exn (Weigh_in_id.of_string row.observation_id)
            in
            Option.value_exn
              (Result.ok (Store_sqlite.delete_weigh_in store ~user id));
            {
              model with
              observations =
                Map.set model.observations ~key:external_id
                  ~data:{ row with deleted = true };
            })
      in
      let refresh =
        rule ~name:"refresh_expected_account_snapshot" ~step:(fun tc model ->
            Hegel.assume tc (not (List.is_empty model.snapshots));
            let connection =
              Hegel.draw_silent tc (Hegel.sampled_from model.snapshots)
            in
            let identity =
              Withings_connection.Account.to_string connection.account
            in
            let access = "refreshed-" ^ Int.to_string model.sequence in
            let refresh = "rotated-" ^ Int.to_string model.sequence in
            let result =
              Store_sqlite.refresh_withings_credentials store ~user ~connection
                ~key
                (credentials identity access refresh)
            in
            match model.link with
            | Some link
              when String.equal link.identity identity
                   && String.equal link.connection_id connection.id ->
                ignore (Option.value_exn (Result.ok result));
                {
                  model with
                  link = Some { link with access; refresh };
                  sequence = model.sequence + 1;
                }
            | _ ->
                assert (Result.is_error result);
                model)
      in
      let module Machine = struct
        type state = lifecycle_model ref

        let rules =
          [ connect; failed_relink; disconnect; import; delete; refresh ]

        let invariants =
          [
            Hegel.Stateful.Invariant.create
              ~name:"ledger_and_connection_match_model" ~always_check:true
              ~inv:(fun _tc state -> invariant !state)
              ();
          ]
      end in
      Hegel.Stateful.run tc
        (module Machine)
        ~step_count:30
        ~init:
          (ref
             {
               link = None;
               observations = Map.empty (module Base.String);
               snapshots = [];
               sequence = 0;
             }))
[@@settings Hegel.Settings.create ~test_cases:1000 ()]

let () =
  Alcotest.run "withings sync"
    [
      ( "properties",
        [
          Alcotest.test_case "connection/import lifecycle state machine" `Quick
            connection_import_lifecycle_model;
          Alcotest.test_case "page partition equivalence" `Quick
            page_partition_equivalence;
          Alcotest.test_case "page failure atomicity" `Quick
            paginated_failure_is_atomic;
          Alcotest.test_case "independent map/cursor sync model" `Quick
            sync_state_matches_map_and_cursor_model;
        ] );
      ( "sync",
        [
          Alcotest.test_case "refresh race with account replacement" `Quick
            test_refresh_race_with_account_replacement;
          Alcotest.test_case "import requires account and group" `Quick
            test_import_requires_account_and_group;
          Alcotest.test_case "raw missing watermark concurrent cursor" `Quick
            test_missing_watermark_race;
          Alcotest.test_case "missing and equal watermark corrections" `Quick
            test_missing_watermark_baseline_and_equal_correction;
          Alcotest.test_case "page two failure retains persisted state" `Quick
            test_page_two_failure_does_not_persist;
          Alcotest.test_case "account identity with missing watermark" `Quick
            test_account_identity_checked_with_missing_watermark;
          Alcotest.test_case "full sync rejects stale watermark" `Quick
            test_full_sync_rejects_stale_watermark;
          Alcotest.test_case "import does not repair corrupt weight" `Quick
            test_import_does_not_repair_corrupt_weight;
          Alcotest.test_case "normalization, duplicate and cursor" `Quick
            test_normalizes_deduplicates_and_advances_cursor;
          Alcotest.test_case "update and tombstone" `Quick
            test_upstream_change_preserves_timestamp_and_tombstone;
          Alcotest.test_case "relink different account resets cursor" `Quick
            test_relink_different_account_resets_cursor;
          Alcotest.test_case "reconnect same account updates import" `Quick
            test_reconnect_same_account_updates_import;
          Alcotest.test_case "failed transaction retains cursor" `Quick
            test_failed_transaction_retains_cursor;
          Alcotest.test_case "stale batch cannot regress" `Quick
            test_stale_batch_cannot_regress_value_or_cursor;
          Alcotest.test_case "permanent refresh failure" `Quick
            test_refresh_failure_requires_reauthorization;
          Alcotest.test_case "transient refresh failure" `Quick
            test_transient_refresh_failure_does_not_require_reauthorization;
        ] );
    ]
