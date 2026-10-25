let now = Option.get (Ptime.of_float_s 1_800_000_000.)
let key = Bytes.make 32 'k'

let measurement ?(value = 80000L) ?(date = 1_700_000_000L) group_id =
  Withings.{ group_id; measured_at = date; measure_type = 1; value; unit_ = -3 }

let with_setup measurements f =
  Test_support.with_store (fun store user ->
      ignore
        (Result.get_ok
           (Store_sqlite.create_withings_connection store ~user
              ~withings_user_id:"upstream-alice"));
      let credentials =
        Withings.
          {
            access_token = "access";
            refresh_token = "refresh";
            expires_at =
              Option.get (Ptime.add_span now (Ptime.Span.of_int_s 3600));
            withings_user_id = "upstream-alice";
          }
      in
      let connection =
        Result.get_ok
          (Store_sqlite.save_withings_credentials store ~user ~key credentials)
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
    Sqlite3.prepare store "SELECT sync_cursor FROM withings_connections"
  in
  Fun.protect
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
      Result.get_ok
        (Withings_sync.sync
           (Withings_sync.make ~store ~client ~token_key:key ~now:(fun () ->
                now))
           ~user ~connection);
      let rows =
        Result.get_ok
          (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
             ~limit:10)
      in
      Alcotest.(check int) "deduplicated" 2 (List.length rows);
      Alcotest.(check (float 0.0001))
        "kg normalization" 80.0 (List.hd rows).weight_kg;
      Alcotest.(check string)
        "imported provenance" "withings"
        (Weigh_in.source_to_string (List.hd rows).source);
      (match (List.hd rows).source with
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
      Result.get_ok (sync [ measurement "one" ]);
      let original =
        List.hd
          (Result.get_ok
             (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                ~limit:10))
      in
      Result.get_ok
        (sync [ measurement ~value:81000L ~date:1_700_000_100L "one" ]);
      let changed =
        List.hd
          (Result.get_ok
             (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                ~limit:10))
      in
      Alcotest.(check (float 0.0001))
        "upstream value updated" 81.0 changed.weight_kg;
      Alcotest.(check string)
        "timestamp preserved"
        (Time.to_utc_string original.measured_at)
        (Time.to_utc_string changed.measured_at);
      Result.get_ok (Store_sqlite.delete_weigh_in store ~user changed.id);
      Result.get_ok (sync [ measurement ~value:82000L "one" ]);
      Alcotest.(check int)
        "tombstone retained" 0
        (List.length
           (Result.get_ok
              (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                 ~limit:10))))

let test_relink_different_account_resets_cursor () =
  with_setup [] (fun store user connection _ ->
      Result.get_ok
        (Test_support.persist_withings_import_raw store ~user ~connection
           ~rows:[] ~cursor:(Some 100L));
      ignore
        (Result.get_ok
           (Store_sqlite.ensure_withings_connection store ~user
              ~withings_user_id:"upstream-alice"));
      Alcotest.(check (option int64))
        "same account retains cursor" (Some 100L) (sync_cursor store);
      ignore
        (Result.get_ok
           (Store_sqlite.ensure_withings_connection store ~user
              ~withings_user_id:"upstream-bob"));
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
      Result.get_ok
        (Test_support.persist_withings_import_raw store ~user ~connection
           ~rows:[ original ] ~cursor:(Some 100L));
      Result.get_ok (Store_sqlite.delete_withings_connection store ~user);
      let replacement =
        Result.get_ok
          (Store_sqlite.ensure_withings_connection store ~user
             ~withings_user_id:"upstream-alice")
      in
      let corrected = Weigh_in.{ original with weight_kg = 81.0 } in
      Result.get_ok
        (Test_support.persist_withings_import_raw store ~user
           ~connection:replacement ~rows:[ corrected ] ~cursor:(Some 101L));
      let rows =
        Result.get_ok
          (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
             ~limit:10)
      in
      Alcotest.(check int) "no duplicate" 1 (List.length rows);
      Alcotest.(check (float 0.0001))
        "corrected weight" 81.0 (List.hd rows).weight_kg;
      let statement =
        Sqlite3.prepare store
          "SELECT withings_connection_id FROM weigh_ins WHERE external_id = \
           'upstream-alice:one'"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize statement))
        (fun () ->
          Alcotest.(check bool)
            "import exists" true
            (Sqlite3.step statement = Sqlite3.Rc.ROW);
          Alcotest.(check string)
            "new connection owns import" replacement.id
            (Sqlite3.column_text statement 0)))

let test_failed_transaction_retains_cursor () =
  with_setup [] (fun store user connection _ ->
      ignore
        (Sqlite3.exec store
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
      Result.get_ok
        (Test_support.persist_withings_import_raw store ~user ~connection
           ~rows:[ current ] ~cursor:(Some 100L));
      let stale = Weigh_in.{ current with weight_kg = 70.0 } in
      Result.get_ok
        (Test_support.persist_withings_import_raw store ~user ~connection
           ~rows:[ stale ] ~cursor:(Some 99L));
      let weight =
        List.hd
          (Result.get_ok
             (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                ~limit:10))
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
                Ptime.add_span now (Ptime.Span.of_int_s 7200) |> Option.get))
           ~user ~connection);
      let (Withings_connection.Connected status) =
        Result.get_ok (Store_sqlite.get_withings_status store ~user)
      in
      Alcotest.(check bool)
        "reauthorization" true status.requires_reauthorization)

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
                   Ptime.add_span now (Ptime.Span.of_int_s 7200) |> Option.get))
              ~user ~connection));
      let (Withings_connection.Connected status) =
        Result.get_ok (Store_sqlite.get_withings_status store ~user)
      in
      Alcotest.(check bool)
        "transient failure retains authorization" false
        status.requires_reauthorization)

let test_import_does_not_repair_corrupt_weight () =
  with_setup
    [ measurement "one" ]
    (fun store user connection client ->
      let sync =
        Withings_sync.make ~store ~client ~token_key:key ~now:(fun () -> now)
      in
      Result.get_ok (Withings_sync.sync sync ~user ~connection);
      Alcotest.(check bool)
        "inject infinity" true
        (Sqlite3.exec store "UPDATE weigh_ins SET weight_kg = 1e309"
        = Sqlite3.Rc.OK);
      Alcotest.(check bool)
        "corrupt correction rejected" true
        (match Withings_sync.sync sync ~user ~connection with
        | Error (Error.Storage_decoding _) -> true
        | _ -> false);
      let weight = ref 0.0 in
      ignore
        (Sqlite3.exec store
           ~cb:(fun row _ -> weight := float_of_string (Option.get row.(0)))
           "SELECT weight_kg FROM weigh_ins");
      Alcotest.(check bool)
        "original corrupt value untouched" true
        (Float.is_infinite !weight);
      Alcotest.(check (option int64))
        "cursor unchanged" (Some 77L) (sync_cursor store))

let test_full_sync_rejects_stale_watermark () =
  with_setup [] (fun store user connection _ ->
      Result.get_ok
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
           ~cursor:(Some 100L));
      let _, connection =
        List.hd (Result.get_ok (Store_sqlite.withings_connections store))
      in
      let request =
        Withings.
          {
            post_form =
              (fun ~uri:_ ~fields ->
                Alcotest.(check (option string))
                  "fetch uses stored cursor" (Some "100")
                  (List.assoc_opt "lastupdate" fields);
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
      Result.get_ok
        (Withings_sync.sync
           (Withings_sync.make ~store ~client ~token_key:key ~now:(fun () ->
                now))
           ~user ~connection);
      let row =
        List.hd
          (Result.get_ok
             (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                ~limit:10))
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
                      (fun (m : Withings.measurement) ->
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
  |> Result.get_ok

let check_state store user cursor weight =
  let rows =
    Result.get_ok
      (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None ~limit:10)
  in
  Alcotest.(check (float 0.0001))
    "stored weight" weight (List.hd rows).weight_kg;
  Alcotest.(check (option int64)) "stored cursor" cursor (sync_cursor store)

let test_missing_watermark_race () =
  with_setup [] (fun store user connection _ ->
      store_batch store user connection (Some 100L) 80.0;
      let connection = { connection with sync_cursor = Some 100L } in
      let request =
        Withings.
          {
            post_form =
              (fun ~uri:_ ~fields:_ ->
                store_batch store user connection (Some 101L) 81.0;
                Ok (batch_json None [ measurement ~value:70000L "one" ]));
          }
      in
      Result.get_ok (sync_request store user connection request);
      check_state store user (Some 101L) 81.0)

let test_missing_watermark_baseline_and_equal_correction () =
  with_setup [] (fun store user connection _ ->
      store_batch store user connection (Some 100L) 80.0;
      let connection = { connection with sync_cursor = Some 100L } in
      List.iter
        (fun (cursor, value, weight) ->
          let request =
            Withings.
              {
                post_form =
                  (fun ~uri:_ ~fields:_ ->
                    Ok (batch_json cursor [ measurement ~value "one" ]));
              }
          in
          Result.get_ok (sync_request store user connection request);
          check_state store user (Some 100L) weight)
        [ (None, 81000L, 81.0); (Some 100L, 82000L, 82.0) ])

let test_page_two_failure_does_not_persist () =
  with_setup [] (fun store user connection _ ->
      store_batch store user connection (Some 100L) 80.0;
      let connection = { connection with sync_cursor = Some 100L } in
      let calls = ref 0 in
      let request =
        Withings.
          {
            post_form =
              (fun ~uri:_ ~fields:_ ->
                incr calls;
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
      let connection = { connection with sync_cursor = Some 100L } in
      let request =
        Withings.
          {
            post_form =
              (fun ~uri:_ ~fields:_ ->
                ignore
                  (Result.get_ok
                     (Store_sqlite.ensure_withings_connection store ~user
                        ~withings_user_id:"replacement"));
                Ok (batch_json None [ measurement ~value:70000L "one" ]));
          }
      in
      Alcotest.(check bool)
        "changed account rejected" true
        (Result.is_error (sync_request store user connection request));
      check_state store user None 80.0)

let test_import_requires_account_and_group () =
  with_setup [] (fun _store _user connection _ ->
      List.iter
        (fun withings_user_id ->
          Alcotest.(check bool)
            "connected identity required" true
            (Result.is_error
               (Withings_connection.connected_account
                  { connection with withings_user_id })))
        [ None; Some "" ]);
  List.iter
    (fun external_id ->
      Alcotest.(check bool)
        "incomplete imported identity rejected" true
        (Result.is_error
           (Weigh_in.Import.of_raw
              Weigh_in.{ external_id; measured_at = now; weight_kg = 80.0 })))
    [ "orphan"; ":group"; "account:" ]

let draw_observations tc step =
  let count = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:8 ()) in
  List.init count (fun group ->
      let value =
        Int64.of_int (Hegel.draw tc (Hegel.integers ~min_value:1 ()))
      in
      measurement ~value
        ~date:(Int64.of_int (1_700_000_000 + (step * 100) + group))
        (string_of_int group))
  |> List.concat_map (fun observation ->
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
        let page, rest = Base.List.split_n observations count in
        if Base.List.is_empty rest then [ page ] else page :: partition rest
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
              match List.assoc_opt "offset" fields with
              | None -> 0
              | Some value -> int_of_string value / 7
            in
            Alcotest.(check int) "continuation order" !calls index;
            Alcotest.(check (option string))
              "fetch baseline unchanged"
              (Option.map Int64.to_string baseline)
              (List.assoc_opt "lastupdate" fields);
            incr calls;
            if Option.equal Int.equal fail_at (Some index) then
              Error (Error.Withings_failure (Http_status 502))
            else
              Ok
                (batch_json
                   ~more:(`Int (if index + 1 = List.length pages then 0 else 1))
                   ~offset:(`Int ((index + 1) * 7))
                   cursor (List.nth pages index)));
      }
  in
  (request, calls)

let visible_map store user =
  Result.get_ok
    (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None ~limit:100)
  |> List.fold_left
       (fun rows (row : Weigh_in.t) ->
         Base.Map.set rows
           ~key:(Option.get row.external_id)
           ~data:(row.weight_kg, row.measured_at))
       (Base.Map.empty (module Base.String))

let%hegel_test page_partition_equivalence tc =
  let observations = draw_observations tc 0 in
  let pages = draw_partition tc observations in
  let cursor = Some (Int64.of_int (Hegel.draw tc (Hegel.integers ()))) in
  with_setup [] (fun store user connection _ ->
      let request, calls = request_pages ~cursor ~baseline:None pages in
      Result.get_ok (sync_request store user connection request);
      Alcotest.(check int)
        "all generated pages fetched" (List.length pages) !calls;
      let partitioned = visible_map store user in
      with_setup [] (fun whole_store whole_user whole_connection _ ->
          let request, _ =
            request_pages ~cursor ~baseline:None [ observations ]
          in
          Result.get_ok
            (sync_request whole_store whole_user whole_connection request);
          let whole = visible_map whole_store whole_user in
          assert (
            Base.Map.equal
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
      let connection = { connection with sync_cursor = Some 100L } in
      let before = visible_map store user in
      let request, calls =
        request_pages ~cursor:(Some 101L) ~baseline:(Some 100L) ~fail_at pages
      in
      assert (Result.is_error (sync_request store user connection request));
      Alcotest.(check int) "requests stop at failing page" (fail_at + 1) !calls;
      assert (
        Base.Map.equal
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
      let model = ref (Base.Map.empty (module Base.String)) in
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
        let cursor = Option.map Int64.of_int received in
        let request, _ =
          request_pages ~cursor ~baseline (draw_partition tc observations)
        in
        Result.get_ok
          (sync_request store user
             { connection with sync_cursor = baseline }
             request);
        let accepted =
          match received with
          | None -> baseline_revision = !revision
          | Some next_revision -> next_revision >= !revision
        in
        if accepted then (
          List.iter
            (fun (m : Withings.measurement) ->
              let id = "upstream-alice:" ^ m.group_id in
              let at =
                match Base.Map.find !model id with
                | Some (_, at) -> at
                | None ->
                    Option.get (Ptime.of_float_s (Int64.to_float m.measured_at))
              in
              model :=
                Base.Map.set !model ~key:id
                  ~data:(Int64.to_float m.value /. 1000., at))
            observations;
          Option.iter (fun next -> revision := next) received);
        let actual = visible_map store user in
        assert (
          Base.Map.equal
            (fun (expected, at) (weight, bt) ->
              (* Independent division versus multiplication by 10^-3 can
                 round differently; allow four floating-point roundoffs. *)
              Float.abs (expected -. weight)
              <= 4. *. Float.epsilon *. Float.abs expected
              && Ptime.equal at bt)
            !model actual);
        let expected_cursor =
          if !revision = -1 then None else Some (Int64.of_int !revision)
        in
        assert (Option.equal Int64.equal expected_cursor (sync_cursor store))
      done)

let () =
  Alcotest.run "withings sync"
    [
      ( "properties",
        [
          Alcotest.test_case "page partition equivalence" `Quick
            page_partition_equivalence;
          Alcotest.test_case "page failure atomicity" `Quick
            paginated_failure_is_atomic;
          Alcotest.test_case "independent map/cursor sync model" `Quick
            sync_state_matches_map_and_cursor_model;
        ] );
      ( "sync",
        [
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
