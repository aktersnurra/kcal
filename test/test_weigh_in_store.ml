module Domain_error = Error
open Base
module Error = Domain_error

let test_recorded_weight_is_manual () =
  Test_support.with_store (fun store user ->
      let weight =
        Option.value_exn
          (Result.ok
             (Test_support.create_manual_weigh_in_raw store ~user
                Weigh_in.{ measured_at = None; weight_kg = 79.6 }))
      in
      Alcotest.(check string)
        "source" "manual"
        (Weigh_in.source_to_string weight.source))

let test_cannot_update_another_users_weight () =
  Test_support.with_two_users (fun store alice bob ->
      let weight = Test_support.create_weight store alice in
      Alcotest.(check bool)
        "hidden" true
        (match
           Test_support.update_manual_weigh_in_raw store ~user:bob weight.id
             Weigh_in.empty_patch
         with
        | Error Error.Not_found -> true
        | _ -> false))

let test_foreign_weight_operations_are_hidden () =
  Test_support.with_two_users (fun store alice bob ->
      let weight = Test_support.create_weight store alice in
      let hidden result =
        match result with Error Error.Not_found -> true | _ -> false
      in
      Alcotest.(check bool)
        "foreign get" true
        (hidden (Store_sqlite.get_weigh_in store ~user:bob weight.id));
      Alcotest.(check bool)
        "foreign update" true
        (hidden
           (Test_support.update_manual_weigh_in_raw store ~user:bob weight.id
              Weigh_in.empty_patch));
      Alcotest.(check bool)
        "foreign delete" true
        (hidden (Store_sqlite.delete_weigh_in store ~user:bob weight.id));
      Alcotest.(check int)
        "foreign query" 0
        (List.length
           (Option.value_exn
              (Result.ok
                 (Store_sqlite.query_weigh_ins store ~user:bob ~from:None
                    ~to_:None ~limit:100))));
      Alcotest.(check bool)
        "foreign latest weight" true
        (hidden (Store_sqlite.latest_weigh_in store ~user:bob)))

let test_weight_crud_and_delete () =
  Test_support.with_store (fun store user ->
      let weight = Test_support.create_weight store user in
      let updated =
        Option.value_exn
          (Result.ok
             (Test_support.update_manual_weigh_in_raw store ~user weight.id
                Weigh_in.{ empty_patch with weight_kg = Some 80.0 }))
      in
      Alcotest.(check (float 0.001)) "updated weight" 80.0 updated.weight_kg;
      Option.value_exn
        (Result.ok (Store_sqlite.delete_weigh_in store ~user weight.id));
      Option.value_exn
        (Result.ok (Store_sqlite.delete_weigh_in store ~user weight.id));
      Alcotest.(check bool)
        "deleted hidden" true
        (match Store_sqlite.get_weigh_in store ~user weight.id with
        | Error Error.Not_found -> true
        | _ -> false))

let test_partial_update_leaves_omitted_field_unchanged () =
  Test_support.with_store (fun store user ->
      let weight = Test_support.create_weight store user in
      let weight_only =
        Option.value_exn
          (Result.ok
             (Test_support.update_manual_weigh_in_raw store ~user weight.id
                Weigh_in.{ empty_patch with weight_kg = Some 80.0 }))
      in
      Alcotest.(check (float 0.0001))
        "weight updated" 80.0 weight_only.weight_kg;
      Alcotest.(check string)
        "measured_at untouched"
        (Time.to_utc_string weight.measured_at)
        (Time.to_utc_string weight_only.measured_at);
      let new_measured_at =
        Option.value_exn
          (Result.ok (Time.parse_offset_datetime "2026-01-01T00:00:00Z"))
      in
      let date_only =
        Option.value_exn
          (Result.ok
             (Test_support.update_manual_weigh_in_raw store ~user weight.id
                Weigh_in.{ empty_patch with measured_at = Some new_measured_at }))
      in
      Alcotest.(check string)
        "measured_at updated"
        (Time.to_utc_string new_measured_at)
        (Time.to_utc_string date_only.measured_at);
      Alcotest.(check (float 0.0001))
        "weight untouched by date-only patch" 80.0 date_only.weight_kg)

let test_locked_weight_get_and_update_are_storage_errors () =
  Test_support.with_exclusive_lock
    (fun store user ->
      (Test_support.create_weight store user, Weigh_in.empty_patch))
    (fun store user (weight, patch) ->
      let is_storage_error = function
        | Error (Error.Storage_error "SQLite operation failed") -> true
        | _ -> false
      in
      Alcotest.(check bool)
        "locked get is sanitized" true
        (is_storage_error (Store_sqlite.get_weigh_in store ~user weight.id));
      Alcotest.(check bool)
        "locked update is sanitized" true
        (is_storage_error
           (Test_support.update_manual_weigh_in_raw store ~user weight.id patch)))

let test_weight_queries_are_ordered_and_bounded () =
  Test_support.with_store (fun store user ->
      let later =
        Option.value_exn
          (Result.ok (Time.parse_offset_datetime "2026-01-02T10:00:00Z"))
      in
      let earlier =
        Option.value_exn
          (Result.ok (Time.parse_offset_datetime "2026-01-01T10:00:00Z"))
      in
      let second =
        Option.value_exn
          (Result.ok
             (Test_support.create_manual_weigh_in_raw store ~user
                Weigh_in.{ measured_at = Some later; weight_kg = 80.0 }))
      in
      let first =
        Option.value_exn
          (Result.ok
             (Test_support.create_manual_weigh_in_raw store ~user
                Weigh_in.{ measured_at = Some earlier; weight_kg = 79.0 }))
      in
      let weights =
        Option.value_exn
          (Result.ok
             (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
                ~limit:100))
      in
      Alcotest.(check string)
        "ascending"
        (Weigh_in_id.to_string first.id)
        (Weigh_in_id.to_string (List.hd_exn weights).id);
      Alcotest.(check bool)
        "limit rejected" true
        (match
           Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None
             ~limit:501
         with
        | Error (Error.Invalid_input _) -> true
        | _ -> false);
      ignore second)

let test_weigh_ins_between_is_half_open () =
  Test_support.with_store (fun store user ->
      let at value =
        Option.value_exn (Result.ok (Time.parse_offset_datetime value))
      in
      let record value weight_kg =
        Option.value_exn
          (Result.ok
             (Test_support.create_manual_weigh_in_raw store ~user
                Weigh_in.{ measured_at = Some (at value); weight_kg }))
      in
      ignore (record "2026-09-21T23:59:59Z" 81.0);
      let inside = record "2026-09-22T00:00:00Z" 80.0 in
      ignore (record "2026-09-23T00:00:00Z" 79.0);
      let weigh_ins =
        Option.value_exn
          (Result.ok
             (Store_sqlite.weigh_ins_between store ~user
                ~start:(at "2026-09-22T00:00:00Z")
                ~stop:(at "2026-09-23T00:00:00Z")))
      in
      Alcotest.(check (list string))
        "only [start, stop)"
        [ Weigh_in_id.to_string inside.id ]
        (List.map
           ~f:(fun (weigh_in : Weigh_in.t) -> Weigh_in_id.to_string weigh_in.id)
           weigh_ins))

let test_latest_weigh_in_returns_the_most_recent () =
  Test_support.with_store (fun store user ->
      let earlier =
        Option.value_exn
          (Result.ok (Time.parse_offset_datetime "2026-01-01T10:00:00Z"))
      in
      let later =
        Option.value_exn
          (Result.ok (Time.parse_offset_datetime "2026-01-02T10:00:00Z"))
      in
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.create_manual_weigh_in_raw store ~user
                 Weigh_in.{ measured_at = Some earlier; weight_kg = 79.0 })));
      let latest =
        Option.value_exn
          (Result.ok
             (Test_support.create_manual_weigh_in_raw store ~user
                Weigh_in.{ measured_at = Some later; weight_kg = 80.0 }))
      in
      let found =
        Option.value_exn (Result.ok (Store_sqlite.latest_weigh_in store ~user))
      in
      Alcotest.(check string)
        "most recent by measured_at"
        (Weigh_in_id.to_string latest.id)
        (Weigh_in_id.to_string found.id);
      Option.value_exn
        (Result.ok (Store_sqlite.delete_weigh_in store ~user latest.id));
      let after_delete =
        Option.value_exn (Result.ok (Store_sqlite.latest_weigh_in store ~user))
      in
      Alcotest.(check bool)
        "falls back once the latest is deleted" true
        (not (Weigh_in_id.equal after_delete.id latest.id)))

let test_latest_weigh_in_is_not_found_when_empty () =
  Test_support.with_store (fun store user ->
      Alcotest.(check bool)
        "not found" true
        (match Store_sqlite.latest_weigh_in store ~user with
        | Error Error.Not_found -> true
        | _ -> false))

let test_rejects_invalid_weight_update () =
  Test_support.with_store (fun store user ->
      let weight = Test_support.create_weight store user in
      Alcotest.(check bool)
        "invalid" true
        (match
           Test_support.update_manual_weigh_in_raw store ~user weight.id
             Weigh_in.{ empty_patch with weight_kg = Some 0.0 }
         with
        | Error
            (Error.Invalid_field
               { field = Numeric Weight_kg; reason = Nonpositive }) ->
            true
        | _ -> false))

let test_corrupt_weight_is_not_repaired () =
  Test_support.with_store (fun store user ->
      let weight = Test_support.create_weight store user in
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
        "query rejects corrupt weight" true
        (match Store_sqlite.latest_weigh_in store ~user with
        | Error (Error.Storage_decoding _) -> true
        | _ -> false);
      Alcotest.(check bool)
        "update rejects corrupt weight" true
        (match
           Test_support.update_manual_weigh_in_raw store ~user weight.id
             Weigh_in.{ empty_patch with weight_kg = Some 80.0 }
         with
        | Error (Error.Storage_decoding _) -> true
        | _ -> false))

let () =
  Alcotest.run "weigh-in store"
    [
      ( "persistence",
        [
          Alcotest.test_case "recorded weight is manual" `Quick
            test_recorded_weight_is_manual;
          Alcotest.test_case "corrupt weight is not repaired" `Quick
            test_corrupt_weight_is_not_repaired;
          Alcotest.test_case "users cannot update each other's weights" `Quick
            test_cannot_update_another_users_weight;
          Alcotest.test_case "foreign weight operations are hidden" `Quick
            test_foreign_weight_operations_are_hidden;
          Alcotest.test_case "weight CRUD and delete" `Quick
            test_weight_crud_and_delete;
          Alcotest.test_case "partial update leaves omitted field unchanged"
            `Quick test_partial_update_leaves_omitted_field_unchanged;
          Alcotest.test_case "locked weight get and update are sanitized" `Quick
            test_locked_weight_get_and_update_are_storage_errors;
          Alcotest.test_case "weight queries are ordered and bounded" `Quick
            test_weight_queries_are_ordered_and_bounded;
          Alcotest.test_case "weigh-ins between is half-open" `Quick
            test_weigh_ins_between_is_half_open;
          Alcotest.test_case "invalid weight update is rejected" `Quick
            test_rejects_invalid_weight_update;
          Alcotest.test_case "latest weigh-in returns the most recent" `Quick
            test_latest_weigh_in_returns_the_most_recent;
          Alcotest.test_case "latest weigh-in is not found when empty" `Quick
            test_latest_weigh_in_is_not_found_when_empty;
        ] );
    ]
