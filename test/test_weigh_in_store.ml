let test_recorded_weight_is_manual () =
  let store, user = Test_support.store_with_user () in
  let weight =
    Result.get_ok
      (Store_sqlite.create_manual_weigh_in store ~user
         Weigh_in.{ measured_at = None; weight_kg = 79.6 })
  in
  Alcotest.(check string) "source" "manual" (Weigh_in.source_to_string weight.source)

let test_cannot_update_another_users_weight () =
  let store, alice, bob = Test_support.store_with_two_users () in
  let weight = Test_support.create_weight store alice in
  Alcotest.(check bool) "hidden" true
    (match Store_sqlite.update_manual_weigh_in store ~user:bob weight.id Weigh_in.empty_patch with
    | Error Error.Not_found -> true
    | _ -> false)

let test_foreign_weight_operations_are_hidden () =
  let store, alice, bob = Test_support.store_with_two_users () in
  let weight = Test_support.create_weight store alice in
  let hidden result = match result with Error Error.Not_found -> true | _ -> false in
  Alcotest.(check bool) "foreign get" true (hidden (Store_sqlite.get_weigh_in store ~user:bob weight.id));
  Alcotest.(check bool) "foreign update" true
    (hidden (Store_sqlite.update_manual_weigh_in store ~user:bob weight.id Weigh_in.empty_patch));
  Alcotest.(check bool) "foreign delete" true (hidden (Store_sqlite.delete_weigh_in store ~user:bob weight.id));
  Alcotest.(check int) "foreign query" 0
    (List.length (Result.get_ok (Store_sqlite.query_weigh_ins store ~user:bob ~from:None ~to_:None ~limit:100)))

let test_weight_crud_and_delete () =
  let store, user = Test_support.store_with_user () in
  let weight = Test_support.create_weight store user in
  let updated =
    Result.get_ok
      (Store_sqlite.update_manual_weigh_in store ~user weight.id
         Weigh_in.{ empty_patch with weight_kg = Some 80.0 })
  in
  Alcotest.(check (float 0.001)) "updated weight" 80.0 updated.weight_kg;
  Result.get_ok (Store_sqlite.delete_weigh_in store ~user weight.id);
  Result.get_ok (Store_sqlite.delete_weigh_in store ~user weight.id);
  Alcotest.(check bool) "deleted hidden" true
    (match Store_sqlite.get_weigh_in store ~user weight.id with Error Error.Not_found -> true | _ -> false)

let test_weight_queries_are_ordered_and_bounded () =
  let store, user = Test_support.store_with_user () in
  let later = Result.get_ok (Time.parse_offset_datetime "2026-01-02T10:00:00Z") in
  let earlier = Result.get_ok (Time.parse_offset_datetime "2026-01-01T10:00:00Z") in
  let second = Result.get_ok (Store_sqlite.create_manual_weigh_in store ~user Weigh_in.{ measured_at = Some later; weight_kg = 80.0 }) in
  let first = Result.get_ok (Store_sqlite.create_manual_weigh_in store ~user Weigh_in.{ measured_at = Some earlier; weight_kg = 79.0 }) in
  let weights = Result.get_ok (Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None ~limit:100) in
  Alcotest.(check string) "ascending" (Weigh_in_id.to_string first.id) (Weigh_in_id.to_string (List.hd weights).id);
  Alcotest.(check bool) "limit rejected" true
    (match Store_sqlite.query_weigh_ins store ~user ~from:None ~to_:None ~limit:501 with Error (Error.Invalid_input _) -> true | _ -> false);
  ignore second

let test_rejects_invalid_weight_update () =
  let store, user = Test_support.store_with_user () in
  let weight = Test_support.create_weight store user in
  Alcotest.(check bool) "invalid" true
    (match Store_sqlite.update_manual_weigh_in store ~user weight.id Weigh_in.{ empty_patch with weight_kg = Some 0.0 } with Error (Error.Invalid_input _) -> true | _ -> false)

let () =
  Alcotest.run "weigh-in store"
    [
      ( "persistence",
        [
          Alcotest.test_case "recorded weight is manual" `Quick test_recorded_weight_is_manual;
          Alcotest.test_case "users cannot update each other's weights" `Quick test_cannot_update_another_users_weight;
          Alcotest.test_case "foreign weight operations are hidden" `Quick test_foreign_weight_operations_are_hidden;
          Alcotest.test_case "weight CRUD and delete" `Quick test_weight_crud_and_delete;
          Alcotest.test_case "weight queries are ordered and bounded" `Quick test_weight_queries_are_ordered_and_bounded;
          Alcotest.test_case "invalid weight update is rejected" `Quick test_rejects_invalid_weight_update;
        ] );
    ]
