let test_user_resolution_is_stable () =
  let store, first = Test_support.store_with_user () in
  let second =
    Result.get_ok
      (Store_sqlite.resolve_user store ~issuer:"https://issuer.example" ~subject:"alice")
  in
  Alcotest.(check string) "same user" (User_id.to_string first.id)
    (User_id.to_string second.id)

let test_cannot_read_another_users_meal () =
  let store, alice, bob = Test_support.store_with_two_users () in
  let meal = Test_support.create_meal store alice in
  Alcotest.(check bool) "hidden" true
    (match Store_sqlite.get_meal store ~user:bob meal.id with
    | Error Error.Not_found -> true
    | _ -> false)

let test_foreign_meal_operations_are_hidden () =
  let store, alice, bob = Test_support.store_with_two_users () in
  let meal = Test_support.create_meal store alice in
  let hidden result = match result with Error Error.Not_found -> true | _ -> false in
  Alcotest.(check bool) "foreign get" true (hidden (Store_sqlite.get_meal store ~user:bob meal.id));
  Alcotest.(check bool) "foreign update" true
    (hidden (Store_sqlite.update_meal store ~user:bob meal.id Meal.empty_patch));
  Alcotest.(check bool) "foreign delete" true (hidden (Store_sqlite.delete_meal store ~user:bob meal.id));
  Alcotest.(check int) "foreign query" 0
    (List.length (Result.get_ok (Store_sqlite.query_meals store ~user:bob ~from:None ~to_:None ~limit:100)))

let test_meal_crud_and_soft_delete () =
  let store, user = Test_support.store_with_user () in
  let meal = Test_support.create_meal store user in
  let patch = Meal.{ empty_patch with description = Some "Stew"; calories_kcal = Some 300 } in
  let updated = Result.get_ok (Store_sqlite.update_meal store ~user meal.id patch) in
  Alcotest.(check string) "description" "Stew" updated.description;
  Alcotest.(check int) "calories" 300 updated.calories_kcal;
  Result.get_ok (Store_sqlite.delete_meal store ~user meal.id);
  Result.get_ok (Store_sqlite.delete_meal store ~user meal.id);
  Alcotest.(check bool) "deleted hidden" true
    (match Store_sqlite.get_meal store ~user meal.id with Error Error.Not_found -> true | _ -> false)

let test_deleted_meal_is_excluded_from_query () =
  let store, user = Test_support.store_with_user () in
  let meal = Test_support.create_meal store user in
  Result.get_ok (Store_sqlite.delete_meal store ~user meal.id);
  Alcotest.(check int) "visible meals" 0
    (List.length
       (Result.get_ok
          (Store_sqlite.query_meals store ~user ~from:None ~to_:None ~limit:100)))

let test_partial_update_leaves_omitted_fields_unchanged () =
  let store, user = Test_support.store_with_user () in
  let meal = Test_support.create_meal store user in
  let updated =
    Result.get_ok
      (Store_sqlite.update_meal store ~user meal.id Meal.{ empty_patch with confidence = Some 0.5 })
  in
  Alcotest.(check (float 0.0001)) "confidence updated" 0.5 (Option.get updated.confidence);
  Alcotest.(check string) "description untouched" meal.description updated.description;
  Alcotest.(check int) "calories untouched" meal.calories_kcal updated.calories_kcal;
  Alcotest.(check (float 0.0001)) "protein untouched" meal.protein_g updated.protein_g;
  Alcotest.(check (option (float 0.0001))) "carbs untouched" meal.carbs_g updated.carbs_g;
  Alcotest.(check (option (float 0.0001))) "fat untouched" meal.fat_g updated.fat_g;
  Alcotest.(check (option string)) "estimate source untouched" meal.estimate_source updated.estimate_source;
  Alcotest.(check (option string)) "notes untouched" meal.notes updated.notes;
  Alcotest.(check string) "eaten_at untouched" (Time.to_utc_string meal.eaten_at) (Time.to_utc_string updated.eaten_at)

let test_locked_meal_get_and_update_are_storage_errors () =
  Test_support.with_exclusive_lock
    (fun store user -> (Test_support.create_meal store user, Meal.empty_patch))
    (fun store user (meal, patch) ->
      let is_storage_error = function
        | Error (Error.Storage_error "SQLite operation failed") -> true
        | _ -> false
      in
      Alcotest.(check bool) "locked get is sanitized" true
        (is_storage_error (Store_sqlite.get_meal store ~user meal.id));
      Alcotest.(check bool) "locked update is sanitized" true
        (is_storage_error (Store_sqlite.update_meal store ~user meal.id patch)))

let test_meals_between_is_half_open_and_skips_deleted () =
  let store, user = Test_support.store_with_user () in
  let at value = Result.get_ok (Time.parse_offset_datetime value) in
  let create value = Result.get_ok (Store_sqlite.create_meal store ~user (Test_support.meal_input ~eaten_at:(at value) ())) in
  let first = create "2026-09-22T00:00:00Z" in
  let second = create "2026-09-22T20:00:00Z" in
  (* Just outside [start, stop) on either side; must not be returned. *)
  ignore (create "2026-09-23T00:00:00Z");
  ignore (create "2026-09-21T23:59:59Z");
  let start = at "2026-09-22T00:00:00Z" and stop = at "2026-09-23T00:00:00Z" in
  let ids () = List.map (fun (meal : Meal.t) -> Meal_id.to_string meal.id) (Result.get_ok (Store_sqlite.meals_between store ~user ~start ~stop)) in
  Alcotest.(check (list string)) "in range, oldest first" [ Meal_id.to_string first.id; Meal_id.to_string second.id ] (ids ());
  Result.get_ok (Store_sqlite.delete_meal store ~user second.id);
  Alcotest.(check (list string)) "deleted meal excluded" [ Meal_id.to_string first.id ] (ids ());
  let empty_start = at "2026-09-25T00:00:00Z" and empty_stop = at "2026-09-26T00:00:00Z" in
  Alcotest.(check int) "empty range" 0 (List.length (Result.get_ok (Store_sqlite.meals_between store ~user ~start:empty_start ~stop:empty_stop)))

let test_rejects_invalid_limit () =
  let store, user = Test_support.store_with_user () in
  List.iter
    (fun limit ->
      Alcotest.(check bool) "invalid limit" true
        (match Store_sqlite.query_meals store ~user ~from:None ~to_:None ~limit with
        | Error (Error.Invalid_input _) -> true
        | _ -> false))
    [ 0; 501 ]

let () =
  Alcotest.run "meal store"
    [
      ( "persistence",
        [
          Alcotest.test_case "user resolution is stable" `Quick test_user_resolution_is_stable;
          Alcotest.test_case "users cannot read each other's meals" `Quick
            test_cannot_read_another_users_meal;
          Alcotest.test_case "foreign meal operations are hidden" `Quick test_foreign_meal_operations_are_hidden;
          Alcotest.test_case "meal CRUD and soft delete" `Quick test_meal_crud_and_soft_delete;
          Alcotest.test_case "partial update leaves omitted fields unchanged" `Quick
            test_partial_update_leaves_omitted_fields_unchanged;
          Alcotest.test_case "deleted meals are excluded from queries" `Quick
            test_deleted_meal_is_excluded_from_query;
          Alcotest.test_case "locked meal get and update are sanitized" `Quick
            test_locked_meal_get_and_update_are_storage_errors;
          Alcotest.test_case "invalid query limits are rejected" `Quick test_rejects_invalid_limit;
          Alcotest.test_case "meals between is half-open and skips deleted meals" `Quick
            test_meals_between_is_half_open_and_skips_deleted;
        ] );
    ]
