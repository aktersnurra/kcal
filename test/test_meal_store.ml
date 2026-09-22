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

let test_daily_totals_aggregates_meals_within_the_utc_day () =
  let store, user = Test_support.store_with_user () in
  let at value = Result.get_ok (Time.parse_offset_datetime value) in
  let first =
    Result.get_ok (Store_sqlite.create_meal store ~user (Test_support.meal_input ~eaten_at:(at "2026-09-22T08:00:00Z") ()))
  in
  let second =
    Result.get_ok
      (Store_sqlite.create_meal store ~user
         Meal.{ (Test_support.meal_input ~eaten_at:(at "2026-09-22T20:00:00Z") ()) with calories_kcal = 400; protein_g = 30.0; carbs_g = None; fat_g = Some 10.0 })
  in
  (* Outside the UTC day on either side; must not be counted. *)
  ignore (Result.get_ok (Store_sqlite.create_meal store ~user (Test_support.meal_input ~eaten_at:(at "2026-09-23T00:00:00Z") ())));
  ignore (Result.get_ok (Store_sqlite.create_meal store ~user (Test_support.meal_input ~eaten_at:(at "2026-09-21T23:59:59Z") ())));
  let day_start, day_end = Time.day_bounds (at "2026-09-22T12:00:00Z") in
  let totals = Result.get_ok (Store_sqlite.daily_totals store ~user ~day_start ~day_end) in
  Alcotest.(check int) "meal count" 2 totals.meal_count;
  Alcotest.(check int) "calories summed" (250 + 400) totals.calories_kcal;
  Alcotest.(check (float 0.0001)) "protein summed" (12.0 +. 30.0) totals.protein_g;
  Alcotest.(check (option (float 0.0001))) "carbs summed over present values only" (Some 20.0) totals.carbs_g;
  Alcotest.(check (option (float 0.0001))) "fat summed over present values only" (Some 15.0) totals.fat_g;
  Result.get_ok (Store_sqlite.delete_meal store ~user second.id);
  let totals_after_delete = Result.get_ok (Store_sqlite.daily_totals store ~user ~day_start ~day_end) in
  Alcotest.(check int) "deleted meal excluded" 1 totals_after_delete.meal_count;
  Alcotest.(check int) "deleted meal's calories excluded" 250 totals_after_delete.calories_kcal;
  ignore first

let test_daily_totals_are_null_and_zero_for_an_empty_day () =
  let store, user = Test_support.store_with_user () in
  let day_start, day_end = Time.day_bounds (Result.get_ok (Time.parse_offset_datetime "2026-09-22T12:00:00Z")) in
  let totals = Result.get_ok (Store_sqlite.daily_totals store ~user ~day_start ~day_end) in
  Alcotest.(check int) "no meals" 0 totals.meal_count;
  Alcotest.(check int) "zero calories" 0 totals.calories_kcal;
  Alcotest.(check (float 0.0001)) "zero protein" 0.0 totals.protein_g;
  Alcotest.(check (option (float 0.0001))) "no carbs" None totals.carbs_g;
  Alcotest.(check (option (float 0.0001))) "no fat" None totals.fat_g

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
          Alcotest.test_case "deleted meals are excluded from queries" `Quick
            test_deleted_meal_is_excluded_from_query;
          Alcotest.test_case "locked meal get and update are sanitized" `Quick
            test_locked_meal_get_and_update_are_storage_errors;
          Alcotest.test_case "invalid query limits are rejected" `Quick test_rejects_invalid_limit;
          Alcotest.test_case "daily totals aggregate meals within the UTC day" `Quick
            test_daily_totals_aggregates_meals_within_the_utc_day;
          Alcotest.test_case "daily totals are null and zero for an empty day" `Quick
            test_daily_totals_are_null_and_zero_for_an_empty_day;
        ] );
    ]
