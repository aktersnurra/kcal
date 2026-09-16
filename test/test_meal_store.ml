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
          Alcotest.test_case "invalid query limits are rejected" `Quick test_rejects_invalid_limit;
        ] );
    ]
