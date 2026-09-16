let test_service_hides_foreign_meal () =
  let store, alice, bob = Test_support.store_with_two_users () in
  let service = Service.make ~store in
  let meal =
    Result.get_ok
      (Service.record_meal service ~user:alice (Test_support.meal_input ()))
  in
  Alcotest.(check bool) "not found" true
    (match Service.get_meal service ~user:bob meal.id with
    | Error Error.Not_found -> true
    | _ -> false)

let test_service_records_for_authenticated_user () =
  let store, alice, _ = Test_support.store_with_two_users () in
  let service = Service.make ~store in
  let meal =
    Result.get_ok
      (Service.record_meal service ~user:alice (Test_support.meal_input ()))
  in
  Alcotest.(check string) "owner" (User_id.to_string alice.id)
    (User_id.to_string meal.user_id)

let () =
  Alcotest.run "service"
    [
      ( "isolation",
        [ Alcotest.test_case "foreign meal is hidden" `Quick test_service_hides_foreign_meal;
          Alcotest.test_case "meal uses supplied identity" `Quick
            test_service_records_for_authenticated_user ] );
    ]
