let fixed_now = Option.get (Ptime.of_float_s 1_700_000_000.)
let later = Option.get (Ptime.of_float_s 1_700_000_601.)

let oauth store now = Withings_oauth.make ~store ~now:(fun () -> now)

let test_states_are_unique_and_metrics_only () =
  let store, user = Test_support.store_with_user () in
  let state_one, url_one = Result.get_ok (Withings_oauth.begin_authorization (oauth store fixed_now) ~user) in
  let state_two, _ = Result.get_ok (Withings_oauth.begin_authorization (oauth store fixed_now) ~user) in
  Alcotest.(check bool) "states differ" true (state_one <> state_two);
  Alcotest.(check bool) "requests only metrics" true (String.contains url_one 'm' && String.ends_with ~suffix:"scope=user.metrics" url_one)

let test_state_is_owned_single_use_and_expires () =
  let store, alice, bob = Test_support.store_with_two_users () in
  let authorization = oauth store fixed_now in
  let state, _ = Result.get_ok (Withings_oauth.begin_authorization authorization ~user:alice) in
  Alcotest.(check bool) "wrong user rejected" true
    (Result.is_error (Withings_oauth.consume_state authorization ~user:bob ~state));
  Alcotest.(check bool) "owner consumes state" true
    (Result.is_ok (Withings_oauth.consume_state authorization ~user:alice ~state));
  Alcotest.(check bool) "replay rejected" true
    (Result.is_error (Withings_oauth.consume_state authorization ~user:alice ~state));
  let expired_state, _ = Result.get_ok (Withings_oauth.begin_authorization authorization ~user:alice) in
  Alcotest.(check bool) "expired state rejected" true
    (Result.is_error (Withings_oauth.consume_state (oauth store later) ~user:alice ~state:expired_state))

let test_connection_status_is_user_scoped () =
  let store, alice, bob = Test_support.store_with_two_users () in
  ignore (Result.get_ok (Store_sqlite.create_withings_connection store ~user:alice ~withings_user_id:"withings-alice"));
  let service = Service.make ~store in
  Alcotest.(check bool) "owner sees connection" true
    (Result.is_ok (Service.get_withings_status service ~user:alice));
  Alcotest.(check bool) "other user does not see connection" true
    (Result.is_error (Service.get_withings_status service ~user:bob))

let () =
  Alcotest.run "withings OAuth"
    [ ( "state", [ Alcotest.test_case "unique metrics-only state" `Quick test_states_are_unique_and_metrics_only;
                    Alcotest.test_case "ownership, expiry, replay" `Quick test_state_is_owned_single_use_and_expires;
                    Alcotest.test_case "connection ownership" `Quick test_connection_status_is_user_scoped ] ) ]
