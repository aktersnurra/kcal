let fixed_now = Option.get (Ptime.of_float_s 1_700_000_000.)
let later = Option.get (Ptime.of_float_s 1_700_000_601.)

let oauth store now = Withings_oauth.make ~store ~now:(fun () -> now)
let client_id = "exact-client"
let redirect_uri = "https://kcal.example/withings/callback"

let begin_authorization oauth ~user =
  Withings_oauth.begin_authorization ~client_id ~redirect_uri oauth ~user

let test_states_are_unique_and_metrics_only () =
  let store, user = Test_support.store_with_user () in
  let state_one, url_one = Result.get_ok (begin_authorization (oauth store fixed_now) ~user) in
  let state_two, _ = Result.get_ok (begin_authorization (oauth store fixed_now) ~user) in
  Alcotest.(check bool) "states differ" true (state_one <> state_two);
  Alcotest.(check bool) "complete configured URL" true (String.contains url_one 'm' && String.contains url_one 'c' && String.contains url_one 'r' && String.contains url_one 's' && String.contains url_one '=' && Uri.query (Uri.of_string url_one) = [ ("response_type", ["code"]); ("client_id", [client_id]); ("redirect_uri", [redirect_uri]); ("scope", ["user.metrics"]); ("state", [state_one]) ])

let test_state_is_owned_single_use_and_expires () =
  let store, alice, bob = Test_support.store_with_two_users () in
  let authorization = oauth store fixed_now in
  let state, _ = Result.get_ok (begin_authorization authorization ~user:alice) in
  Alcotest.(check bool) "wrong user rejected" true
    (Result.is_error (Withings_oauth.consume_state authorization ~user:bob ~state));
  Alcotest.(check bool) "owner consumes state" true
    (Result.is_ok (Withings_oauth.consume_state authorization ~user:alice ~state));
  Alcotest.(check bool) "replay rejected" true
    (Result.is_error (Withings_oauth.consume_state authorization ~user:alice ~state));
  let expired_state, _ = Result.get_ok (begin_authorization authorization ~user:alice) in
  Alcotest.(check bool) "expired state rejected" true
    (Result.is_error (Withings_oauth.consume_state (oauth store later) ~user:alice ~state:expired_state))

let test_callback_state_is_bound_single_use_and_expires () =
  let store, alice, bob = Test_support.store_with_two_users () in
  let authorization = oauth store fixed_now in
  let alice_state, _ = Result.get_ok (begin_authorization authorization ~user:alice) in
  let bob_state, _ = Result.get_ok (begin_authorization authorization ~user:bob) in
  Alcotest.(check string) "callback state binds alice" (User_id.to_string alice.User.id)
    (User_id.to_string (Result.get_ok (Withings_oauth.consume_callback_state authorization ~state:alice_state)).User.id);
  Alcotest.(check bool) "callback replay rejected" true
    (Result.is_error (Withings_oauth.consume_callback_state authorization ~state:alice_state));
  Alcotest.(check string) "callback state binds bob" (User_id.to_string bob.User.id)
    (User_id.to_string (Result.get_ok (Withings_oauth.consume_callback_state authorization ~state:bob_state)).User.id);
  let expired_state, _ = Result.get_ok (begin_authorization authorization ~user:alice) in
  Alcotest.(check bool) "expired callback state rejected" true
    (Result.is_error (Withings_oauth.consume_callback_state (oauth store later) ~state:expired_state))

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
                    Alcotest.test_case "callback binding, expiry, replay" `Quick test_callback_state_is_bound_single_use_and_expires;
                    Alcotest.test_case "connection ownership" `Quick test_connection_status_is_user_scoped ] ) ]
