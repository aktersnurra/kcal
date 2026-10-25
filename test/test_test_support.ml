exception Fixture_failure

let check_closed store =
  let is_closed =
    try
      ignore (Sqlite3.exec store "SELECT 1");
      false
    with Sqlite3.Error _ -> true
  in
  Alcotest.(check bool) "SQLite handle is actually closed" true is_closed

let test_success fixture () =
  let store, value = fixture (fun store -> (store, 42)) in
  Alcotest.(check int) "callback result preserved" 42 value;
  check_closed store

let test_exception fixture () =
  let captured = ref None in
  Alcotest.check_raises "callback exception preserved" Fixture_failure
    (fun () ->
      fixture (fun store ->
          captured := Some store;
          raise Fixture_failure));
  check_closed (Option.get !captured)

let one f = Test_support.with_store (fun store _ -> f store)

let two f =
  Test_support.with_two_users (fun store alice bob ->
      Alcotest.(check bool)
        "distinct users" false
        (User_id.equal alice.User.id bob.User.id);
      f store)

let () =
  Alcotest.run "test support"
    [
      ( "lifetime",
        [
          Alcotest.test_case "with_store success" `Quick (test_success one);
          Alcotest.test_case "with_store exception" `Quick (test_exception one);
          Alcotest.test_case "with_two_users success" `Quick (test_success two);
          Alcotest.test_case "with_two_users exception" `Quick
            (test_exception two);
        ] );
    ]
