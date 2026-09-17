let key = Bytes.make 32 'k'
let now = Option.get (Ptime.of_float_s 1_800_000_000.)

let connected store user identity =
  ignore (Result.get_ok (Store_sqlite.ensure_withings_connection store ~user ~withings_user_id:identity));
  Result.get_ok (Store_sqlite.save_withings_credentials store ~user ~key Withings.{ access_token = "access"; refresh_token = "refresh"; expires_at = now; withings_user_id = identity })

let test_reconciles_each_connected_user () =
  let store, first = Test_support.store_with_user () in
  let second = Result.get_ok (Store_sqlite.resolve_user store ~issuer:"issuer" ~subject:"second") in
  let first_connection = connected store first "upstream-first" in
  let second_connection = connected store second "upstream-second" in
  let seen = ref [] in
  Result.get_ok (Withings_reconciliation.sync_all ~store ~sync:(fun ~user ~connection -> seen := (User_id.to_string user.User.id, connection.Withings_connection.id) :: !seen; Ok ()));
  Alcotest.(check int) "independent connections" 2 (List.length !seen);
  Alcotest.(check bool) "first connection" true (List.mem (User_id.to_string first.User.id, first_connection.id) !seen);
  Alcotest.(check bool) "second connection" true (List.mem (User_id.to_string second.User.id, second_connection.id) !seen)

let () = Alcotest.run "withings reconciliation" [ ("sync", [ Alcotest.test_case "connected users" `Quick test_reconciles_each_connected_user ]) ]
