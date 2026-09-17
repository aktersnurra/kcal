let expires = Option.get (Ptime.of_float_s 1_800_000_000.)

let credentials token refresh =
  Withings.{ access_token = token; refresh_token = refresh; expires_at = expires; withings_user_id = "withings-alice" }

let test_exchange_and_refresh_response_are_sanitized () =
  let calls = ref [] in
  let request =
    Withings.{ post_form = (fun ~uri ~fields ->
      calls := (uri, fields) :: !calls;
      Ok {|{"status":0,"body":{"access_token":"access","refresh_token":"refresh","expires_in":3600,"userid":"42"}}|}) }
  in
  let module Client = (val Withings.make ~request ~config:Withings.{ client_id = "id"; client_secret = "secret" }) in
  let exchanged = Result.get_ok (Client.exchange_code ~code:"code") in
  Alcotest.(check string) "exchange token" "access" exchanged.access_token;
  let refreshed = Result.get_ok (Client.refresh ~refresh_token:"refresh") in
  Alcotest.(check string) "refresh token" "refresh" refreshed.refresh_token;
  Alcotest.(check bool) "HTTPS only" true (List.for_all (fun (uri, _) -> String.starts_with ~prefix:"https://" uri) !calls)

let test_measurement_fixture_accepts_withings_numbers () =
  let fixture = {|{"status":0,"body":{"lastupdate":1700000001,"measuregrps":[{"grpid":12345,"date":1700000000,"measures":[{"type":1,"value":80500,"unit":-3}]}]}}|} in
  let request = Withings.{ post_form = (fun ~uri:_ ~fields:_ -> Ok fixture) } in
  let module Client = (val Withings.make ~request ~config:Withings.{ client_id = "id"; client_secret = "secret" }) in
  match Result.get_ok (Client.get_measurements ~access_token:"token" ~lastupdate:None) with
  | { Withings.measurements = [ measurement ]; lastupdate = Some cursor } ->
      Alcotest.(check string) "numeric group id" "12345" measurement.group_id;
      Alcotest.(check int64) "numeric cursor" 1700000001L cursor
  | _ -> Alcotest.fail "fixture was not decoded"

let test_credentials_are_replaced_and_refresh_failure_marks_reauthorization () =
  let store, user = Test_support.store_with_user () in
  ignore (Result.get_ok (Store_sqlite.create_withings_connection store ~user ~withings_user_id:"withings-alice"));
  let key = Bytes.make 32 'k' in
  ignore (Result.get_ok (Store_sqlite.save_withings_credentials store ~user ~key (credentials "old-access" "old-refresh")));
  let connection = Result.get_ok (Store_sqlite.save_withings_credentials store ~user ~key (credentials "new-access" "new-refresh")) in
  let saved = Result.get_ok (Store_sqlite.withings_credentials store ~user ~connection ~key) in
  Alcotest.(check string) "rotated access" "new-access" saved.access_token;
  Alcotest.(check string) "rotated refresh" "new-refresh" saved.refresh_token;
  Result.get_ok (Store_sqlite.mark_withings_reauthorization store ~user ~connection);
  let Withings_connection.Connected status = Result.get_ok (Store_sqlite.get_withings_status store ~user) in
  Alcotest.(check bool) "reauthorization required" true status.requires_reauthorization;
  ignore connection

let () =
  Alcotest.run "withings client"
    [ ("credentials", [ Alcotest.test_case "exchange and refresh" `Quick test_exchange_and_refresh_response_are_sanitized;
                           Alcotest.test_case "realistic numeric measurement fixture" `Quick test_measurement_fixture_accepts_withings_numbers;
                           Alcotest.test_case "rotation and permanent failure" `Quick test_credentials_are_replaced_and_refresh_failure_marks_reauthorization ]) ]
