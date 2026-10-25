let expires = Option.get (Ptime.of_float_s 1_800_000_000.)

let credentials token refresh =
  Withings.
    {
      access_token = token;
      refresh_token = refresh;
      expires_at = expires;
      withings_user_id = "withings-alice";
    }

let test_exchange_and_refresh_response_are_sanitized () =
  let calls = ref [] in
  let request =
    Withings.
      {
        post_form =
          (fun ~uri ~fields ->
            calls := (uri, fields) :: !calls;
            Ok
              {|{"status":0,"body":{"access_token":"access","refresh_token":"refresh","expires_in":3600,"userid":"42"}}|});
      }
  in
  let module Client =
    (val Withings.make
           ~now:(fun () -> expires)
           ~request
           ~config:Withings.{ client_id = "id"; client_secret = "secret" })
  in
  let exchanged =
    Result.get_ok
      (Client.exchange_code
         ~redirect_uri:"https://kcal.example.com/withings/callback" ~code:"code")
  in
  Alcotest.(check string) "exchange token" "access" exchanged.access_token;
  let refreshed = Result.get_ok (Client.refresh ~refresh_token:"refresh") in
  Alcotest.(check string) "refresh token" "refresh" refreshed.refresh_token;
  Alcotest.(check bool)
    "HTTPS only" true
    (List.for_all
       (fun (uri, _) -> String.starts_with ~prefix:"https://" uri)
       !calls)

let test_measurement_fixture_accepts_withings_numbers () =
  let fixture =
    {|{"status":0,"body":{"lastupdate":1700000001,"measuregrps":[{"grpid":12345,"date":1700000000,"measures":[{"type":1,"value":80500,"unit":-3}]}]}}|}
  in
  let request = Withings.{ post_form = (fun ~uri:_ ~fields:_ -> Ok fixture) } in
  let module Client =
    (val Withings.make
           ~now:(fun () -> expires)
           ~request
           ~config:Withings.{ client_id = "id"; client_secret = "secret" })
  in
  match
    Result.get_ok
      (Client.get_measurements ~access_token:"token" ~lastupdate:None)
  with
  | { Withings.measurements = [ measurement ]; lastupdate = Some cursor } ->
      Alcotest.(check string) "numeric group id" "12345" measurement.group_id;
      Alcotest.(check int64) "numeric cursor" 1700000001L cursor
  | _ -> Alcotest.fail "fixture was not decoded"

let test_credentials_are_replaced_and_refresh_failure_marks_reauthorization () =
  Test_support.with_store (fun store user ->
      ignore
        (Result.get_ok
           (Test_support.ensure_withings_connection store ~user
              ~withings_user_id:"withings-alice"));
      let key = Result.get_ok (Secret.Key.of_bytes (Bytes.make 32 'k')) in
      ignore
        (Result.get_ok
           (Store_sqlite.replace_withings_connection store ~user ~key
              (credentials "old-access" "old-refresh")));
      let connection =
        Result.get_ok
          (Store_sqlite.replace_withings_connection store ~user ~key
             (credentials "new-access" "new-refresh"))
      in
      let saved =
        Result.get_ok
          (Store_sqlite.withings_credentials store ~user ~connection ~key)
      in
      Alcotest.(check string) "rotated access" "new-access" saved.access_token;
      Alcotest.(check string)
        "rotated refresh" "new-refresh" saved.refresh_token;
      Result.get_ok
        (Store_sqlite.mark_withings_reauthorization store ~user ~connection);
      let status =
        Result.get_ok (Store_sqlite.get_withings_status store ~user)
      in
      Alcotest.(check bool)
        "reauthorization required" true
        (match status with
        | Withings_connection.Reauthorization_required _ -> true
        | _ -> false);
      ignore connection)

let test_exchange_code_sends_redirect_uri () =
  let calls = ref [] in
  let request =
    Withings.
      {
        post_form =
          (fun ~uri:_ ~fields ->
            calls := fields :: !calls;
            Ok
              {|{"status":0,"body":{"access_token":"access","refresh_token":"refresh","expires_in":3600,"userid":"42"}}|});
      }
  in
  let module Client =
    (val Withings.make
           ~now:(fun () -> expires)
           ~request
           ~config:Withings.{ client_id = "id"; client_secret = "secret" })
  in
  ignore
    (Result.get_ok
       (Client.exchange_code
          ~redirect_uri:"https://kcal.example.com/withings/callback"
          ~code:"code"));
  match !calls with
  | [ fields ] ->
      Alcotest.(check (option string))
        "redirect_uri is posted"
        (Some "https://kcal.example.com/withings/callback")
        (List.assoc_opt "redirect_uri" fields)
  | _ -> Alcotest.fail "expected exactly one token request"

let test_upstream_status_is_preserved_in_error () =
  let request =
    Withings.
      {
        post_form =
          (fun ~uri:_ ~fields:_ -> Ok {|{"status":503,"error":"nope"}|});
      }
  in
  let module Client =
    (val Withings.make
           ~now:(fun () -> expires)
           ~request
           ~config:Withings.{ client_id = "id"; client_secret = "secret" })
  in
  match
    Client.exchange_code
      ~redirect_uri:"https://kcal.example.com/withings/callback" ~code:"code"
  with
  | Ok _ -> Alcotest.fail "expected the exchange to fail"
  | Error (Error.Withings_failure (Api_status status)) ->
      Alcotest.(check int) "API status" 503 status
  | Error _ -> Alcotest.fail "expected a structured API status"

let test_transport_error_names_the_http_status () =
  match Withings_transport.error_of_status `Bad_gateway with
  | Error.Withings_failure (Http_status status) ->
      Alcotest.(check int) "HTTP status" 502 status
  | _ -> Alcotest.fail "expected a structured HTTP status"

let test_transport_error_names_the_exception () =
  match Withings_transport.error_of_exn (Failure "connection reset") with
  | Error.Withings_failure (Transport_failure message) ->
      Alcotest.(check string)
        "exception retained" {|Failure("connection reset")|} message
  | _ -> Alcotest.fail "expected a structured transport failure"

let test_complete_pagination () =
  let calls = ref [] in
  let request =
    Withings.
      {
        post_form =
          (fun ~uri:_ ~fields ->
            calls := fields :: !calls;
            match List.assoc_opt "offset" fields with
            | None ->
                Ok
                  {|{"status":0,"body":{"lastupdate":100,"more":1,"offset":42,"measuregrps":[]}}|}
            | Some "42" ->
                Ok
                  {|{"status":0,"body":{"lastupdate":100,"more":0,"measuregrps":[]}}|}
            | _ -> Alcotest.fail "unexpected continuation");
      }
  in
  let module Client =
    (val Withings.make
           ~now:(fun () -> expires)
           ~request
           ~config:Withings.{ client_id = "id"; client_secret = "secret" })
  in
  ignore
    (Result.get_ok
       (Client.get_measurements ~access_token:"token" ~lastupdate:(Some 90L)));
  Alcotest.(check int) "all pages fetched" 2 (List.length !calls);
  List.iter
    (fun fields ->
      Alcotest.(check (option string))
        "original query watermark" (Some "90")
        (List.assoc_opt "lastupdate" fields))
    !calls

let test_second_page_failure () =
  let request =
    Withings.
      {
        post_form =
          (fun ~uri:_ ~fields ->
            if List.mem_assoc "offset" fields then
              Error (Error.Invalid_input "page two")
            else
              Ok
                {|{"status":0,"body":{"lastupdate":100,"more":1,"offset":42,"measuregrps":[]}}|});
      }
  in
  let module Client =
    (val Withings.make
           ~now:(fun () -> expires)
           ~request
           ~config:Withings.{ client_id = "id"; client_secret = "secret" })
  in
  Alcotest.(check bool)
    "no partial batch" true
    (Result.is_error
       (Client.get_measurements ~access_token:"token" ~lastupdate:None))

let test_malformed_continuations () =
  List.iter
    (fun fixture ->
      let calls = ref 0 in
      let request =
        Withings.
          {
            post_form =
              (fun ~uri:_ ~fields:_ ->
                incr calls;
                if !calls > 2 then
                  Error (Error.Withings_failure (Http_status 502))
                else Ok fixture);
          }
      in
      let module Client =
        (val Withings.make
               ~now:(fun () -> expires)
               ~request
               ~config:Withings.{ client_id = "id"; client_secret = "secret" })
      in
      Alcotest.(check bool)
        "malformed continuation rejected" true
        (match
           Client.get_measurements ~access_token:"token" ~lastupdate:None
         with
        | Error (Error.Withings_failure Malformed_response) -> true
        | _ -> false))
    [
      {|{"status":0,"body":{"more":1,"measuregrps":[]}}|};
      {|{"status":0,"body":{"more":1,"offset":42,"measuregrps":[]}}|};
      {|{"status":0,"body":{"more":1,"offset":0,"measuregrps":[]}}|};
      {|{"status":0,"body":{"more":1,"offset":-1,"measuregrps":[]}}|};
      {|{"status":0,"body":{"more":1,"offset":1e300,"measuregrps":[]}}|};
    ]

let test_credentials_use_injected_clock () =
  let body =
    {|{"status":0,"body":{"access_token":"access","refresh_token":"refresh","expires_in":3600,"userid":"42"}}|}
  in
  let first =
    Result.get_ok (Withings.credentials_of_response ~now:expires body)
  in
  let second_now =
    Option.get (Ptime.add_span expires (Ptime.Span.of_int_s 90))
  in
  let request = Withings.{ post_form = (fun ~uri:_ ~fields:_ -> Ok body) } in
  let module Client =
    (val Withings.make
           ~now:(fun () -> second_now)
           ~request
           ~config:Withings.{ client_id = "id"; client_secret = "secret" })
  in
  let second = Result.get_ok (Client.refresh ~refresh_token:"refresh") in
  Alcotest.(check bool)
    "pure expiry" true
    (Ptime.equal first.expires_at
       (Option.get (Ptime.add_span expires (Ptime.Span.of_int_s 3600))));
  Alcotest.(check bool)
    "injected client expiry" true
    (Ptime.equal second.expires_at
       (Option.get (Ptime.add_span second_now (Ptime.Span.of_int_s 3600))))

let test_final_page_raw_watermark () =
  List.iter
    (fun (last_page, expected) ->
      let request =
        Withings.
          {
            post_form =
              (fun ~uri:_ ~fields ->
                Ok
                  (if List.mem_assoc "offset" fields then last_page
                   else
                     {|{"status":0,"body":{"more":1,"offset":42,"lastupdate":101,"measuregrps":[]}}|}));
          }
      in
      let module Client =
        (val Withings.make
               ~now:(fun () -> expires)
               ~request
               ~config:Withings.{ client_id = "id"; client_secret = "secret" })
      in
      let batch =
        Result.get_ok
          (Client.get_measurements ~access_token:"token" ~lastupdate:(Some 100L))
      in
      Alcotest.(check (option int64))
        "final page raw watermark" expected batch.lastupdate)
    [
      ( {|{"status":0,"body":{"more":0,"lastupdate":99,"measuregrps":[]}}|},
        Some 99L );
      ({|{"status":0,"body":{"more":0,"measuregrps":[]}}|}, None);
    ]

let test_structured_failures () =
  let rejected = {|{"status":401}|} in
  Alcotest.(check bool)
    "refresh credential rejection" true
    (match
       Withings.credentials_of_response ~now:expires ~refresh:true rejected
     with
    | Error (Error.Withings_failure (Credential_rejection 401)) -> true
    | _ -> false);
  Alcotest.(check bool)
    "exchange API status" true
    (match Withings.credentials_of_response ~now:expires rejected with
    | Error (Error.Withings_failure (Api_status 401)) -> true
    | _ -> false);
  List.iter
    (fun body ->
      Alcotest.(check bool)
        "malformed response variant" true
        (match Withings.measurements_of_response body with
        | Error (Error.Withings_failure Malformed_response) -> true
        | _ -> false))
    [
      "not json";
      {|{"status":0,"body":{}}|};
      {|{"status":0,"body":{"more":1,"measuregrps":[]}}|};
    ];
  let request =
    Withings.
      {
        post_form =
          (fun ~uri:_ ~fields:_ ->
            Error (Error.Withings_failure (Http_status 503)));
      }
  in
  let module Client =
    (val Withings.make
           ~now:(fun () -> expires)
           ~request
           ~config:Withings.{ client_id = "id"; client_secret = "secret" })
  in
  Alcotest.(check bool)
    "HTTP failure preserved through client" true
    (match Client.get_measurements ~access_token:"token" ~lastupdate:None with
    | Error (Error.Withings_failure (Http_status 503)) -> true
    | _ -> false)

let test_failed_relink_preserves_connection () =
  Test_support.with_store (fun store user ->
      let key = Result.get_ok (Secret.Key.of_bytes (Bytes.make 32 'k')) in
      ignore
        (Result.get_ok
           (Test_support.ensure_withings_connection store ~user
              ~withings_user_id:"withings-alice"));
      let old =
        Result.get_ok
          (Store_sqlite.replace_withings_connection store ~user ~key
             (credentials "old" "old-refresh"))
      in
      Result.get_ok
        (Store_sqlite.persist_withings_import store ~user ~connection:old
           ~rows:[] ~expected_cursor:None ~cursor:(Some 42L));
      ignore
        (Sqlite3.exec
           (Store_sqlite.For_testing.raw_database store)
           "CREATE TRIGGER fail_save BEFORE UPDATE OF access_token_encrypted \
            ON withings_connections BEGIN SELECT RAISE(ABORT, 'injected'); END");
      let replacement =
        Withings.
          {
            (credentials "new" "new-refresh") with
            withings_user_id = "replacement";
          }
      in
      Alcotest.(check bool)
        "save fails" true
        (Result.is_error
           (Store_sqlite.replace_withings_connection store ~user ~key
              replacement));
      let _, current =
        Result.get_ok
          (Store_sqlite.withings_connection_for_identity store
             ~withings_user_id:"withings-alice")
      in
      Alcotest.(check (option int64))
        "cursor unchanged" (Some 42L) current.sync_cursor;
      let saved =
        Result.get_ok
          (Store_sqlite.withings_credentials store ~user ~connection:current
             ~key)
      in
      Alcotest.(check string) "access unchanged" "old" saved.access_token;
      Alcotest.(check string)
        "refresh unchanged" "old-refresh" saved.refresh_token)

let test_refresh_scoped_to_expected_account () =
  Test_support.with_store (fun store user ->
      let key = Result.get_ok (Secret.Key.of_bytes (Bytes.make 32 'k')) in
      let old =
        Result.get_ok
          (Store_sqlite.replace_withings_connection store ~user ~key
             (credentials "old" "old-refresh"))
      in
      let replacement =
        Withings.
          {
            (credentials "replacement" "replacement-refresh") with
            withings_user_id = "replacement";
          }
      in
      let current =
        Result.get_ok
          (Store_sqlite.replace_withings_connection store ~user ~key replacement)
      in
      Alcotest.(check bool)
        "stale refresh rejected" true
        (Result.is_error
           (Store_sqlite.refresh_withings_credentials store ~user
              ~connection:old ~key
              (credentials "stale" "stale-refresh")));
      Alcotest.(check bool)
        "stale rejection cannot mark new account" true
        (Result.is_error
           (Store_sqlite.mark_withings_reauthorization store ~user
              ~connection:old));
      let saved =
        Result.get_ok
          (Store_sqlite.withings_credentials store ~user ~connection:current
             ~key)
      in
      Alcotest.(check string)
        "new identity" "replacement" saved.withings_user_id;
      Alcotest.(check string) "new access" "replacement" saved.access_token;
      Alcotest.(check string)
        "new refresh" "replacement-refresh" saved.refresh_token)

let test_disconnected_and_incomplete_status () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      Alcotest.(check bool)
        "disconnected is ordinary absence" true
        (match Service.withings_status service ~user with
        | Ok Withings_connection.Disconnected -> true
        | _ -> false);
      List.iter
        (fun assignment ->
          ignore
            (Result.get_ok
               (Test_support.ensure_withings_connection store ~user
                  ~withings_user_id:"withings-alice"));
          Result.get_ok
            (Migration.For_testing.execute
               (Store_sqlite.For_testing.raw_database store)
               ("UPDATE withings_connections SET " ^ assignment));
          Alcotest.(check bool)
            "incomplete durable row is a decoding error" true
            (match Service.withings_status service ~user with
            | Error (Error.Storage_decoding _) -> true
            | _ -> false))
        [
          "withings_user_id=NULL";
          "withings_user_id=''";
          "token_expires_at=NULL";
          "token_expires_at='invalid'";
          "access_token_encrypted=NULL";
          "refresh_token_encrypted=NULL";
        ])

let () =
  Alcotest.run "withings client"
    [
      ( "atomic lifecycle",
        [
          Alcotest.test_case "stale refresh scoped to expected account" `Quick
            test_refresh_scoped_to_expected_account;
          Alcotest.test_case "disconnected and incomplete status" `Quick
            test_disconnected_and_incomplete_status;
          Alcotest.test_case "failed relink preserves connection" `Quick
            test_failed_relink_preserves_connection;
        ] );
      ( "credentials",
        [
          Alcotest.test_case "pure credentials and injected clock" `Quick
            test_credentials_use_injected_clock;
          Alcotest.test_case "final page raw watermark" `Quick
            test_final_page_raw_watermark;
          Alcotest.test_case "structured provider failures" `Quick
            test_structured_failures;
          Alcotest.test_case "complete pagination" `Quick
            test_complete_pagination;
          Alcotest.test_case "second page failure" `Quick
            test_second_page_failure;
          Alcotest.test_case "malformed continuations" `Quick
            test_malformed_continuations;
          Alcotest.test_case "exchange and refresh" `Quick
            test_exchange_and_refresh_response_are_sanitized;
          Alcotest.test_case "realistic numeric measurement fixture" `Quick
            test_measurement_fixture_accepts_withings_numbers;
          Alcotest.test_case "rotation and permanent failure" `Quick
            test_credentials_are_replaced_and_refresh_failure_marks_reauthorization;
          Alcotest.test_case "exchange posts redirect_uri" `Quick
            test_exchange_code_sends_redirect_uri;
          Alcotest.test_case "upstream status preserved" `Quick
            test_upstream_status_is_preserved_in_error;
          Alcotest.test_case "transport error names HTTP status" `Quick
            test_transport_error_names_the_http_status;
          Alcotest.test_case "transport error names exception" `Quick
            test_transport_error_names_the_exception;
        ] );
    ]
