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
           (Store_sqlite.create_withings_connection store ~user
              ~withings_user_id:"withings-alice"));
      let key = Bytes.make 32 'k' in
      ignore
        (Result.get_ok
           (Store_sqlite.save_withings_credentials store ~user ~key
              (credentials "old-access" "old-refresh")));
      let connection =
        Result.get_ok
          (Store_sqlite.save_withings_credentials store ~user ~key
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
      let (Withings_connection.Connected status) =
        Result.get_ok (Store_sqlite.get_withings_status store ~user)
      in
      Alcotest.(check bool)
        "reauthorization required" true status.requires_reauthorization;
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

let () =
  Alcotest.run "withings client"
    [
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
