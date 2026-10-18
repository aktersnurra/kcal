let identity user scopes = Auth.{ user; scopes }

let call name arguments =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "tools/call");
    ("params", `Assoc [ ("name", `String name); ("arguments", arguments) ]);
  ]

let response_has_error_code code = function
  | Ok (`Assoc fields) ->
      (match List.assoc_opt "error" fields with
      | Some (`Assoc error) -> List.assoc_opt "code" error = Some (`Int code)
      | _ -> false)
  | _ -> false

let response_has_result = function
  | Ok (`Assoc fields) -> Option.is_some (List.assoc_opt "result" fields)
  | Error `Forbidden -> false

let result_member name = function
  | Ok (`Assoc fields) ->
      (match List.assoc_opt "result" fields with
      | Some (`Assoc result) -> List.assoc_opt name result
      | _ -> None)
  | Error `Forbidden -> None

let is_forbidden = function Error `Forbidden -> true | Ok _ -> false

let test_record_meal_rejects_user_id () =
  let store, user = Test_support.store_with_user () in
  let request = call "record_meal"
    (`Assoc [ ("user_id", `String "other-user");
      ("description", `String "Soup"); ("calories_kcal", `Int 250);
      ("protein_g", `Float 12.) ]) in
  Alcotest.(check bool) "invalid parameters" true
    (response_has_error_code (-32602)
      (Mcp.handle ~service:(Service.make ~store)
        ~identity:(identity user [ "ledger:write" ]) request))

let test_mcp_rejects_bad_jsonrpc_and_provenance_fields () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let bad_version = `Assoc [ ("jsonrpc", `String "1.0"); ("method", `String "tools/list") ] in
  let source_argument = call "record_weight"
    (`Assoc [ ("weight_kg", `Float 70.); ("source", `String "withings") ]) in
  Alcotest.(check bool) "version" true
    (response_has_error_code (-32600)
      (Mcp.handle ~service ~identity:(identity user []) bad_version));
  Alcotest.(check bool) "provenance" true
    (response_has_error_code (-32602)
      (Mcp.handle ~service ~identity:(identity user [ "ledger:write" ]) source_argument))

let test_read_scope_allows_reads_only () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  Alcotest.(check bool) "read allowed" true
    (Result.is_ok (Mcp.handle ~service ~identity:(identity user [ "ledger:read" ])
      (call "query_meals" (`Assoc []))));
  Alcotest.(check bool) "write forbidden" true
    (match Mcp.handle ~service ~identity:(identity user [ "ledger:read" ])
      (call "record_weight" (`Assoc [ ("weight_kg", `Float 70.) ])) with
    | Error `Forbidden -> true | _ -> false)

let test_write_scope_allows_writes_only () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  Alcotest.(check bool) "write allowed" true
    (Result.is_ok (Mcp.handle ~service ~identity:(identity user [ "ledger:write" ])
      (call "record_weight" (`Assoc [ ("weight_kg", `Float 70.) ]))));
  Alcotest.(check bool) "read forbidden" true
    (match Mcp.handle ~service ~identity:(identity user [ "ledger:write" ])
      (call "query_weights" (`Assoc [])) with
    | Error `Forbidden -> true | _ -> false)

let withings store =
  let oauth = Withings_oauth.make ~store
    ~now:(fun () -> Option.get (Ptime.of_float_s 1_800_000_000.)) in
  Mcp.{ oauth; client_id = "mcp-client";
        redirect_uri = "https://kcal.example/mcp-callback" }

let test_withings_scope_isolated_and_requires_configuration () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let withings = withings store in
  let managed = identity user [ "withings:manage" ] in
  List.iter (fun name ->
    Alcotest.(check bool) (name ^ " unavailable without configuration") true
      (response_has_error_code (-32603)
        (Mcp.handle ~service ~identity:managed (call name (`Assoc []))));
    Alcotest.(check bool) (name ^ " configured result") true
      (response_has_result
        (Mcp.handle ~withings ~service ~identity:managed (call name (`Assoc [])))))
    [ "begin_withings_connection"; "get_withings_status"; "disconnect_withings" ];
  List.iter (fun (name, arguments) ->
    Alcotest.(check bool) (name ^ " forbidden to Withings-only identity") true
      (is_forbidden
        (Mcp.handle ~service ~identity:managed (call name arguments))))
    [ ("query_meals", `Assoc []);
      ("record_weight", `Assoc [ ("weight_kg", `Float 70.) ]) ];
  List.iter (fun scopes ->
    Alcotest.(check bool) "Withings forbidden to ledger identity" true
      (is_forbidden
        (Mcp.handle ~withings ~service ~identity:(identity user scopes)
          (call "get_withings_status" (`Assoc [])))))
    [ [ "ledger:read" ]; [ "ledger:write" ] ]

let test_zero_parameter_withings_tools_accept_omitted_arguments () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let withings = withings store in
  List.iter (fun name ->
    let request = `Assoc [ ("jsonrpc", `String "2.0"); ("method", `String "tools/call");
                           ("params", `Assoc [ ("name", `String name) ]) ] in
    Alcotest.(check bool) (name ^ " accepts omitted arguments") true
      (response_has_result (Mcp.handle ~withings ~service
        ~identity:(identity user [ "withings:manage" ]) request)))
    [ "begin_withings_connection"; "get_withings_status"; "disconnect_withings" ]

let test_update_meal_partial_patch_leaves_other_fields_unchanged () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let meal = Result.get_ok (Service.record_meal service ~user (Test_support.meal_input ())) in
  let response =
    Mcp.handle ~service ~identity:(identity user [ "ledger:write" ])
      (call "update_meal" (`Assoc [ ("id", `String (Meal_id.to_string meal.id)); ("confidence", `Float 0.5) ]))
  in
  let updated =
    match result_member "structuredContent" response with
    | Some (`Assoc fields) -> fields
    | _ -> Alcotest.fail "missing structuredContent"
  in
  Alcotest.(check (option string)) "description untouched" (Some meal.description)
    (match List.assoc_opt "description" updated with Some (`String v) -> Some v | _ -> None);
  Alcotest.(check (option int)) "calories untouched" (Some meal.calories_kcal)
    (match List.assoc_opt "calories_kcal" updated with Some (`Int v) -> Some v | _ -> None);
  Alcotest.(check (option (float 0.0001))) "protein untouched" (Some meal.protein_g)
    (match List.assoc_opt "protein_g" updated with Some (`Float v) -> Some v | _ -> None);
  Alcotest.(check (option (float 0.0001))) "confidence updated" (Some 0.5)
    (match List.assoc_opt "confidence" updated with Some (`Float v) -> Some v | _ -> None)

let test_get_daily_totals_defaults_to_today_and_respects_scope () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  ignore (Result.get_ok (Service.record_meal service ~user (Test_support.meal_input ())));
  Alcotest.(check bool) "read allowed with omitted arguments" true
    (Result.is_ok (Mcp.handle ~service ~identity:(identity user [ "ledger:read" ])
      (call "get_daily_totals" (`Assoc []))));
  Alcotest.(check bool) "write forbidden" true
    (is_forbidden (Mcp.handle ~service ~identity:(identity user [ "ledger:write" ])
      (call "get_daily_totals" (`Assoc []))))

let test_get_daily_totals_rejects_a_malformed_date () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  Alcotest.(check bool) "invalid date rejected" true
    (response_has_error_code (-32602)
      (Mcp.handle ~service ~identity:(identity user [ "ledger:read" ])
        (call "get_daily_totals" (`Assoc [ ("date", `String "not-a-date") ]))))

let read_identity user = identity user [ "ledger:read" ]

let test_get_daily_totals_uses_the_requested_timezone () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let eaten_at = Result.get_ok (Time.parse_offset_datetime "2026-07-14T23:30:00Z") in
  ignore (Result.get_ok (Service.record_meal service ~user (Test_support.meal_input ~eaten_at ())));
  let totals arguments =
    match result_member "structuredContent" (Mcp.handle ~service ~identity:(read_identity user) (call "get_daily_totals" (`Assoc arguments))) with
    | Some (`Assoc fields) -> (List.assoc "date" fields, List.assoc "meal_count" fields)
    | _ -> Alcotest.fail "no structured content" in
  Alcotest.(check bool) "utc day" true (totals [ ("date", `String "2026-07-14") ] = (`String "2026-07-14", `Int 1));
  Alcotest.(check bool) "not the next utc day" true (totals [ ("date", `String "2026-07-15") ] = (`String "2026-07-15", `Int 0));
  Alcotest.(check bool) "stockholm day" true
    (totals [ ("date", `String "2026-07-15"); ("timezone", `String "Europe/Stockholm") ] = (`String "2026-07-15", `Int 1))

let test_get_daily_series_returns_every_day_in_one_call () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let eaten_at = Result.get_ok (Time.parse_offset_datetime "2026-07-02T10:00:00Z") in
  ignore (Result.get_ok (Service.record_meal service ~user (Test_support.meal_input ~eaten_at ())));
  let response = Mcp.handle ~service ~identity:(read_identity user)
    (call "get_daily_series" (`Assoc [ ("from_date", `String "2026-07-01"); ("to_date", `String "2026-07-03") ])) in
  (match result_member "structuredContent" response with
  | Some (`Assoc [ ("days", `List days) ]) ->
      Alcotest.(check int) "three days" 3 (List.length days);
      let counts = List.map (function `Assoc f -> List.assoc "meal_count" f | _ -> `Null) days in
      Alcotest.(check bool) "only the middle day has a meal" true (counts = [ `Int 0; `Int 1; `Int 0 ])
  | _ -> Alcotest.fail "unexpected series shape");
  Alcotest.(check bool) "write scope forbidden" true
    (is_forbidden (Mcp.handle ~service ~identity:(identity user [ "ledger:write" ])
      (call "get_daily_series" (`Assoc [ ("from_date", `String "2026-07-01") ]))));
  Alcotest.(check bool) "from_date required" true
    (response_has_error_code (-32602)
      (Mcp.handle ~service ~identity:(read_identity user) (call "get_daily_series" (`Assoc []))));
  Alcotest.(check bool) "bad timezone rejected" true
    (response_has_error_code (-32602)
      (Mcp.handle ~service ~identity:(read_identity user)
        (call "get_daily_series" (`Assoc [ ("from_date", `String "2026-07-01"); ("timezone", `String "Mars/Olympus") ]))))

let test_get_summary_reports_averages_targets_and_trend () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let at value = Result.get_ok (Time.parse_offset_datetime value) in
  ignore (Result.get_ok (Service.record_meal service ~user (Test_support.meal_input ~eaten_at:(at "2026-07-02T10:00:00Z") ())));
  ignore (Result.get_ok (Service.record_manual_weigh_in service ~user Weigh_in.{ measured_at = Some (at "2026-07-01T07:00:00Z"); weight_kg = 80.0 }));
  ignore (Result.get_ok (Service.record_manual_weigh_in service ~user Weigh_in.{ measured_at = Some (at "2026-07-09T07:00:00Z"); weight_kg = 79.2 }));
  let response = Mcp.handle ~service ~identity:(read_identity user)
    (call "get_summary" (`Assoc [ ("from_date", `String "2026-07-01"); ("to_date", `String "2026-07-10"); ("calorie_target", `Int 300) ])) in
  match result_member "structuredContent" response with
  | Some (`Assoc fields) ->
      Alcotest.(check bool) "days" true (List.assoc "days" fields = `Int 10);
      Alcotest.(check bool) "logged days" true (List.assoc "logged_days" fields = `Int 1);
      Alcotest.(check bool) "average" true (List.assoc "avg_calories_kcal" fields = `Float 250.0);
      Alcotest.(check bool) "under target" true (List.assoc "days_at_or_under_calorie_target" fields = `Int 1);
      Alcotest.(check bool) "no protein target" true (List.assoc "days_at_or_over_protein_target" fields = `Null);
      Alcotest.(check bool) "maintenance present" true (List.assoc "implied_maintenance_kcal" fields <> `Null);
      (match List.assoc "weight" fields with
      | `Assoc weight -> Alcotest.(check bool) "weight change" true (match List.assoc "change_kg" weight with `Float v -> abs_float (v +. 0.8) < 1e-9 | _ -> false)
      | _ -> Alcotest.fail "no weight trend")
  | _ -> Alcotest.fail "no structured content"

let test_get_latest_weight_returns_not_found_when_empty () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let response =
    Mcp.handle ~service ~identity:(identity user [ "ledger:read" ]) (call "get_latest_weight" (`Assoc []))
  in
  Alcotest.(check bool) "record not found result" true
    (match result_member "isError" response with Some (`Bool true) -> true | _ -> false)

let test_combined_scopes_allow_their_union () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let scopes = [ "ledger:read"; "ledger:write" ] in
  Alcotest.(check bool) "read allowed" true
    (Result.is_ok (Mcp.handle ~service ~identity:(identity user scopes)
      (call "query_weights" (`Assoc []))));
  Alcotest.(check bool) "write allowed" true
    (Result.is_ok (Mcp.handle ~service ~identity:(identity user scopes)
      (call "record_weight" (`Assoc [ ("weight_kg", `Float 70.) ]))));
  Alcotest.(check bool) "Withings forbidden" true
    (is_forbidden
      (Mcp.handle ~withings:(withings store) ~service ~identity:(identity user scopes)
        (call "get_withings_status" (`Assoc []))))

let test_all_scopes_allow_each_tool_class () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let scopes = [ "ledger:read"; "ledger:write"; "withings:manage" ] in
  List.iter (fun (name, arguments) ->
    Alcotest.(check bool) (name ^ " allowed") true
      (Result.is_ok (Mcp.handle ~withings:(withings store) ~service
        ~identity:(identity user scopes) (call name arguments))))
    [ ("query_meals", `Assoc []);
      ("record_weight", `Assoc [ ("weight_kg", `Float 70.) ]);
      ("get_withings_status", `Assoc []) ]

let test_withings_mcp_uses_configured_oauth_url () =
  let store, user = Test_support.store_with_user () in
  let response = Mcp.handle ~withings:(withings store)
    ~service:(Service.make ~store) ~identity:(identity user [ "withings:manage" ])
    (call "begin_withings_connection" (`Assoc [])) in
  let url =
    match result_member "content" response with
    | Some (`List [ `Assoc content ]) ->
        (match List.assoc_opt "text" content with
        | Some (`String body) ->
            (match Yojson.Safe.from_string body with
            | `Assoc [ (_, `String url) ] -> url
            | _ -> Alcotest.fail "missing authorization URL")
        | _ -> Alcotest.fail "missing text content")
    | _ -> Alcotest.fail "missing MCP result"
  in
  Alcotest.(check (list (pair string (list string)))) "configured OAuth query"
    [ ("response_type", ["code"]); ("client_id", ["mcp-client"]); ("redirect_uri", ["https://kcal.example/mcp-callback"]); ("scope", ["user.metrics"]); ("state", [List.assoc "state" (Uri.query (Uri.of_string url)) |> List.hd]) ]
    (Uri.query (Uri.of_string url))

let test_server_discover_advertises_stateless_protocol () =
  let store, user = Test_support.store_with_user () in
  let request =
    `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int 1);
      ("method", `String "server/discover");
      ("params", `Assoc [
        ("_meta", `Assoc [
          ("io.modelcontextprotocol/protocolVersion", `String "2026-07-28");
          ("io.modelcontextprotocol/clientCapabilities", `Assoc []);
        ]);
      ]);
    ]
  in
  let response =
    Mcp.handle ~service:(Service.make ~store) ~identity:(identity user []) request
  in
  Alcotest.(check (option (list string))) "supported versions"
    (Some [ "2026-07-28"; "2025-03-26" ])
    (match result_member "supportedVersions" response with
    | Some (`List versions) ->
        Some (List.filter_map (function `String version -> Some version | _ -> None) versions)
    | _ -> None);
  Alcotest.(check (option string)) "result type" (Some "complete")
    (match result_member "resultType" response with Some (`String value) -> Some value | _ -> None);
  Alcotest.(check (option string)) "cache scope" (Some "public")
    (match result_member "cacheScope" response with Some (`String value) -> Some value | _ -> None);
  Alcotest.(check bool) "tools capability" true
    (match result_member "capabilities" response with
    | Some (`Assoc capabilities) -> List.mem_assoc "tools" capabilities
    | _ -> false)

let test_modern_results_include_required_metadata () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let tool_list =
    Mcp.handle ~service ~identity:(identity user [])
      (`Assoc [ ("jsonrpc", `String "2.0"); ("id", `Int 1);
                ("method", `String "tools/list") ])
  in
  let tool_call =
    Mcp.handle ~service ~identity:(identity user [ "ledger:read" ])
      (call "query_meals" (`Assoc []))
  in
  List.iter
    (fun (name, response) ->
      Alcotest.(check (option string)) (name ^ " result type") (Some "complete")
        (match result_member "resultType" response with
        | Some (`String value) -> Some value
        | _ -> None))
    [ ("tools/list", tool_list); ("tools/call", tool_call) ];
  Alcotest.(check bool) "tools/list cache ttl" true
    (match result_member "ttlMs" tool_list with Some (`Int ttl) -> ttl > 0 | _ -> false);
  Alcotest.(check (option string)) "tools/list cache scope" (Some "public")
    (match result_member "cacheScope" tool_list with
    | Some (`String value) -> Some value
    | _ -> None)

let test_tool_list_is_exact () =
  let store, user = Test_support.store_with_user () in
  let response = Mcp.handle ~service:(Service.make ~store) ~identity:(identity user [])
    (`Assoc [ ("jsonrpc", `String "2.0"); ("method", `String "tools/list") ]) in
  let names = match response with
    | Ok (`Assoc fields) -> (match List.assoc_opt "result" fields with Some (`Assoc result) -> (match List.assoc_opt "tools" result with Some (`List tools) -> List.filter_map (function `Assoc tool -> (match List.assoc_opt "name" tool with Some (`String name) -> Some name | _ -> None) | _ -> None) tools | _ -> []) | _ -> [])
    | Error `Forbidden -> [] in
  Alcotest.(check (list string)) "approved tools"
    [ "record_meal"; "get_meal"; "query_meals"; "update_meal"; "delete_meal"; "get_daily_totals"; "get_daily_series"; "get_summary"; "record_weight"; "get_weight"; "query_weights"; "update_weight"; "delete_weight"; "get_latest_weight"; "begin_withings_connection"; "get_withings_status"; "disconnect_withings" ] names

let () = Alcotest.run "mcp" [
  ("boundary", [
    Alcotest.test_case "rejects user id" `Quick test_record_meal_rejects_user_id;
    Alcotest.test_case "rejects bad JSON-RPC and provenance" `Quick test_mcp_rejects_bad_jsonrpc_and_provenance_fields;
    Alcotest.test_case "read scope permits reads only" `Quick test_read_scope_allows_reads_only;
    Alcotest.test_case "write scope permits writes only" `Quick test_write_scope_allows_writes_only;
    Alcotest.test_case "update_meal partial patch leaves other fields unchanged" `Quick test_update_meal_partial_patch_leaves_other_fields_unchanged;
    Alcotest.test_case "get_daily_totals defaults to today and respects scope" `Quick test_get_daily_totals_defaults_to_today_and_respects_scope;
    Alcotest.test_case "get_daily_totals rejects a malformed date" `Quick test_get_daily_totals_rejects_a_malformed_date;
    Alcotest.test_case "get_daily_totals uses the requested timezone" `Quick test_get_daily_totals_uses_the_requested_timezone;
    Alcotest.test_case "get_daily_series returns every day in one call" `Quick test_get_daily_series_returns_every_day_in_one_call;
    Alcotest.test_case "get_summary reports averages, targets and trend" `Quick test_get_summary_reports_averages_targets_and_trend;
    Alcotest.test_case "get_latest_weight is not found when empty" `Quick test_get_latest_weight_returns_not_found_when_empty;
    Alcotest.test_case "Withings scope is isolated and requires configuration" `Quick test_withings_scope_isolated_and_requires_configuration;
    Alcotest.test_case "zero-parameter Withings tools accept omitted arguments" `Quick test_zero_parameter_withings_tools_accept_omitted_arguments;
    Alcotest.test_case "combined scopes permit union" `Quick test_combined_scopes_allow_their_union;
    Alcotest.test_case "all scopes permit each tool class" `Quick test_all_scopes_allow_each_tool_class;
    Alcotest.test_case "configured Withings OAuth URL" `Quick test_withings_mcp_uses_configured_oauth_url;
    Alcotest.test_case "discovers stateless protocol" `Quick test_server_discover_advertises_stateless_protocol;
    Alcotest.test_case "modern result metadata" `Quick test_modern_results_include_required_metadata;
    Alcotest.test_case "lists approved tools" `Quick test_tool_list_is_exact;
  ]) ]
