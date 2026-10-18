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

let json = Alcotest.testable Yojson.Safe.pretty_print Yojson.Safe.equal
let read_identity user = identity user [ "ledger:read" ]

let structured_content response =
  match result_member "structuredContent" response with
  | Some (`Assoc fields) -> fields
  | _ -> Alcotest.fail "no structured content"

let field name fields = match List.assoc_opt name fields with Some value -> value | None -> Alcotest.fail ("missing " ^ name)

(* Argument mistakes come back as a tool result with isError, so the model
   sees the explanation and can retry. *)
let tool_error_text response =
  match result_member "isError" response, result_member "content" response with
  | Some (`Bool true), Some (`List [ `Assoc content ]) -> (match List.assoc_opt "text" content with Some (`String text) -> Some text | _ -> None)
  | _ -> None

let contains text fragment =
  let length = String.length fragment in
  let rec scan index = index + length <= String.length text && (String.sub text index length = fragment || scan (index + 1)) in
  scan 0

let check_tool_error message fragment response =
  match tool_error_text response with
  | Some text -> Alcotest.(check bool) (message ^ ": " ^ text) true (contains text fragment)
  | None -> Alcotest.fail (message ^ ": expected an isError tool result")

let test_get_daily_totals_explains_a_malformed_date () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  check_tool_error "invalid date explained" "expected YYYY-MM-DD"
    (Mcp.handle ~service ~identity:(read_identity user) (call "get_daily_totals" (`Assoc [ ("date", `String "not-a-date") ])))

let test_get_daily_totals_uses_the_requested_timezone () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let eaten_at = Result.get_ok (Time.parse_offset_datetime "2026-07-14T23:30:00Z") in
  ignore (Result.get_ok (Service.record_meal service ~user (Test_support.meal_input ~eaten_at ())));
  let totals arguments =
    let fields = structured_content (Mcp.handle ~service ~identity:(read_identity user) (call "get_daily_totals" (`Assoc arguments))) in
    (field "date" fields, field "meal_count" fields, field "calories_kcal" fields) in
  Alcotest.(check (triple json json json)) "utc day" (`String "2026-07-14", `Int 1, `Int 250) (totals [ ("date", `String "2026-07-14") ]);
  Alcotest.(check (triple json json json)) "unlogged day is unknown, not zero" (`String "2026-07-15", `Int 0, `Null)
    (totals [ ("date", `String "2026-07-15") ]);
  Alcotest.(check (triple json json json)) "stockholm day" (`String "2026-07-15", `Int 1, `Int 250)
    (totals [ ("date", `String "2026-07-15"); ("timezone", `String "Europe/Stockholm") ])

let test_get_daily_series_returns_intake_and_weight_per_day () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let at value = Result.get_ok (Time.parse_offset_datetime value) in
  ignore (Result.get_ok (Service.record_meal service ~user (Test_support.meal_input ~eaten_at:(at "2026-07-02T10:00:00Z") ())));
  ignore (Result.get_ok (Service.record_manual_weigh_in service ~user Weigh_in.{ measured_at = Some (at "2026-07-02T07:00:00Z"); weight_kg = 80.0 }));
  ignore (Result.get_ok (Service.record_manual_weigh_in service ~user Weigh_in.{ measured_at = Some (at "2026-07-02T21:00:00Z"); weight_kg = 81.0 }));
  ignore (Result.get_ok (Service.record_manual_weigh_in service ~user Weigh_in.{ measured_at = Some (at "2026-07-03T07:00:00Z"); weight_kg = 79.5 }));
  let fields = structured_content (Mcp.handle ~service ~identity:(read_identity user)
    (call "get_daily_series" (`Assoc [ ("from_date", `String "2026-07-01"); ("to_date", `String "2026-07-03") ]))) in
  Alcotest.(check (list json)) "range echoed" [ `String "2026-07-01"; `String "2026-07-03"; `String "UTC" ]
    [ field "from_date" fields; field "to_date" fields; field "timezone" fields ];
  let days = match field "days" fields with `List days -> List.map (function `Assoc day -> day | _ -> Alcotest.fail "day is not an object") days | _ -> Alcotest.fail "days is not a list" in
  let column name = List.map (field name) days in
  Alcotest.(check (list json)) "dates" [ `String "2026-07-01"; `String "2026-07-02"; `String "2026-07-03" ] (column "date");
  Alcotest.(check (list json)) "meal counts" [ `Int 0; `Int 1; `Int 0 ] (column "meal_count");
  Alcotest.(check (list json)) "calories" [ `Null; `Int 250; `Null ] (column "calories_kcal");
  Alcotest.(check (list json)) "weigh-in counts" [ `Int 0; `Int 2; `Int 1 ] (column "weigh_in_count");
  Alcotest.(check (list json)) "daily mean weight" [ `Null; `Float 80.5; `Float 79.5 ] (column "mean_weight_kg");
  Alcotest.(check bool) "write scope forbidden" true
    (is_forbidden (Mcp.handle ~service ~identity:(identity user [ "ledger:write" ])
      (call "get_daily_series" (`Assoc [ ("from_date", `String "2026-07-01") ]))));
  Alcotest.(check bool) "from_date required" true
    (response_has_error_code (-32602)
      (Mcp.handle ~service ~identity:(read_identity user) (call "get_daily_series" (`Assoc []))))

let test_range_tools_explain_bad_arguments () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let handle name arguments = Mcp.handle ~service ~identity:(read_identity user) (call name (`Assoc arguments)) in
  check_tool_error "bad timezone lists the supported ones" "Europe/Stockholm"
    (handle "get_daily_series" [ ("from_date", `String "2026-07-01"); ("timezone", `String "Mars/Olympus") ]);
  check_tool_error "reversed range" "is after to_date"
    (handle "get_daily_series" [ ("from_date", `String "2026-07-03"); ("to_date", `String "2026-07-01") ]);
  check_tool_error "range too long" "split it into several calls"
    (handle "get_summary" [ ("from_date", `String "2025-01-01"); ("to_date", `String "2026-07-01") ]);
  check_tool_error "nonpositive target" "calorie_target_kcal must be positive"
    (handle "get_summary" [ ("from_date", `String "2026-07-01"); ("to_date", `String "2026-07-02"); ("calorie_target_kcal", `Int 0) ])

let test_get_summary_reports_averages_targets_and_trend () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let at value = Result.get_ok (Time.parse_offset_datetime value) in
  ignore (Result.get_ok (Service.record_meal service ~user (Test_support.meal_input ~eaten_at:(at "2026-07-02T10:00:00Z") ())));
  ignore (Result.get_ok (Service.record_manual_weigh_in service ~user Weigh_in.{ measured_at = Some (at "2026-07-01T07:00:00Z"); weight_kg = 80.0 }));
  ignore (Result.get_ok (Service.record_manual_weigh_in service ~user Weigh_in.{ measured_at = Some (at "2026-07-09T07:00:00Z"); weight_kg = 79.2 }));
  let summary arguments = structured_content (Mcp.handle ~service ~identity:(read_identity user) (call "get_summary" (`Assoc arguments))) in
  let fields = summary [ ("from_date", `String "2026-07-01"); ("to_date", `String "2026-07-10"); ("calorie_target_kcal", `Int 300) ] in
  Alcotest.(check json) "days" (`Int 10) (field "day_count" fields);
  Alcotest.(check json) "logged days" (`Int 1) (field "logged_day_count" fields);
  Alcotest.(check json) "average" (`Float 250.0) (field "avg_calories_kcal" fields);
  Alcotest.(check json) "calorie target echoed with its count" (`Assoc [ ("target_kcal", `Int 300); ("days_at_or_under", `Int 1) ])
    (field "calorie_target" fields);
  Alcotest.(check json) "no protein target" `Null (field "protein_target" fields);
  (match field "weight_trend" fields with
  | `Assoc trend ->
      Alcotest.(check json) "kind" (`String "trend") (field "kind" trend);
      (match field "change_kg" trend with `Float change -> Alcotest.(check (float 1e-9)) "weight change" (-0.8) change | _ -> Alcotest.fail "change_kg")
  | _ -> Alcotest.fail "no weight trend");
  (match field "implied_maintenance" fields with
  | `Assoc maintenance -> (match field "kcal" maintenance with `Float _ -> () | _ -> Alcotest.fail "expected a maintenance estimate")
  | _ -> Alcotest.fail "implied_maintenance is not an object");
  let unlogged = summary [ ("from_date", `String "2026-07-20"); ("to_date", `String "2026-07-21") ] in
  Alcotest.(check json) "maintenance says why it is missing"
    (`Assoc [ ("kcal", `Null); ("unavailable_reason", `String "no_intake_logged") ]) (field "implied_maintenance" unlogged)

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
    Alcotest.test_case "get_daily_totals explains a malformed date" `Quick test_get_daily_totals_explains_a_malformed_date;
    Alcotest.test_case "get_daily_totals uses the requested timezone" `Quick test_get_daily_totals_uses_the_requested_timezone;
    Alcotest.test_case "get_daily_series returns intake and weight per day" `Quick test_get_daily_series_returns_intake_and_weight_per_day;
    Alcotest.test_case "range tools explain bad arguments" `Quick test_range_tools_explain_bad_arguments;
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
