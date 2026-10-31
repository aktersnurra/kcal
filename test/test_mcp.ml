open Base

let identity user scopes = Auth.{ user; scopes }

let call name arguments =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("method", `String "tools/call");
      ("params", `Assoc [ ("name", `String name); ("arguments", arguments) ]);
    ]

let response_has_error_code code = function
  | Ok (`Assoc fields) -> (
      match List.Assoc.find fields ~equal:String.equal "error" with
      | Some (`Assoc error) ->
          Option.equal Yojson.Safe.equal
            (List.Assoc.find error ~equal:String.equal "code")
            (Some (`Int code))
      | _ -> false)
  | _ -> false

let result_member name = function
  | Ok (`Assoc fields) -> (
      match List.Assoc.find fields ~equal:String.equal "result" with
      | Some (`Assoc result) -> List.Assoc.find result ~equal:String.equal name
      | _ -> None)
  | _ -> None

let is_forbidden = function Error `Forbidden -> true | Ok _ -> false

let successful_tool_payload response =
  match
    ( result_member "isError" response,
      result_member "structuredContent" response )
  with
  | (None | Some (`Bool false)), Some (`Assoc _ as payload) -> payload
  | _ -> Alcotest.fail "expected successful tool payload"

let response_has_successful_payload response =
  ignore (successful_tool_payload response);
  true

let test_record_meal_rejects_user_id () =
  Test_support.with_store (fun store user ->
      let request =
        call "record_meal"
          (`Assoc
             [
               ("user_id", `String "other-user");
               ( "items",
                 `List
                   [
                     `Assoc
                       [
                         ("description", `String "Soup");
                         ("calories_kcal", `Int 250);
                         ("protein_g", `Float 12.);
                       ];
                   ] );
             ])
      in
      Alcotest.(check bool)
        "invalid parameters" true
        (response_has_error_code (-32602)
           (Mcp.handle ~service:(Service.make ~store)
              ~identity:(identity user [ "ledger:write" ])
              request)))

let test_mcp_rejects_bad_jsonrpc_and_provenance_fields () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let bad_version =
        `Assoc [ ("jsonrpc", `String "1.0"); ("method", `String "tools/list") ]
      in
      let source_argument =
        call "record_weight"
          (`Assoc [ ("weight_kg", `Float 70.); ("source", `String "withings") ])
      in
      Alcotest.(check bool)
        "version" true
        (response_has_error_code (-32600)
           (Mcp.handle ~service ~identity:(identity user []) bad_version));
      Alcotest.(check bool)
        "provenance" true
        (response_has_error_code (-32602)
           (Mcp.handle ~service
              ~identity:(identity user [ "ledger:write" ])
              source_argument)))

let test_read_scope_allows_reads_only () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      Alcotest.(check bool)
        "read allowed" true
        (response_has_successful_payload
           (Mcp.handle ~service
              ~identity:(identity user [ "ledger:read" ])
              (call "query_meals" (`Assoc []))));
      Alcotest.(check bool)
        "write forbidden" true
        (match
           Mcp.handle ~service
             ~identity:(identity user [ "ledger:read" ])
             (call "record_weight" (`Assoc [ ("weight_kg", `Float 70.) ]))
         with
        | Error `Forbidden -> true
        | _ -> false))

let test_write_scope_allows_writes_only () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      Alcotest.(check bool)
        "write allowed" true
        (response_has_successful_payload
           (Mcp.handle ~service
              ~identity:(identity user [ "ledger:write" ])
              (call "record_weight" (`Assoc [ ("weight_kg", `Float 70.) ]))));
      Alcotest.(check bool)
        "read forbidden" true
        (match
           Mcp.handle ~service
             ~identity:(identity user [ "ledger:write" ])
             (call "query_weights" (`Assoc []))
         with
        | Error `Forbidden -> true
        | _ -> false))

let withings store =
  let oauth =
    Withings_oauth.make ~store ~now:(fun () ->
        Option.value_exn (Ptime.of_float_s 1_800_000_000.))
  in
  Mcp.
    {
      oauth;
      client_id = "mcp-client";
      redirect_uri = "https://kcal.example/mcp-callback";
    }

let test_withings_scope_isolated_and_requires_configuration () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let withings = withings store in
      let managed = identity user [ "withings:manage" ] in
      List.iter
        ~f:(fun name ->
          Alcotest.(check bool)
            (name ^ " unavailable without configuration")
            true
            (response_has_error_code (-32603)
               (Mcp.handle ~service ~identity:managed (call name (`Assoc []))));
          Alcotest.(check bool)
            (name ^ " configured result")
            true
            (response_has_successful_payload
               (Mcp.handle ~withings ~service ~identity:managed
                  (call name (`Assoc [])))))
        [
          "begin_withings_connection";
          "get_withings_status";
          "disconnect_withings";
        ];
      List.iter
        ~f:(fun (name, arguments) ->
          Alcotest.(check bool)
            (name ^ " forbidden to Withings-only identity")
            true
            (is_forbidden
               (Mcp.handle ~service ~identity:managed (call name arguments))))
        [
          ("query_meals", `Assoc []);
          ("record_weight", `Assoc [ ("weight_kg", `Float 70.) ]);
        ];
      List.iter
        ~f:(fun scopes ->
          Alcotest.(check bool)
            "Withings forbidden to ledger identity" true
            (is_forbidden
               (Mcp.handle ~withings ~service ~identity:(identity user scopes)
                  (call "get_withings_status" (`Assoc [])))))
        [ [ "ledger:read" ]; [ "ledger:write" ] ])

let test_zero_parameter_withings_tools_accept_omitted_arguments () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let withings = withings store in
      List.iter
        ~f:(fun name ->
          let request =
            `Assoc
              [
                ("jsonrpc", `String "2.0");
                ("method", `String "tools/call");
                ("params", `Assoc [ ("name", `String name) ]);
              ]
          in
          Alcotest.(check bool)
            (name ^ " accepts omitted arguments")
            true
            (response_has_successful_payload
               (Mcp.handle ~withings ~service
                  ~identity:(identity user [ "withings:manage" ])
                  request)))
        [
          "begin_withings_connection";
          "get_withings_status";
          "disconnect_withings";
        ])

let test_zero_argument_tools () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let withings = withings store in
      let identity =
        identity user [ "ledger:read"; "ledger:write"; "withings:manage" ]
      in
      ignore
        (successful_tool_payload
           (Mcp.handle ~service ~identity
              (call "record_weight" (`Assoc [ ("weight_kg", `Float 70.) ]))));
      List.iter
        ~f:(fun name ->
          let run arguments =
            Mcp.handle ~withings ~service ~identity
              (`Assoc
                 [
                   ("jsonrpc", `String "2.0");
                   ("method", `String "tools/call");
                   ("params", `Assoc (("name", `String name) :: arguments));
                 ])
          in
          Alcotest.(check bool)
            (name ^ " rejects unknown fields")
            true
            (response_has_error_code (-32602)
               (run [ ("arguments", `Assoc [ ("unexpected", `Bool true) ]) ]));
          List.iter
            ~f:(fun arguments ->
              let payload = successful_tool_payload (run arguments) in
              let fields =
                match payload with
                | `Assoc fields -> fields
                | _ -> Alcotest.fail "expected object"
              in
              let member key =
                List.Assoc.find_exn fields ~equal:String.equal key
              in
              match name with
              | "get_context" -> (
                  match member "latest_weight" with
                  | `Assoc weight ->
                      Alcotest.check
                        (Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal)
                        "latest weight" (`Float 70.)
                        (List.Assoc.find_exn weight ~equal:String.equal
                           "weight_kg")
                  | _ -> Alcotest.fail "missing latest weight")
              | "get_withings_status" ->
                  Alcotest.check
                    (Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal)
                    "disconnected status" (`Bool false) (member "connected")
              | "disconnect_withings" ->
                  Alcotest.check
                    (Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal)
                    "disconnect acknowledged" (`Bool true)
                    (member "disconnected")
              | _ -> (
                  match member "authorization_url" with
                  | `String url ->
                      Alcotest.(check (list string))
                        "configured client" [ "mcp-client" ]
                        (List.Assoc.find_exn
                           (Uri.query (Uri.of_string url))
                           ~equal:String.equal "client_id")
                  | _ -> Alcotest.fail "missing authorization URL"))
            [ []; [ ("arguments", `Assoc []) ] ])
        [
          "get_context";
          "begin_withings_connection";
          "get_withings_status";
          "disconnect_withings";
        ])

let test_update_meal_partial_patch_leaves_other_fields_unchanged () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let meal =
        Test_support.only_item
          (Option.value_exn
             (Result.ok
                (Test_support.record_meal_raw service ~user
                   (Test_support.meal_input ()))))
      in
      let response =
        Mcp.handle ~service
          ~identity:(identity user [ "ledger:write" ])
          (call "update_meal_item"
             (`Assoc
                [
                  ("id", `String (Meal_item_id.to_string meal.id));
                  ("confidence", `Float 0.5);
                ]))
      in
      let updated =
        match result_member "structuredContent" response with
        | Some (`Assoc fields) -> (
            match List.Assoc.find fields ~equal:String.equal "items" with
            | Some (`List [ `Assoc item ]) -> item
            | _ -> Alcotest.fail "expected one item")
        | _ -> Alcotest.fail "missing structuredContent"
      in
      Alcotest.(check (option string))
        "description untouched" (Some meal.description)
        (match List.Assoc.find updated ~equal:String.equal "description" with
        | Some (`String v) -> Some v
        | _ -> None);
      Alcotest.(check (option int))
        "calories untouched" (Some meal.calories_kcal)
        (match List.Assoc.find updated ~equal:String.equal "calories_kcal" with
        | Some (`Int v) -> Some v
        | _ -> None);
      Alcotest.(check (option (float 0.0001)))
        "protein untouched" (Some meal.protein_g)
        (match List.Assoc.find updated ~equal:String.equal "protein_g" with
        | Some (`Float v) -> Some v
        | _ -> None);
      Alcotest.(check (option (float 0.0001)))
        "confidence updated" (Some 0.5)
        (match List.Assoc.find updated ~equal:String.equal "confidence" with
        | Some (`Float v) -> Some v
        | _ -> None))

let test_get_context_respects_scope () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      Alcotest.(check bool)
        "read allowed with omitted arguments" true
        (response_has_successful_payload
           (Mcp.handle ~service
              ~identity:(identity user [ "ledger:read" ])
              (call "get_context" (`Assoc []))));
      Alcotest.(check bool)
        "write forbidden" true
        (is_forbidden
           (Mcp.handle ~service
              ~identity:(identity user [ "ledger:write" ])
              (call "get_context" (`Assoc [])))))

let json =
  Alcotest.testable (Yojson.Safe.pretty_print ?std:None) Yojson.Safe.equal

let read_identity user = identity user [ "ledger:read" ]

let structured_content response =
  match result_member "structuredContent" response with
  | Some (`Assoc fields) -> fields
  | _ -> Alcotest.fail "no structured content"

let field name fields =
  match List.Assoc.find fields ~equal:String.equal name with
  | Some value -> value
  | None -> Alcotest.fail ("missing " ^ name)

(* Argument mistakes come back as a tool result with isError, so the model
   sees the explanation and can retry. *)
let tool_error_text response =
  match
    (result_member "isError" response, result_member "content" response)
  with
  | Some (`Bool true), Some (`List [ `Assoc content ]) -> (
      match List.Assoc.find content ~equal:String.equal "text" with
      | Some (`String text) -> Some text
      | _ -> None)
  | _ -> None

let contains text fragment =
  let length = String.length fragment in
  let rec scan index =
    index + length <= String.length text
    && (String.equal (String.sub text ~pos:index ~len:length) fragment
       || scan (index + 1))
  in
  scan 0

let check_tool_error message fragment response =
  match tool_error_text response with
  | Some text ->
      Alcotest.(check bool)
        (message ^ ": " ^ text)
        true (contains text fragment)
  | None -> Alcotest.fail (message ^ ": expected an isError tool result")

let test_range_tools_explain_a_malformed_date () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      check_tool_error "invalid date explained" "expected YYYY-MM-DD"
        (Mcp.handle ~service ~identity:(read_identity user)
           (call "get_daily_series"
              (`Assoc [ ("from_date", `String "not-a-date") ]))))

let test_daily_series_uses_the_requested_timezone () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let eaten_at =
        Option.value_exn
          (Result.ok (Time.parse_offset_datetime "2026-07-14T23:30:00Z"))
      in
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.record_meal_raw service ~user
                 (Test_support.meal_input ~eaten_at ()))));
      let totals arguments =
        match
          field "days"
            (structured_content
               (Mcp.handle ~service ~identity:(read_identity user)
                  (call "get_daily_series" (`Assoc arguments))))
        with
        | `List [ `Assoc fields ] ->
            ( field "date" fields,
              field "meal_count" fields,
              field "calories_kcal" fields )
        | _ -> Alcotest.fail "expected one day"
      in
      let day date =
        [ ("from_date", `String date); ("to_date", `String date) ]
      in
      Alcotest.(check (triple json json json))
        "utc day"
        (`String "2026-07-14", `Int 1, `Int 250)
        (totals (day "2026-07-14"));
      Alcotest.(check (triple json json json))
        "unlogged day is unknown, not zero"
        (`String "2026-07-15", `Int 0, `Null)
        (totals (day "2026-07-15"));
      Alcotest.(check (triple json json json))
        "stockholm day"
        (`String "2026-07-15", `Int 1, `Int 250)
        (totals (("timezone", `String "Europe/Stockholm") :: day "2026-07-15"));
      ignore
        (successful_tool_payload
           (Mcp.handle ~service
              ~identity:(identity user [ "ledger:write" ])
              (call "set_goals"
                 (`Assoc
                    [
                      ("timezone", `String "Europe/Stockholm");
                      ("weekday_calorie_target_kcal", `Int 1750);
                      ("weekend_calorie_target_kcal", `Int 1950);
                    ]))));
      Alcotest.(check (triple json json json))
        "goals timezone is the default"
        (`String "2026-07-15", `Int 1, `Int 250)
        (totals (day "2026-07-15")))

let test_get_daily_series_returns_intake_and_weight_per_day () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let at value =
        Option.value_exn (Result.ok (Time.parse_offset_datetime value))
      in
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.record_meal_raw service ~user
                 (Test_support.meal_input
                    ~eaten_at:(at "2026-07-02T10:00:00Z")
                    ()))));
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.record_manual_weigh_in_raw service ~user
                 Weigh_in.
                   {
                     measured_at = Some (at "2026-07-02T07:00:00Z");
                     weight_kg = 80.0;
                   })));
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.record_manual_weigh_in_raw service ~user
                 Weigh_in.
                   {
                     measured_at = Some (at "2026-07-02T21:00:00Z");
                     weight_kg = 81.0;
                   })));
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.record_manual_weigh_in_raw service ~user
                 Weigh_in.
                   {
                     measured_at = Some (at "2026-07-03T07:00:00Z");
                     weight_kg = 79.5;
                   })));
      let fields =
        structured_content
          (Mcp.handle ~service ~identity:(read_identity user)
             (call "get_daily_series"
                (`Assoc
                   [
                     ("from_date", `String "2026-07-01");
                     ("to_date", `String "2026-07-03");
                   ])))
      in
      Alcotest.(check (list json))
        "range echoed"
        [ `String "2026-07-01"; `String "2026-07-03"; `String "UTC" ]
        [
          field "from_date" fields;
          field "to_date" fields;
          field "timezone" fields;
        ];
      let days =
        match field "days" fields with
        | `List days ->
            List.map
              ~f:(function
                | `Assoc day -> day | _ -> Alcotest.fail "day is not an object")
              days
        | _ -> Alcotest.fail "days is not a list"
      in
      let column name = List.map ~f:(field name) days in
      Alcotest.(check (list json))
        "dates"
        [ `String "2026-07-01"; `String "2026-07-02"; `String "2026-07-03" ]
        (column "date");
      Alcotest.(check (list json))
        "meal counts"
        [ `Int 0; `Int 1; `Int 0 ]
        (column "meal_count");
      Alcotest.(check (list json))
        "calories"
        [ `Null; `Int 250; `Null ]
        (column "calories_kcal");
      Alcotest.(check (list json))
        "weigh-in counts"
        [ `Int 0; `Int 2; `Int 1 ]
        (column "weigh_in_count");
      Alcotest.(check (list json))
        "daily mean weight"
        [ `Null; `Float 80.5; `Float 79.5 ]
        (column "mean_weight_kg");
      Alcotest.(check bool)
        "write scope forbidden" true
        (is_forbidden
           (Mcp.handle ~service
              ~identity:(identity user [ "ledger:write" ])
              (call "get_daily_series"
                 (`Assoc [ ("from_date", `String "2026-07-01") ]))));
      Alcotest.(check bool)
        "from_date required" true
        (response_has_error_code (-32602)
           (Mcp.handle ~service ~identity:(read_identity user)
              (call "get_daily_series" (`Assoc [])))))

let test_range_tools_explain_bad_arguments () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let handle name arguments =
        Mcp.handle ~service ~identity:(read_identity user)
          (call name (`Assoc arguments))
      in
      check_tool_error "bad timezone lists the supported ones"
        "Europe/Stockholm"
        (handle "get_daily_series"
           [
             ("from_date", `String "2026-07-01");
             ("timezone", `String "Mars/Olympus");
           ]);
      check_tool_error "reversed range" "is after to_date"
        (handle "get_daily_series"
           [
             ("from_date", `String "2026-07-03");
             ("to_date", `String "2026-07-01");
           ]);
      check_tool_error "range too long" "split it into several calls"
        (handle "get_summary"
           [
             ("from_date", `String "2025-01-01");
             ("to_date", `String "2026-07-01");
           ]);
      check_tool_error "nonpositive target"
        "weekday_calorie_target_kcal must be positive"
        (Mcp.handle ~service
           ~identity:(identity user [ "ledger:write" ])
           (call "set_goals"
              (`Assoc
                 [
                   ("timezone", `String "UTC");
                   ("weekday_calorie_target_kcal", `Int 0);
                   ("weekend_calorie_target_kcal", `Int 1950);
                 ]))))

let test_get_summary_reports_averages_targets_and_trend () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let at value =
        Option.value_exn (Result.ok (Time.parse_offset_datetime value))
      in
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.record_meal_raw service ~user
                 (Test_support.meal_input
                    ~eaten_at:(at "2026-07-02T10:00:00Z")
                    ()))));
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.record_manual_weigh_in_raw service ~user
                 Weigh_in.
                   {
                     measured_at = Some (at "2026-07-01T07:00:00Z");
                     weight_kg = 80.0;
                   })));
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.record_manual_weigh_in_raw service ~user
                 Weigh_in.
                   {
                     measured_at = Some (at "2026-07-09T07:00:00Z");
                     weight_kg = 79.2;
                   })));
      let summary arguments =
        structured_content
          (Mcp.handle ~service ~identity:(read_identity user)
             (call "get_summary" (`Assoc arguments)))
      in
      ignore
        (successful_tool_payload
           (Mcp.handle ~service
              ~identity:(identity user [ "ledger:write" ])
              (call "set_goals"
                 (`Assoc
                    [
                      ("timezone", `String "UTC");
                      ("weekday_calorie_target_kcal", `Int 300);
                      ("weekend_calorie_target_kcal", `Int 400);
                      ("target_weight_kg", `Int 70);
                    ]))));
      let fields =
        summary
          [
            ("from_date", `String "2026-07-01");
            ("to_date", `String "2026-07-10");
          ]
      in
      Alcotest.(check json) "days" (`Int 10) (field "day_count" fields);
      Alcotest.(check json)
        "logged days" (`Int 1)
        (field "logged_day_count" fields);
      Alcotest.(check json)
        "average" (`Float 250.0)
        (field "avg_calories_kcal" fields);
      Alcotest.(check json)
        "weekday target met, 50 kcal under"
        (`Assoc
           [
             ("days_at_or_under", `Int 1); ("avg_vs_target_kcal", `Float (-50.));
           ])
        (field "calorie_target" fields);
      (match field "target_weight" fields with
      | `Assoc target -> (
          match
            (field "remaining_kg" target, field "weeks_to_target" target)
          with
          | `Float remaining, `Float weeks ->
              Alcotest.(check (float 1e-9)) "remaining" 9.2 remaining;
              Alcotest.(check (float 1e-6))
                "weeks at 0.7 kg/week" (9.2 /. 0.7) weeks
          | _ -> Alcotest.fail "target weight fields")
      | _ -> Alcotest.fail "no target weight");
      (match field "previous_period" fields with
      | `Assoc previous ->
          Alcotest.(check (pair json json))
            "previous period is the ten days before"
            (`String "2026-06-21", `String "2026-06-30")
            (field "from_date" previous, field "to_date" previous);
          Alcotest.(check json)
            "nothing logged then" (`Int 0)
            (field "logged_day_count" previous)
      | _ -> Alcotest.fail "no previous period");
      Alcotest.(check json)
        "no protein target" `Null
        (field "protein_target" fields);
      (match field "weight_trend" fields with
      | `Assoc trend -> (
          Alcotest.(check json) "kind" (`String "trend") (field "kind" trend);
          match field "change_kg" trend with
          | `Float change ->
              Alcotest.(check (float 1e-9)) "weight change" (-0.8) change
          | _ -> Alcotest.fail "change_kg")
      | _ -> Alcotest.fail "no weight trend");
      (match field "implied_maintenance" fields with
      | `Assoc maintenance -> (
          match field "kcal" maintenance with
          | `Float _ -> ()
          | _ -> Alcotest.fail "expected a maintenance estimate")
      | _ -> Alcotest.fail "implied_maintenance is not an object");
      let unlogged =
        summary
          [
            ("from_date", `String "2026-07-20");
            ("to_date", `String "2026-07-21");
          ]
      in
      Alcotest.(check json)
        "maintenance says why it is missing"
        (`Assoc
           [
             ("kcal", `Null); ("unavailable_reason", `String "no_intake_logged");
           ])
        (field "implied_maintenance" unlogged))

let test_get_context_is_empty_without_records () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let fields =
        structured_content
          (Mcp.handle ~service
             ~identity:(identity user [ "ledger:read" ])
             (call "get_context" (`Assoc [])))
      in
      Alcotest.(check (list json))
        "no goals, weight or window"
        [ `Null; `Null; `Null; `String "UTC" ]
        [
          field "goals" fields;
          field "latest_weight" fields;
          field "eating_window" fields;
          field "timezone" fields;
        ])

let test_combined_scopes_allow_their_union () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let scopes = [ "ledger:read"; "ledger:write" ] in
      Alcotest.(check bool)
        "read allowed" true
        (response_has_successful_payload
           (Mcp.handle ~service ~identity:(identity user scopes)
              (call "query_weights" (`Assoc []))));
      Alcotest.(check bool)
        "write allowed" true
        (response_has_successful_payload
           (Mcp.handle ~service ~identity:(identity user scopes)
              (call "record_weight" (`Assoc [ ("weight_kg", `Float 70.) ]))));
      Alcotest.(check bool)
        "Withings forbidden" true
        (is_forbidden
           (Mcp.handle ~withings:(withings store) ~service
              ~identity:(identity user scopes)
              (call "get_withings_status" (`Assoc [])))))

let test_all_scopes_allow_each_tool_class () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let scopes = [ "ledger:read"; "ledger:write"; "withings:manage" ] in
      List.iter
        ~f:(fun (name, arguments) ->
          Alcotest.(check bool)
            (name ^ " allowed") true
            (response_has_successful_payload
               (Mcp.handle ~withings:(withings store) ~service
                  ~identity:(identity user scopes) (call name arguments))))
        [
          ("query_meals", `Assoc []);
          ("record_weight", `Assoc [ ("weight_kg", `Float 70.) ]);
          ("get_withings_status", `Assoc []);
        ])

let test_disconnected_withings_status_is_success () =
  Test_support.with_store (fun store user ->
      let response =
        Mcp.handle ~withings:(withings store) ~service:(Service.make ~store)
          ~identity:(identity user [ "withings:manage" ])
          (call "get_withings_status" (`Assoc []))
      in
      Alcotest.(check bool)
        "ordinary disconnected status is not a tool error" true
        (match result_member "isError" response with
        | Some (`Bool true) -> false
        | _ -> true);
      let content =
        match result_member "content" response with
        | Some (`List [ `Assoc fields ]) -> (
            match List.Assoc.find fields ~equal:String.equal "text" with
            | Some (`String body) -> Yojson.Safe.from_string body
            | _ -> Alcotest.fail "missing text")
        | _ -> Alcotest.fail "missing successful status content"
      in
      Alcotest.check json "explicit disconnected payload"
        (`Assoc
           [
             ("connected", `Bool false);
             ("requires_reauthorization", `Bool false);
             ("token_expires_at", `Null);
           ])
        content)

let test_withings_mcp_uses_configured_oauth_url () =
  Test_support.with_store (fun store user ->
      let response =
        Mcp.handle ~withings:(withings store) ~service:(Service.make ~store)
          ~identity:(identity user [ "withings:manage" ])
          (call "begin_withings_connection" (`Assoc []))
      in
      let url =
        match result_member "content" response with
        | Some (`List [ `Assoc content ]) -> (
            match List.Assoc.find content ~equal:String.equal "text" with
            | Some (`String body) -> (
                match Yojson.Safe.from_string body with
                | `Assoc [ (_, `String url) ] -> url
                | _ -> Alcotest.fail "missing authorization URL")
            | _ -> Alcotest.fail "missing text content")
        | _ -> Alcotest.fail "missing MCP result"
      in
      Alcotest.(check (list (pair string (list string))))
        "configured OAuth query"
        [
          ("response_type", [ "code" ]);
          ("client_id", [ "mcp-client" ]);
          ("redirect_uri", [ "https://kcal.example/mcp-callback" ]);
          ("scope", [ "user.metrics" ]);
          ( "state",
            [
              List.Assoc.find_exn
                (Uri.query (Uri.of_string url))
                ~equal:String.equal "state"
              |> List.hd_exn;
            ] );
        ]
        (Uri.query (Uri.of_string url)))

let test_server_discover_advertises_stateless_protocol () =
  Test_support.with_store (fun store user ->
      let request =
        `Assoc
          [
            ("jsonrpc", `String "2.0");
            ("id", `Int 1);
            ("method", `String "server/discover");
            ( "params",
              `Assoc
                [
                  ( "_meta",
                    `Assoc
                      [
                        ( "io.modelcontextprotocol/protocolVersion",
                          `String "2026-07-28" );
                        ("io.modelcontextprotocol/clientCapabilities", `Assoc []);
                      ] );
                ] );
          ]
      in
      let response =
        Mcp.handle ~service:(Service.make ~store) ~identity:(identity user [])
          request
      in
      Alcotest.(check (option (list string)))
        "supported versions"
        (Some [ "2026-07-28"; "2025-03-26" ])
        (match result_member "supportedVersions" response with
        | Some (`List versions) ->
            Some
              (List.filter_map
                 ~f:(function `String version -> Some version | _ -> None)
                 versions)
        | _ -> None);
      Alcotest.(check (option string))
        "result type" (Some "complete")
        (match result_member "resultType" response with
        | Some (`String value) -> Some value
        | _ -> None);
      Alcotest.(check (option string))
        "cache scope" (Some "public")
        (match result_member "cacheScope" response with
        | Some (`String value) -> Some value
        | _ -> None);
      Alcotest.(check bool)
        "tools capability" true
        (match result_member "capabilities" response with
        | Some (`Assoc capabilities) ->
            List.Assoc.mem capabilities ~equal:String.equal "tools"
        | _ -> false))

let test_modern_results_include_required_metadata () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let tool_list =
        Mcp.handle ~service ~identity:(identity user [])
          (`Assoc
             [
               ("jsonrpc", `String "2.0");
               ("id", `Int 1);
               ("method", `String "tools/list");
             ])
      in
      let tool_call =
        Mcp.handle ~service
          ~identity:(identity user [ "ledger:read" ])
          (call "query_meals" (`Assoc []))
      in
      List.iter
        ~f:(fun (name, response) ->
          Alcotest.(check (option string))
            (name ^ " result type") (Some "complete")
            (match result_member "resultType" response with
            | Some (`String value) -> Some value
            | _ -> None))
        [ ("tools/list", tool_list); ("tools/call", tool_call) ];
      Alcotest.(check bool)
        "tools/list cache ttl" true
        (match result_member "ttlMs" tool_list with
        | Some (`Int ttl) -> ttl > 0
        | _ -> false);
      Alcotest.(check (option string))
        "tools/list cache scope" (Some "public")
        (match result_member "cacheScope" tool_list with
        | Some (`String value) -> Some value
        | _ -> None))

let test_tool_list_is_exact () =
  Test_support.with_store (fun store user ->
      let response =
        Mcp.handle ~service:(Service.make ~store) ~identity:(identity user [])
          (`Assoc
             [ ("jsonrpc", `String "2.0"); ("method", `String "tools/list") ])
      in
      let names =
        match response with
        | Ok (`Assoc fields) -> (
            match List.Assoc.find fields ~equal:String.equal "result" with
            | Some (`Assoc result) -> (
                match List.Assoc.find result ~equal:String.equal "tools" with
                | Some (`List tools) ->
                    List.filter_map
                      ~f:(function
                        | `Assoc tool -> (
                            match
                              List.Assoc.find tool ~equal:String.equal "name"
                            with
                            | Some (`String name) -> Some name
                            | _ -> None)
                        | _ -> None)
                      tools
                | _ -> [])
            | _ -> [])
        | _ -> []
      in
      Alcotest.(check (list string))
        "approved tools"
        [
          "get_context";
          "set_goals";
          "save_product";
          "search_products";
          "correct_meals_from_product";
          "record_meal";
          "get_meal";
          "query_meals";
          "update_meal";
          "delete_meal";
          "add_meal_item";
          "update_meal_item";
          "delete_meal_item";
          "get_daily_series";
          "get_summary";
          "record_weight";
          "get_weight";
          "query_weights";
          "update_weight";
          "delete_weight";
          "begin_withings_connection";
          "get_withings_status";
          "disconnect_withings";
        ]
        names)

let product_arguments =
  [
    ("name", `String "Whey 100");
    ("basis", `String "40 g");
    ("calories_kcal", `Int 150);
    ("protein_g", `Int 31);
    ("source", `String "label");
  ]

let test_product_schemas () =
  Test_support.with_store (fun store user ->
      let response =
        Mcp.handle ~service:(Service.make ~store) ~identity:(identity user [])
          (`Assoc
             [ ("jsonrpc", `String "2.0"); ("method", `String "tools/list") ])
      in
      let tools =
        match result_member "tools" response with
        | Some (`List tools) -> tools
        | _ -> Alcotest.fail "tools"
      in
      let schema name =
        let tool =
          List.find_exn
            ~f:(function
              | `Assoc fields ->
                  Yojson.Safe.equal (field "name" fields) (`String name)
              | _ -> false)
            tools
        in
        match tool with
        | `Assoc fields -> (
            match field "inputSchema" fields with
            | `Assoc s -> s
            | _ -> assert false)
        | _ -> assert false
      in
      List.iter
        ~f:(fun (name, required, properties) ->
          let s = schema name in
          Alcotest.(check json)
            "required"
            (`List (List.map ~f:(fun s -> `String s) required))
            (field "required" s);
          Alcotest.(check json)
            "strict" (`Bool false)
            (field "additionalProperties" s);
          let props =
            match field "properties" s with `Assoc p -> p | _ -> assert false
          in
          Alcotest.(check (list string))
            "properties"
            (List.sort properties ~compare:String.compare)
            (List.sort (List.map ~f:fst props) ~compare:String.compare);
          List.iter
            ~f:(fun (_, p) ->
              match p with
              | `Assoc fields -> ignore (field "description" fields)
              | _ -> Alcotest.fail "property")
            props)
        [
          ( "save_product",
            [ "name"; "basis"; "calories_kcal"; "protein_g"; "source" ],
            [
              "id";
              "name";
              "basis";
              "calories_kcal";
              "protein_g";
              "carbs_g";
              "fat_g";
              "basis_quantity";
              "basis_unit";
              "source";
            ] );
          ("search_products", [ "query" ], [ "query"; "limit" ]);
          ("query_meals", [], [ "from"; "to"; "limit"; "search" ]);
          ("query_weights", [], [ "from"; "to"; "limit" ]);
        ];
      List.iter
        ~f:(fun (name, key) ->
          let properties =
            match field "properties" (schema name) with
            | `Assoc p -> p
            | _ -> assert false
          in
          let choices =
            match field key properties with
            | `Assoc p -> field "oneOf" p
            | _ -> assert false
          in
          Alcotest.(check json)
            "string or nonempty string array advertised"
            (`List
               [
                 `Assoc [ ("type", `String "string") ];
                 `Assoc
                   [
                     ("type", `String "array");
                     ("items", `Assoc [ ("type", `String "string") ]);
                     ("minItems", `Int 1);
                   ];
               ])
            choices)
        [ ("search_products", "query"); ("query_meals", "search") ])

let test_product_save_search_replace () =
  Test_support.with_two_users (fun store user other ->
      let service = Service.make ~store in
      let handle user name args =
        Mcp.handle ~service
          ~identity:(identity user [ "ledger:read"; "ledger:write" ])
          (call name (`Assoc args))
      in
      let saved =
        structured_content
          (handle user "save_product"
             (("carbs_g", `Float 2.) :: product_arguments))
      in
      List.iter
        ~f:(fun (name, want) ->
          Alcotest.(check json) name want (field name saved))
        [
          ("name", `String "Whey 100");
          ("basis", `String "40 g");
          ("calories_kcal", `Int 150);
          ("protein_g", `Float 31.);
          ("source", `String "label");
          ("carbs_g", `Float 2.);
          ("fat_g", `Null);
        ];
      Alcotest.(check bool)
        "no ownership" false
        (List.Assoc.mem saved ~equal:String.equal "user_id");
      List.iter
        ~f:(fun name ->
          match field name saved with
          | `String s ->
              ignore
                (Option.value_exn (Result.ok (Time.parse_offset_datetime s)))
          | _ -> Alcotest.fail name)
        [ "created_at"; "updated_at" ];
      let id = field "id" saved in
      let search user =
        field "products"
          (structured_content
             (handle user "search_products" [ ("query", `String "WHEY") ]))
      in
      Alcotest.(check json)
        "search full reference"
        (`List [ `Assoc saved ])
        (search user);
      Alcotest.(check json) "owner isolation" (`List []) (search other);
      check_tool_error "foreign replacement" "record not found"
        (handle other "save_product" (("id", id) :: product_arguments));
      check_tool_error "unknown replacement" "record not found"
        (handle user "save_product"
           (("id", `String (Product_id.to_string (Product_id.fresh ())))
           :: product_arguments));
      let replaced =
        structured_content
          (handle user "save_product" (("id", id) :: product_arguments))
      in
      Alcotest.(check json) "id preserved" id (field "id" replaced);
      Alcotest.(check json)
        "creation preserved" (field "created_at" saved)
        (field "created_at" replaced);
      List.iter
        ~f:(fun name ->
          Alcotest.(check json)
            "omitted macros cleared" `Null (field name replaced))
        [ "carbs_g"; "fat_g" ];
      Alcotest.(check json)
        "replacement searchable"
        (`List [ `Assoc replaced ])
        (search user))

let omitted_call name =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("method", `String "tools/call");
      ("params", `Assoc [ ("name", `String name) ]);
    ]

let test_product_argument_validation_and_scopes () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let handle scopes request =
        Mcp.handle ~service ~identity:(identity user scopes) request
      in
      let invalid name args =
        Alcotest.(check bool)
          (name ^ " Invalid_input remains -32602")
          true
          (response_has_error_code (-32602)
             (handle
                [ "ledger:read"; "ledger:write" ]
                (call name (`Assoc args))))
      in
      List.iter
        ~f:(fun name ->
          let args =
            if String.equal name "save_product" then product_arguments
            else [ ("query", `String "whey") ]
          in
          List.iter
            ~f:(fun key -> invalid name ((key, `String "forbidden") :: args))
            [ "user_id"; "created_at"; "updated_at"; "unknown" ];
          List.iter
            ~f:(fun request ->
              Alcotest.(check bool)
                "scope before parsing" true
                (is_forbidden (handle [] request)))
            [ call name (`Assoc args); omitted_call name ];
          Alcotest.(check bool)
            "omitted required arguments" true
            (response_has_error_code (-32602)
               (handle [ "ledger:read"; "ledger:write" ] (omitted_call name))))
        [ "save_product"; "search_products" ];
      List.iter
        ~f:(fun key ->
          invalid "save_product"
            (List.Assoc.remove product_arguments ~equal:String.equal key))
        [ "name"; "basis"; "calories_kcal"; "protein_g"; "source" ];
      List.iter
        ~f:(fun id -> invalid "save_product" (("id", id) :: product_arguments))
        [ `String "bad"; `Int 1; `Null ];
      List.iter
        ~f:(fun (key, value) ->
          invalid "save_product"
            ((key, value)
            :: List.Assoc.remove product_arguments ~equal:String.equal key))
        [
          ("name", `String " ");
          ("basis", `String "");
          ("source", `String "");
          ("calories_kcal", `Int (-1));
          ("protein_g", `Float (-1.));
          ("carbs_g", `Null);
          ("fat_g", `Float (-1.));
        ];
      invalid "search_products" [];
      List.iter
        ~f:(fun value -> invalid "search_products" [ ("query", value) ])
        [ `String " "; `Int 1; `Null ];
      List.iter
        ~f:(fun value ->
          invalid "search_products"
            [ ("query", `String "whey"); ("limit", value) ])
        [ `Int 0; `Int 501; `Int (-1); `String "2"; `Null ];
      Alcotest.(check bool)
        "read cannot save" true
        (is_forbidden
           (handle [ "ledger:read" ]
              (call "save_product" (`Assoc product_arguments))));
      Alcotest.(check bool)
        "write cannot search" true
        (is_forbidden
           (handle [ "ledger:write" ]
              (call "search_products" (`Assoc [ ("query", `String "whey") ]))));
      Alcotest.(check json)
        "invalid and unauthorized calls created nothing" (`List [])
        (field "products"
           (structured_content
              (handle [ "ledger:read" ]
                 (call "search_products" (`Assoc [ ("query", `String "whey") ])))));
      for _ = 1 to 101 do
        ignore
          (structured_content
             (handle [ "ledger:write" ]
                (call "save_product" (`Assoc product_arguments))))
      done;
      List.iter
        ~f:(fun (args, count) ->
          let products =
            field "products"
              (structured_content
                 (handle [ "ledger:read" ]
                    (call "search_products"
                       (`Assoc (("query", `String "whey") :: args)))))
          in
          match products with
          | `List rows -> Alcotest.(check int) "limit" count (List.length rows)
          | _ -> Alcotest.fail "products")
        [
          ([], 100); ([ ("limit", `Int 1) ], 1); ([ ("limit", `Int 500) ], 101);
        ])

let test_query_meals_search_composes_with_dates_and_limit () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      List.iter
        ~f:(fun (date, description) ->
          let eaten_at =
            Option.value_exn (Result.ok (Time.parse_offset_datetime date))
          in
          let input =
            { (Test_support.meal_input ~eaten_at ()) with description }
          in
          ignore
            (Option.value_exn
               (Result.ok (Test_support.record_meal_raw service ~user input))))
        [
          ("2026-07-01T10:00:00Z", "Whey old");
          ("2026-07-02T10:00:00Z", "Soup");
          ("2026-07-03T10:00:00Z", "Whey first");
          ("2026-07-04T10:00:00Z", "Whey second");
          ("2026-07-05T10:00:00Z", "Whey late");
        ];
      let handle name args =
        Mcp.handle ~service ~identity:(read_identity user)
          (call name (`Assoc args))
      in
      let args =
        [
          ("from", `String "2026-07-02T00:00:00Z");
          ("to", `String "2026-07-04T23:59:59Z");
          ("limit", `Int 1);
        ]
      in
      let descriptions response =
        match field "meals" (structured_content response) with
        | `List rows ->
            List.map
              ~f:(function
                | `Assoc row -> (
                    match field "items" row with
                    | `List [ `Assoc item ] -> field "description" item
                    | _ -> assert false)
                | _ -> assert false)
              rows
        | _ -> assert false
      in
      Alcotest.(check (list json))
        "filter before limit with bounds"
        [ `String "Whey first" ]
        (descriptions
           (handle "query_meals" (("search", `String "WHEY") :: args)));
      Alcotest.(check (list json))
        "omitted search unchanged"
        [ `String "Soup" ]
        (descriptions (handle "query_meals" args));
      List.iter
        ~f:(fun value ->
          Alcotest.(check bool)
            "invalid meal search" true
            (response_has_error_code (-32602)
               (handle "query_meals" [ ("search", value) ])))
        [ `String " "; `Int 1; `Null ];
      Alcotest.(check bool)
        "weight search rejected" true
        (response_has_error_code (-32602)
           (handle "query_weights" [ ("search", `String "whey") ])))

let test_calendar_endpoint_mcp date timezone () =
  let open Base in
  Test_support.with_store (fun store user ->
      (* Any accidental query would now fail instead of yielding a calendar error. *)
      Alcotest.(check bool)
        "remove query tables" true
        (match
           Sqlite3.exec
             (Store_sqlite.For_testing.raw_database store)
             "DROP TABLE meals; DROP TABLE weigh_ins"
         with
        | Sqlite3.Rc.OK -> true
        | _ -> false);
      List.iter [ "get_daily_series"; "get_summary" ] ~f:(fun name ->
          let dates =
            [ ("from_date", `String date); ("to_date", `String date) ]
          in
          let response =
            Mcp.handle ~service:(Service.make ~store)
              ~identity:(identity user [ "ledger:read" ])
              (call name (`Assoc (("timezone", `String timezone) :: dates)))
          in
          check_tool_error "calendar error before queries" "calendar limit:"
            response;
          check_tool_error "requested date explained" date response;
          check_tool_error "requested timezone explained" timezone response;
          check_tool_error "actionable alternative" "choose a date" response))

let () =
  Alcotest.run "mcp"
    [
      ( "boundary",
        [
          Alcotest.test_case "upper calendar endpoint MCP" `Quick
            (test_calendar_endpoint_mcp "9999-12-31" "UTC");
          Alcotest.test_case "lower calendar endpoint MCP" `Quick
            (test_calendar_endpoint_mcp "0000-01-01" "+02:00");
          Alcotest.test_case "product schemas" `Quick test_product_schemas;
          Alcotest.test_case "product save search replace" `Quick
            test_product_save_search_replace;
          Alcotest.test_case "product validation and scopes" `Quick
            test_product_argument_validation_and_scopes;
          Alcotest.test_case "meal search dates and limit" `Quick
            test_query_meals_search_composes_with_dates_and_limit;
          Alcotest.test_case "rejects user id" `Quick
            test_record_meal_rejects_user_id;
          Alcotest.test_case "rejects bad JSON-RPC and provenance" `Quick
            test_mcp_rejects_bad_jsonrpc_and_provenance_fields;
          Alcotest.test_case "read scope permits reads only" `Quick
            test_read_scope_allows_reads_only;
          Alcotest.test_case "write scope permits writes only" `Quick
            test_write_scope_allows_writes_only;
          Alcotest.test_case
            "update_meal partial patch leaves other fields unchanged" `Quick
            test_update_meal_partial_patch_leaves_other_fields_unchanged;
          Alcotest.test_case "get_context respects scope" `Quick
            test_get_context_respects_scope;
          Alcotest.test_case "range tools explain a malformed date" `Quick
            test_range_tools_explain_a_malformed_date;
          Alcotest.test_case "daily series uses the requested timezone" `Quick
            test_daily_series_uses_the_requested_timezone;
          Alcotest.test_case
            "get_daily_series returns intake and weight per day" `Quick
            test_get_daily_series_returns_intake_and_weight_per_day;
          Alcotest.test_case "range tools explain bad arguments" `Quick
            test_range_tools_explain_bad_arguments;
          Alcotest.test_case "get_summary reports averages, targets and trend"
            `Quick test_get_summary_reports_averages_targets_and_trend;
          Alcotest.test_case "get_context is empty without records" `Quick
            test_get_context_is_empty_without_records;
          Alcotest.test_case
            "Withings scope is isolated and requires configuration" `Quick
            test_withings_scope_isolated_and_requires_configuration;
          Alcotest.test_case
            "zero-parameter Withings tools accept omitted arguments" `Quick
            test_zero_parameter_withings_tools_accept_omitted_arguments;
          Alcotest.test_case "zero argument tools enforce schema" `Quick
            test_zero_argument_tools;
          Alcotest.test_case "combined scopes permit union" `Quick
            test_combined_scopes_allow_their_union;
          Alcotest.test_case "all scopes permit each tool class" `Quick
            test_all_scopes_allow_each_tool_class;
          Alcotest.test_case "disconnected Withings status succeeds" `Quick
            test_disconnected_withings_status_is_success;
          Alcotest.test_case "configured Withings OAuth URL" `Quick
            test_withings_mcp_uses_configured_oauth_url;
          Alcotest.test_case "discovers stateless protocol" `Quick
            test_server_discover_advertises_stateless_protocol;
          Alcotest.test_case "modern result metadata" `Quick
            test_modern_results_include_required_metadata;
          Alcotest.test_case "lists approved tools" `Quick
            test_tool_list_is_exact;
        ] );
    ]
