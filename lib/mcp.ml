let invalid () = Error (Error.Invalid_input "invalid MCP parameters")

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let only_fields allowed = function
  | `Assoc fields when List.for_all (fun (name, _) -> List.mem name allowed) fields -> Ok fields
  | _ -> invalid ()

let required_string name fields : (string, Error.t) result =
  match List.assoc_opt name fields with Some (`String value) -> Ok value | _ -> invalid ()

let required_int name fields : (int, Error.t) result =
  match List.assoc_opt name fields with Some (`Int value) -> Ok value | _ -> invalid ()

let required_float name fields : (float, Error.t) result =
  match List.assoc_opt name fields with
  | Some (`Float value) -> Ok value
  | Some (`Int value) -> Ok (float_of_int value)
  | _ -> invalid ()

let optional_string name fields =
  match List.assoc_opt name fields with None -> Ok None | Some (`String value) -> Ok (Some value) | _ -> invalid ()

let optional_int name fields =
  match List.assoc_opt name fields with None -> Ok None | Some (`Int value) -> Ok (Some value) | _ -> invalid ()

let optional_float name fields =
  match List.assoc_opt name fields with
  | None -> Ok None
  | Some (`Float value) -> Ok (Some value)
  | Some (`Int value) -> Ok (Some (float_of_int value))
  | _ -> invalid ()

let optional_time name fields =
  match List.assoc_opt name fields with
  | None -> Ok None
  | Some (`String value) -> Result.map Option.some (Time.parse_offset_datetime value)
  | _ -> invalid ()

let bind result f = match result with Ok value -> f value | Error _ as error -> error
let ( let* ) = bind

let id value of_string = match of_string value with Some id -> Ok id | None -> invalid ()

let json_option f = function None -> `Null | Some value -> f value

let meal_json (meal : Meal.t) =
  `Assoc [
    ("id", `String (Meal_id.to_string meal.id)); ("eaten_at", `String (Time.to_utc_string meal.eaten_at));
    ("description", `String meal.description); ("calories_kcal", `Int meal.calories_kcal);
    ("protein_g", `Float meal.protein_g); ("carbs_g", json_option (fun v -> `Float v) meal.carbs_g);
    ("fat_g", json_option (fun v -> `Float v) meal.fat_g);
    ("confidence", json_option (fun v -> `Float v) meal.confidence);
    ("estimate_source", json_option (fun v -> `String v) meal.estimate_source);
    ("notes", json_option (fun v -> `String v) meal.notes);
    ("created_at", `String (Time.to_utc_string meal.created_at)); ("updated_at", `String (Time.to_utc_string meal.updated_at)) ]

let totals_json ~date (totals : Meal.totals) =
  `Assoc [
    ("date", `String date); ("meal_count", `Int totals.meal_count);
    ("calories_kcal", `Int totals.calories_kcal); ("protein_g", `Float totals.protein_g);
    ("carbs_g", json_option (fun v -> `Float v) totals.carbs_g);
    ("fat_g", json_option (fun v -> `Float v) totals.fat_g) ]

let weight_json (weight : Weigh_in.t) =
  `Assoc [
    ("id", `String (Weigh_in_id.to_string weight.id)); ("measured_at", `String (Time.to_utc_string weight.measured_at));
    ("weight_kg", `Float weight.weight_kg); ("source", `String (Weigh_in.source_to_string weight.source));
    ("created_at", `String (Time.to_utc_string weight.created_at)); ("updated_at", `String (Time.to_utc_string weight.updated_at)) ]

let complete fields = `Assoc (("resultType", `String "complete") :: fields)

let cacheable fields =
  complete (("ttlMs", `Int 300_000) :: ("cacheScope", `String "public") :: fields)

let text_result value =
  complete [ ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String (Yojson.Safe.to_string value)) ] ]); ("structuredContent", value) ]

let tool_error = complete [ ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String "record not found") ] ]); ("isError", `Bool true) ]

let schema properties required =
  `Assoc [ ("type", `String "object"); ("properties", `Assoc properties); ("required", `List (List.map (fun name -> `String name) required)); ("additionalProperties", `Bool false) ]

let string = `Assoc [ ("type", `String "string") ]
let integer = `Assoc [ ("type", `String "integer") ]
let number = `Assoc [ ("type", `String "number") ]

let describe text = function
  | `Assoc fields -> `Assoc (fields @ [ ("description", `String text) ])
  | other -> other

let property name text field_schema = (name, describe text field_schema)

let meal_properties = [
  property "description" "Free-text description of what was eaten." string;
  property "calories_kcal" "Energy in kilocalories." integer;
  property "protein_g" "Protein in grams." number;
  property "carbs_g" "Carbohydrates in grams, if known." number;
  property "fat_g" "Fat in grams, if known." number;
  property "confidence" "Estimate confidence from 0.0 to 1.0, if the macros were estimated rather than measured." number;
  property "estimate_source" "Where the estimate came from (e.g. a nutrition label or an AI estimate), if applicable." string;
  property "notes" "Freeform notes." string;
  property "eaten_at" "When the meal was eaten, as an RFC 3339 UTC timestamp (e.g. 2026-09-22T12:30:00Z). Defaults to now if omitted." string;
]
let weight_properties = [
  property "weight_kg" "Body weight in kilograms." number;
  property "measured_at" "When the weigh-in was taken, as an RFC 3339 UTC timestamp. Defaults to now if omitted." string;
]
let id_properties = [ property "id" "The record's id, as returned by a previous call." string ]
let query_properties = [
  property "from" "Inclusive lower bound as an RFC 3339 UTC timestamp (e.g. 2026-09-22T00:00:00Z for the start of that day)." string;
  property "to" "Inclusive upper bound as an RFC 3339 UTC timestamp." string;
  property "limit"
    "Maximum number of records to return (default 100, max 500). Results are ordered oldest first, so once you have more history than limit, pass from/to to scope the query instead of relying on limit alone. For a single day's totals, prefer get_daily_totals; for the current weight, prefer get_latest_weight."
    integer;
]
let daily_totals_properties = [
  property "date" "Calendar date as YYYY-MM-DD, interpreted as a UTC day. Defaults to today (UTC) if omitted." string;
]

let tool ?description name input_schema =
  let fields = [ ("name", `String name); ("inputSchema", input_schema) ] in
  `Assoc (match description with None -> fields | Some text -> fields @ [ ("description", `String text) ])
let withings_tools = [
  tool "begin_withings_connection" ~description:"Start linking a Withings account; returns an authorization_url to open in a browser." (schema [] []);
  tool "get_withings_status" ~description:"Check whether a Withings account is connected and whether it needs reauthorization." (schema [] []);
  tool "disconnect_withings" ~description:"Unlink the connected Withings account." (schema [] []) ]
let tools = [
  tool "record_meal" ~description:"Record a new meal in the nutrition ledger." (schema meal_properties [ "description"; "calories_kcal"; "protein_g" ]);
  tool "get_meal" ~description:"Fetch a single meal by id." (schema id_properties [ "id" ]);
  tool "query_meals" ~description:"List meals, oldest first, optionally bounded by from/to. For a day's totals, prefer get_daily_totals, which aggregates server-side instead of requiring client-side summation." (schema query_properties []);
  tool "update_meal" ~description:"Update fields on an existing meal; omitted fields are left unchanged." (schema (id_properties @ meal_properties) [ "id" ]);
  tool "delete_meal" ~description:"Delete a meal." (schema id_properties [ "id" ]);
  tool "get_daily_totals" ~description:"Aggregate calories, protein, carbs, and fat for all meals on a given UTC calendar day (default: today). Prefer this over query_meals plus client-side summation." (schema daily_totals_properties []);
  tool "record_weight" ~description:"Record a manual weigh-in." (schema weight_properties [ "weight_kg" ]);
  tool "get_weight" ~description:"Fetch a single weigh-in by id." (schema id_properties [ "id" ]);
  tool "query_weights" ~description:"List weigh-ins, oldest first, optionally bounded by from/to. For the current weight, prefer get_latest_weight." (schema query_properties []);
  tool "update_weight" ~description:"Update a manual weigh-in; omitted fields are left unchanged." (schema (id_properties @ weight_properties) [ "id" ]);
  tool "delete_weight" ~description:"Delete a weigh-in." (schema id_properties [ "id" ]);
  tool "get_latest_weight" ~description:"Fetch the single most recent weigh-in (manual or Withings-imported), if any." (schema [] []) ] @ withings_tools

let meal_create fields : (Meal.create, Error.t) result =
  let* fields = only_fields (List.map fst meal_properties) (`Assoc fields) in
  let* description = required_string "description" fields in let* calories_kcal = required_int "calories_kcal" fields in let* protein_g = required_float "protein_g" fields in
  let* carbs_g = optional_float "carbs_g" fields in let* fat_g = optional_float "fat_g" fields in let* confidence = optional_float "confidence" fields in let* estimate_source = optional_string "estimate_source" fields in let* notes = optional_string "notes" fields in let* eaten_at = optional_time "eaten_at" fields in
  Ok ({ description; calories_kcal; protein_g; carbs_g; fat_g; confidence; estimate_source; notes; eaten_at } : Meal.create)

let meal_patch fields : (Meal.patch, Error.t) result =
  let* description = optional_string "description" fields in let* calories_kcal = optional_int "calories_kcal" fields in let* protein_g = optional_float "protein_g" fields in let* carbs_g = optional_float "carbs_g" fields in let* fat_g = optional_float "fat_g" fields in let* confidence = optional_float "confidence" fields in let* estimate_source = optional_string "estimate_source" fields in let* notes = optional_string "notes" fields in let* eaten_at = optional_time "eaten_at" fields in
  Ok ({ description; calories_kcal; protein_g; carbs_g; fat_g; confidence; estimate_source; notes; eaten_at } : Meal.patch)

let query fields =
  let* fields = only_fields (List.map fst query_properties) (`Assoc fields) in
  let* from = optional_time "from" fields in let* to_ = optional_time "to" fields in let* limit = optional_int "limit" fields in
  Ok (from, to_, Option.value limit ~default:100)

let now () = Option.get (Ptime.of_float_s (Unix.gettimeofday ()))

let day_bounds_query fields =
  let* fields = only_fields (List.map fst daily_totals_properties) (`Assoc fields) in
  let* date = optional_string "date" fields in
  match date with
  | None -> Ok (Time.day_bounds (now ()))
  | Some value -> Result.map Time.day_bounds (Time.parse_date value)

type withings = {
  oauth : Withings_oauth.t;
  client_id : string;
  redirect_uri : string;
}

let withings_status_json = function
  | Withings_connection.Connected status ->
      `Assoc [ ("connected", `Bool true); ("requires_reauthorization", `Bool status.requires_reauthorization);
               ("token_expires_at", json_option (fun value -> `String (Time.to_utc_string value)) status.token_expires_at) ]

let required_scope = function
  | "get_meal" | "query_meals" | "get_daily_totals" | "get_weight" | "query_weights" | "get_latest_weight" ->
      Some "ledger:read"
  | "record_meal" | "update_meal" | "delete_meal"
  | "record_weight" | "update_weight" | "delete_weight" ->
      Some "ledger:write"
  | "begin_withings_connection" | "get_withings_status"
  | "disconnect_withings" -> Some "withings:manage"
  | _ -> None

let withings_for_call = function
  | Some withings -> Ok withings
  | None -> Error (Error.Storage_error "Withings unavailable")

let call_begin_withings_connection withings ~user =
  let* withings = withings_for_call withings in
  Result.map (fun (_, url) -> text_result (`Assoc [ ("authorization_url", `String url) ]))
    (Withings_oauth.begin_authorization ~client_id:withings.client_id
      ~redirect_uri:withings.redirect_uri withings.oauth ~user)

let call_get_withings_status withings ~user =
  let* withings = withings_for_call withings in
  Result.map (fun status -> text_result (withings_status_json status))
    (Store_sqlite.get_withings_status withings.oauth.store ~user)

let call_disconnect_withings withings ~user =
  let* withings = withings_for_call withings in
  Result.map (fun () -> text_result (`Assoc [ ("disconnected", `Bool true) ]))
    (Store_sqlite.delete_withings_connection withings.oauth.store ~user)

let call_record_meal service ~user fields =
  let* input = meal_create fields in
  Result.map (fun meal -> text_result (meal_json meal)) (Service.record_meal service ~user input)

let call_get_meal service ~user fields =
  let* fields = only_fields [ "id" ] (`Assoc fields) in
  let* value = required_string "id" fields in
  let* meal_id = id value Meal_id.of_string in
  Result.map (fun meal -> text_result (meal_json meal)) (Service.get_meal service ~user meal_id)

let call_query_meals service ~user fields =
  let* from, to_, limit = query fields in
  Result.map (fun meals -> text_result (`Assoc [ ("meals", `List (List.map meal_json meals)) ]))
    (Service.query_meals service ~user ~from ~to_ ~limit)

let call_get_daily_totals service ~user fields =
  let* day_start, day_end = day_bounds_query fields in
  Result.map (fun totals -> text_result (totals_json ~date:(Time.to_date_string day_start) totals))
    (Service.daily_totals service ~user ~day_start ~day_end)

let call_update_meal service ~user fields =
  let* fields = only_fields ("id" :: List.map fst meal_properties) (`Assoc fields) in
  let* value = required_string "id" fields in
  let* meal_id = id value Meal_id.of_string in
  let* patch = meal_patch fields in
  Result.map (fun meal -> text_result (meal_json meal))
    (Service.update_meal service ~user meal_id patch)

let call_delete_meal service ~user fields =
  let* fields = only_fields [ "id" ] (`Assoc fields) in
  let* value = required_string "id" fields in
  let* meal_id = id value Meal_id.of_string in
  Result.map (fun () -> text_result (`Assoc [ ("deleted", `Bool true) ]))
    (Service.delete_meal service ~user meal_id)

let call_record_weight service ~user fields =
  let* fields = only_fields (List.map fst weight_properties) (`Assoc fields) in
  let* weight_kg = required_float "weight_kg" fields in
  let* measured_at = optional_time "measured_at" fields in
  Result.map (fun weight -> text_result (weight_json weight))
    (Service.record_manual_weigh_in service ~user Weigh_in.{ measured_at; weight_kg })

let call_get_weight service ~user fields =
  let* fields = only_fields [ "id" ] (`Assoc fields) in
  let* value = required_string "id" fields in
  let* weight_id = id value Weigh_in_id.of_string in
  Result.map (fun weight -> text_result (weight_json weight))
    (Service.get_weigh_in service ~user weight_id)

let call_query_weights service ~user fields =
  let* from, to_, limit = query fields in
  Result.map (fun weights -> text_result (`Assoc [ ("weights", `List (List.map weight_json weights)) ]))
    (Service.query_weigh_ins service ~user ~from ~to_ ~limit)

let call_get_latest_weight service ~user =
  Result.map (fun weight -> text_result (weight_json weight))
    (Service.latest_weigh_in service ~user)

let call_update_weight service ~user fields =
  let* fields = only_fields ("id" :: List.map fst weight_properties) (`Assoc fields) in
  let* value = required_string "id" fields in
  let* weight_id = id value Weigh_in_id.of_string in
  let* measured_at = optional_time "measured_at" fields in
  let* weight_kg = optional_float "weight_kg" fields in
  Result.map (fun weight -> text_result (weight_json weight))
    (Service.update_manual_weigh_in service ~user weight_id Weigh_in.{ measured_at; weight_kg })

let call_delete_weight service ~user fields =
  let* fields = only_fields [ "id" ] (`Assoc fields) in
  let* value = required_string "id" fields in
  let* weight_id = id value Weigh_in_id.of_string in
  Result.map (fun () -> text_result (`Assoc [ ("deleted", `Bool true) ]))
    (Service.delete_weigh_in service ~user weight_id)

let call ?withings service (identity : Auth.identity) name arguments =
  match arguments with
  | `Assoc fields ->
      let user = identity.user in
      (match name with
      | "begin_withings_connection" -> call_begin_withings_connection withings ~user
      | "get_withings_status" -> call_get_withings_status withings ~user
      | "disconnect_withings" -> call_disconnect_withings withings ~user
      | "record_meal" -> call_record_meal service ~user fields
      | "get_meal" -> call_get_meal service ~user fields
      | "query_meals" -> call_query_meals service ~user fields
      | "get_daily_totals" -> call_get_daily_totals service ~user fields
      | "update_meal" -> call_update_meal service ~user fields
      | "delete_meal" -> call_delete_meal service ~user fields
      | "record_weight" -> call_record_weight service ~user fields
      | "get_weight" -> call_get_weight service ~user fields
      | "query_weights" -> call_query_weights service ~user fields
      | "get_latest_weight" -> call_get_latest_weight service ~user
      | "update_weight" -> call_update_weight service ~user fields
      | "delete_weight" -> call_delete_weight service ~user fields
      | _ -> invalid ())
  | _ -> invalid ()

let response ?(id = `Null) result = `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ]
let rpc_error ?(id = `Null) code message = `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("error", `Assoc [ ("code", `Int code); ("message", `String message) ]) ]

let stateless_protocol_version = "2026-07-28"
let legacy_protocol_version = "2025-03-26"

let discover_result =
  cacheable [
    ("supportedVersions", `List [
      `String stateless_protocol_version;
      `String legacy_protocol_version;
    ]);
    ("capabilities", `Assoc [ ("tools", `Assoc []) ]);
    ("_meta", `Assoc [
      ("io.modelcontextprotocol/serverInfo", `Assoc [
        ("name", `String "kcal");
        ("version", `String "dev");
      ]);
    ]);
  ]

let handle ?withings ~service ~identity request =
  match request with
  | `Assoc fields ->
      let request_id = Option.value (List.assoc_opt "id" fields) ~default:`Null in
      let valid_request = List.assoc_opt "jsonrpc" fields = Some (`String "2.0") in
      (match List.assoc_opt "method" fields with
      | Some (`String "server/discover") when valid_request ->
          Ok (response ~id:request_id discover_result)
      | Some (`String "initialize") when valid_request ->
          (match List.assoc_opt "params" fields with
          | Some (`Assoc params) when List.assoc_opt "protocolVersion" params = Some (`String legacy_protocol_version) ->
              Ok (response ~id:request_id (`Assoc [ ("protocolVersion", `String legacy_protocol_version); ("capabilities", `Assoc [ ("tools", `Assoc []) ]); ("serverInfo", `Assoc [ ("name", `String "kcal"); ("version", `String "dev") ]) ]))
          | _ -> Ok (rpc_error ~id:request_id (-32602) "Invalid params"))
      | Some (`String "tools/list") when valid_request ->
          Ok (response ~id:request_id (cacheable [ ("tools", `List tools) ]))
      | Some (`String "tools/call") when valid_request ->
          (match List.assoc_opt "params" fields with
          | Some (`Assoc params) ->
              (match required_string "name" params, List.assoc_opt "arguments" params with
              | Ok name, Some arguments ->
                  (match required_scope name with
                  | Some scope when not (Auth.has_scope identity scope) -> Error `Forbidden
                  | _ ->
                      (match call ?withings service identity name arguments with
                      | Ok result -> Ok (response ~id:request_id result)
                      | Error Error.Not_found -> Ok (response ~id:request_id tool_error)
                      | Error (Error.Invalid_input _) -> Ok (rpc_error ~id:request_id (-32602) "Invalid params")
                      | Error _ -> Ok (rpc_error ~id:request_id (-32603) "Internal error")))
              | Ok ("query_meals" | "query_weights" | "get_daily_totals" | "get_latest_weight"
                  | "begin_withings_connection" | "get_withings_status" | "disconnect_withings" as name), None ->
                  (match required_scope name with
                  | Some scope when not (Auth.has_scope identity scope) -> Error `Forbidden
                  | _ ->
                      (match call ?withings service identity name (`Assoc []) with
                      | Ok result -> Ok (response ~id:request_id result)
                      | Error Error.Not_found -> Ok (response ~id:request_id tool_error)
                      | Error (Error.Invalid_input _) -> Ok (rpc_error ~id:request_id (-32602) "Invalid params")
                      | Error _ -> Ok (rpc_error ~id:request_id (-32603) "Internal error")))
              | _ -> Ok (rpc_error ~id:request_id (-32602) "Invalid params"))
          | _ -> Ok (rpc_error ~id:request_id (-32602) "Invalid params"))
      | Some (`String _) when valid_request -> Ok (rpc_error ~id:request_id (-32601) "Method not found")
      | _ -> Ok (rpc_error ~id:request_id (-32600) "Invalid Request"))
  | _ -> Ok (rpc_error (-32700) "Parse error")
