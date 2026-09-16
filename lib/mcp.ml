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

let weight_json (weight : Weigh_in.t) =
  `Assoc [
    ("id", `String (Weigh_in_id.to_string weight.id)); ("measured_at", `String (Time.to_utc_string weight.measured_at));
    ("weight_kg", `Float weight.weight_kg); ("source", `String "manual");
    ("created_at", `String (Time.to_utc_string weight.created_at)); ("updated_at", `String (Time.to_utc_string weight.updated_at)) ]

let text_result value =
  `Assoc [ ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String (Yojson.Safe.to_string value)) ] ]); ("structuredContent", value) ]

let tool_error = `Assoc [ ("content", `List [ `Assoc [ ("type", `String "text"); ("text", `String "record not found") ] ]); ("isError", `Bool true) ]

let schema properties required =
  `Assoc [ ("type", `String "object"); ("properties", `Assoc properties); ("required", `List (List.map (fun name -> `String name) required)); ("additionalProperties", `Bool false) ]

let string = `Assoc [ ("type", `String "string") ]
let integer = `Assoc [ ("type", `String "integer") ]
let number = `Assoc [ ("type", `String "number") ]
let meal_properties = [ ("description", string); ("calories_kcal", integer); ("protein_g", number); ("carbs_g", number); ("fat_g", number); ("confidence", number); ("estimate_source", string); ("notes", string); ("eaten_at", string) ]
let weight_properties = [ ("weight_kg", number); ("measured_at", string) ]
let id_properties = [ ("id", string) ]
let query_properties = [ ("from", string); ("to", string); ("limit", integer) ]

let tool name input_schema = `Assoc [ ("name", `String name); ("inputSchema", input_schema) ]
let tools = [
  tool "record_meal" (schema meal_properties [ "description"; "calories_kcal"; "protein_g" ]);
  tool "get_meal" (schema id_properties [ "id" ]);
  tool "query_meals" (schema query_properties []);
  tool "update_meal" (schema (id_properties @ meal_properties) [ "id" ]);
  tool "delete_meal" (schema id_properties [ "id" ]);
  tool "record_weight" (schema weight_properties [ "weight_kg" ]);
  tool "get_weight" (schema id_properties [ "id" ]);
  tool "query_weights" (schema query_properties []);
  tool "update_weight" (schema (id_properties @ weight_properties) [ "id" ]);
  tool "delete_weight" (schema id_properties [ "id" ]) ]

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

let call service user name arguments =
  match arguments with
  | `Assoc fields ->
      (match name with
      | "record_meal" -> Result.map (fun record -> text_result (meal_json record)) (Result.bind (meal_create fields) (Service.record_meal service ~user))
      | "get_meal" -> let* fields = only_fields [ "id" ] (`Assoc fields) in let* value = required_string "id" fields in let* record = id value Meal_id.of_string in Result.map (fun x -> text_result (meal_json x)) (Service.get_meal service ~user record)
      | "query_meals" -> let* from, to_, limit = query fields in Result.map (fun records -> text_result (`Assoc [ ("meals", `List (List.map meal_json records)) ])) (Service.query_meals service ~user ~from ~to_ ~limit)
      | "update_meal" -> let* fields = only_fields ("id" :: List.map fst meal_properties) (`Assoc fields) in let* value = required_string "id" fields in let* record = id value Meal_id.of_string in let* patch = meal_patch fields in Result.map (fun x -> text_result (meal_json x)) (Service.update_meal service ~user record patch)
      | "delete_meal" -> let* fields = only_fields [ "id" ] (`Assoc fields) in let* value = required_string "id" fields in let* record = id value Meal_id.of_string in Result.map (fun () -> text_result (`Assoc [ ("deleted", `Bool true) ])) (Service.delete_meal service ~user record)
      | "record_weight" -> let* fields = only_fields (List.map fst weight_properties) (`Assoc fields) in let* weight_kg = required_float "weight_kg" fields in let* measured_at = optional_time "measured_at" fields in Result.map (fun x -> text_result (weight_json x)) (Service.record_manual_weigh_in service ~user Weigh_in.{ measured_at; weight_kg })
      | "get_weight" -> let* fields = only_fields [ "id" ] (`Assoc fields) in let* value = required_string "id" fields in let* record = id value Weigh_in_id.of_string in Result.map (fun x -> text_result (weight_json x)) (Service.get_weigh_in service ~user record)
      | "query_weights" -> let* from, to_, limit = query fields in Result.map (fun records -> text_result (`Assoc [ ("weights", `List (List.map weight_json records)) ])) (Service.query_weigh_ins service ~user ~from ~to_ ~limit)
      | "update_weight" -> let* fields = only_fields ("id" :: List.map fst weight_properties) (`Assoc fields) in let* value = required_string "id" fields in let* record = id value Weigh_in_id.of_string in let* measured_at = optional_time "measured_at" fields in let* weight_kg = optional_float "weight_kg" fields in Result.map (fun x -> text_result (weight_json x)) (Service.update_manual_weigh_in service ~user record Weigh_in.{ measured_at; weight_kg })
      | "delete_weight" -> let* fields = only_fields [ "id" ] (`Assoc fields) in let* value = required_string "id" fields in let* record = id value Weigh_in_id.of_string in Result.map (fun () -> text_result (`Assoc [ ("deleted", `Bool true) ])) (Service.delete_weigh_in service ~user record)
      | _ -> invalid ())
  | _ -> invalid ()

let response ?(id = `Null) result = `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ]
let rpc_error ?(id = `Null) code message = `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("error", `Assoc [ ("code", `Int code); ("message", `String message) ]) ]

let protocol_version = "2025-03-26"

let handle ~service ~user request =
  match request with
  | `Assoc fields ->
      let request_id = Option.value (List.assoc_opt "id" fields) ~default:`Null in
      let valid_request = List.assoc_opt "jsonrpc" fields = Some (`String "2.0") in
      (match List.assoc_opt "method" fields with
      | Some (`String "initialize") when valid_request ->
          (match List.assoc_opt "params" fields with
          | Some (`Assoc params) when List.assoc_opt "protocolVersion" params = Some (`String protocol_version) ->
              response ~id:request_id (`Assoc [ ("protocolVersion", `String protocol_version); ("capabilities", `Assoc [ ("tools", `Assoc []) ]); ("serverInfo", `Assoc [ ("name", `String "kcal"); ("version", `String "dev") ]) ])
          | _ -> rpc_error ~id:request_id (-32602) "Invalid params")
      | Some (`String "tools/list") when valid_request -> response ~id:request_id (`Assoc [ ("tools", `List tools) ])
      | Some (`String "tools/call") when valid_request ->
          (match List.assoc_opt "params" fields with
          | Some (`Assoc params) ->
              (match required_string "name" params, List.assoc_opt "arguments" params with
              | Ok name, Some arguments ->
                  (match call service user name arguments with
                  | Ok result -> response ~id:request_id result
                  | Error Error.Not_found -> response ~id:request_id tool_error
                  | Error (Error.Invalid_input _) -> rpc_error ~id:request_id (-32602) "Invalid params"
                  | Error _ -> rpc_error ~id:request_id (-32603) "Internal error")
              | Ok ("query_meals" | "query_weights" as name), None ->
                  (match call service user name (`Assoc []) with Ok result -> response ~id:request_id result | Error _ -> rpc_error ~id:request_id (-32603) "Internal error")
              | _ -> rpc_error ~id:request_id (-32602) "Invalid params")
          | _ -> rpc_error ~id:request_id (-32602) "Invalid params")
      | Some (`String _) when valid_request -> rpc_error ~id:request_id (-32601) "Method not found"
      | _ -> rpc_error ~id:request_id (-32600) "Invalid Request")
  | _ -> rpc_error (-32700) "Parse error"
