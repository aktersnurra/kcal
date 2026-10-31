module Domain_error = Error
open Base
module Error = Domain_error

let ( let* ) result f = Result.bind result ~f

type withings = {
  oauth : Withings_oauth.t;
  client_id : string;
  redirect_uri : string;
}

let required_scope = function
  | "get_context" | "search_products" | "get_meal" | "query_meals"
  | "get_daily_series" | "get_summary" | "get_weight" | "query_weights" ->
      Some "ledger:read"
  | "set_goals" | "save_product" | "correct_meals_from_product" | "record_meal"
  | "update_meal" | "delete_meal" | "record_weight" | "update_weight"
  | "delete_weight" ->
      Some "ledger:write"
  | "begin_withings_connection" | "get_withings_status" | "disconnect_withings"
    ->
      Some "withings:manage"
  | _ -> None

let withings_for_call = function
  | Some withings -> Ok withings
  | None -> Error (Error.Storage_error "Withings unavailable")

let call_begin_withings_connection withings ~user =
  let* withings = withings_for_call withings in
  Result.map
    ~f:(fun (_, url) ->
      Mcp_json.text_result (`Assoc [ ("authorization_url", `String url) ]))
    (Service.begin_withings_authorization ~client_id:withings.client_id
       ~redirect_uri:withings.redirect_uri ~oauth:withings.oauth ~user)

let call_get_withings_status service withings ~user =
  let* _ = withings_for_call withings in
  Result.map
    ~f:(fun status ->
      Mcp_json.text_result (Mcp_json.withings_status_json status))
    (Service.withings_status service ~user)

let call_disconnect_withings service withings ~user =
  let* _ = withings_for_call withings in
  Result.map
    ~f:(fun () ->
      Mcp_json.text_result (`Assoc [ ("disconnected", `Bool true) ]))
    (Service.disconnect_withings service ~user)

let call ?withings service (identity : Auth.identity) name arguments =
  let now = Option.value_exn (Ptime.of_float_s (Unix.gettimeofday ())) in
  let* request =
    Mcp_arguments.parse ~now
      ~default_zone:(fun () -> Service.default_zone service ~user:identity.user)
      ~compile_search:(Service.compile_search_query service)
      ~name arguments
  in
  let user = identity.user in
  let open Mcp_arguments in
  match request with
  | Begin_withings_connection -> call_begin_withings_connection withings ~user
  | Get_withings_status -> call_get_withings_status service withings ~user
  | Disconnect_withings -> call_disconnect_withings service withings ~user
  | Save_product (product_id, input) ->
      Result.map
        ~f:(fun product -> Mcp_json.text_result (Mcp_json.product_json product))
        (Service.save_product service ~user ~id:product_id input)
  | Search_products (query, limit) ->
      Result.map
        ~f:(fun products ->
          Mcp_json.text_result
            (`Assoc
               [
                 ("products", `List (List.map ~f:Mcp_json.product_json products));
               ]))
        (Service.search_products service ~user ~query ~limit)
  | Get_context ->
      Result.map
        ~f:(fun context -> Mcp_json.text_result (Mcp_json.context_json context))
        (Service.context service ~user ~now)
  | Set_goals goals ->
      Result.map
        ~f:(fun goals -> Mcp_json.text_result (Mcp_json.goals_json goals))
        (Service.set_goals service ~user goals)
  | Correct_meals_from_product { product_id; reason; dry_run } ->
      Result.map
        ~f:(fun correction ->
          Mcp_json.text_result (Mcp_json.correction_json correction))
        (Service.correct_meals_from_product service ~user product_id ~reason
           ~dry_run)
  | Record_meal { idempotency_key; entry } ->
      Result.map
        ~f:(fun meal -> Mcp_json.text_result (Mcp_json.meal_json meal))
        (match entry with
        | Manual input ->
            Service.record_meal ?idempotency_key service ~user input
        | From_product entry ->
            Service.record_meal_from_product ?idempotency_key service ~user
              entry)
  | Get_meal meal_id ->
      Result.map
        ~f:(fun (meal, revisions) ->
          Mcp_json.text_result
            (Mcp_json.meal_with_revisions_json meal revisions))
        (Service.get_meal service ~user meal_id)
  | Query_meals (search, { from; to_; limit }) ->
      Result.map
        ~f:(fun meals ->
          Mcp_json.text_result
            (`Assoc [ ("meals", `List (List.map ~f:Mcp_json.meal_json meals)) ]))
        (Service.query_meals ?search service ~user ~from ~to_ ~limit)
  | Update_meal { id; patch; reason } ->
      Result.map
        ~f:(fun meal -> Mcp_json.text_result (Mcp_json.meal_json meal))
        (Service.update_meal ?reason service ~user id patch)
  | Delete_meal meal_id ->
      Result.map
        ~f:(fun () -> Mcp_json.text_result (`Assoc [ ("deleted", `Bool true) ]))
        (Service.delete_meal service ~user meal_id)
  | Record_weight input ->
      Result.map
        ~f:(fun weight -> Mcp_json.text_result (Mcp_json.weight_json weight))
        (Service.record_manual_weigh_in service ~user input)
  | Get_weight weight_id ->
      Result.map
        ~f:(fun weight -> Mcp_json.text_result (Mcp_json.weight_json weight))
        (Service.get_weigh_in service ~user weight_id)
  | Query_weights { from; to_; limit } ->
      Result.map
        ~f:(fun weights ->
          Mcp_json.text_result
            (`Assoc
               [ ("weights", `List (List.map ~f:Mcp_json.weight_json weights)) ]))
        (Service.query_weigh_ins service ~user ~from ~to_ ~limit)
  | Update_weight (weight_id, patch) ->
      Result.map
        ~f:(fun weight -> Mcp_json.text_result (Mcp_json.weight_json weight))
        (Service.update_manual_weigh_in service ~user weight_id patch)
  | Delete_weight weight_id ->
      Result.map
        ~f:(fun () -> Mcp_json.text_result (`Assoc [ ("deleted", `Bool true) ]))
        (Service.delete_weigh_in service ~user weight_id)
  | Get_daily_series { zone; range } ->
      let* series = Service.daily_series service ~user ~zone ~range in
      Ok
        (Mcp_json.text_result
           (`Assoc
              (Mcp_json.range_json zone range
              @ [ ("days", `List (List.map ~f:Mcp_json.day_json series)) ])))
  | Get_summary { zone; range } ->
      let* summary = Service.summary service ~user ~zone ~range in
      Ok (Mcp_json.text_result (Mcp_json.summary_json zone range summary))

let response ?(id = `Null) result =
  `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ]

let rpc_error ?(id = `Null) code message =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", id);
      ("error", `Assoc [ ("code", `Int code); ("message", `String message) ]);
    ]

let stateless_protocol_version = "2026-07-28"
let legacy_protocol_version = "2025-03-26"

let discover_result =
  Mcp_json.cacheable
    [
      ( "supportedVersions",
        `List
          [
            `String stateless_protocol_version; `String legacy_protocol_version;
          ] );
      ("capabilities", `Assoc [ ("tools", `Assoc []) ]);
      ( "_meta",
        `Assoc
          [
            ( "io.modelcontextprotocol/serverInfo",
              `Assoc [ ("name", `String "kcal"); ("version", `String "dev") ] );
          ] );
    ]

(* Argument errors the model can fix itself come back as tool results, which
   MCP clients show to the model, rather than as protocol errors. *)
let argument_error_message = function
  | Error.Unsupported_timezone _ as error ->
      Error.to_string error
      ^ ": use a fixed UTC offset such as +02:00, or one of "
      ^ String.concat ~sep:", " Time.supported_zone_names
  | error -> Error.to_string error

let call_response ~request_id = function
  | Ok result -> response ~id:request_id result
  | Error Error.Not_found ->
      response ~id:request_id (Mcp_json.tool_error "record not found")
  | Error
      (( Error.Calendar_date_addition _ | Error.Calendar_day_bounds _
       | Error.Calendar_local_date _ | Error.Invalid_date _
       | Error.Unsupported_timezone _ | Error.Reversed_date_range _
       | Error.Date_range_too_long _ | Error.Nonpositive_argument _
       | Error.Rejected _ | Error.Conflict _ ) as error) ->
      response ~id:request_id
        (Mcp_json.tool_error (argument_error_message error))
  | Error
      (Error.Invalid_field
         { field = Numeric (Calorie_target_kcal | Protein_target_g); _ } as
       error) ->
      response ~id:request_id (Mcp_json.tool_error (Error.to_string error))
  | Error (Error.Invalid_input _ | Error.Invalid_field _) ->
      rpc_error ~id:request_id (-32602) "Invalid params"
  | Error ((Error.Numeric_overflow _ | Error.Unrepresentable_result _) as error)
    ->
      response ~id:request_id (Mcp_json.tool_error (Error.to_string error))
  | Error
      ( Error.Unauthorized | Error.Storage_error _ | Error.Storage_decoding _
      | Error.Withings_failure _ ) ->
      rpc_error ~id:request_id (-32603) "Internal error"

let handle ?withings ~service ~identity request =
  match request with
  | `Assoc fields -> (
      let request_id =
        Option.value
          (List.Assoc.find fields ~equal:String.equal "id")
          ~default:`Null
      in
      let valid_request =
        Option.equal Yojson.Safe.equal
          (List.Assoc.find fields ~equal:String.equal "jsonrpc")
          (Some (`String "2.0"))
      in
      match List.Assoc.find fields ~equal:String.equal "method" with
      | Some (`String "server/discover") when valid_request ->
          Ok (response ~id:request_id discover_result)
      | Some (`String "initialize") when valid_request -> (
          match List.Assoc.find fields ~equal:String.equal "params" with
          | Some (`Assoc params)
            when Option.equal Yojson.Safe.equal
                   (List.Assoc.find params ~equal:String.equal "protocolVersion")
                   (Some (`String legacy_protocol_version)) ->
              Ok
                (response ~id:request_id
                   (`Assoc
                      [
                        ("protocolVersion", `String legacy_protocol_version);
                        ("capabilities", `Assoc [ ("tools", `Assoc []) ]);
                        ( "serverInfo",
                          `Assoc
                            [
                              ("name", `String "kcal");
                              ("version", `String "dev");
                            ] );
                      ]))
          | _ -> Ok (rpc_error ~id:request_id (-32602) "Invalid params"))
      | Some (`String "tools/list") when valid_request ->
          Ok
            (response ~id:request_id
               (Mcp_json.cacheable [ ("tools", `List Mcp_schema.tools) ]))
      | Some (`String "tools/call") when valid_request -> (
          match List.Assoc.find fields ~equal:String.equal "params" with
          | Some (`Assoc params) -> (
              match
                ( (match List.Assoc.find params ~equal:String.equal "name" with
                  | Some (`String name) -> Ok name
                  | _ -> Error ()),
                  List.Assoc.find params ~equal:String.equal "arguments" )
              with
              | Ok name, Some arguments -> (
                  match required_scope name with
                  | Some scope when not (Auth.has_scope identity scope) ->
                      Error `Forbidden
                  | _ ->
                      Ok
                        (call_response ~request_id
                           (call ?withings service identity name arguments)))
              | ( Ok
                    (( "save_product" | "search_products" | "query_meals"
                     | "query_weights" | "get_context"
                     | "begin_withings_connection" | "get_withings_status"
                     | "disconnect_withings" ) as name),
                  None ) -> (
                  match required_scope name with
                  | Some scope when not (Auth.has_scope identity scope) ->
                      Error `Forbidden
                  | _ ->
                      Ok
                        (call_response ~request_id
                           (call ?withings service identity name (`Assoc []))))
              | _ -> Ok (rpc_error ~id:request_id (-32602) "Invalid params"))
          | _ -> Ok (rpc_error ~id:request_id (-32602) "Invalid params"))
      | Some (`String _) when valid_request ->
          Ok (rpc_error ~id:request_id (-32601) "Method not found")
      | _ -> Ok (rpc_error ~id:request_id (-32600) "Invalid Request"))
  | _ -> Ok (rpc_error (-32700) "Parse error")
