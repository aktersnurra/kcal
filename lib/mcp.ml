module Domain_error = Error
open Base
module Error = Domain_error

let ( let* ) result f = Result.bind result ~f

let tool_log_src =
  Logs.Src.create "kcal.mcp.tools" ~doc:"MCP tool calls and their outcomes"

module Tool_log = (val Logs.src_log tool_log_src : Logs.LOG)

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
  | "update_meal" | "delete_meal" | "add_meal_item" | "update_meal_item"
  | "delete_meal_item" | "record_weight" | "update_weight" | "delete_weight" ->
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

let meal_result meal =
  Result.map (Mcp_json.meal_json meal) ~f:Mcp_json.text_result

let run ?withings service (identity : Auth.identity) ~now request =
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
  | Record_meal { idempotency; eaten_at; label; items } ->
      let* meal, outcome =
        Service.record_meal ?idempotency service ~user ~eaten_at ~label items
      in
      let* json = Mcp_json.meal_json meal in
      Ok
        (Mcp_json.text_result
           (Mcp_json.with_field json
              ( "replayed",
                `Bool
                  (match outcome with `Replayed -> true | `Recorded -> false) )))
  | Get_meal meal_id ->
      let* meal, revisions = Service.get_meal service ~user meal_id in
      Result.map ~f:Mcp_json.text_result
        (Mcp_json.meal_with_revisions_json meal revisions)
  | Query_meals (search, { from; to_; limit }) ->
      let* meals =
        Service.query_meals ?search service ~user ~from ~to_ ~limit
      in
      let* meals = Result.all (List.map meals ~f:Mcp_json.meal_json) in
      Ok (Mcp_json.text_result (`Assoc [ ("meals", `List meals) ]))
  | Update_meal (id, patch) ->
      Result.bind (Service.update_meal service ~user id patch) ~f:meal_result
  | Add_meal_item (meal_id, entry) ->
      Result.bind (Service.add_item service ~user meal_id entry) ~f:meal_result
  | Update_meal_item { id; patch; reason; quantity } ->
      Result.bind
        (Service.update_item ?reason ?quantity service ~user id patch)
        ~f:meal_result
  | Delete_meal_item id ->
      Result.bind (Service.delete_item service ~user id) ~f:meal_result
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

(* Which stage failed: rejected arguments never reach the service. *)
type stage = Arguments | Service

let call ?withings service (identity : Auth.identity) name arguments =
  let now = Option.value_exn (Ptime.of_float_s (Unix.gettimeofday ())) in
  match
    Mcp_arguments.parse ~now
      ~default_zone:(fun () -> Service.default_zone service ~user:identity.user)
      ~compile_search:(Service.compile_search_query service)
      ~name arguments
  with
  | Error error -> Error (Arguments, error)
  | Ok request ->
      Result.map_error (run ?withings service identity ~now request)
        ~f:(fun error -> (Service, error))

let response ?(id = `Null) result =
  `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ]

let rpc_error ?(id = `Null) ?data code message =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", id);
      ( "error",
        `Assoc
          ([ ("code", `Int code); ("message", `String message) ]
          @ match data with None -> [] | Some data -> [ ("data", data) ]) );
    ]

(* A protocol-level parameter error names the field and what was wrong, so a
   client never gets a bare "Invalid params". *)
let invalid_params ~id ~field ~reason =
  rpc_error ~id (-32602) "Invalid params"
    ~data:(`Assoc [ ("field", `String field); ("reason", `String reason) ])

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

(* Every argument error the model can fix itself comes back as a tool result
   that names the field, which MCP clients show to the model. Only a malformed
   JSON-RPC envelope is a protocol error. *)
let argument_error_message = function
  | Error.Unsupported_timezone _ as error ->
      Error.to_string error
      ^ ": use a fixed UTC offset such as +02:00, or one of "
      ^ String.concat ~sep:", " Time.supported_zone_names
  | error -> Error.to_string error

(* A stable code per error kind, for logs; never the message, which can
   quote argument values. *)
let error_code = function
  | Error.Withings_failure _ -> "withings_failure"
  | Invalid_field _ -> "invalid_field"
  | Numeric_overflow _ -> "numeric_overflow"
  | Unrepresentable_result _ -> "unrepresentable_result"
  | Storage_decoding _ -> "storage_decoding"
  | Unauthorized -> "unauthorized"
  | Not_found -> "not_found"
  | Invalid_input _ -> "invalid_input"
  | Conflict _ -> "conflict"
  | Storage_error _ -> "storage_error"
  | Calendar_date_addition _ | Calendar_day_bounds _ | Calendar_local_date _ ->
      "calendar_limit"
  | Invalid_date _ -> "invalid_date"
  | Unsupported_timezone _ -> "unsupported_timezone"
  | Reversed_date_range _ -> "reversed_date_range"
  | Date_range_too_long _ -> "date_range_too_long"
  | Nonpositive_argument _ -> "nonpositive_argument"
  | Rejected _ -> "rejected"

let call_response ~request_id = function
  | Ok result -> response ~id:request_id result
  | Error (_, Error.Not_found) ->
      response ~id:request_id (Mcp_json.tool_error "record not found")
  | Error
      ( _,
        (( Error.Calendar_date_addition _ | Error.Calendar_day_bounds _
         | Error.Calendar_local_date _ | Error.Invalid_date _
         | Error.Unsupported_timezone _ | Error.Reversed_date_range _
         | Error.Date_range_too_long _ | Error.Nonpositive_argument _
         | Error.Rejected _ | Error.Conflict _ | Error.Invalid_input _
         | Error.Invalid_field _ ) as error) ) ->
      response ~id:request_id
        (Mcp_json.tool_error (argument_error_message error))
  | Error
      (_, ((Error.Numeric_overflow _ | Error.Unrepresentable_result _) as error))
    ->
      response ~id:request_id (Mcp_json.tool_error (Error.to_string error))
  | Error
      ( _,
        ( Error.Unauthorized | Error.Storage_error _ | Error.Storage_decoding _
        | Error.Withings_failure _ ) ) ->
      rpc_error ~id:request_id (-32603) "Internal error"

(* Bounded and escaped: these strings come from the client. *)
let truncated text =
  if String.length text > 64 then String.prefix text 64 ^ "..." else text

let loggable text = Printf.sprintf "%S" (truncated text)

let argument_keys = function
  | Some (`Assoc fields) ->
      let keys = List.map fields ~f:fst in
      let shown = List.take keys 32 |> List.map ~f:loggable in
      "["
      ^ String.concat ~sep:"," shown
      ^ (if List.length keys > 32 then ",..." else "")
      ^ "]"
  | None -> "-"
  | Some _ -> "<not an object>"

(* One line per tools/call, payload values never included: the tool, the
   request id, the argument keys and the outcome. A call that reaches kcal
   always leaves this line, even when it is rejected before any business
   logic runs, so a missing line means the call never got here. *)
let log_tool_call ~name ~request_id ~arguments outcome =
  let log, outcome =
    match outcome with
    | `Ok -> (Tool_log.info, "ok")
    | `Tool_error (stage, error) ->
        ( Tool_log.info,
          Printf.sprintf "tool_error stage=%s error=%s"
            (match stage with Arguments -> "arguments" | Service -> "service")
            (error_code error) )
    | `Rpc_error (code, cause) ->
        ( Tool_log.warn,
          Printf.sprintf "rpc_error error=%d%s" code
            (match cause with None -> "" | Some cause -> " cause=" ^ cause) )
    | `Forbidden scope ->
        (Tool_log.warn, Printf.sprintf "forbidden scope=%s" scope)
  in
  log (fun m ->
      m "tool=%s request_id=%s keys=%s outcome=%s"
        (Option.value_map name ~default:"-" ~f:loggable)
        (match request_id with
        | `Null -> "-"
        | id -> truncated (Yojson.Safe.to_string id))
        (argument_keys arguments) outcome)

let call_outcome = function
  | Ok _ -> `Ok
  | Error
      ( _,
        (( Error.Unauthorized | Error.Storage_error _ | Error.Storage_decoding _
         | Error.Withings_failure _ ) as error) ) ->
      `Rpc_error (-32603, Some (error_code error))
  | Error (stage, error) -> `Tool_error (stage, error)

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
          | Some (`Assoc _) ->
              Ok
                (invalid_params ~id:request_id ~field:"params.protocolVersion"
                   ~reason:
                     ("must be \"" ^ legacy_protocol_version
                    ^ "\" for initialize; newer clients use server/discover \
                       and protocol version " ^ stateless_protocol_version))
          | _ ->
              Ok
                (invalid_params ~id:request_id ~field:"params"
                   ~reason:"must be an object with protocolVersion"))
      | Some (`String "tools/list") when valid_request ->
          Ok
            (response ~id:request_id
               (Mcp_json.cacheable [ ("tools", `List Mcp_schema.tools) ]))
      | Some (`String "tools/call") when valid_request -> (
          let params = List.Assoc.find fields ~equal:String.equal "params" in
          let find name =
            match params with
            | Some (`Assoc params) ->
                List.Assoc.find params ~equal:String.equal name
            | _ -> None
          in
          let arguments = find "arguments" in
          let rejected ~name ~field ~reason =
            log_tool_call ~name ~request_id ~arguments
              (`Rpc_error (-32602, None));
            Ok (invalid_params ~id:request_id ~field ~reason)
          in
          match (params, find "name") with
          | Some (`Assoc _), Some (`String name) -> (
              match required_scope name with
              | Some scope when not (Auth.has_scope identity scope) ->
                  log_tool_call ~name:(Some name) ~request_id ~arguments
                    (`Forbidden scope);
                  Error `Forbidden
              | _ ->
                  (* Omitted arguments are an empty object, so the parser
                     explains what is missing. *)
                  let result =
                    call ?withings service identity name
                      (Option.value arguments ~default:(`Assoc []))
                  in
                  log_tool_call ~name:(Some name) ~request_id ~arguments
                    (call_outcome result);
                  Ok (call_response ~request_id result))
          | Some (`Assoc _), None ->
              rejected ~name:None ~field:"params.name"
                ~reason:"is required: the name of a tool from tools/list"
          | Some (`Assoc _), Some _ ->
              rejected ~name:None ~field:"params.name"
                ~reason:"must be a string: the name of a tool from tools/list"
          | _ ->
              rejected ~name:None ~field:"params"
                ~reason:"must be an object with name and arguments")
      | Some (`String _) when valid_request ->
          Ok (rpc_error ~id:request_id (-32601) "Method not found")
      | _ -> Ok (rpc_error ~id:request_id (-32600) "Invalid Request"))
  | _ -> Ok (rpc_error (-32700) "Parse error")
