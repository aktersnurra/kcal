module Domain_error = Error
open Base
module Error = Domain_error

let http_log_src = Logs.Src.create "kcal.http" ~doc:"HTTP request handling"

module Http_log = (val Logs.src_log http_log_src : Logs.LOG)

let mcp_log_src = Logs.Src.create "kcal.mcp" ~doc:"MCP JSON-RPC handling"

module Mcp_log = (val Logs.src_log mcp_log_src : Logs.LOG)

let withings_log_src =
  Logs.Src.create "kcal.withings.http"
    ~doc:"Withings OAuth callback and webhook handling"

module Withings_log = (val Logs.src_log withings_log_src : Logs.LOG)

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

type protected_resource = {
  resource : string;
  authorization_server : string;
  metadata_url : string;
}

let protected_resource_metadata ~resource ~authorization_server =
  `Assoc
    [
      ("resource", `String resource);
      ("authorization_servers", `List [ `String authorization_server ]);
      ( "scopes_supported",
        `List
          [
            `String "ledger:read";
            `String "ledger:write";
            `String "withings:manage";
          ] );
    ]

let json status body =
  { status; headers = [ ("content-type", "application/json") ]; body }

let plain status body = { status; headers = []; body }
let forbidden = plain 403 "Forbidden"

let unauthorized protected_resource =
  let headers =
    match protected_resource with
    | Some { metadata_url; _ } ->
        [
          ( "www-authenticate",
            Stdlib.Printf.sprintf {|Bearer resource_metadata="%s"|} metadata_url
          );
        ]
    | None -> []
  in
  { status = 401; headers; body = "Unauthorized" }

let is_json_media_type value =
  match String.split (String.lowercase value) ~on:';' with
  | media_type :: parameters ->
      String.equal (String.strip media_type) "application/json"
      && List.for_all
           ~f:(fun parameter -> not (String.equal (String.strip parameter) ""))
           parameters
  | [] -> false

let header name headers =
  List.find_map headers ~f:(fun (key, value) ->
      Option.some_if (String.equal (String.lowercase key) name) value)

(* The scheme name is case-insensitive (RFC 9110, section 11.1). *)
let bearer headers =
  match header "authorization" headers with
  | Some value
    when String.length value > 7
         && String.equal (String.lowercase (String.prefix value 7)) "bearer " ->
      Some (String.drop_prefix value 7)
  | _ -> None

let query path =
  let uri = Uri.of_string path in
  (Uri.path uri, Uri.query uri)

let query_value name query =
  match List.Assoc.find query ~equal:String.equal name with
  | Some [ value ] when not (String.equal value "") -> Some value
  | _ -> None

let authenticated ?protected_resource auth headers ?scope f =
  match bearer headers with
  | None -> unauthorized protected_resource
  | Some token -> (
      match Auth.authenticate_bearer auth token with
      | Error Error.Unauthorized -> unauthorized protected_resource
      | Error error ->
          Http_log.err (fun m ->
              m "bearer authentication failed unexpectedly: %s"
                (Error.to_string error));
          plain 500 "Internal Server Error"
      | Ok identity -> (
          match scope with
          | Some scope when not (Auth.has_scope identity scope) -> forbidden
          | None | Some _ -> f identity))

let withings_callback withings query =
  match (query_value "state" query, query_value "code" query) with
  | Some state, Some code -> (
      match
        Withings_integration.complete_authorization withings ~state ~code
      with
      | Ok () -> plain 200 "Withings connected"
      | Error failure ->
          let status, body, error =
            match failure with
            | Withings_integration.Invalid_state error ->
                (400, "Invalid OAuth state", error)
            | Exchange_failed error ->
                (502, "Withings authorization failed", error)
            | Persistence_failed error -> (500, "Internal Server Error", error)
            | Sync_failed error ->
                (502, "Withings synchronization failed", error)
            | Subscription_failed error ->
                (502, "Withings subscription failed", error)
          in
          Withings_log.err (fun m -> m "%s: %s" body (Error.to_string error));
          plain status body)
  | _ -> plain 400 "Invalid OAuth callback"

(* The endpoint is unauthenticated: Withings only posts an account id. An
   unknown account gets the same answer as a known one, so the response does
   not reveal which accounts are connected. *)
let webhook ~schedule_sync withings headers body =
  match header "content-type" headers with
  | Some content_type
    when String.equal
           (String.strip (String.lowercase content_type))
           "application/x-www-form-urlencoded"
         && String.length body <= 8192 -> (
      let fields = Uri.query (Uri.of_string ("?" ^ body)) in
      match (query_value "userid" fields, query_value "appli" fields) with
      | Some identity, Some "1" when String.length identity <= 128 -> (
          match
            Withings_integration.notify withings ~withings_user_id:identity
              ~schedule:schedule_sync
          with
          | Ok (Sync_requested user) ->
              Withings_log.info (fun m ->
                  m "user=%s webhook notification received"
                    (User_id.to_string user.User.id));
              plain 200 "OK"
          | Ok Unknown_account ->
              Withings_log.info (fun m ->
                  m "webhook notification for an unconnected account ignored");
              plain 200 "OK"
          | Error error ->
              Withings_log.err (fun m ->
                  m "webhook lookup for withings_user_id=%s failed: %s" identity
                    (Error.to_string error));
              plain 500 "Internal Server Error")
      | _ -> plain 400 "Malformed webhook")
  | Some _ | None -> plain 415 "Unsupported Media Type"

let metadata_alias = "/.well-known/oauth-protected-resource"

let is_metadata_route protected_resource route =
  String.equal route metadata_alias
  ||
  match protected_resource with
  | Some { metadata_url; _ } ->
      String.equal route (Uri.path (Uri.of_string metadata_url))
  | None -> false

(* What the request log says about a JSON-RPC message: its method and whether
   it is a notification, which gets no response body. *)
let rpc_summary = function
  | `Assoc fields ->
      let method_name =
        match List.Assoc.find fields ~equal:String.equal "method" with
        | Some (`String method_name) -> method_name
        | _ -> "<invalid>"
      in
      (method_name, not (List.Assoc.mem fields ~equal:String.equal "id"))
  | _ -> ("<invalid>", false)

let rpc_outcome = function
  | `Assoc fields when List.Assoc.mem fields ~equal:String.equal "error" ->
      "error"
  | `Assoc fields when List.Assoc.mem fields ~equal:String.equal "result" ->
      "result"
  | _ -> "invalid"

let mcp ~withings ~service ~identity ~headers ~body =
  match header "content-type" headers with
  | Some content_type when is_json_media_type content_type -> (
      match Yojson.Safe.from_string body with
      | exception Yojson.Json_error message ->
          Mcp_log.warn (fun m ->
              m "rejected malformed JSON-RPC request: %s" message);
          json 400
            (Yojson.Safe.to_string (Mcp.rpc_error (-32700) "Parse error"))
      | request -> (
          let method_name, notification = rpc_summary request in
          match Mcp.handle ?withings ~service ~identity request with
          | Ok response ->
              Mcp_log.info (fun m ->
                  m "method=%s notification=%b outcome=%s" method_name
                    notification (rpc_outcome response));
              if notification then plain 202 ""
              else json 200 (Mcp_json.to_string response)
          | Error `Forbidden ->
              Mcp_log.warn (fun m ->
                  m "method=%s notification=%b outcome=forbidden" method_name
                    notification);
              forbidden))
  | _ -> plain 415 "Unsupported Media Type"

let handle_withings ~protected_resource ~schedule_sync ~withings ~auth ~service
    ~method_ ~path ~headers ~body =
  let route, query = query path in
  match (method_, route) with
  | `GET, route when is_metadata_route protected_resource route -> (
      match protected_resource with
      | Some { resource; authorization_server; _ } ->
          json 200
            (Yojson.Safe.to_string
               (protected_resource_metadata ~resource ~authorization_server))
      | None -> plain 404 "Not Found")
  | `GET, "/health" -> (
      match Service.check_storage service with
      | Ok () -> json 200 "{\"status\":\"ok\"}"
      | Error error ->
          Http_log.err (fun m ->
              m "health check failed: %s" (Error.to_string error));
          json 503 "{\"status\":\"unavailable\"}")
  | `GET, "/withings/connect" -> (
      match withings with
      | Some withings ->
          authenticated ?protected_resource auth headers
            ~scope:"withings:manage" (fun identity ->
              match
                Withings_integration.begin_authorization withings
                  ~user:identity.user
              with
              | Ok url ->
                  { status = 302; headers = [ ("location", url) ]; body = "" }
              | Error error ->
                  Withings_log.err (fun m ->
                      m "user=%s failed to begin Withings authorization: %s"
                        (User_id.to_string identity.user.User.id)
                        (Error.to_string error));
                  plain 500 "Internal Server Error")
      | None -> plain 404 "Not Found")
  | `GET, "/withings/callback" -> (
      match withings with
      | Some withings -> withings_callback withings query
      | None -> plain 404 "Not Found")
  | `HEAD, "/withings/webhook" -> plain 200 ""
  | `POST, "/withings/webhook" -> (
      match withings with
      | Some withings -> webhook ~schedule_sync withings headers body
      | None -> plain 404 "Not Found")
  | `POST, "/mcp" ->
      authenticated ?protected_resource auth headers (fun identity ->
          mcp ~withings ~service ~identity ~headers ~body)
  | _ -> plain 404 "Not Found"

let handle ?protected_resource ~auth ~service ~method_ ~path ~headers ~body () =
  handle_withings ~protected_resource
    ~schedule_sync:(fun sync -> sync ())
    ~withings:None ~auth ~service ~method_ ~path ~headers ~body

let status = function
  | 200 -> `OK
  | 202 -> `Accepted
  | 400 -> `Bad_request
  | 401 -> `Unauthorized
  | 403 -> `Forbidden
  | 404 -> `Not_found
  | 413 -> `Payload_too_large
  | 415 -> `Unsupported_media_type
  | 502 -> `Bad_gateway
  | 503 -> `Service_unavailable
  | 302 -> `Found
  | _ -> `Internal_server_error

let framed_response response =
  let headers =
    List.fold response.headers ~init:Httpun.Headers.empty
      ~f:(fun headers (key, value) -> Httpun.Headers.add headers key value)
    |> fun headers ->
    Httpun.Headers.add
      (Httpun.Headers.remove headers "content-length")
      "content-length"
      (Int.to_string (String.length response.body))
  in
  Httpun.Response.create ~headers (status response.status)

let respond_with_string reqd response =
  Httpun.Reqd.respond_with_string reqd (framed_response response) response.body

let path_of_target target =
  match String.index target '?' with
  | Some index -> String.prefix target index
  | None -> target

let serve_request ~sw ~protected_resource ~withings ~auth ~service reqd =
  let request = Httpun.Reqd.request reqd in
  let method_ =
    match request.meth with
    | `GET -> `GET
    | `POST -> `POST
    | `HEAD -> `HEAD
    | _ -> `OTHER
  in
  let method_name =
    match method_ with
    | `GET -> "GET"
    | `POST -> "POST"
    | `HEAD -> "HEAD"
    | `OTHER -> "OTHER"
  in
  let path = path_of_target request.target in
  let respond_with_log response =
    let log =
      if response.status >= 500 then Http_log.err
      else if response.status >= 400 then Http_log.warn
      else Http_log.info
    in
    log (fun m ->
        m "method=%s path=%s status=%d" method_name path response.status);
    respond_with_string reqd response
  in
  let respond body =
    let response =
      match method_ with
      | `OTHER -> plain 404 "Not Found"
      | `GET | `POST | `HEAD ->
          handle_withings ~protected_resource
            ~schedule_sync:(fun sync -> Eio.Fiber.fork ~sw sync)
            ~withings ~auth ~service ~method_ ~path:request.target
            ~headers:(Httpun.Headers.to_list request.headers)
            ~body
    in
    respond_with_log response
  in
  let buffer = Stdlib.Buffer.create 1024 in
  let responded = ref false in
  let respond_once body =
    if not !responded then (
      responded := true;
      respond body)
  in
  let rec read () =
    Httpun.Body.Reader.schedule_read
      (Httpun.Reqd.request_body reqd)
      ~on_eof:(fun () -> respond_once (Stdlib.Buffer.contents buffer))
      ~on_read:(fun bytes ~off ~len ->
        if Stdlib.Buffer.length buffer + len > 1_048_576 then (
          responded := true;
          respond_with_log (plain 413 "Payload Too Large"))
        else (
          Stdlib.Buffer.add_string buffer
            (Bigstringaf.substring bytes ~off ~len);
          read ()))
  in
  read ()

(* httpun calls this when it cannot hand a request to serve_request: a
   malformed or truncated request, or an exception while handling one. It used
   to be ignored, so such a request got no response and left no log line. *)
let transport_error ?request error start_response =
  let path =
    match request with
    | Some (request : Httpun.Request.t) -> path_of_target request.target
    | None -> "-"
  in
  let kind =
    match error with
    | `Bad_request -> "bad_request"
    | `Bad_gateway -> "bad_gateway"
    | `Internal_server_error -> "internal_server_error"
    | `Exn exn -> "exception " ^ Exn.to_string exn
  in
  Http_log.err (fun m ->
      m "request failed before a response: path=%s error=%s" path kind);
  let body = start_response Httpun.Headers.empty in
  Httpun.Body.Writer.write_string body
    (match error with
    | `Bad_request -> "Bad Request"
    | _ -> "Internal Server Error");
  Httpun.Body.Writer.close body

let run ~withings env ~config ~auth service : unit =
  let protected_resource =
    Some
      {
        resource = config.Config.protected_resource;
        authorization_server = config.oidc_issuer;
        metadata_url = config.Config.metadata_url;
      }
  in
  let { Config.host; port } = config.Config.listener in
  Eio.Switch.run @@ fun sw ->
  let address =
    List.hd_exn
      (Eio.Net.getaddrinfo_stream ~service:(Int.to_string port) env#net host)
  in
  let listener =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:128 env#net address
  in
  let handler =
    Httpun_eio.Server.create_connection_handler ~sw
      ~request_handler:(fun _ reqd ->
        serve_request ~sw ~protected_resource ~withings ~auth ~service
          reqd.Gluten.Reqd.reqd)
      ~error_handler:(fun _ ?request error start_response ->
        transport_error ?request error start_response)
  in
  Eio.Net.run_server
    ~on_error:(fun exn ->
      Http_log.warn (fun m -> m "connection failed: %s" (Exn.to_string exn)))
    listener
    (fun socket address -> handler address socket)

module For_testing = struct
  let framed_response = framed_response
end
