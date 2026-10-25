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

let bearer headers =
  match
    List.Assoc.find
      (List.map ~f:(fun (k, v) -> (String.lowercase k, v)) headers)
      ~equal:String.equal "authorization"
  with
  | Some value
    when String.is_prefix ~prefix:"Bearer " value && String.length value > 7 ->
      Some (String.sub value ~pos:7 ~len:(String.length value - 7))
  | _ -> None

type withings_config = {
  oauth : Withings_oauth.t;
  client : (module Withings.S);
  token_key : Secret.Key.t;
  sync : User.t -> Withings_connection.connected -> (unit, Error.t) Result.t;
  callback_url : string;
  client_id : string;
  redirect_uri : string;
}

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

let withings_callback service withings query =
  match (query_value "state" query, query_value "code" query) with
  | Some state, Some code -> (
      match
        Service.complete_withings_authorization service ~oauth:withings.oauth
          ~key:withings.token_key ~client:withings.client
          ~redirect_uri:withings.redirect_uri
          ~callback_url:withings.callback_url ~sync:withings.sync ~state ~code
      with
      | Ok () -> plain 200 "Withings connected"
      | Error failure ->
          let status, body, error =
            match failure with
            | Service.Invalid_state error -> (400, "Invalid OAuth state", error)
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

let webhook ~schedule_sync service withings headers body =
  match
    List.Assoc.find
      (List.map ~f:(fun (k, v) -> (String.lowercase k, v)) headers)
      ~equal:String.equal "content-type"
  with
  | Some content_type
    when String.equal
           (String.strip (String.lowercase content_type))
           "application/x-www-form-urlencoded"
         && String.length body <= 8192 -> (
      let fields = Uri.query (Uri.of_string ("?" ^ body)) in
      match (query_value "userid" fields, query_value "appli" fields) with
      | Some identity, Some "1" when String.length identity <= 128 -> (
          match
            Service.withings_connection_for_identity service
              ~withings_user_id:identity
          with
          | Ok (user, connection) ->
              Withings_log.info (fun m ->
                  m "user=%s webhook notification received, scheduling sync"
                    (User_id.to_string user.User.id));
              schedule_sync (fun () ->
                  match withings.sync user connection with
                  | Ok () -> ()
                  | Error error ->
                      Withings_log.err (fun m ->
                          m "user=%s webhook-triggered sync failed: %s"
                            (User_id.to_string user.User.id)
                            (Error.to_string error)));
              plain 200 "OK"
          | Error Error.Not_found -> plain 404 "Not Found"
          | Error error ->
              Withings_log.err (fun m ->
                  m "webhook lookup for withings_user_id=%s failed: %s" identity
                    (Error.to_string error));
              plain 500 "Internal Server Error")
      | _ -> plain 400 "Malformed webhook")
  | Some _ -> plain 415 "Unsupported Media Type"
  | None -> plain 415 "Unsupported Media Type"

let metadata_alias = "/.well-known/oauth-protected-resource"

let is_metadata_route protected_resource route =
  String.equal route metadata_alias
  ||
  match protected_resource with
  | Some { metadata_url; _ } ->
      String.equal route (Uri.path (Uri.of_string metadata_url))
  | None -> false

let mcp_handle (withings : withings_config option) ~service ~identity request =
  match withings with
  | None -> Mcp.handle ~service ~identity request
  | Some withings ->
      Mcp.handle
        ~withings:
          {
            Mcp.oauth = withings.oauth;
            client_id = withings.client_id;
            redirect_uri = withings.redirect_uri;
          }
        ~service ~identity request

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
  | `GET, "/health" -> json 200 "{\"status\":\"ok\"}"
  | `GET, "/withings/connect" -> (
      match withings with
      | Some withings ->
          authenticated ?protected_resource auth headers
            ~scope:"withings:manage" (fun identity ->
              match
                Service.begin_withings_authorization
                  ~client_id:withings.client_id
                  ~redirect_uri:withings.redirect_uri ~oauth:withings.oauth
                  ~user:identity.user
              with
              | Ok (_, url) ->
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
      | Some withings -> withings_callback service withings query
      | None -> plain 404 "Not Found")
  | `HEAD, "/withings/webhook" -> plain 200 ""
  | `POST, "/withings/webhook" -> (
      match withings with
      | Some withings -> webhook ~schedule_sync service withings headers body
      | None -> plain 404 "Not Found")
  | `POST, "/mcp" ->
      authenticated ?protected_resource auth headers (fun identity ->
          match
            List.Assoc.find
              (List.map ~f:(fun (k, v) -> (String.lowercase k, v)) headers)
              ~equal:String.equal "content-type"
          with
          | Some content_type when is_json_media_type content_type -> (
              let parsed =
                try Ok (Yojson.Safe.from_string body)
                with Yojson.Json_error message -> Error message
              in
              match parsed with
              | Error message ->
                  Mcp_log.warn (fun m ->
                      m "rejected malformed JSON-RPC request: %s" message);
                  json 400
                    (Yojson.Safe.to_string
                       (Mcp.rpc_error (-32700) "Parse error"))
              | Ok request -> (
                  let method_name, notification =
                    match request with
                    | `Assoc fields ->
                        let method_name =
                          match
                            List.Assoc.find fields ~equal:String.equal "method"
                          with
                          | Some (`String method_name) -> method_name
                          | _ -> "<invalid>"
                        in
                        ( method_name,
                          not (List.Assoc.mem fields ~equal:String.equal "id")
                        )
                    | _ -> ("<invalid>", false)
                  in
                  match mcp_handle withings ~service ~identity request with
                  | Ok response ->
                      let outcome =
                        match response with
                        | `Assoc fields
                          when List.Assoc.mem fields ~equal:String.equal "error"
                          ->
                            "error"
                        | `Assoc fields
                          when List.Assoc.mem fields ~equal:String.equal
                                 "result" ->
                            "result"
                        | _ -> "invalid"
                      in
                      Mcp_log.info (fun m ->
                          m "method=%s notification=%b outcome=%s" method_name
                            notification outcome);
                      if notification then plain 202 ""
                      else json 200 (Yojson.Safe.to_string response)
                  | Error `Forbidden ->
                      Mcp_log.warn (fun m ->
                          m "method=%s notification=%b outcome=forbidden"
                            method_name notification);
                      forbidden))
          | _ -> plain 415 "Unsupported Media Type")
  | _ -> plain 404 "Not Found"

let handle ?protected_resource ~auth ~service ~method_ ~path ~headers ~body () =
  handle_withings ~protected_resource
    ~schedule_sync:(fun sync -> sync ())
    ~withings:None ~auth ~service ~method_ ~path ~headers ~body

let split_address value =
  match String.rindex value ':' with
  | Some index when index > 0 && index < String.length value - 1 ->
      ( String.sub value ~pos:0 ~len:index,
        String.sub value ~pos:(index + 1) ~len:(String.length value - index - 1)
      )
  | _ -> invalid_arg "invalid listen address"

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
  let path =
    match String.index request.target '?' with
    | Some index -> String.sub request.target ~pos:0 ~len:index
    | None -> request.target
  in
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

let run ~withings env ~config ~auth service : unit =
  let protected_resource =
    Some
      {
        resource = config.Config.oidc_audience;
        authorization_server = config.oidc_issuer;
        metadata_url =
          Config.protected_resource_metadata_url
            ~audience:config.Config.oidc_audience;
      }
  in
  let host, port = split_address config.Config.listen_address in
  Eio.Switch.run @@ fun sw ->
  let address =
    List.hd_exn (Eio.Net.getaddrinfo_stream ~service:port env#net host)
  in
  let listener =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:128 env#net address
  in
  let handler =
    Httpun_eio.Server.create_connection_handler ~sw
      ~request_handler:(fun _ reqd ->
        serve_request ~sw ~protected_resource ~withings ~auth ~service
          reqd.Gluten.Reqd.reqd)
      ~error_handler:(fun _ ?request:_ _ _ -> ())
  in
  Eio.Net.run_server
    ~on_error:(fun _ -> ())
    listener
    (fun socket address -> handler address socket)

module For_testing = struct
  let framed_response = framed_response
end
