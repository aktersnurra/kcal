type response = { status : int; headers : (string * string) list; body : string }

let json status body = { status; headers = [ ("content-type", "application/json") ]; body }
let plain status body = { status; headers = []; body }
let forbidden = plain 403 "Forbidden"

let json_media_type value =
  match String.split_on_char ';' (String.lowercase_ascii value) with
  | media_type :: parameters ->
      String.trim media_type = "application/json"
      && List.for_all (fun parameter -> String.trim parameter <> "") parameters
  | [] -> false

let bearer headers =
  match List.assoc_opt "authorization" (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) headers) with
  | Some value when String.starts_with ~prefix:"Bearer " value && String.length value > 7 -> Some (String.sub value 7 (String.length value - 7))
  | _ -> None

type withings_config = {
  oauth : Withings_oauth.t;
  client : (module Withings.S);
  token_key : bytes;
  sync : User.t -> Withings_connection.t -> (unit, Error.t) result;
  callback_url : string;
  client_id : string;
  redirect_uri : string;
}

let query path =
  let uri = Uri.of_string path in
  (Uri.path uri, Uri.query uri)

let query_value name query = match List.assoc_opt name query with Some [ value ] when value <> "" -> Some value | _ -> None

let authenticated auth headers ?scope f =
  match bearer headers with
  | None -> plain 401 "Unauthorized"
  | Some token ->
      (match Auth.authenticate_bearer auth token with
      | Error Error.Unauthorized -> plain 401 "Unauthorized"
      | Error _ -> plain 500 "Internal Server Error"
      | Ok identity ->
          (match scope with
          | Some scope when not (Auth.has_scope identity scope) -> forbidden
          | None | Some _ -> f identity))

let withings_callback withings query =
  match query_value "state" query, query_value "code" query with
  | Some state, Some code ->
      (match Withings_oauth.consume_callback_state withings.oauth ~state with
      | Error _ -> plain 400 "Invalid OAuth state"
      | Ok user ->
          let module Client = (val withings.client : Withings.S) in
          (match Client.exchange_code ~code with
          | Error _ -> plain 502 "Withings authorization failed"
          | Ok credentials ->
              (match Store_sqlite.ensure_withings_connection withings.oauth.store ~user ~withings_user_id:credentials.withings_user_id with
              | Error _ -> plain 500 "Internal Server Error"
              | Ok connection ->
                  (match Store_sqlite.save_withings_credentials withings.oauth.store ~user ~key:withings.token_key credentials with
                  | Error _ -> plain 500 "Internal Server Error"
                  | Ok connection ->
                      (match withings.sync user connection with
                      | Error _ -> plain 502 "Withings synchronization failed"
                      | Ok () ->
                          (match Client.subscribe ~access_token:credentials.access_token ~callback_url:withings.callback_url with
                          | Ok () -> plain 200 "Withings connected"
                          | Error _ -> plain 502 "Withings subscription failed"))))))
  | _ -> plain 400 "Invalid OAuth callback"

let webhook ~schedule_sync withings headers body =
  match List.assoc_opt "content-type" (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) headers) with
  | Some content_type when String.trim (String.lowercase_ascii content_type) = "application/x-www-form-urlencoded" && String.length body <= 8192 ->
      (try
         let fields = Uri.query (Uri.of_string ("?" ^ body)) in
         match query_value "userid" fields, query_value "appli" fields with
         | Some identity, Some "1" when String.length identity <= 128 ->
             (match Store_sqlite.withings_connection_for_identity withings.oauth.store ~withings_user_id:identity with
             | Ok (user, connection) ->
                 schedule_sync (fun () -> ignore (withings.sync user connection));
                 plain 200 "OK"
             | Error Error.Not_found -> plain 404 "Not Found"
             | Error _ -> plain 500 "Internal Server Error")
         | _ -> plain 400 "Malformed webhook"
       with _ -> plain 400 "Malformed webhook")
  | Some _ -> plain 415 "Unsupported Media Type"
  | None -> plain 415 "Unsupported Media Type"

let mcp_handle (withings : withings_config option) ~service ~identity request =
  match withings with
  | None -> Mcp.handle ~service ~identity request
  | Some withings ->
      Mcp.handle ~withings:Mcp.{ oauth = withings.oauth; client_id = withings.client_id;
                                 redirect_uri = withings.redirect_uri } ~service ~identity request

let handle_withings ~schedule_sync ~withings ~auth ~service ~method_ ~path ~headers ~body =
  let route, query = query path in
  match method_, route with
  | `GET, "/health" -> json 200 "{\"status\":\"ok\"}"
  | `GET, "/withings/connect" ->
      (match withings with
      | Some withings -> authenticated auth headers ~scope:"withings:manage" (fun identity -> match Withings_oauth.begin_authorization ~client_id:withings.client_id ~redirect_uri:withings.redirect_uri withings.oauth ~user:identity.user with Ok (_, url) -> { status = 302; headers = [ ("location", url) ]; body = "" } | Error _ -> plain 500 "Internal Server Error")
      | None -> plain 404 "Not Found")
  | `GET, "/withings/callback" ->
      (match withings with Some withings -> withings_callback withings query | None -> plain 404 "Not Found")
  | `HEAD, "/withings/webhook" -> plain 200 ""
  | `POST, "/withings/webhook" -> (match withings with Some withings -> webhook ~schedule_sync withings headers body | None -> plain 404 "Not Found")
  | `POST, "/mcp" ->
      authenticated auth headers (fun identity ->
        match List.assoc_opt "content-type" (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) headers) with
        | Some content_type when json_media_type content_type ->
            (try
               match mcp_handle withings ~service ~identity (Yojson.Safe.from_string body) with
               | Ok response -> json 200 (Yojson.Safe.to_string response)
               | Error `Forbidden -> forbidden
             with Yojson.Json_error _ ->
               json 400 (Yojson.Safe.to_string (Mcp.rpc_error (-32700) "Parse error")))
        | _ -> plain 415 "Unsupported Media Type")
  | _ -> plain 404 "Not Found"

let handle ~auth ~service ~method_ ~path ~headers ~body =
  handle_withings ~schedule_sync:(fun sync -> sync ()) ~withings:None ~auth ~service ~method_ ~path ~headers ~body

let split_address value =
  match String.rindex_opt value ':' with
  | Some index when index > 0 && index < String.length value - 1 ->
      (String.sub value 0 index, String.sub value (index + 1) (String.length value - index - 1))
  | _ -> invalid_arg "invalid listen address"

let status = function
  | 200 -> `OK | 400 -> `Bad_request
  | 401 -> `Unauthorized | 403 -> `Forbidden | 404 -> `Not_found | 413 -> `Payload_too_large
  | 415 -> `Unsupported_media_type | 302 -> `Found | _ -> `Internal_server_error

let serve_request ~sw ~withings ~auth ~service reqd =
  let request = Httpun.Reqd.request reqd in
  let method_ = match request.meth with `GET -> `GET | `POST -> `POST | `HEAD -> `HEAD | _ -> `OTHER in
  let respond body =
    let response =
      match method_ with
      | `OTHER -> plain 404 "Not Found"
      | (`GET | `POST | `HEAD) ->
          handle_withings
            ~schedule_sync:(fun sync -> Eio.Fiber.fork ~sw sync)
            ~withings ~auth ~service ~method_ ~path:request.target
            ~headers:(Httpun.Headers.to_list request.headers) ~body
    in
    let headers = List.fold_left (fun headers (key, value) -> Httpun.Headers.add headers key value) Httpun.Headers.empty response.headers in
    Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers (status response.status)) response.body
  in
  let buffer = Buffer.create 1024 in
  let responded = ref false in
  let respond_once body = if not !responded then (responded := true; respond body) in
  let rec read () =
    Httpun.Body.Reader.schedule_read (Httpun.Reqd.request_body reqd)
      ~on_eof:(fun () -> respond_once (Buffer.contents buffer))
      ~on_read:(fun bytes ~off ~len ->
        if Buffer.length buffer + len > 1_048_576 then (
          responded := true;
          Httpun.Reqd.respond_with_string reqd (Httpun.Response.create (status 413)) "Payload Too Large")
        else (Buffer.add_string buffer (Bigstringaf.substring bytes ~off ~len); read ()))
  in
  read ()

let run ~withings env ~config ~auth service : unit =
  let host, port = split_address config.Config.listen_address in
  Eio.Switch.run @@ fun sw ->
  let address = List.hd (Eio.Net.getaddrinfo_stream ~service:port env#net host) in
  let listener = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:128 env#net address in
  let handler = Httpun_eio.Server.create_connection_handler ~sw
    ~request_handler:(fun _ reqd -> serve_request ~sw ~withings ~auth ~service reqd.Gluten.Reqd.reqd)
    ~error_handler:(fun _ ?request:_ _ _ -> ()) in
  Eio.Net.run_server ~on_error:(fun _ -> ()) listener (fun socket address -> handler address socket)
