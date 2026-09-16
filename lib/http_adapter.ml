type response = { status : int; headers : (string * string) list; body : string }

let json status body = { status; headers = [ ("content-type", "application/json") ]; body }
let plain status body = { status; headers = []; body }

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

let handle ~auth ~service ~method_ ~path ~headers ~body =
  match method_, path with
  | `GET, "/health" -> json 200 "{\"status\":\"ok\"}"
  | `POST, "/mcp" ->
      (match List.assoc_opt "content-type" (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) headers), bearer headers with
      | Some content_type, Some token when json_media_type content_type ->
          (match Auth.authenticate_bearer auth token with
          | Error Error.Unauthorized -> plain 401 "Unauthorized"
          | Error _ -> plain 500 "Internal Server Error"
          | Ok user ->
              (try json 200 (Yojson.Safe.to_string (Mcp.handle ~service ~user (Yojson.Safe.from_string body)))
               with Yojson.Json_error _ ->
                 json 400 (Yojson.Safe.to_string (Mcp.rpc_error (-32700) "Parse error"))))
      | _, None -> plain 401 "Unauthorized"
      | _ -> plain 415 "Unsupported Media Type")
  | _ -> plain 404 "Not Found"

let split_address value =
  match String.rindex_opt value ':' with
  | Some index when index > 0 && index < String.length value - 1 ->
      (String.sub value 0 index, String.sub value (index + 1) (String.length value - index - 1))
  | _ -> invalid_arg "invalid listen address"

let status = function
  | 200 -> `OK | 400 -> `Bad_request
  | 401 -> `Unauthorized | 404 -> `Not_found | 413 -> `Payload_too_large
  | 415 -> `Unsupported_media_type | _ -> `Internal_server_error

let serve_request ~auth ~service reqd =
  let request = Httpun.Reqd.request reqd in
  let method_ = match request.meth with `GET -> `GET | `POST -> `POST | _ -> `OTHER in
  let respond body =
    let response =
      match method_ with
      | `OTHER -> plain 404 "Not Found"
      | (`GET | `POST) -> handle ~auth ~service ~method_ ~path:request.target
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

let run env ~config ~auth service : unit =
  let host, port = split_address config.Config.listen_address in
  Eio.Switch.run @@ fun sw ->
  let address = List.hd (Eio.Net.getaddrinfo_stream ~service:port env#net host) in
  let listener = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:128 env#net address in
  let handler = Httpun_eio.Server.create_connection_handler ~sw
    ~request_handler:(fun _ reqd -> serve_request ~auth ~service reqd.Gluten.Reqd.reqd)
    ~error_handler:(fun _ ?request:_ _ _ -> ()) in
  Eio.Net.run_server ~on_error:(fun _ -> ()) listener (fun socket address -> handler address socket)
