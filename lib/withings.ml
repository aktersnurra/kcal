type credentials = {
  access_token : string;
  refresh_token : string;
  expires_at : Ptime.t;
  withings_user_id : string;
}

type measurement = {
  group_id : string;
  measured_at : int64;
  measure_type : int;
  value : int64;
  unit_ : int;
}

type measurement_batch = { measurements : measurement list; lastupdate : int64 option }

module type S = sig
  val exchange_code : redirect_uri:string -> code:string -> (credentials, Error.t) result
  val refresh : refresh_token:string -> (credentials, Error.t) result
  val get_measurements : access_token:string -> lastupdate:int64 option -> (measurement_batch, Error.t) result
  val subscribe : access_token:string -> callback_url:string -> (unit, Error.t) result
end

type request =
  { post_form : uri:string -> fields:(string * string) list -> (string, Error.t) result }

type config = { client_id : string; client_secret : string }

let oauth_url = "https://wbsapi.withings.net/v2/oauth2"
let measure_url = "https://wbsapi.withings.net/measure"
let notification_url = "https://wbsapi.withings.net/notify"
let upstream_error () = Error.Invalid_input "Withings request failed"
let upstream_status_error status = Error.Invalid_input (Printf.sprintf "Withings request failed (status %d)" status)
let permanent_refresh_error () = Error.Invalid_input "Withings authorization failure"
let is_permanent_refresh_error = function Error.Invalid_input "Withings authorization failure" -> true | _ -> false
let int64_member json name =
  match Yojson.Safe.Util.member name json with
  | `Int value -> Int64.of_int value | `Intlit value | `String value -> Int64.of_string value
  | `Float value -> Int64.of_float value | _ -> raise Exit
let string_member json name =
  match Yojson.Safe.Util.member name json with `String value -> value | `Int value -> string_of_int value | `Intlit value -> value | _ -> raise Exit
let int_member json name =
  match Yojson.Safe.Util.member name json with `Int value -> value | `Intlit value | `String value -> int_of_string value | _ -> raise Exit

let credentials_of_response ?(refresh = false) body =
  try
    let json = Yojson.Safe.from_string body in
    let status = int_member json "status" in
    if status <> 0 then
      if refresh && List.mem status [ 401; 403; 400 ] then Error (permanent_refresh_error ()) else Error (upstream_status_error status)
    else
      let body = Yojson.Safe.Util.member "body" json in
      let expires_in = int_member body "expires_in" in
      if expires_in <= 0 then Error (upstream_error ())
      else
        let expires_at = Ptime.add_span (Option.get (Ptime.of_float_s (Unix.gettimeofday ()))) (Ptime.Span.of_int_s expires_in) in
        match expires_at with
        | None -> Error (upstream_error ())
        | Some expires_at ->
            Ok { access_token = string_member body "access_token"; refresh_token = string_member body "refresh_token";
                 expires_at; withings_user_id = string_member body "userid" }
  with _ -> Error (upstream_error ())

let measurements_of_response body =
  try
    let json = Yojson.Safe.from_string body in
    if Yojson.Safe.Util.member "status" json |> Yojson.Safe.Util.to_int <> 0 then Error (upstream_error ())
    else
      let body = Yojson.Safe.Util.member "body" json in
      let measurements =
        Yojson.Safe.Util.member "measuregrps" body |> Yojson.Safe.Util.to_list
        |> List.concat_map (fun group ->
          let group_id = string_member group "grpid" and measured_at = int64_member group "date" in
          Yojson.Safe.Util.member "measures" group |> Yojson.Safe.Util.to_list
          |> List.map (fun measure -> { group_id; measured_at; measure_type = int_member measure "type";
                                         value = int64_member measure "value"; unit_ = int_member measure "unit" }))
      in
      let lastupdate =
        match Yojson.Safe.Util.member "lastupdate" body with
        | `Null -> None | value -> Some (int64_member (`Assoc [ ("lastupdate", value) ]) "lastupdate")
      in
      Ok { measurements; lastupdate }
  with _ -> Error (upstream_error ())

let make ~request ~config : (module S) =
  let module Client = struct
    let token ~refresh fields =
      match request.post_form ~uri:oauth_url ~fields:(fields @ [ ("client_id", config.client_id); ("client_secret", config.client_secret) ]) with
      | Error _ -> Error (upstream_error ()) | Ok body -> credentials_of_response ~refresh body
    let exchange_code ~redirect_uri ~code =
      token ~refresh:false [ ("action", "requesttoken"); ("grant_type", "authorization_code"); ("code", code); ("redirect_uri", redirect_uri) ]
    let refresh ~refresh_token = token ~refresh:true [ ("action", "requesttoken"); ("grant_type", "refresh_token"); ("refresh_token", refresh_token) ]
    let get_measurements ~access_token ~lastupdate =
      let fields = [ ("action", "getmeas"); ("category", "1"); ("access_token", access_token) ] in
      let fields = match lastupdate with None -> fields | Some value -> ("lastupdate", Int64.to_string value) :: fields in
      match request.post_form ~uri:measure_url ~fields with Error _ -> Error (upstream_error ()) | Ok body -> measurements_of_response body
    let subscribe ~access_token ~callback_url =
      match request.post_form ~uri:notification_url ~fields:[ ("action", "subscribe"); ("callbackurl", callback_url); ("appli", "1"); ("access_token", access_token) ] with
      | Error _ -> Error (upstream_error ())
      | Ok body ->
          (try if Yojson.Safe.Util.(member "status" (Yojson.Safe.from_string body) |> to_int) = 0 then Ok () else Error (upstream_error ()) with _ -> Error (upstream_error ()))
  end in
  (module Client : S)
