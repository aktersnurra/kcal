module Domain_error = Error
open Base
module Error = Domain_error

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

type continuation = Complete | Continue of int64

type measurement_page = {
  measurements : measurement list;
  lastupdate : int64 option;
  continuation : continuation;
}

type measurement_batch = {
  measurements : measurement list;
  lastupdate : int64 option;
}

module type S = sig
  val exchange_code :
    redirect_uri:string -> code:string -> (credentials, Error.t) Result.t

  val refresh : refresh_token:string -> (credentials, Error.t) Result.t

  val get_measurements :
    access_token:string ->
    lastupdate:int64 option ->
    (measurement_batch, Error.t) Result.t

  val subscribe :
    access_token:string -> callback_url:string -> (unit, Error.t) Result.t
end

type request = {
  post_form :
    uri:string -> fields:(string * string) list -> (string, Error.t) Result.t;
}

type config = { client_id : string; client_secret : string }

let oauth_url = "https://wbsapi.withings.net/v2/oauth2"
let measure_url = "https://wbsapi.withings.net/measure"
let notification_url = "https://wbsapi.withings.net/notify"
let ( let* ) result f = Result.bind result ~f
let malformed () = Error (Error.Withings_failure Malformed_response)

let int64_member json name =
  match Yojson.Safe.Util.member name json with
  | `Int value -> Ok (Int64.of_int value)
  | `Intlit value | `String value -> (
      try Ok (Int64.of_string value) with Failure _ -> malformed ())
  | `Float value -> (
      try Ok (Int64.of_float value) with Invalid_argument _ -> malformed ())
  | _ -> malformed ()

let string_member json name =
  match Yojson.Safe.Util.member name json with
  | `String value -> Ok value
  | `Int value -> Ok (Int.to_string value)
  | `Intlit value -> Ok value
  | _ -> malformed ()

let int_member json name =
  match Yojson.Safe.Util.member name json with
  | `Int value -> Ok value
  | `Intlit value | `String value -> (
      try Ok (Int.of_string value) with Failure _ -> malformed ())
  | _ -> malformed ()

let decode_response ?(refresh = false) response ~f =
  try
    let json = Yojson.Safe.from_string response in
    let* status = int_member json "status" in
    if status = 0 then f (Yojson.Safe.Util.member "body" json)
    else if refresh && List.mem [ 401; 403; 400 ] status ~equal:Int.equal then
      Error (Error.Withings_failure (Credential_rejection status))
    else Error (Error.Withings_failure (Api_status status))
  with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> malformed ()

let credentials_of_response ~now ?(refresh = false) response =
  decode_response ~refresh response ~f:(fun body ->
      let* expires_in = int_member body "expires_in" in
      if expires_in <= 0 then malformed ()
      else
        match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
        | None -> malformed ()
        | Some expires_at ->
            let* access_token = string_member body "access_token" in
            let* refresh_token = string_member body "refresh_token" in
            let* withings_user_id = string_member body "userid" in
            Ok { access_token; refresh_token; expires_at; withings_user_id })

let measurements_of_response response =
  decode_response response ~f:(fun body ->
      let* measurements =
        Yojson.Safe.Util.member "measuregrps" body
        |> Yojson.Safe.Util.to_list
        |> List.map ~f:(fun group ->
            let* group_id = string_member group "grpid" in
            let* measured_at = int64_member group "date" in
            Yojson.Safe.Util.member "measures" group
            |> Yojson.Safe.Util.to_list
            |> List.map ~f:(fun measure ->
                let* measure_type = int_member measure "type" in
                let* value = int64_member measure "value" in
                let* unit_ = int_member measure "unit" in
                Ok { group_id; measured_at; measure_type; value; unit_ })
            |> Result.all)
        |> Result.all |> Result.map ~f:List.concat
      in
      let* lastupdate =
        match Yojson.Safe.Util.member "lastupdate" body with
        | `Null -> Ok None
        | _ -> Result.map (int64_member body "lastupdate") ~f:Option.some
      in
      let* continuation =
        match Yojson.Safe.Util.member "more" body with
        | `Null | `Int 0 -> Ok Complete
        | `Int 1 ->
            Result.map (int64_member body "offset") ~f:(fun offset ->
                Continue offset)
        | _ -> malformed ()
      in
      Ok { measurements; lastupdate; continuation })

let make ~now ~request ~config : (module S) =
  let module Client = struct
    let token ~refresh fields =
      match
        request.post_form ~uri:oauth_url
          ~fields:
            (fields
            @ [
                ("client_id", config.client_id);
                ("client_secret", config.client_secret);
              ])
      with
      | Error error -> Error error
      | Ok body -> credentials_of_response ~now:(now ()) ~refresh body

    let exchange_code ~redirect_uri ~code =
      token ~refresh:false
        [
          ("action", "requesttoken");
          ("grant_type", "authorization_code");
          ("code", code);
          ("redirect_uri", redirect_uri);
        ]

    let refresh ~refresh_token =
      token ~refresh:true
        [
          ("action", "requesttoken");
          ("grant_type", "refresh_token");
          ("refresh_token", refresh_token);
        ]

    let get_measurements ~access_token ~lastupdate =
      let fields =
        [
          ("action", "getmeas");
          ("category", "1");
          ("access_token", access_token);
        ]
      in
      let fields =
        match lastupdate with
        | None -> fields
        | Some value -> ("lastupdate", Int64.to_string value) :: fields
      in
      let rec fetch offset measurements =
        let page_fields =
          match offset with
          | None -> fields
          | Some offset -> ("offset", Int64.to_string offset) :: fields
        in
        match request.post_form ~uri:measure_url ~fields:page_fields with
        | Error error -> Error error
        | Ok body -> (
            match measurements_of_response body with
            | Error error -> Error error
            | Ok page -> (
                let measurements =
                  List.rev_append page.measurements measurements
                in
                match page.continuation with
                | Complete ->
                    Ok
                      {
                        measurements = List.rev measurements;
                        lastupdate = page.lastupdate;
                      }
                | Continue next ->
                    if Int64.compare next (Option.value offset ~default:0L) <= 0
                    then malformed ()
                    else fetch (Some next) measurements))
      in
      fetch None []

    let subscribe ~access_token ~callback_url =
      match
        request.post_form ~uri:notification_url
          ~fields:
            [
              ("action", "subscribe");
              ("callbackurl", callback_url);
              ("appli", "1");
              ("access_token", access_token);
            ]
      with
      | Error error -> Error error
      | Ok body -> decode_response body ~f:(fun _ -> Ok ())
  end in
  (module Client : S)
