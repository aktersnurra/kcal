open Base

let log_src =
  Logs.Src.create "kcal.oidc"
    ~doc:"OIDC discovery and bearer token verification"

module Log = (val Logs.src_log log_src : Logs.LOG)

type claims = {
  issuer : string;
  subject : string;
  audience : string list;
  expires_at : Ptime.t;
  scopes : string list;
}

type clock = unit -> Ptime.t
type discovery = { issuer : string; jwks_uri : string; max_age_s : int option }
type jwks_response = { keys : Jose.Jwks.t; max_age_s : int option }

type client = {
  discover : issuer:string -> (discovery, unit) Result.t;
  fetch_jwks : uri:string -> (jwks_response, unit) Result.t;
}

type t = string -> (claims, unit) Result.t

let fallback_cache_ttl_s = 300
let maximum_cache_ttl_s = 3600

let bounded_ttl = function
  | Some seconds when seconds >= 0 -> min seconds maximum_cache_ttl_s
  | _ -> fallback_cache_ttl_s

let unix_clock () = Option.value_exn (Ptime.of_float_s (Unix.gettimeofday ()))

let audience_of_payload = function
  | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "aud" with
      | Some (`String audience) -> Some [ audience ]
      | Some (`List audiences) ->
          let rec strings values =
            match values with
            | [] -> Some []
            | `String value :: rest ->
                Option.map ~f:(fun tail -> value :: tail) (strings rest)
            | _ -> None
          in
          strings audiences
      | None -> None
      | _ -> None)
  | _ -> None

let scopes_of_payload = function
  | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "scope" with
      | Some (`String scopes) ->
          String.split scopes ~on:' '
          |> List.filter ~f:(fun scope -> not (String.equal scope ""))
      | Some (`List scopes) ->
          List.filter_map
            ~f:(function
              | `String scope when not (String.equal scope "") -> Some scope
              | _ -> None)
            scopes
      | _ -> [])
  | _ -> []

let expiration_of_payload = function
  | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "exp" with
      | Some (`Int seconds) -> Ptime.of_float_s (Float.of_int seconds)
      | Some (`Intlit seconds) -> (
          try Ptime.of_float_s (Float.of_string seconds)
          with Failure _ -> None)
      | Some (`Float seconds) -> Ptime.of_float_s seconds
      | _ -> None)
  | _ -> None

let claims_of_jwt ~issuer ~audience ~now jwt =
  match
    ( Jose.Jwt.get_string_claim jwt "iss",
      Jose.Jwt.get_string_claim jwt "sub",
      audience_of_payload jwt.Jose.Jwt.payload,
      expiration_of_payload jwt.Jose.Jwt.payload )
  with
  | Some token_issuer, Some subject, Some token_audience, Some expires_at
    when String.equal token_issuer issuer
         && (not (String.equal subject ""))
         && List.mem token_audience audience ~equal:String.equal
         && Ptime.compare expires_at now > 0 ->
      Ok
        {
          issuer = token_issuer;
          subject;
          audience = token_audience;
          expires_at;
          scopes = scopes_of_payload jwt.Jose.Jwt.payload;
        }
  | Some token_issuer, _, _, _ when not (String.equal token_issuer issuer) ->
      Log.warn (fun m ->
          m
            "rejected bearer token: issuer %S does not match configured issuer \
             %S"
            token_issuer issuer);
      Error ()
  | _, _, _, Some expires_at when Ptime.compare expires_at now <= 0 ->
      Log.debug (fun m ->
          m "rejected bearer token: expired at %s"
            (Ptime.to_rfc3339 ~tz_offset_s:0 expires_at));
      Error ()
  | _, _, None, _ ->
      Log.warn (fun m ->
          m "rejected bearer token: missing or malformed audience claim");
      Error ()
  | _ ->
      Log.warn (fun m ->
          m "rejected bearer token: missing required claims (iss/sub/exp)");
      Error ()

(* Jose decodes payload JSON outside its result and unwraps malformed EC points. *)
let decode_jose f =
  try Result.map_error (f ()) ~f:(fun _ -> ())
  with
  | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ | Invalid_argument _ ->
    Error ()

let key_for_token keys token =
  match decode_jose (fun () -> Jose.Jwt.unsafe_of_string token) with
  | Error _ ->
      Log.warn (fun m -> m "rejected bearer token: not a well-formed JWT");
      Error ()
  | Ok jwt -> (
      match (jwt.Jose.Jwt.header.alg, jwt.Jose.Jwt.header.kid) with
      | `None, _ ->
          Log.warn (fun m ->
              m "rejected bearer token: alg \"none\" is not accepted");
          Error ()
      | _, None ->
          Log.warn (fun m ->
              m "rejected bearer token: missing key id (kid) header");
          Error ()
      | _, Some kid -> (
          match Jose.Jwks.find_key keys kid with
          | Some key -> Ok key
          | None ->
              Log.warn (fun m ->
                  m "rejected bearer token: kid=%s not found in the cached JWKS"
                    kid);
              Error ()))

let make ~issuer ~audience ?(clock = unix_clock) ~client () =
  let cached_keys = ref None in
  let fresh_keys now =
    match client.discover ~issuer with
    | Error () ->
        Log.err (fun m -> m "OIDC discovery for issuer=%s failed" issuer);
        Error ()
    | Ok discovery when not (String.equal discovery.issuer issuer) ->
        Log.err (fun m ->
            m
              "OIDC discovery document issuer=%s does not match configured \
               issuer=%s"
              discovery.issuer issuer);
        Error ()
    | Ok discovery -> (
        match client.fetch_jwks ~uri:discovery.jwks_uri with
        | Error () ->
            Log.err (fun m ->
                m "fetching JWKS from %s failed" discovery.jwks_uri);
            Error ()
        | Ok response -> (
            let ttl = bounded_ttl response.max_age_s in
            match Ptime.add_span now (Ptime.Span.of_int_s ttl) with
            | None -> Error ()
            | Some expires_at ->
                Log.info (fun m ->
                    m "refreshed JWKS from %s, caching for %ds"
                      discovery.jwks_uri ttl);
                cached_keys := Some (response.keys, expires_at);
                Ok response.keys))
  in
  let keys now =
    match !cached_keys with
    | Some (keys, expires_at) when Ptime.compare now expires_at < 0 -> Ok keys
    | _ -> fresh_keys now
  in
  fun token ->
    let now = clock () in
    match keys now with
    | Error () -> Error ()
    | Ok keys -> (
        match key_for_token keys token with
        | Error () -> Error ()
        | Ok key -> (
            match
              decode_jose (fun () -> Jose.Jwt.of_string ~jwk:key ~now token)
            with
            | Error _ ->
                Log.warn (fun m ->
                    m "rejected bearer token: signature verification failed");
                Error ()
            | Ok jwt ->
                decode_jose (fun () -> claims_of_jwt ~issuer ~audience ~now jwt)
            ))

let max_age headers =
  match Cohttp.Header.get headers "cache-control" with
  | None -> None
  | Some value ->
      String.split value ~on:','
      |> List.find_map ~f:(fun directive ->
          match String.split (String.strip directive) ~on:'=' with
          | [ name; seconds ]
            when String.equal (String.lowercase name) "max-age" -> (
              try Some (Int.of_string (String.strip seconds))
              with Failure _ -> None)
          | _ -> None)

let discovery_uri issuer =
  let uri = Uri.of_string issuer in
  let path = Uri.path uri in
  let path =
    if String.is_suffix ~suffix:"/" path then
      String.sub path ~pos:0 ~len:(String.length path - 1)
    else path
  in
  Uri.to_string (Uri.with_path uri (path ^ "/.well-known/openid-configuration"))

let client_of_get get =
  let get uri =
    match Https_transport.catch_transport_failure (fun () -> get uri) with
    | Ok result -> result
    | Error _ -> Error ()
  in
  {
    discover =
      (fun ~issuer ->
        if
          not
            (Option.equal String.equal
               (Uri.scheme (Uri.of_string issuer))
               (Some "https"))
        then (
          Log.err (fun m ->
              m "refusing OIDC discovery for issuer=%s: only https is allowed"
                issuer);
          Error ())
        else
          match get (discovery_uri issuer) with
          | Error () -> Error ()
          | Ok (body, headers) -> (
              try
                match Yojson.Safe.from_string body with
                | `Assoc fields -> (
                    match
                      ( List.Assoc.find fields ~equal:String.equal "issuer",
                        List.Assoc.find fields ~equal:String.equal "jwks_uri" )
                    with
                    | Some (`String discovered_issuer), Some (`String jwks_uri)
                      when Option.equal String.equal
                             (Uri.scheme (Uri.of_string jwks_uri))
                             (Some "https") ->
                        Ok
                          {
                            issuer = discovered_issuer;
                            jwks_uri;
                            max_age_s = max_age headers;
                          }
                    | _ ->
                        Log.err (fun m ->
                            m
                              "OIDC discovery document for issuer=%s is \
                               missing issuer/jwks_uri or jwks_uri is not \
                               https"
                              issuer);
                        Error ())
                | _ ->
                    Log.err (fun m ->
                        m
                          "OIDC discovery document for issuer=%s is not a JSON \
                           object"
                          issuer);
                    Error ()
              with Yojson.Json_error message ->
                Log.err (fun m ->
                    m
                      "OIDC discovery document for issuer=%s is not valid \
                       JSON: %s"
                      issuer message);
                Error ()));
    fetch_jwks =
      (fun ~uri ->
        match get uri with
        | Error () -> Error ()
        | Ok (body, headers) -> (
            try
              Ok
                { keys = Jose.Jwks.of_string body; max_age_s = max_age headers }
            with
            | ( Yojson.Json_error _ | Yojson.Safe.Util.Type_error _
              | Invalid_argument _ ) as exn
            ->
              Log.err (fun m ->
                  m "JWKS at %s could not be parsed: %s" uri (Exn.to_string exn));
              Error ()));
  }

let https_client env =
  match Https_transport.make_client ~net:env#net with
  | Error _ ->
      {
        discover = (fun ~issuer:_ -> Error ());
        fetch_jwks = (fun ~uri:_ -> Error ());
      }
  | Ok client ->
      let get uri =
        let uri = Uri.of_string uri in
        if not (Option.equal String.equal (Uri.scheme uri) (Some "https")) then (
          Log.err (fun m ->
              m "refusing to fetch %s: only https is allowed"
                (Uri.to_string uri));
          Error ())
        else
          Eio.Switch.run @@ fun sw ->
          let response, body = Cohttp_eio.Client.get client ~sw uri in
          if
            not
              (match Cohttp.Response.status response with
              | `OK -> true
              | _ -> false)
          then (
            Log.warn (fun m ->
                m "GET %s returned %s" (Uri.to_string uri)
                  (Cohttp.Code.string_of_status
                     (Cohttp.Response.status response)));
            Error ())
          else Ok (Eio.Flow.read_all body, Cohttp.Response.headers response)
      in
      client_of_get get

let of_verified_claims ?(issuer = "https://issuer.example")
    ?(audience = "kcal-client") ?(clock = unix_clock) verify token =
  match verify token with
  | Error () -> Error ()
  | Ok (claims : claims)
    when String.equal claims.issuer issuer
         && List.mem claims.audience audience ~equal:String.equal
         && Ptime.compare claims.expires_at (clock ()) > 0 ->
      Ok claims
  | Ok _ -> Error ()

module For_testing = struct
  let client_of_get = client_of_get
end
