type claims = {
  issuer : string;
  subject : string;
  audience : string list;
  expires_at : Ptime.t;
}

type clock = unit -> Ptime.t

type discovery = {
  issuer : string;
  jwks_uri : string;
  max_age_s : int option;
}

type jwks_response = {
  keys : Jose.Jwks.t;
  max_age_s : int option;
}

type client = {
  discover : issuer:string -> (discovery, unit) result;
  fetch_jwks : uri:string -> (jwks_response, unit) result;
}

type t = string -> (claims, unit) result

let fallback_cache_ttl_s = 300
let maximum_cache_ttl_s = 3600

let bounded_ttl = function
  | Some seconds when seconds >= 0 -> min seconds maximum_cache_ttl_s
  | _ -> fallback_cache_ttl_s

let unix_clock () = Option.get (Ptime.of_float_s (Unix.gettimeofday ()))

let audience_of_payload = function
  | `Assoc fields ->
      (match List.assoc_opt "aud" fields with
      | Some (`String audience) -> Some [ audience ]
      | Some (`List audiences) ->
          let rec strings values =
            match values with
            | [] -> Some []
            | `String value :: rest -> Option.map (fun tail -> value :: tail) (strings rest)
            | _ -> None
          in
          strings audiences
      | None -> None
      | _ -> None)
  | _ -> None

let expiration_of_payload = function
  | `Assoc fields ->
      (match List.assoc_opt "exp" fields with
      | Some (`Int seconds) -> Ptime.of_float_s (float_of_int seconds)
      | Some (`Intlit seconds) ->
          (try Ptime.of_float_s (float_of_string seconds) with Failure _ -> None)
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
    when token_issuer = issuer
         && subject <> ""
         && List.mem audience token_audience
         && Ptime.compare expires_at now > 0 ->
      Ok { issuer = token_issuer; subject; audience = token_audience; expires_at }
  | _ -> Error ()

let key_for_token keys token =
  match Jose.Jwt.unsafe_of_string token with
  | Error _ -> Error ()
  | Ok jwt ->
      (match jwt.Jose.Jwt.header.alg, jwt.Jose.Jwt.header.kid with
      | `None, _ | _, None -> Error ()
      | _, Some kid ->
          (match Jose.Jwks.find_key keys kid with Some key -> Ok key | None -> Error ()))

let make ~issuer ~audience ?(clock = unix_clock) ~client () =
  let cached_keys = ref None in
  let fresh_keys now =
    match client.discover ~issuer with
    | Error () -> Error ()
    | Ok discovery when discovery.issuer <> issuer -> Error ()
    | Ok discovery ->
        (match client.fetch_jwks ~uri:discovery.jwks_uri with
        | Error () -> Error ()
        | Ok response ->
            let ttl = bounded_ttl response.max_age_s in
            (match Ptime.add_span now (Ptime.Span.of_int_s ttl) with
            | None -> Error ()
            | Some expires_at ->
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
    | Ok keys ->
        (match key_for_token keys token with
        | Error () -> Error ()
        | Ok key ->
            match Jose.Jwt.of_string ~jwk:key ~now token with
            | Error _ -> Error ()
            | Ok jwt -> claims_of_jwt ~issuer ~audience ~now jwt)

let of_verified_claims ?(issuer = "https://issuer.example")
    ?(audience = "kcal-client") ?(clock = unix_clock) verify token =
  match verify token with
  | Error () -> Error ()
  | Ok (claims : claims) when claims.issuer = issuer
                              && List.mem audience claims.audience
                              && Ptime.compare claims.expires_at (clock ()) > 0 ->
      Ok claims
  | Ok _ -> Error ()
