open Base

type error = Missing of string | Invalid of string * string
[@@deriving sexp_of]

let error_to_string = function
  | Missing field -> "missing required configuration: " ^ field
  | Invalid (field, reason) ->
      "invalid configuration: " ^ field ^ " (" ^ reason ^ ")"

type database = { path : string } [@@deriving sexp_of]
type listener = { host : string; port : int } [@@deriving sexp_of]

type withings = {
  client_id : string;
  client_secret : string;
  token_encryption_key : Secret.Key.t;
}

let sexp_of_withings config =
  Sexp.List
    [
      Sexp.List [ Sexp.Atom "client_id"; Sexp.Atom config.client_id ];
      Sexp.List [ Sexp.Atom "client_secret"; Sexp.Atom "<redacted>" ];
      Sexp.List [ Sexp.Atom "token_encryption_key"; Sexp.Atom "<redacted>" ];
    ]

type server = {
  database : database;
  listener : listener;
  public_base_url : string;
  oidc_issuer : string;
  oidc_audience : string;
  protected_resource : string;
  metadata_url : string;
  withings : withings option;
}
[@@deriving sexp_of]

type sync = { database : database; withings : withings } [@@deriving sexp_of]

let nonempty field value =
  if String.is_empty (String.strip value) then Error (Missing field)
  else Ok value

let database ~path =
  Result.map (nonempty "KCAL_DATABASE_PATH" path) ~f:(fun path -> { path })

let encryption_key value =
  let invalid () =
    Error
      (Invalid
         ("KCAL_TOKEN_ENCRYPTION_KEY", "expected 64 hexadecimal characters"))
  in
  if
    String.length value <> 64 || not (String.for_all value ~f:Char.is_hex_digit)
  then invalid ()
  else
    let bytes =
      Bytes.of_string
        (String.init 32 ~f:(fun index ->
             Char.of_int_exn
               (Int.of_string ("0x" ^ String.sub value ~pos:(index * 2) ~len:2))))
    in
    match Secret.Key.of_bytes bytes with
    | Ok key -> Ok key
    | Error _ -> invalid ()

let withings ~client_id ~client_secret ~token_encryption_key =
  let open Result.Let_syntax in
  let%bind client_id = nonempty "WITHINGS_CLIENT_ID" client_id in
  let%bind client_secret = nonempty "WITHINGS_CLIENT_SECRET" client_secret in
  let%map token_encryption_key = encryption_key token_encryption_key in
  { client_id; client_secret; token_encryption_key }

let absent = function
  | None -> true
  | Some value -> String.is_empty (String.strip value)

let optional_withings ~client_id ~client_secret ~token_encryption_key =
  if List.for_all [ client_id; client_secret; token_encryption_key ] ~f:absent
  then Ok None
  else
    Result.map
      (withings
         ~client_id:(Option.value client_id ~default:"")
         ~client_secret:(Option.value client_secret ~default:"")
         ~token_encryption_key:(Option.value token_encryption_key ~default:""))
      ~f:Option.some

let url field value =
  let invalid () =
    Error
      (Invalid
         ( field,
           "expected an absolute HTTPS URL without credentials, query or \
            fragment" ))
  in
  if String.is_empty (String.strip value) then Error (Missing field)
  else if
    String.exists value ~f:(fun c ->
        Char.is_whitespace c || Char.to_int c < 32 || Char.to_int c = 127)
  then invalid ()
  else
    try
      let uri = Uri.of_string value in
      (* Uri.of_string is permissive: :abc becomes a path, not a bad port. *)
      let authority_valid =
        match
          if String.is_prefix (String.lowercase value) ~prefix:"https://" then
            Some (String.drop_prefix value 8)
          else None
        with
        | None -> false
        | Some rest -> (
            let authority =
              String.prefix rest
                (Option.value
                   (String.lfindi rest ~f:(fun _ c ->
                        List.mem [ '/'; '?'; '#' ] c ~equal:Char.equal))
                   ~default:(String.length rest))
            in
            match String.lsplit2 authority ~on:':' with
            | None ->
                Option.value_map (Uri.host uri) ~default:false
                  ~f:(String.Caseless.equal authority)
            | Some (host, port) ->
                Option.value_map (Uri.host uri) ~default:false
                  ~f:(String.Caseless.equal host)
                && (not (String.is_empty port))
                && String.for_all port ~f:Char.is_digit
                && Option.exists (Int.of_string_opt port) ~f:(fun port ->
                    port >= 1 && port <= 65535))
      in
      match Https_transport.endpoint_host uri with
      | Error () -> invalid ()
      | Ok _ ->
          if
            (not authority_valid)
            || Option.is_some (Uri.userinfo uri)
            || Option.is_some (Uri.verbatim_query uri)
            || Option.is_some (Uri.fragment uri)
          then invalid ()
          else Ok value
    with Failure _ | Invalid_argument _ -> invalid ()

let listener value =
  let invalid () =
    Error
      (Invalid ("KCAL_LISTEN_ADDRESS", "expected HOST:PORT with port 1..65535"))
  in
  match String.rsplit2 value ~on:':' with
  | None -> invalid ()
  | Some (host, port) -> (
      let host =
        if
          String.is_prefix host ~prefix:"[" && String.is_suffix host ~suffix:"]"
        then String.sub host ~pos:1 ~len:(String.length host - 2)
        else host
      in
      let host_valid =
        (not (String.is_empty host))
        &&
          try
            ignore (Unix.inet_addr_of_string host);
            true
          with Failure _ -> (
            match Domain_name.of_string host with
            | Ok domain ->
                Domain_name.count_labels domain > 0
                && Result.is_ok (Domain_name.host domain)
            | Error _ -> false)
      in
      match
        if String.for_all port ~f:Char.is_digit then Int.of_string_opt port
        else None
      with
      | Some port when host_valid && port >= 1 && port <= 65535 ->
          Ok { host; port }
      | _ -> invalid ())

let server ~database ~listen_address ~public_base_url ~oidc_issuer
    ~oidc_audience ~withings =
  let open Result.Let_syntax in
  let%bind listener = listener listen_address in
  let%bind public_base_url = url "KCAL_PUBLIC_BASE_URL" public_base_url in
  let%bind oidc_issuer = url "KCAL_OIDC_ISSUER" oidc_issuer in
  let%map oidc_audience = nonempty "KCAL_OIDC_AUDIENCE" oidc_audience in
  let public_base_url = String.rstrip public_base_url ~drop:(Char.equal '/') in
  let protected_resource = public_base_url ^ "/mcp" in
  let uri = Uri.of_string protected_resource in
  let metadata_url =
    Uri.to_string
      (Uri.with_path uri
         ("/.well-known/oauth-protected-resource" ^ Uri.path uri))
  in
  {
    database;
    listener;
    public_base_url;
    oidc_issuer;
    oidc_audience;
    protected_resource;
    metadata_url;
    withings;
  }

module Fields = struct
  let database_path = ("database-path", "KCAL_DATABASE_PATH")
  let listen_address = ("listen-address", "KCAL_LISTEN_ADDRESS")
  let public_base_url = ("public-base-url", "KCAL_PUBLIC_BASE_URL")
  let oidc_issuer = ("oidc-issuer", "KCAL_OIDC_ISSUER")
  let oidc_audience = ("oidc-audience", "KCAL_OIDC_AUDIENCE")
  let withings_client_id = ("withings-client-id", "WITHINGS_CLIENT_ID")

  let withings_client_secret =
    ("withings-client-secret", "WITHINGS_CLIENT_SECRET")

  let token_encryption_key =
    ("token-encryption-key", "KCAL_TOKEN_ENCRYPTION_KEY")

  let all =
    [
      database_path;
      listen_address;
      public_base_url;
      oidc_issuer;
      oidc_audience;
      withings_client_id;
      withings_client_secret;
      token_encryption_key;
    ]
end

(* Command does not accept inline values and includes unknown flags verbatim in
   errors. Split only our known value-taking flags, never their value tokens. *)
let normalize_argv argv =
  let names = List.map Fields.all ~f:fst in
  let known_flag argument =
    match String.chop_prefix argument ~prefix:"--" with
    | Some name -> List.find names ~f:(String.equal name)
    | None -> (
        match String.chop_prefix argument ~prefix:"-" with
        | None -> None
        | Some prefix -> (
            match
              List.filter names ~f:(fun name -> String.is_prefix name ~prefix)
            with
            | [ name ] -> Some name
            | _ -> None))
  in
  let rec normalize = function
    | [] -> []
    | "--" :: _ as arguments -> arguments
    | argument :: rest -> (
        match String.lsplit2 argument ~on:'=' with
        | Some (flag, value) when Option.is_some (known_flag flag) ->
            ("-" ^ Option.value_exn (known_flag flag))
            :: value :: normalize rest
        | _ -> (
            match (known_flag argument, rest) with
            | Some _, value :: rest -> argument :: value :: normalize rest
            | _ -> argument :: normalize rest))
  in
  match argv with
  | [] -> []
  | executable :: arguments -> executable :: normalize arguments

(* Command's parser prints invalid raw arguments. Accept strings here and validate
   only the selected flag/environment input, using value-free structured errors. *)
let input (flag_name, env_name) =
  let open Core.Command.Param in
  map
    (flag flag_name (optional string) ~doc:("VALUE overrides " ^ env_name))
    ~f:(fun flag ->
      match flag with Some _ -> flag | None -> Stdlib.Sys.getenv_opt env_name)

let migrate_param () =
  Core.Command.Param.map (input Fields.database_path) ~f:(fun path ->
      database ~path:(Option.value path ~default:""))

let withings_inputs () =
  let open Core.Command.Let_syntax in
  let%map_open client_id = input Fields.withings_client_id
  and client_secret = input Fields.withings_client_secret
  and token_encryption_key = input Fields.token_encryption_key in
  (client_id, client_secret, token_encryption_key)

let serve_param () =
  let open Core.Command.Let_syntax in
  let%map_open database = migrate_param ()
  and listen_address = input Fields.listen_address
  and public_base_url = input Fields.public_base_url
  and oidc_issuer = input Fields.oidc_issuer
  and oidc_audience = input Fields.oidc_audience
  and integration = withings_inputs () in
  let open Result.Let_syntax in
  let%bind database = database in
  let client_id, client_secret, token_encryption_key = integration in
  let%bind withings =
    optional_withings ~client_id ~client_secret ~token_encryption_key
  in
  server ~database
    ~listen_address:(Option.value listen_address ~default:"127.0.0.1:8080")
    ~public_base_url:(Option.value public_base_url ~default:"")
    ~oidc_issuer:(Option.value oidc_issuer ~default:"")
    ~oidc_audience:(Option.value oidc_audience ~default:"")
    ~withings

let sync_param () =
  let open Core.Command.Let_syntax in
  let%map_open database = migrate_param ()
  and integration = withings_inputs () in
  let open Result.Let_syntax in
  let%bind database = database in
  let client_id, client_secret, token_encryption_key = integration in
  let%map withings =
    withings
      ~client_id:(Option.value client_id ~default:"")
      ~client_secret:(Option.value client_secret ~default:"")
      ~token_encryption_key:(Option.value token_encryption_key ~default:"")
  in
  { database; withings }
