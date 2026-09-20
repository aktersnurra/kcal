type t = {
  database_path : string;
  listen_address : string;
  public_base_url : string;
  oidc_issuer : string;
  oidc_audience : string;
  withings_client_id : string;
  withings_client_secret : string;
  token_encryption_key : bytes;
}

let required name =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> Ok value
  | _ -> Error (Error.Invalid_input "missing required configuration")

let encryption_key () =
  match required "KCAL_TOKEN_ENCRYPTION_KEY" with
  | Ok value when String.length value = 64 ->
      (try Ok (Bytes.of_string (String.init 32 (fun i -> Char.chr (int_of_string ("0x" ^ String.sub value (i * 2) 2)))))
       with _ -> Error (Error.Invalid_input "invalid encryption key"))
  | _ -> Error (Error.Invalid_input "invalid encryption key")

let load_from_environment () =
  match required "KCAL_DATABASE_PATH", required "KCAL_LISTEN_ADDRESS", required "KCAL_PUBLIC_BASE_URL", required "KCAL_OIDC_ISSUER", required "KCAL_OIDC_AUDIENCE", required "WITHINGS_CLIENT_ID", required "WITHINGS_CLIENT_SECRET", encryption_key () with
  | Ok database_path, Ok listen_address, Ok public_base_url, Ok oidc_issuer, Ok oidc_audience, Ok withings_client_id, Ok withings_client_secret, Ok token_encryption_key ->
      Ok { database_path; listen_address; public_base_url; oidc_issuer; oidc_audience; withings_client_id; withings_client_secret; token_encryption_key }
  | _ -> Error (Error.Invalid_input "missing required configuration")

let protected_resource_metadata_url ~audience =
  let uri = Uri.of_string audience in
  let path = match Uri.path uri with "" | "/" -> "" | path -> path in
  Uri.to_string (Uri.with_path uri ("/.well-known/oauth-protected-resource" ^ path))
