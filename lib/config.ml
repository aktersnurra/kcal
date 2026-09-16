type t = {
  database_path : string;
  listen_address : string;
  public_base_url : string;
  oidc_issuer : string;
  oidc_audience : string;
}

let required name =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> Ok value
  | _ -> Error (Error.Invalid_input "missing required configuration")

let load_from_environment () =
  match required "KCAL_DATABASE_PATH", required "KCAL_LISTEN_ADDRESS", required "KCAL_PUBLIC_BASE_URL", required "KCAL_OIDC_ISSUER", required "KCAL_OIDC_AUDIENCE" with
  | Ok database_path, Ok listen_address, Ok public_base_url, Ok oidc_issuer, Ok oidc_audience ->
      Ok { database_path; listen_address; public_base_url; oidc_issuer; oidc_audience }
  | _ -> Error (Error.Invalid_input "missing required configuration")
