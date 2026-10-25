type t = {
  database_path : string;
  listen_address : string;
  public_base_url : string;
  oidc_issuer : string;
  oidc_audience : string;
  withings_client_id : string;
  withings_client_secret : string;
  token_encryption_key : Secret.Key.t;
}

val load_from_environment : unit -> (t, Error.t) result
val protected_resource_metadata_url : audience:string -> string
