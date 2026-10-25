type error = Missing of string | Invalid of string * string
[@@deriving sexp_of]

val error_to_string : error -> string

type database = private { path : string } [@@deriving sexp_of]
type listener = private { host : string; port : int } [@@deriving sexp_of]

type withings = private {
  client_id : string;
  client_secret : string;
  token_encryption_key : Secret.Key.t;
}

val sexp_of_withings : withings -> Base.Sexp.t

type server = private {
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

type sync = private { database : database; withings : withings }
[@@deriving sexp_of]

val database : path:string -> (database, error) result

val withings :
  client_id:string ->
  client_secret:string ->
  token_encryption_key:string ->
  (withings, error) result

val optional_withings :
  client_id:string option ->
  client_secret:string option ->
  token_encryption_key:string option ->
  (withings option, error) result

val server :
  database:database ->
  listen_address:string ->
  public_base_url:string ->
  oidc_issuer:string ->
  oidc_audience:string ->
  withings:withings option ->
  (server, error) result

val migrate_param : unit -> (database, error) result Core.Command.Param.t
val serve_param : unit -> (server, error) result Core.Command.Param.t

val sync_param : unit -> (sync, error) result Core.Command.Param.t
(** Flags override environment inputs before validation. No credentials are
    parsed by Command's exception printer, which includes the raw argument on
    failure. *)

val normalize_argv : string list -> string list
(** Splits known inline configuration flags into Command's space-separated form.
    Preserves value tokens and everything after the [--] delimiter. *)
