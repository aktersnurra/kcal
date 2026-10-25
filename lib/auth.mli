type identity = { user : User.t; scopes : string list }
type t

val has_scope : identity -> string -> bool

val make :
  resolve_user:(issuer:string -> subject:string -> (User.t, Error.t) result) ->
  verifier:Oidc.t ->
  t

val authenticate_bearer : t -> string -> (identity, Error.t) result
