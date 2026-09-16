type t = {
  store : Store_sqlite.t;
  verifier : Oidc.t;
}

let make ~store ~verifier = { store; verifier }

let authenticate_bearer auth token =
  match auth.verifier token with
  | Error () -> Error Error.Unauthorized
  | Ok claims ->
      Store_sqlite.resolve_user auth.store ~issuer:claims.issuer ~subject:claims.subject
