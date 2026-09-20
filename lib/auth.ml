type identity = { user : User.t; scopes : string list }

let has_scope identity scope = List.mem scope identity.scopes

type t = {
  resolve_user : issuer:string -> subject:string -> (User.t, Error.t) result;
  verifier : Oidc.t;
}

let make ~resolve_user ~verifier = { resolve_user; verifier }

let authenticate_bearer auth token =
  match auth.verifier token with
  | Error () -> Error Error.Unauthorized
  | Ok claims ->
      Result.map
        (fun user -> { user; scopes = claims.scopes })
        (auth.resolve_user ~issuer:claims.issuer ~subject:claims.subject)
