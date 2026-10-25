module Domain_error = Error
open Base
module Error = Domain_error

type identity = { user : User.t; scopes : string list }

let has_scope identity scope =
  List.mem identity.scopes scope ~equal:String.equal

type t = {
  resolve_user : issuer:string -> subject:string -> (User.t, Error.t) Result.t;
  verifier : Oidc.t;
}

let make ~resolve_user ~verifier = { resolve_user; verifier }

let authenticate_bearer auth token =
  match auth.verifier token with
  | Error () -> Error Error.Unauthorized
  | Ok claims ->
      Result.map
        ~f:(fun user -> { user; scopes = claims.scopes })
        (auth.resolve_user ~issuer:claims.issuer ~subject:claims.subject)
