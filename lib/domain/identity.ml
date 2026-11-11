open Base

type t = { user : User.t; scopes : string list }

let has_scope identity scope =
  List.mem identity.scopes scope ~equal:String.equal
