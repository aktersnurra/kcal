type t = { user : User.t; scopes : string list }
(** Who a request acts as and what it was granted, once authenticated. *)

val has_scope : t -> string -> bool
