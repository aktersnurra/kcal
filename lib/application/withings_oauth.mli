type t

val make : store:Store_sqlite.t -> now:(unit -> Ptime.t) -> t

val begin_authorization :
  client_id:string ->
  redirect_uri:string ->
  t ->
  user:User.t ->
  (string * string, Error.t) result

val consume_state : t -> user:User.t -> state:string -> (unit, Error.t) result
val consume_callback_state : t -> state:string -> (User.t, Error.t) result
