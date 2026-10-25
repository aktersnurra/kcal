module Account : sig
  type t

  val to_string : t -> string
end

type connected = private {
  id : string;
  user_id : User_id.t;
  account : Account.t;
  token_expires_at : Ptime.t;
  sync_cursor : int64 option;
}

type status =
  | Disconnected
  | Connected of connected
  | Reauthorization_required of connected

val decode :
  id:string ->
  user_id:User_id.t ->
  withings_user_id:string ->
  token_expires_at:Ptime.t ->
  sync_cursor:int64 option ->
  (connected, Error.t) result
