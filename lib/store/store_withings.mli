val create_withings_oauth_state :
  Sqlite_db.t ->
  user:User.t ->
  state_hash:string ->
  expires_at:Ptime.t ->
  now:Ptime.t ->
  (unit, Error.t) result
(** Also discards the states that expired before [now]. *)

val consume_withings_oauth_state :
  Sqlite_db.t ->
  user:User.t ->
  state_hash:string ->
  now:Ptime.t ->
  (unit, Error.t) result

val get_withings_status :
  Sqlite_db.t -> user:User.t -> (Withings_connection.status, Error.t) result

val replace_withings_connection :
  Sqlite_db.t ->
  user:User.t ->
  key:Secret.Key.t ->
  Withings.credentials ->
  (Withings_connection.connected, Error.t) result

val refresh_withings_credentials :
  Sqlite_db.t ->
  user:User.t ->
  connection:Withings_connection.connected ->
  key:Secret.Key.t ->
  Withings.credentials ->
  (Withings_connection.connected, Error.t) result

val withings_credentials :
  Sqlite_db.t ->
  user:User.t ->
  connection:Withings_connection.connected ->
  key:Secret.Key.t ->
  (Withings.credentials, Error.t) result

val mark_withings_reauthorization :
  Sqlite_db.t ->
  user:User.t ->
  connection:Withings_connection.connected ->
  (unit, Error.t) result
(** Not found when the stored tokens are no longer the ones [connection] was
    read with, so a rejected stale refresh token cannot condemn a connection
    another sync has just refreshed. *)

val persist_withings_import :
  Sqlite_db.t ->
  user:User.t ->
  connection:Withings_connection.connected ->
  rows:Weigh_in.Import.t list ->
  expected_cursor:int64 option ->
  cursor:int64 option ->
  (unit, Error.t) result

val delete_withings_connection :
  Sqlite_db.t -> user:User.t -> (unit, Error.t) result

val withings_connection_for_identity :
  Sqlite_db.t ->
  withings_user_id:string ->
  (User.t * Withings_connection.connected, Error.t) result

val withings_connections :
  Sqlite_db.t -> ((User.t * Withings_connection.connected) list, Error.t) result

val consume_withings_oauth_state_for_callback :
  Sqlite_db.t -> state_hash:string -> now:Ptime.t -> (User.t, Error.t) result
