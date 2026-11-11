val get_goals : Sqlite_db.t -> user:User.t -> (Goals.t, Error.t) result

val set_goals :
  Sqlite_db.t -> user:User.t -> Goals.t -> (Goals.t, Error.t) result

val update_goals :
  Sqlite_db.t ->
  user:User.t ->
  (Goals.t option -> (Goals.t, Error.t) result) ->
  (Goals.t, Error.t) result
(** Replaces the goals with [f] of the current ones, in one transaction. *)
