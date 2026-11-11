val create_manual_weigh_in :
  Sqlite_db.t ->
  user:User.t ->
  Weigh_in.Create.t ->
  (Weigh_in.t, Error.t) result

val get_weigh_in :
  Sqlite_db.t -> user:User.t -> Weigh_in_id.t -> (Weigh_in.t, Error.t) result

val query_weigh_ins :
  Sqlite_db.t ->
  user:User.t ->
  from:Ptime.t option ->
  to_:Ptime.t option ->
  limit:int ->
  (Weigh_in.t list, Error.t) result

val update_manual_weigh_in :
  Sqlite_db.t ->
  user:User.t ->
  Weigh_in_id.t ->
  Weigh_in.Patch.t ->
  (Weigh_in.t, Error.t) result

val delete_weigh_in :
  Sqlite_db.t -> user:User.t -> Weigh_in_id.t -> (unit, Error.t) result

val weigh_ins_between :
  Sqlite_db.t ->
  user:User.t ->
  start:Ptime.t ->
  stop:Ptime.t ->
  (Weigh_in.t list, Error.t) result

val latest_weigh_in : Sqlite_db.t -> user:User.t -> (Weigh_in.t, Error.t) result
