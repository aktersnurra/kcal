(** What the aggregates' stores share about the domain: rows scoped to a user,
    and row decoders that more than one of them reads with. Each [*_of_row]
    expects the columns of its [*_columns], in order from column 0, and is
    [None] for a row that does not decode. *)

val owner : User.t -> Sqlite3.Data.t
(** The [user_id] value of the user's rows. *)

val soft_delete :
  Sqlite_db.t -> table:string -> user:User.t -> string -> (unit, Error.t) result
(** Marks the user's row with that id deleted. Succeeds for a row already
    deleted; not found for one that never existed for this user. *)

val user_columns : string
val user_of_row : Sqlite3.stmt -> User.t option
val weigh_in_columns : string
val weigh_in_of_row : Sqlite3.stmt -> Weigh_in.t option
