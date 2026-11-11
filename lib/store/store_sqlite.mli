type t = Sqlite_db.t
(** The ledger's SQLite store. Each aggregate lives in its own module; this one
    gathers them, so callers need only [Store_sqlite]. *)

include module type of struct
  include Store_users
end

include module type of struct
  include Store_products
end

include module type of struct
  include Store_meals
end

include module type of struct
  include Store_weigh_ins
end

include module type of struct
  include Store_withings
end

include module type of struct
  include Store_goals
end

val open_database : path:string -> (t, Error.t) result
(** Applies pending migrations. A file database uses write-ahead logging and
    waits up to five seconds for another process's write. *)

val close : t -> (unit, Error.t) result
val compile_search_query : t -> string list -> (Search_query.t, Error.t) result

module For_testing : sig
  val raw_database : t -> Sqlite3.db

  val cached_statement_count : t -> int
  (** How many prepared statements the connection is keeping for reuse. *)
end
