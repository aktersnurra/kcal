val apply_all : Sqlite3.db -> (unit, Error.t) result
val migrations : (int * string) list

module For_testing : sig
  val execute : Sqlite3.db -> string -> (unit, Error.t) result
  val record_version : Sqlite3.db -> int -> (unit, Error.t) result
end
