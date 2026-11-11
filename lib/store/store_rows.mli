(** Row decoders that more than one aggregate's store reads with. Each
    [*_of_row] expects the columns of its [*_columns], in order from column 0,
    and is [None] for a row that does not decode. *)

val user_columns : string
val user_of_row : Sqlite3.stmt -> User.t option
val weigh_in_columns : string
val weigh_in_of_row : Sqlite3.stmt -> Weigh_in.t option
