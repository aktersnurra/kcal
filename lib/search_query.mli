(** Plain-text alternatives compiled to a safe FTS5 disjunction. *)
type t
val of_strings : string list -> (t, Error.t) result
val fts5_match_expression : t -> string
