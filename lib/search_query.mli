(** Plain-text alternatives compiled to a safe FTS5 disjunction. *)
type t
(** Tokenizes with SQLite unicode61 (remove_diacritics=0) in a private,
    short-lived in-memory database. SQLite failures return a sanitized
    [Error.Storage_error]; alternatives without terms are invalid. *)
val of_strings : string list -> (t, Error.t) result
val fts5_match_expression : t -> string
