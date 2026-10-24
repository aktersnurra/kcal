(** Plain-text alternatives compiled to a safe FTS5 disjunction. *)
type t
(** Tokenizes with SQLite unicode61 (remove_diacritics=0) using private TEMP
    tables on the caller-owned connection. Query rows are cleared on return;
    tables live until the owner closes the connection. Calls on a connection
    must be serialized. SQLite failures return sanitized [Error.Storage_error];
    alternatives without terms are invalid. *)
val of_strings : Sqlite3.db -> string list -> (t, Error.t) result
val fts5_match_expression : t -> string
