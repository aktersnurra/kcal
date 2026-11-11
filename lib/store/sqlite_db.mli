(** One SQLite connection and the vocabulary the store's modules share for
    talking to it. Every failure is logged here with SQLite's own reason and
    reaches the caller as the sanitized [Error.Storage_error]. Calls on a
    connection must be serialized. *)

type t

val open_database :
  path:string ->
  migrate:(Sqlite3.db -> (unit, Error.t) result) ->
  (t, Error.t) result
(** A file database uses write-ahead logging and waits up to five seconds for
    another process's write. [migrate] runs before the connection is handed out.
*)

val close : t -> (unit, Error.t) result
val raw : t -> Sqlite3.db

val cached_statement_count : t -> int
(** Statements are prepared once per distinct SQL text and reused. *)

val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val storage_error : unit -> Error.t
val invalid_input : unit -> Error.t
val now : unit -> Ptime.t
val timestamp : Ptime.t -> string

(** {1 Running SQL}

    [sql] must be a literal from the store, never caller-controlled text: it is
    logged on failure and kept as a cache key. A row [decode] rejects with
    [None] fails the call with [undecodable], by default
    [Error.Storage_decoding "row"]. *)

val first_row :
  t ->
  string ->
  Sqlite3.Data.t list ->
  absent:(unit -> ('a, Error.t) result) ->
  present:(Sqlite3.stmt -> ('a, Error.t) result) ->
  ('a, Error.t) result
(** [present] on the first row, [absent] when there is none. *)

val query_one :
  ?undecodable:Error.t ->
  t ->
  string ->
  Sqlite3.Data.t list ->
  (Sqlite3.stmt -> 'a option) ->
  ('a, Error.t) result
(** The first row; [Error.Not_found] when there is none. *)

val query_optional :
  ?undecodable:Error.t ->
  t ->
  string ->
  Sqlite3.Data.t list ->
  (Sqlite3.stmt -> 'a option) ->
  ('a option, Error.t) result

val query :
  ?undecodable:Error.t ->
  t ->
  string ->
  Sqlite3.Data.t list ->
  (Sqlite3.stmt -> 'a option) ->
  ('a list, Error.t) result

val execute : t -> string -> Sqlite3.Data.t list -> (unit, Error.t) result

val execute_changes :
  t -> string -> Sqlite3.Data.t list -> (int, Error.t) result
(** The number of rows the statement changed. *)

val in_transaction : t -> (unit -> ('a, Error.t) result) -> ('a, Error.t) result
(** Commits when [f] returns [Ok], rolls back otherwise. Does not nest. *)

val with_checked_numeric_update :
  t ->
  check:(unit -> (unit, Error.t) result) ->
  update:(unit -> ('a, Error.t) result) ->
  ('a, Error.t) result
(** [check] then [update] in one transaction, so invalid stored numeric values
    are reported, never repaired as a side effect of an ordinary update. *)

(** {1 Building queries} *)

val text_value : string -> Sqlite3.Data.t
val int_value : int -> Sqlite3.Data.t
val float_value : float -> Sqlite3.Data.t
val time_value : Ptime.t -> Sqlite3.Data.t
val nullable : string option -> Sqlite3.Data.t
val nullable_float : float option -> Sqlite3.Data.t
val nullable_int : int option -> Sqlite3.Data.t

val json_ids : string list -> Sqlite3.Data.t
(** A JSON array to match against with [json_each]. *)

val qualified_columns : string -> string -> string
(** [qualified_columns table "a, b"] is ["table.a, table.b"]. *)

val range_clause :
  string ->
  from:Ptime.t option ->
  to_:Ptime.t option ->
  string * Sqlite3.Data.t list
(** Optional inclusive bounds on a column, as SQL to append to a WHERE clause
    and the values it binds. An absent bound is left out of the SQL, so SQLite
    can use the index range on the column. *)

val is_valid_query :
  from:Ptime.t option -> to_:Ptime.t option -> limit:int -> bool
(** A limit from 1 to 500 and bounds that are not reversed. *)

(** {1 Reading rows} *)

val text : Sqlite3.stmt -> int -> string
val float : Sqlite3.stmt -> int -> float
val int : Sqlite3.stmt -> int -> int
val optional_text : Sqlite3.stmt -> int -> string option
val optional_float : Sqlite3.stmt -> int -> float option
val optional_int64 : Sqlite3.stmt -> int -> int64 option
val parse_time : string -> Ptime.t option

val numeric_column : Sqlite3.stmt -> int -> bool
(** Whether the column holds an integer or a float, without coercing text. *)

val optional_numeric_column : Sqlite3.stmt -> int -> bool

val calorie_column : Sqlite3.stmt -> int -> bool
(** Whether the column holds a nonnegative integer that fits an [int]. *)
