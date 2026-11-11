module Domain_error = Error
open Base
module Error = Domain_error

(* Prepared statements are kept by their SQL and reused: every query in the
   store is one of a small fixed set of literals, so the cache stays small. A
   statement is taken out while it runs, which keeps a nested use of the same
   SQL from sharing its cursor. *)
type t = { raw : Sqlite3.db; statements : (string, Sqlite3.stmt) Hashtbl.t }

let raw db = db.raw
let cached_statement_count db = Hashtbl.length db.statements
let log_src = Logs.Src.create "kcal.store" ~doc:"SQLite persistence"

module Log = (val Logs.src_log log_src : Logs.LOG)

let close db =
  try
    Hashtbl.iter db.statements ~f:(fun statement ->
        ignore (Sqlite3.finalize statement));
    Hashtbl.clear db.statements;
    if Sqlite3.db_close db.raw then Ok ()
    else Error (Error.Storage_error "SQLite close failed")
  with Sqlite3.Error _ | Sqlite3.SqliteError _ ->
    Error (Error.Storage_error "SQLite close failed")

(* How long a writer waits for another process's write, such as
   [kcal sync-withings] running beside [kcal serve], before giving up. *)
let busy_timeout_ms = 5000

(* WAL lets readers proceed beside a writer; with it, NORMAL still survives a
   process crash and only risks the last commits on power loss. An in-memory
   database keeps its own journal mode. *)
let pragmas =
  "PRAGMA journal_mode = WAL; PRAGMA synchronous = NORMAL; PRAGMA foreign_keys \
   = ON"

let open_database ~path ~migrate =
  try
    let raw = Sqlite3.db_open path in
    let initialize () =
      Sqlite3.busy_timeout raw busy_timeout_ms;
      match Sqlite3.exec raw pragmas with
      | Sqlite3.Rc.OK ->
          Result.map (migrate raw) ~f:(fun () ->
              { raw; statements = Hashtbl.create (module String) })
      | rc ->
          Log.err (fun m ->
              m "SQLite initialization failed: %s: %s" (Sqlite3.Rc.to_string rc)
                (Sqlite3.errmsg raw));
          Error (Error.Storage_error "SQLite initialization failed")
    in
    match initialize () with
    | Ok _ as result -> result
    | Error _ as error ->
        ignore (Sqlite3.db_close raw);
        error
    | exception exn ->
        ignore (Sqlite3.db_close raw);
        raise exn
  with Sqlite3.Error _ | Sqlite3.SqliteError _ ->
    Error (Error.Storage_error "SQLite initialization failed")

let ( let* ) result f = Result.bind result ~f
let storage_error () = Error.Storage_error "SQLite operation failed"
let invalid_input () = Error.Invalid_input "invalid query or update"
let now () = Option.value_exn (Ptime.of_float_s (Unix.gettimeofday ()))
let timestamp time = Time.to_utc_string time

(* Every SQLite failure is logged here with its result code and message; the
   caller only ever sees the sanitized storage error. [sql] is a literal query
   defined in the store, never user-controlled, so logging it carries no
   injection or PII risk. *)
let failed db ~sql rc =
  Log.err (fun m ->
      m "query %S failed: %s: %s" sql (Sqlite3.Rc.to_string rc)
        (Sqlite3.errmsg db.raw));
  Error (storage_error ())

(* Back into the cache when it ran cleanly and no nested use of the same SQL
   got there first; finalized otherwise. *)
let release db sql statement ~reusable =
  try
    if reusable && not (Hashtbl.mem db.statements sql) then (
      ignore (Sqlite3.reset statement);
      ignore (Sqlite3.clear_bindings statement);
      Hashtbl.set db.statements ~key:sql ~data:statement)
    else ignore (Sqlite3.finalize statement)
  with Sqlite3.Error _ | Sqlite3.SqliteError _ -> ()

let with_statement db sql f =
  let raised exn =
    Log.err (fun m -> m "query %S raised %s" sql (Exn.to_string exn));
    Error (storage_error ())
  in
  try
    let statement =
      match Hashtbl.find_and_remove db.statements sql with
      | Some statement -> statement
      | None -> Sqlite3.prepare db.raw sql
    in
    let reusable = ref false in
    Stdlib.Fun.protect
      ~finally:(fun () -> release db sql statement ~reusable:!reusable)
      (fun () ->
        try
          let result = f statement in
          reusable := true;
          result
        with
        | ( Sqlite3.Error _ | Sqlite3.SqliteError _ | Sqlite3.RangeError _
          | Sqlite3.DataTypeError _ | Sqlite3.InternalError _ ) as exn
        ->
          raised exn)
  with
  | ( Sqlite3.Error _ | Sqlite3.SqliteError _ | Sqlite3.RangeError _
    | Sqlite3.DataTypeError _ | Sqlite3.InternalError _ ) as exn
  ->
    raised exn

let prepared db sql values f =
  with_statement db sql (fun statement ->
      match Sqlite3.bind_values statement values with
      | Sqlite3.Rc.OK -> f statement
      | rc -> failed db ~sql rc)

(* The first row of [sql], or [absent] when there is none. *)
let first_row db sql values ~absent ~present =
  prepared db sql values (fun statement ->
      match Sqlite3.step statement with
      | Sqlite3.Rc.ROW -> present statement
      | Sqlite3.Rc.DONE -> absent ()
      | rc -> failed db ~sql rc)

let undecodable_row = Error.Storage_decoding "row"

let query_one ?(undecodable = undecodable_row) db sql values decode =
  first_row db sql values
    ~absent:(fun () -> Error Error.Not_found)
    ~present:(fun statement ->
      Result.of_option (decode statement) ~error:undecodable)

let query_optional ?(undecodable = undecodable_row) db sql values decode =
  first_row db sql values
    ~absent:(fun () -> Ok None)
    ~present:(fun statement ->
      match decode statement with
      | Some value -> Ok (Some value)
      | None -> Error undecodable)

let query ?(undecodable = undecodable_row) db sql values decode =
  prepared db sql values (fun statement ->
      let rec rows values =
        match Sqlite3.step statement with
        | Sqlite3.Rc.ROW -> (
            match decode statement with
            | Some value -> rows (value :: values)
            | None -> Error undecodable)
        | Sqlite3.Rc.DONE -> Ok (List.rev values)
        | rc -> failed db ~sql rc
      in
      rows [])

(* Runs a statement that returns no rows; the number of rows it changed. *)
let execute_changes db sql values =
  prepared db sql values (fun statement ->
      match Sqlite3.step statement with
      | Sqlite3.Rc.DONE -> Ok (Sqlite3.changes db.raw)
      | rc -> failed db ~sql rc)

let execute db sql values =
  Result.map (execute_changes db sql values) ~f:(fun (_ : int) -> ())

let in_transaction db f =
  try
    match Sqlite3.exec db.raw "BEGIN IMMEDIATE" with
    | Sqlite3.Rc.OK ->
        let committed = ref false in
        Stdlib.Fun.protect
          ~finally:(fun () ->
            if not !committed then ignore (Sqlite3.exec db.raw "ROLLBACK"))
          (fun () ->
            let* value = f () in
            match Sqlite3.exec db.raw "COMMIT" with
            | Sqlite3.Rc.OK ->
                committed := true;
                Ok value
            | rc -> failed db ~sql:"COMMIT" rc)
    | rc -> failed db ~sql:"BEGIN IMMEDIATE" rc
  with Sqlite3.Error _ | Sqlite3.SqliteError _ -> Error (storage_error ())

(* The read must share the write transaction: invalid durable numeric values
   are reported, never repaired as a side effect of an ordinary update. *)
let with_checked_numeric_update db ~check ~update =
  in_transaction db (fun () ->
      let* () = check () in
      update ())

let text_value value = Sqlite3.Data.TEXT value
let int_value value = Sqlite3.Data.INT (Int64.of_int value)
let float_value value = Sqlite3.Data.FLOAT value
let time_value time = text_value (timestamp time)

let nullable = function
  | None -> Sqlite3.Data.NULL
  | Some value -> Sqlite3.Data.TEXT value

let nullable_float = function
  | None -> Sqlite3.Data.NULL
  | Some value -> Sqlite3.Data.FLOAT value

let nullable_int = function
  | None -> Sqlite3.Data.NULL
  | Some value -> int_value value

let owner user = text_value (User_id.to_string user.User.id)

let qualified_columns table columns =
  String.split columns ~on:','
  |> List.map ~f:(fun column -> table ^ "." ^ String.strip column)
  |> String.concat ~sep:", "

(* Optional inclusive bounds on [column], as SQL to append to a WHERE clause
   and the values it binds. Leaving an absent bound out of the SQL, rather
   than binding NULL, lets SQLite use the index range on the column. *)
let range_clause column ~from ~to_ =
  let bound operator =
    Option.map ~f:(fun time ->
        (Printf.sprintf " AND %s %s ?" column operator, time_value time))
  in
  let bounds = List.filter_opt [ bound ">=" from; bound "<=" to_ ] in
  (String.concat (List.map bounds ~f:fst), List.map bounds ~f:snd)

let text statement column = Sqlite3.column_text statement column
let float statement column = Sqlite3.column_double statement column
let int statement column = Sqlite3.column_int statement column

let optional_text statement column =
  if Sqlite3.column_is_null statement column then None
  else Some (text statement column)

let optional_float statement column =
  if Sqlite3.column_is_null statement column then None
  else Some (float statement column)

let optional_int64 statement column =
  if Sqlite3.column_is_null statement column then None
  else Some (Sqlite3.column_int64 statement column)

let parse_time value =
  match Time.parse_offset_datetime value with
  | Ok time -> Some time
  | Error _ -> None

(* Decode SQLite numeric storage classes without coercing text or truncating
   integer calories. Domain constructors check the resulting numeric values. *)
let numeric_column statement column =
  match Sqlite3.column statement column with
  | Sqlite3.Data.INT _ | Sqlite3.Data.FLOAT _ -> true
  | _ -> false

let optional_numeric_column statement column =
  Sqlite3.column_is_null statement column || numeric_column statement column

let calorie_column statement column =
  match Sqlite3.column statement column with
  | Sqlite3.Data.INT value ->
      Int64.compare value 0L >= 0
      && Int64.compare value (Int64.of_int Int.max_value) <= 0
  | _ -> false

let json_ids ids =
  Sqlite3.Data.TEXT
    (Yojson.Safe.to_string (`List (List.map ids ~f:(fun id -> `String id))))

let is_valid_query ~from ~to_ ~limit =
  limit >= 1 && limit <= 500
  &&
  match (from, to_) with
  | Some lower, Some upper -> Ptime.compare lower upper <= 0
  | _ -> true

(* Deleting a record that is already deleted succeeds; one that never existed
   for this user is not found. [table] is a literal from this module. *)
let soft_delete db ~table ~user id =
  let current = time_value (now ()) in
  let* changed =
    execute_changes db
      ("UPDATE " ^ table
     ^ " SET deleted_at = ?, updated_at = ? WHERE id = ? AND user_id = ? AND \
        deleted_at IS NULL")
      [ current; current; text_value id; owner user ]
  in
  if changed = 1 then Ok ()
  else
    first_row db
      ("SELECT 1 FROM " ^ table
     ^ " WHERE id = ? AND user_id = ? AND deleted_at IS NOT NULL")
      [ text_value id; owner user ]
      ~absent:(fun () -> Error Error.Not_found)
      ~present:(fun _ -> Ok ())
