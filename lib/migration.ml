module Domain_error = Error
open Base
module Error = Domain_error

let is_ok = function Sqlite3.Rc.OK -> true | _ -> false
let is_done = function Sqlite3.Rc.DONE -> true | _ -> false
let storage_error () = Error.Storage_error "migration failed"

let execute db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> Ok ()
  | _ -> Error (storage_error ())

let with_statement db sql f =
  try
    let statement = Sqlite3.prepare db sql in
    Exn.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize statement))
      ~f:(fun () -> f statement)
  with Sqlite3.Error _ | Sqlite3.SqliteError _ -> Error (storage_error ())

let is_applied db version =
  with_statement db "SELECT 1 FROM schema_migrations WHERE version = ? LIMIT 1"
    (fun statement ->
      if
        not
          (is_ok
             (Sqlite3.bind statement 1
                (Sqlite3.Data.INT (Int64.of_int version))))
      then Error (storage_error ())
      else
        match Sqlite3.step statement with
        | Sqlite3.Rc.ROW -> Ok true
        | Sqlite3.Rc.DONE -> Ok false
        | _ -> Error (storage_error ()))

let record_version db version =
  with_statement db
    "INSERT INTO schema_migrations (version, applied_at) VALUES (?, \
     strftime('%Y-%m-%dT%H:%M:%fZ', 'now'))" (fun statement ->
      if
        not
          (is_ok
             (Sqlite3.bind statement 1
                (Sqlite3.Data.INT (Int64.of_int version))))
      then Error (storage_error ())
      else if is_done (Sqlite3.step statement) then Ok ()
      else Error (storage_error ()))

(* Migration 001 begins with bookkeeping. Bootstrap that exact SQL before
   querying versions, then apply the remainder as version 1. *)
let bookkeeping_sql, ledger_sql =
  let sql = List.Assoc.find_exn Embedded_migrations.all 1 ~equal:Int.equal in
  let boundary = String.index_exn sql ';' + 1 in
  ( String.sub sql ~pos:0 ~len:boundary,
    String.sub sql ~pos:boundary ~len:(String.length sql - boundary) )

let bootstrap_bookkeeping db =
  with_statement db
    "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = \
     'schema_migrations'" (fun statement ->
      match Sqlite3.step statement with
      | Sqlite3.Rc.ROW -> Ok ()
      | Sqlite3.Rc.DONE -> execute db bookkeeping_sql
      | _ -> Error (storage_error ()))

let foreign_key_check db =
  with_statement db "PRAGMA foreign_key_check" (fun statement ->
      if is_done (Sqlite3.step statement) then Ok ()
      else Error (storage_error ()))

let apply_all db =
  match execute db "BEGIN IMMEDIATE" with
  | Error _ as error -> error
  | Ok () -> (
      let rollback error =
        ignore (Sqlite3.exec db "ROLLBACK");
        error
      in
      let result =
        List.fold Embedded_migrations.all ~init:(bootstrap_bookkeeping db)
          ~f:(fun result (version, sql) ->
            match result with
            | Error _ -> result
            | Ok () -> (
                let already_applied = is_applied db version in
                match already_applied with
                | Error _ as error -> error
                | Ok true -> Ok ()
                | Ok false -> (
                    match
                      execute db (if version = 1 then ledger_sql else sql)
                    with
                    | Error _ as error -> error
                    | Ok () -> record_version db version)))
      in
      match Result.bind result ~f:(fun () -> foreign_key_check db) with
      | Error _ as error -> rollback error
      | Ok () -> (
          match execute db "COMMIT" with
          | Ok () -> Ok ()
          | Error _ as error -> rollback error))

let migrations = Embedded_migrations.all

module For_testing = struct
  let execute = execute
  let record_version = record_version
end
