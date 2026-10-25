(* Source-derived shim: real statements are finalized before injecting failures. *)
module Sqlite3 = struct
  open Base
  include Sqlite3

  type cleanup_fault = No_fault | Return_failure | Raise_failure

  let finalize_fault = ref No_fault
  let fail_step = ref false
  let fail_prepare = ref false
  let fail_bind = ref false
  let fail_setup = ref false
  let fail_cleanup = ref false
  let deletes = ref 0
  let finalized = ref 0
  let opened = ref 0
  let closed = ref 0

  let db_open name =
    Int.incr opened;
    Sqlite3.db_open name

  let db_close db =
    Int.incr closed;
    Sqlite3.db_close db

  let finalize statement =
    let result = Sqlite3.finalize statement in
    Int.incr finalized;
    match !finalize_fault with
    | No_fault -> result
    | Return_failure -> Rc.ERROR
    | Raise_failure -> raise (SqliteError "private finalize details")

  let prepare db sql =
    if !fail_prepare then raise (SqliteError "private prepare details");
    Sqlite3.prepare db sql

  let bind statement index value =
    if !fail_bind then Rc.ERROR else Sqlite3.bind statement index value

  let exec db sql =
    if !fail_setup && String.is_prefix ~prefix:"CREATE" sql then Rc.ERROR
    else if String.is_prefix ~prefix:"DELETE" sql then (
      Int.incr deletes;
      if !fail_cleanup && !deletes = 2 then
        let result = Sqlite3.exec db sql in
        if match result with Rc.OK -> true | _ -> false then Rc.ERROR
        else result
      else Sqlite3.exec db sql)
    else Sqlite3.exec db sql

  let step statement = if !fail_step then Rc.ERROR else Sqlite3.step statement
end
