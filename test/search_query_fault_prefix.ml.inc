(* Prepended to the production compiler by dune, without changing its API.
   Real resources are released before injecting SQLite cleanup failures. *)
module Sqlite3 = struct
  include Sqlite3

  type cleanup_fault = No_fault | Return_failure | Raise_failure
  let finalize_fault = ref No_fault
  let close_fault = ref No_fault
  let fail_step = ref false
  let finalized = ref 0
  let closed = ref 0

  let finalize statement =
    let result = Sqlite3.finalize statement in
    incr finalized;
    match !finalize_fault with
    | No_fault -> result
    | Return_failure -> Rc.ERROR
    | Raise_failure -> raise (SqliteError "private finalize details")

  let db_close db =
    let result = Sqlite3.db_close db in
    incr closed;
    match !close_fault with
    | No_fault -> result
    | Return_failure -> false
    | Raise_failure -> raise (Error "private close details")

  let step statement = if !fail_step then Rc.ERROR else Sqlite3.step statement
end
