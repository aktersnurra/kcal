type t = string

(* Fun.protect wraps cleanup exceptions, including errors reported by finalize
   after a failed step. Normalize only SQLite failures, not unrelated bugs. *)
let rec is_sqlite_failure = function
  | Sqlite3.Error _ | Sqlite3.SqliteError _ | Sqlite3.InternalError _
  | Sqlite3.RangeError _ | Sqlite3.DataTypeError _ -> true
  | Fun.Finally_raised error -> is_sqlite_failure error
  | _ -> false

let with_statement db sql f =
  let statement = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> Sqlite3.Rc.check (Sqlite3.finalize statement))
    (fun () -> f statement)

(* Use a private, short-lived database so query words and indexed documents use
   exactly the same tokenizer. No application database or shared state is used. *)
let of_strings alternatives =
  let invalid () = Error (Error.Invalid_input "search requires nonblank alternatives with searchable words") in
  if alternatives = [] then invalid ()
  else
    try
      let db = Sqlite3.db_open ":memory:" in
      Fun.protect
        ~finally:(fun () -> if not (Sqlite3.db_close db) then
          raise (Sqlite3.SqliteError "query tokenizer close failed"))
        (fun () ->
          Sqlite3.Rc.check (Sqlite3.exec db
            "CREATE VIRTUAL TABLE input USING fts5(text, tokenize='unicode61 remove_diacritics 0'); CREATE VIRTUAL TABLE terms USING fts5vocab(input, 'row')");
          let words text =
            Sqlite3.Rc.check (Sqlite3.exec db "DELETE FROM input");
            with_statement db "INSERT INTO input(text) VALUES (?)" (fun statement ->
              Sqlite3.Rc.check (Sqlite3.bind statement 1 (Sqlite3.Data.TEXT text));
              match Sqlite3.step statement with
              | Sqlite3.Rc.DONE -> ()
              | _ -> raise (Sqlite3.SqliteError "query tokenizer insert failed"));
            with_statement db "SELECT term FROM terms ORDER BY term" (fun statement ->
              let rec collect acc =
                match Sqlite3.step statement with
                | Sqlite3.Rc.ROW -> collect (Sqlite3.column_text statement 0 :: acc)
                | Sqlite3.Rc.DONE -> List.rev acc
                | _ -> raise (Sqlite3.SqliteError "query tokenizer read failed")
              in
              collect [])
          in
          let groups = List.map words alternatives in
          if List.exists (fun group -> group = []) groups then invalid ()
          else
            let words = List.concat groups |> List.sort_uniq String.compare in
            let quote word = "\"" ^ String.concat "\"\"" (String.split_on_char '"' word) ^ "\"" in
            Ok (String.concat " OR " (List.map quote words)))
    with
    | error when is_sqlite_failure error ->
        Error (Error.Storage_error "search query tokenization failed")

let fts5_match_expression query = query
