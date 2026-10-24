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

(* TEMP resources belong to the caller's connection; only query rows are transient. *)
let of_strings db alternatives =
  let invalid () = Error (Error.Invalid_input "search requires nonblank alternatives with searchable words") in
  if alternatives = [] then invalid ()
  else
    try
      Fun.protect
        ~finally:(fun () -> Sqlite3.Rc.check (Sqlite3.exec db
          "DELETE FROM temp.kcal_search_query_input"))
        (fun () ->
          Sqlite3.Rc.check (Sqlite3.exec db
            "CREATE VIRTUAL TABLE IF NOT EXISTS temp.kcal_search_query_input USING fts5(text, tokenize='unicode61 remove_diacritics 0'); CREATE VIRTUAL TABLE IF NOT EXISTS temp.kcal_search_query_terms USING fts5vocab('temp', 'kcal_search_query_input', 'row')");
          let words text =
            Sqlite3.Rc.check (Sqlite3.exec db "DELETE FROM temp.kcal_search_query_input");
            with_statement db "INSERT INTO temp.kcal_search_query_input(text) VALUES (?)" (fun statement ->
              Sqlite3.Rc.check (Sqlite3.bind statement 1 (Sqlite3.Data.TEXT text));
              match Sqlite3.step statement with
              | Sqlite3.Rc.DONE -> ()
              | _ -> raise (Sqlite3.SqliteError "query tokenizer insert failed"));
            with_statement db "SELECT term FROM temp.kcal_search_query_terms ORDER BY term" (fun statement ->
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
