let migration_directory = "migrations"

(* Embedded fallback keeps the executable and tests independent of the process CWD.
   The canonical migration remains migrations/001_ledger.sql. *)
let fallback_001 = {|
CREATE TABLE schema_migrations (version INTEGER PRIMARY KEY, applied_at TEXT NOT NULL);
CREATE TABLE users (id TEXT PRIMARY KEY, oidc_issuer TEXT NOT NULL, oidc_subject TEXT NOT NULL, created_at TEXT NOT NULL, UNIQUE (oidc_issuer, oidc_subject));
CREATE TABLE meals (id TEXT PRIMARY KEY, user_id TEXT NOT NULL REFERENCES users(id), eaten_at TEXT NOT NULL, description TEXT NOT NULL, calories_kcal INTEGER NOT NULL, protein_g REAL NOT NULL, carbs_g REAL, fat_g REAL, confidence REAL, estimate_source TEXT, notes TEXT, created_at TEXT NOT NULL, updated_at TEXT NOT NULL, deleted_at TEXT);
CREATE INDEX meals_user_eaten_at_idx ON meals(user_id, eaten_at);
CREATE TABLE weigh_ins (id TEXT PRIMARY KEY, user_id TEXT NOT NULL REFERENCES users(id), measured_at TEXT NOT NULL, weight_kg REAL NOT NULL, source TEXT NOT NULL CHECK (source IN ('manual', 'withings')), external_id TEXT, created_at TEXT NOT NULL, updated_at TEXT NOT NULL, deleted_at TEXT, UNIQUE(source, external_id));
CREATE INDEX weigh_ins_user_measured_at_idx ON weigh_ins(user_id, measured_at);
|}

let storage_error () = Error.Storage_error "migration failed"

let execute db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> Ok ()
  | _ -> Error (storage_error ())

let table_exists db name =
  let statement =
    Sqlite3.prepare db
      "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ? LIMIT 1"
  in
  ignore (Sqlite3.bind statement 1 (Sqlite3.Data.TEXT name));
  let exists = Sqlite3.step statement = Sqlite3.Rc.ROW in
  ignore (Sqlite3.finalize statement);
  exists

let version_of_filename filename =
  match String.split_on_char '_' filename with
  | prefix :: _ when Filename.extension filename = ".sql" ->
      (try Some (int_of_string prefix) with Failure _ -> None)
  | _ -> None

let migration_files () =
  try
    Sys.readdir migration_directory
    |> Array.to_list
    |> List.filter_map (fun filename ->
           match version_of_filename filename with
           | Some version -> Some (version, Filename.concat migration_directory filename)
           | None -> None)
    |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
    |> fun files -> Ok files
  with Sys_error _ -> Ok [ (1, "") ]

let is_applied db version =
  let statement =
    Sqlite3.prepare db "SELECT 1 FROM schema_migrations WHERE version = ? LIMIT 1"
  in
  ignore (Sqlite3.bind statement 1 (Sqlite3.Data.INT (Int64.of_int version)));
  let applied = Sqlite3.step statement = Sqlite3.Rc.ROW in
  ignore (Sqlite3.finalize statement);
  applied

let read_file path =
  if path = "" then Ok fallback_001 else
  try
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> Ok (really_input_string channel (in_channel_length channel)))
  with Sys_error _ -> Error (storage_error ())

let record_version db version =
  let statement =
    Sqlite3.prepare db
      "INSERT INTO schema_migrations (version, applied_at) VALUES (?, strftime('%Y-%m-%dT%H:%M:%fZ', 'now'))"
  in
  ignore (Sqlite3.bind statement 1 (Sqlite3.Data.INT (Int64.of_int version)));
  let result = if Sqlite3.step statement = Sqlite3.Rc.DONE then Ok () else Error (storage_error ()) in
  ignore (Sqlite3.finalize statement);
  result

let apply_all db =
  match migration_files () with
  | Error _ as error -> error
  | Ok files ->
      let unapplied =
        if table_exists db "schema_migrations" then
          List.filter (fun (version, _) -> not (is_applied db version)) files
        else files
      in
      match execute db "BEGIN IMMEDIATE" with
      | Error _ as error -> error
      | Ok () ->
          let result =
            List.fold_left
              (fun result (version, path) ->
                match result, read_file path with
                | Ok (), Ok sql ->
                    (match execute db sql with
                    | Ok () -> record_version db version
                    | Error _ as error -> error)
                | (Error _ as error), _ -> error
                | _, (Error _ as error) -> error)
              (Ok ()) unapplied
          in
          (match result with
          | Ok () ->
              (match execute db "COMMIT" with
              | Ok () -> Ok ()
              | Error _ as error -> error)
          | Error _ as error ->
              ignore (Sqlite3.exec db "ROLLBACK");
              error)
