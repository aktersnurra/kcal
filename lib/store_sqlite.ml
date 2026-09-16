type t = Sqlite3.db

(* The application schema is embedded here so the installed executable does not
   depend on its current working directory.  Migration executes these versions
   transactionally. *)
let migrations =
  [
    ( 1,
      {|
CREATE TABLE schema_migrations (version INTEGER PRIMARY KEY, applied_at TEXT NOT NULL);
CREATE TABLE users (id TEXT PRIMARY KEY, oidc_issuer TEXT NOT NULL, oidc_subject TEXT NOT NULL, created_at TEXT NOT NULL, UNIQUE (oidc_issuer, oidc_subject));
CREATE TABLE meals (id TEXT PRIMARY KEY, user_id TEXT NOT NULL REFERENCES users(id), eaten_at TEXT NOT NULL, description TEXT NOT NULL, calories_kcal INTEGER NOT NULL, protein_g REAL NOT NULL, carbs_g REAL, fat_g REAL, confidence REAL, estimate_source TEXT, notes TEXT, created_at TEXT NOT NULL, updated_at TEXT NOT NULL, deleted_at TEXT);
CREATE INDEX meals_user_eaten_at_idx ON meals(user_id, eaten_at);
CREATE TABLE weigh_ins (id TEXT PRIMARY KEY, user_id TEXT NOT NULL REFERENCES users(id), measured_at TEXT NOT NULL, weight_kg REAL NOT NULL, source TEXT NOT NULL CHECK (source IN ('manual', 'withings')), external_id TEXT, created_at TEXT NOT NULL, updated_at TEXT NOT NULL, deleted_at TEXT, UNIQUE(source, external_id));
CREATE INDEX weigh_ins_user_measured_at_idx ON weigh_ins(user_id, measured_at);
|} );
  ]

let storage_error () = Error.Storage_error "SQLite operation failed"
let invalid_input () = Error.Invalid_input "invalid query or update"
let now () = Option.get (Ptime.of_float_s (Unix.gettimeofday ()))
let timestamp time = Time.to_utc_string time
let nullable = function None -> Sqlite3.Data.NULL | Some value -> Sqlite3.Data.TEXT value
let nullable_float = function None -> Sqlite3.Data.NULL | Some value -> Sqlite3.Data.FLOAT value

let with_statement db sql f =
  try
    let statement = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> try ignore (Sqlite3.finalize statement) with _ -> ())
      (fun () -> try f statement with _ -> Error (storage_error ()))
  with _ -> Error (storage_error ())

let bind statement values =
  if Sqlite3.bind_values statement values = Sqlite3.Rc.OK then Ok () else Error (storage_error ())

let text statement column = Sqlite3.column_text statement column
let float statement column = Sqlite3.column_double statement column
let int statement column = Sqlite3.column_int statement column
let optional_text statement column = if Sqlite3.column_is_null statement column then None else Some (text statement column)
let optional_float statement column = if Sqlite3.column_is_null statement column then None else Some (float statement column)

let parse_time value =
  match Time.parse_offset_datetime value with Ok time -> Some time | Error _ -> None

let user_of_row statement =
  match User_id.of_string (text statement 0), parse_time (text statement 3) with
  | Some id, Some created_at -> Some User.{ id; oidc_issuer = text statement 1; oidc_subject = text statement 2; created_at }
  | _ -> None

let meal_columns =
  "id, user_id, eaten_at, description, calories_kcal, protein_g, carbs_g, fat_g, confidence, estimate_source, notes, created_at, updated_at, deleted_at"

let meal_of_row statement =
  match
    ( Meal_id.of_string (text statement 0), User_id.of_string (text statement 1),
      parse_time (text statement 2), parse_time (text statement 11), parse_time (text statement 12) )
  with
  | Some id, Some user_id, Some eaten_at, Some created_at, Some updated_at ->
      Some Meal.{ id; user_id; eaten_at; description = text statement 3; calories_kcal = int statement 4;
                  protein_g = float statement 5; carbs_g = optional_float statement 6;
                  fat_g = optional_float statement 7; confidence = optional_float statement 8;
                  estimate_source = optional_text statement 9; notes = optional_text statement 10;
                  created_at; updated_at; deleted_at = Option.bind (optional_text statement 13) parse_time }
  | _ -> None

let resolve_user db ~issuer ~subject =
  let created_at = timestamp (now ()) in
  with_statement db
    "INSERT INTO users (id, oidc_issuer, oidc_subject, created_at) VALUES (?, ?, ?, ?) \
     ON CONFLICT(oidc_issuer, oidc_subject) DO UPDATE SET oidc_issuer = excluded.oidc_issuer \
     RETURNING id, oidc_issuer, oidc_subject, created_at"
    (fun statement ->
      match bind statement [ Sqlite3.Data.TEXT (User_id.to_string (User_id.fresh ())); Sqlite3.Data.TEXT issuer; Sqlite3.Data.TEXT subject; Sqlite3.Data.TEXT created_at ] with
      | Error _ as error -> error
      | Ok () ->
          if Sqlite3.step statement = Sqlite3.Rc.ROW then
            match user_of_row statement with Some user -> Ok user | None -> Error (storage_error ())
          else Error (storage_error ()))

let create_meal db ~user input =
  match Meal.validate_create input with
  | Error _ as error -> error
  | Ok () ->
      let current = now () in
      let eaten_at = Option.value input.eaten_at ~default:current |> timestamp in
      let current = timestamp current in
      with_statement db
        ("INSERT INTO meals (" ^ meal_columns ^ ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL) RETURNING " ^ meal_columns)
        (fun statement ->
          let values = [ Sqlite3.Data.TEXT (Meal_id.to_string (Meal_id.fresh ())); Sqlite3.Data.TEXT (User_id.to_string user.User.id); Sqlite3.Data.TEXT eaten_at;
                         Sqlite3.Data.TEXT input.description; Sqlite3.Data.INT (Int64.of_int input.calories_kcal); Sqlite3.Data.FLOAT input.protein_g;
                         nullable_float input.carbs_g; nullable_float input.fat_g; nullable_float input.confidence; nullable input.estimate_source;
                         nullable input.notes; Sqlite3.Data.TEXT current; Sqlite3.Data.TEXT current ] in
          match bind statement values with
          | Error _ as error -> error
          | Ok () -> if Sqlite3.step statement = Sqlite3.Rc.ROW then match meal_of_row statement with Some meal -> Ok meal | None -> Error (storage_error ()) else Error (storage_error ()))

let get_meal db ~user id =
  with_statement db ("SELECT " ^ meal_columns ^ " FROM meals WHERE id = ? AND user_id = ? AND deleted_at IS NULL")
    (fun statement ->
      match bind statement [ Sqlite3.Data.TEXT (Meal_id.to_string id); Sqlite3.Data.TEXT (User_id.to_string user.User.id) ] with
      | Error _ as error -> error
      | Ok () -> if Sqlite3.step statement = Sqlite3.Rc.ROW then match meal_of_row statement with Some meal -> Ok meal | None -> Error (storage_error ()) else Error Error.Not_found)

let valid_meal_patch (patch : Meal.patch) =
  let nonnegative = function None -> true | Some value -> value >= 0.0 in
  let confidence = function None -> true | Some value -> value >= 0.0 && value <= 1.0 in
  (match patch.description with None -> true | Some value -> String.trim value <> "")
  && (match patch.calories_kcal with None -> true | Some value -> value >= 0)
  && nonnegative patch.protein_g && nonnegative patch.carbs_g && nonnegative patch.fat_g && confidence patch.confidence

let update_meal db ~user id patch =
  if not (valid_meal_patch patch) then Error (invalid_input ()) else
  let updated_at = timestamp (now ()) in
  with_statement db
    ("UPDATE meals SET eaten_at = COALESCE(?, eaten_at), description = COALESCE(?, description), calories_kcal = COALESCE(?, calories_kcal), protein_g = COALESCE(?, protein_g), carbs_g = COALESCE(?, carbs_g), fat_g = COALESCE(?, fat_g), confidence = COALESCE(?, confidence), estimate_source = COALESCE(?, estimate_source), notes = COALESCE(?, notes), updated_at = ? WHERE id = ? AND user_id = ? AND deleted_at IS NULL RETURNING " ^ meal_columns)
    (fun statement ->
      let values = [ nullable (Option.map timestamp patch.eaten_at); nullable patch.description;
                     (match patch.calories_kcal with None -> Sqlite3.Data.NULL | Some value -> Sqlite3.Data.INT (Int64.of_int value)); nullable_float patch.protein_g;
                     nullable_float patch.carbs_g; nullable_float patch.fat_g; nullable_float patch.confidence; nullable patch.estimate_source; nullable patch.notes;
                     Sqlite3.Data.TEXT updated_at; Sqlite3.Data.TEXT (Meal_id.to_string id); Sqlite3.Data.TEXT (User_id.to_string user.User.id) ] in
      match bind statement values with
      | Error _ as error -> error
      | Ok () -> if Sqlite3.step statement = Sqlite3.Rc.ROW then match meal_of_row statement with Some meal -> Ok meal | None -> Error (storage_error ()) else Error Error.Not_found)

let valid_query ~from ~to_ ~limit =
  limit >= 1 && limit <= 500 && match from, to_ with Some lower, Some upper -> Ptime.compare lower upper <= 0 | _ -> true

let query_meals db ~user ~from ~to_ ~limit =
  if not (valid_query ~from ~to_ ~limit) then Error (invalid_input ()) else
  with_statement db
    ("SELECT " ^ meal_columns ^ " FROM meals WHERE user_id = ? AND deleted_at IS NULL AND (? IS NULL OR eaten_at >= ?) AND (? IS NULL OR eaten_at <= ?) ORDER BY eaten_at ASC LIMIT ?")
    (fun statement ->
      let from_value = nullable (Option.map timestamp from) and to_value = nullable (Option.map timestamp to_) in
      match bind statement [ Sqlite3.Data.TEXT (User_id.to_string user.User.id); from_value; from_value; to_value; to_value; Sqlite3.Data.INT (Int64.of_int limit) ] with
      | Error _ as error -> error
      | Ok () ->
          let rec rows values =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW -> (match meal_of_row statement with Some meal -> rows (meal :: values) | None -> Error (storage_error ()))
            | Sqlite3.Rc.DONE -> Ok (List.rev values)
            | _ -> Error (storage_error ())
          in rows [])

let delete_meal db ~user id =
  let current = timestamp (now ()) in
  with_statement db
    "UPDATE meals SET deleted_at = COALESCE(deleted_at, ?), updated_at = ? WHERE id = ? AND user_id = ? AND deleted_at IS NULL"
    (fun statement ->
      match bind statement [ Sqlite3.Data.TEXT current; Sqlite3.Data.TEXT current; Sqlite3.Data.TEXT (Meal_id.to_string id); Sqlite3.Data.TEXT (User_id.to_string user.User.id) ] with
      | Error _ as error -> error
      | Ok () ->
          if Sqlite3.step statement <> Sqlite3.Rc.DONE then Error (storage_error ())
          else if Sqlite3.changes db = 1 then Ok ()
          else
            with_statement db "SELECT 1 FROM meals WHERE id = ? AND user_id = ? AND deleted_at IS NOT NULL"
              (fun tombstone ->
                match bind tombstone [ Sqlite3.Data.TEXT (Meal_id.to_string id); Sqlite3.Data.TEXT (User_id.to_string user.User.id) ] with
                | Error _ as error -> error
                | Ok () -> if Sqlite3.step tombstone = Sqlite3.Rc.ROW then Ok () else Error Error.Not_found))


let weigh_in_columns =
  "id, user_id, measured_at, weight_kg, source, external_id, created_at, updated_at, deleted_at"

let weigh_in_of_row statement =
  match
    ( Weigh_in_id.of_string (text statement 0), User_id.of_string (text statement 1),
      parse_time (text statement 2), parse_time (text statement 6), parse_time (text statement 7) )
  with
  | Some id, Some user_id, Some measured_at, Some created_at, Some updated_at ->
      let source = match text statement 4 with "manual" -> Some Weigh_in.Manual | "withings" -> Some Weigh_in.Withings | _ -> None in
      (match source with
      | Some source -> Some Weigh_in.{ id; user_id; measured_at; weight_kg = float statement 3; source;
                                       external_id = optional_text statement 5; created_at; updated_at;
                                       deleted_at = Option.bind (optional_text statement 8) parse_time }
      | None -> None)
  | _ -> None

let create_manual_weigh_in db ~user (input : Weigh_in.manual_create) =
  match Weigh_in.validate_manual_create input.measured_at input.weight_kg with
  | Error _ as error -> error
  | Ok () ->
      let current_time = now () in
      let measured_at = timestamp (Option.value input.measured_at ~default:current_time) in
      let current = timestamp current_time in
      with_statement db
        ("INSERT INTO weigh_ins (" ^ weigh_in_columns ^ ") VALUES (?, ?, ?, ?, 'manual', NULL, ?, ?, NULL) RETURNING " ^ weigh_in_columns)
        (fun statement ->
          match bind statement [ Sqlite3.Data.TEXT (Weigh_in_id.to_string (Weigh_in_id.fresh ())); Sqlite3.Data.TEXT (User_id.to_string user.User.id);
                                 Sqlite3.Data.TEXT measured_at; Sqlite3.Data.FLOAT input.weight_kg; Sqlite3.Data.TEXT current; Sqlite3.Data.TEXT current ] with
          | Error _ as error -> error
          | Ok () -> if Sqlite3.step statement = Sqlite3.Rc.ROW then match weigh_in_of_row statement with Some weight -> Ok weight | None -> Error (storage_error ()) else Error (storage_error ()))

let get_weigh_in db ~user id =
  with_statement db ("SELECT " ^ weigh_in_columns ^ " FROM weigh_ins WHERE id = ? AND user_id = ? AND deleted_at IS NULL")
    (fun statement ->
      match bind statement [ Sqlite3.Data.TEXT (Weigh_in_id.to_string id); Sqlite3.Data.TEXT (User_id.to_string user.User.id) ] with
      | Error _ as error -> error
      | Ok () -> if Sqlite3.step statement = Sqlite3.Rc.ROW then match weigh_in_of_row statement with Some weight -> Ok weight | None -> Error (storage_error ()) else Error Error.Not_found)

let valid_weigh_in_patch (patch : Weigh_in.patch) =
  match patch.weight_kg with None -> true | Some weight -> weight > 0.0

let update_manual_weigh_in db ~user id patch =
  if not (valid_weigh_in_patch patch) then Error (invalid_input ()) else
  let current = timestamp (now ()) in
  with_statement db
    ("UPDATE weigh_ins SET measured_at = COALESCE(?, measured_at), weight_kg = COALESCE(?, weight_kg), updated_at = ? WHERE id = ? AND user_id = ? AND deleted_at IS NULL AND source = 'manual' RETURNING " ^ weigh_in_columns)
    (fun statement ->
      match bind statement [ nullable (Option.map timestamp patch.measured_at); nullable_float patch.weight_kg; Sqlite3.Data.TEXT current;
                             Sqlite3.Data.TEXT (Weigh_in_id.to_string id); Sqlite3.Data.TEXT (User_id.to_string user.User.id) ] with
      | Error _ as error -> error
      | Ok () -> if Sqlite3.step statement = Sqlite3.Rc.ROW then match weigh_in_of_row statement with Some weight -> Ok weight | None -> Error (storage_error ()) else Error Error.Not_found)

let query_weigh_ins db ~user ~from ~to_ ~limit =
  if not (valid_query ~from ~to_ ~limit) then Error (invalid_input ()) else
  with_statement db
    ("SELECT " ^ weigh_in_columns ^ " FROM weigh_ins WHERE user_id = ? AND deleted_at IS NULL AND (? IS NULL OR measured_at >= ?) AND (? IS NULL OR measured_at <= ?) ORDER BY measured_at ASC LIMIT ?")
    (fun statement ->
      let from_value = nullable (Option.map timestamp from) and to_value = nullable (Option.map timestamp to_) in
      match bind statement [ Sqlite3.Data.TEXT (User_id.to_string user.User.id); from_value; from_value; to_value; to_value; Sqlite3.Data.INT (Int64.of_int limit) ] with
      | Error _ as error -> error
      | Ok () ->
          let rec rows values =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW -> (match weigh_in_of_row statement with Some weight -> rows (weight :: values) | None -> Error (storage_error ()))
            | Sqlite3.Rc.DONE -> Ok (List.rev values)
            | _ -> Error (storage_error ())
          in rows [])

let delete_weigh_in db ~user id =
  let current = timestamp (now ()) in
  with_statement db
    "UPDATE weigh_ins SET deleted_at = COALESCE(deleted_at, ?), updated_at = ? WHERE id = ? AND user_id = ? AND deleted_at IS NULL"
    (fun statement ->
      match bind statement [ Sqlite3.Data.TEXT current; Sqlite3.Data.TEXT current; Sqlite3.Data.TEXT (Weigh_in_id.to_string id); Sqlite3.Data.TEXT (User_id.to_string user.User.id) ] with
      | Error _ as error -> error
      | Ok () ->
          if Sqlite3.step statement <> Sqlite3.Rc.DONE then Error (storage_error ())
          else if Sqlite3.changes db = 1 then Ok ()
          else
            with_statement db "SELECT 1 FROM weigh_ins WHERE id = ? AND user_id = ? AND deleted_at IS NOT NULL"
              (fun tombstone ->
                match bind tombstone [ Sqlite3.Data.TEXT (Weigh_in_id.to_string id); Sqlite3.Data.TEXT (User_id.to_string user.User.id) ] with
                | Error _ as error -> error
                | Ok () -> if Sqlite3.step tombstone = Sqlite3.Rc.ROW then Ok () else Error Error.Not_found))
