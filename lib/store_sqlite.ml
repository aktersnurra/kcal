module Domain_error = Error
open Base
module Error = Domain_error

type t = Sqlite3.db

let close db =
  try
    if Sqlite3.db_close db then Ok ()
    else Error (Error.Storage_error "SQLite close failed")
  with Sqlite3.Error _ | Sqlite3.SqliteError _ ->
    Error (Error.Storage_error "SQLite close failed")

let open_database ~path =
  try
    let db = Sqlite3.db_open path in
    let initialize () =
      if
        not
          (match Sqlite3.exec db "PRAGMA foreign_keys = ON" with
          | Sqlite3.Rc.OK -> true
          | _ -> false)
      then Error (Error.Storage_error "SQLite initialization failed")
      else Result.map ~f:(fun () -> db) (Migration.apply_all db)
    in
    match initialize () with
    | Ok _ as result -> result
    | Error _ as error ->
        ignore (close db);
        error
    | exception exn ->
        ignore (close db);
        raise exn
  with Sqlite3.Error _ | Sqlite3.SqliteError _ ->
    Error (Error.Storage_error "SQLite initialization failed")

module For_testing = struct
  let raw_database db = db
end

let log_src = Logs.Src.create "kcal.store" ~doc:"SQLite persistence"

module Log = (val Logs.src_log log_src : Logs.LOG)

let storage_error () = Error.Storage_error "SQLite operation failed"
let invalid_input () = Error.Invalid_input "invalid query or update"
let now () = Option.value_exn (Ptime.of_float_s (Unix.gettimeofday ()))
let timestamp time = Time.to_utc_string time

let nullable = function
  | None -> Sqlite3.Data.NULL
  | Some value -> Sqlite3.Data.TEXT value

let nullable_float = function
  | None -> Sqlite3.Data.NULL
  | Some value -> Sqlite3.Data.FLOAT value

(* [sql] is only ever a literal query defined in this module, never
   user-controlled, so logging it carries no injection or PII risk. *)
let with_statement db sql f =
  try
    let statement = Sqlite3.prepare db sql in
    Stdlib.Fun.protect
      ~finally:(fun () ->
        try ignore (Sqlite3.finalize statement)
        with Sqlite3.Error _ | Sqlite3.SqliteError _ -> ())
      (fun () ->
        try f statement
        with
        | ( Sqlite3.Error _ | Sqlite3.SqliteError _ | Sqlite3.RangeError _
          | Sqlite3.DataTypeError _ | Sqlite3.InternalError _ ) as exn
        ->
          Log.err (fun m -> m "query %S raised %s" sql (Exn.to_string exn));
          Error (storage_error ()))
  with
  | ( Sqlite3.Error _ | Sqlite3.SqliteError _ | Sqlite3.RangeError _
    | Sqlite3.DataTypeError _ | Sqlite3.InternalError _ ) as exn
  ->
    Log.err (fun m -> m "preparing query %S raised %s" sql (Exn.to_string exn));
    Error (storage_error ())

let bind statement values =
  if
    match Sqlite3.bind_values statement values with
    | Sqlite3.Rc.OK -> true
    | _ -> false
  then Ok ()
  else (
    Log.err (fun m -> m "binding statement parameters failed");
    Error (storage_error ()))

let row_or_not_found statement decode =
  match Sqlite3.step statement with
  | Sqlite3.Rc.ROW -> (
      match decode statement with
      | Some value -> Ok value
      | None -> Error (Error.Storage_decoding "row"))
  | Sqlite3.Rc.DONE -> Error Error.Not_found
  | _ -> Error (storage_error ())

let tombstone_or_not_found statement =
  match Sqlite3.step statement with
  | Sqlite3.Rc.ROW -> Ok ()
  | Sqlite3.Rc.DONE -> Error Error.Not_found
  | _ -> Error (storage_error ())

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

module Withings_connection_id = Id.Make ()

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

(* The read must share the write transaction: invalid durable numeric values
   are reported, never repaired as a side effect of an ordinary update. *)
let with_checked_numeric_update db ~check ~update =
  try
    match Sqlite3.exec db "BEGIN IMMEDIATE" with
    | Sqlite3.Rc.OK ->
        let committed = ref false in
        Stdlib.Fun.protect
          ~finally:(fun () ->
            if not !committed then ignore (Sqlite3.exec db "ROLLBACK"))
          (fun () ->
            match check () with
            | Error _ as error -> error
            | Ok () -> (
                match update () with
                | Error _ as error -> error
                | Ok value -> (
                    match Sqlite3.exec db "COMMIT" with
                    | Sqlite3.Rc.OK ->
                        committed := true;
                        Ok value
                    | _ -> Error (storage_error ()))))
    | _ -> Error (storage_error ())
  with Sqlite3.Error _ | Sqlite3.SqliteError _ -> Error (storage_error ())

let user_of_row statement =
  match
    (User_id.of_string (text statement 0), parse_time (text statement 3))
  with
  | Some id, Some created_at ->
      Some
        User.
          {
            id;
            oidc_issuer = text statement 1;
            oidc_subject = text statement 2;
            created_at;
          }
  | _ -> None

let meal_columns =
  "id, user_id, eaten_at, description, calories_kcal, protein_g, carbs_g, \
   fat_g, confidence, estimate_source, notes, created_at, updated_at, \
   deleted_at"

let meal_of_row statement =
  match
    ( Meal_id.of_string (text statement 0),
      User_id.of_string (text statement 1),
      parse_time (text statement 2),
      parse_time (text statement 11),
      parse_time (text statement 12) )
  with
  | Some id, Some user_id, Some eaten_at, Some created_at, Some updated_at
    when calorie_column statement 4 && numeric_column statement 5
         && List.for_all [ 6; 7; 8 ] ~f:(optional_numeric_column statement) ->
      Result.ok
        (Meal.of_storage ~id ~user_id ~eaten_at ~description:(text statement 3)
           ~calories_kcal:(int statement 4) ~protein_g:(float statement 5)
           ~carbs_g:(optional_float statement 6)
           ~fat_g:(optional_float statement 7)
           ~confidence:(optional_float statement 8)
           ~estimate_source:(optional_text statement 9)
           ~notes:(optional_text statement 10)
           ~created_at ~updated_at
           ~deleted_at:(Option.bind (optional_text statement 13) ~f:parse_time))
  | _ -> None

let resolve_user db ~issuer ~subject =
  let created_at = timestamp (now ()) in
  with_statement db
    "INSERT INTO users (id, oidc_issuer, oidc_subject, created_at) VALUES (?, \
     ?, ?, ?) ON CONFLICT(oidc_issuer, oidc_subject) DO UPDATE SET oidc_issuer \
     = excluded.oidc_issuer RETURNING id, oidc_issuer, oidc_subject, \
     created_at" (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT (User_id.to_string (User_id.fresh ()));
            Sqlite3.Data.TEXT issuer;
            Sqlite3.Data.TEXT subject;
            Sqlite3.Data.TEXT created_at;
          ]
      with
      | Error _ as error -> error
      | Ok () ->
          if
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW -> true
            | _ -> false
          then
            match user_of_row statement with
            | Some user -> Ok user
            | None -> Error (storage_error ())
          else Error (storage_error ()))

let create_meal db ~user input =
  let input = Meal.Create.to_raw input in
  let current = now () in
  let eaten_at = Option.value input.eaten_at ~default:current |> timestamp in
  let current = timestamp current in
  with_statement db
    ("INSERT INTO meals (" ^ meal_columns
   ^ ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL) RETURNING "
   ^ meal_columns)
    (fun statement ->
      let values =
        [
          Sqlite3.Data.TEXT (Meal_id.to_string (Meal_id.fresh ()));
          Sqlite3.Data.TEXT (User_id.to_string user.User.id);
          Sqlite3.Data.TEXT eaten_at;
          Sqlite3.Data.TEXT input.description;
          Sqlite3.Data.INT (Int64.of_int input.calories_kcal);
          Sqlite3.Data.FLOAT input.protein_g;
          nullable_float input.carbs_g;
          nullable_float input.fat_g;
          nullable_float input.confidence;
          nullable input.estimate_source;
          nullable input.notes;
          Sqlite3.Data.TEXT current;
          Sqlite3.Data.TEXT current;
        ]
      in
      match bind statement values with
      | Error _ as error -> error
      | Ok () ->
          if
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW -> true
            | _ -> false
          then
            match meal_of_row statement with
            | Some meal -> Ok meal
            | None -> Error (Error.Storage_decoding "row")
          else Error (storage_error ()))

let get_meal db ~user id =
  with_statement db
    ("SELECT " ^ meal_columns
   ^ " FROM meals WHERE id = ? AND user_id = ? AND deleted_at IS NULL")
    (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT (Meal_id.to_string id);
            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
          ]
      with
      | Error _ as error -> error
      | Ok () -> row_or_not_found statement meal_of_row)

let update_meal db ~user id patch =
  let patch = Meal.Patch.to_raw patch in
  with_checked_numeric_update db
    ~check:(fun () -> Result.map ~f:(fun _ -> ()) (get_meal db ~user id))
    ~update:(fun () ->
      let updated_at = timestamp (now ()) in
      with_statement db
        ("UPDATE meals SET eaten_at = COALESCE(?, eaten_at), description = \
          COALESCE(?, description), calories_kcal = COALESCE(?, \
          calories_kcal), protein_g = COALESCE(?, protein_g), carbs_g = \
          COALESCE(?, carbs_g), fat_g = COALESCE(?, fat_g), confidence = \
          COALESCE(?, confidence), estimate_source = COALESCE(?, \
          estimate_source), notes = COALESCE(?, notes), updated_at = ? WHERE \
          id = ? AND user_id = ? AND deleted_at IS NULL RETURNING "
       ^ meal_columns) (fun statement ->
          let values =
            [
              nullable (Option.map ~f:timestamp patch.eaten_at);
              nullable patch.description;
              (match patch.calories_kcal with
              | None -> Sqlite3.Data.NULL
              | Some value -> Sqlite3.Data.INT (Int64.of_int value));
              nullable_float patch.protein_g;
              nullable_float patch.carbs_g;
              nullable_float patch.fat_g;
              nullable_float patch.confidence;
              nullable patch.estimate_source;
              nullable patch.notes;
              Sqlite3.Data.TEXT updated_at;
              Sqlite3.Data.TEXT (Meal_id.to_string id);
              Sqlite3.Data.TEXT (User_id.to_string user.User.id);
            ]
          in
          match bind statement values with
          | Error _ as error -> error
          | Ok () -> row_or_not_found statement meal_of_row))

let is_valid_query ~from ~to_ ~limit =
  limit >= 1 && limit <= 500
  &&
  match (from, to_) with
  | Some lower, Some upper -> Ptime.compare lower upper <= 0
  | _ -> true

let decode_all_rows statement decode =
  let rec rows values =
    match Sqlite3.step statement with
    | Sqlite3.Rc.ROW -> (
        match decode statement with
        | Some value -> rows (value :: values)
        | None -> Error (Error.Storage_decoding "row"))
    | Sqlite3.Rc.DONE -> Ok (List.rev values)
    | _ -> Error (storage_error ())
  in
  rows []

let product_columns =
  "id, user_id, name, basis, calories_kcal, protein_g, carbs_g, fat_g, source, \
   created_at, updated_at"

let product_of_row statement =
  match
    ( Product_id.of_string (text statement 0),
      User_id.of_string (text statement 1),
      parse_time (text statement 9),
      parse_time (text statement 10) )
  with
  | Some id, Some user_id, Some created_at, Some updated_at
    when calorie_column statement 4 && numeric_column statement 5
         && List.for_all [ 6; 7 ] ~f:(optional_numeric_column statement) ->
      Result.ok
        (Product.of_storage ~id ~user_id ~name:(text statement 2)
           ~basis:(text statement 3) ~calories_kcal:(int statement 4)
           ~protein_g:(float statement 5)
           ~carbs_g:(optional_float statement 6)
           ~fat_g:(optional_float statement 7)
           ~source:(text statement 8) ~created_at ~updated_at)
  | _ -> None

let save_product db ~user ~id input =
  let input = Product.Create.to_raw input in
  let persist () =
    let current = Sqlite3.Data.TEXT (timestamp (now ())) in
    let owner = Sqlite3.Data.TEXT (User_id.to_string user.User.id) in
    let fields =
      [
        Sqlite3.Data.TEXT input.name;
        Sqlite3.Data.TEXT input.basis;
        Sqlite3.Data.INT (Int64.of_int input.calories_kcal);
        Sqlite3.Data.FLOAT input.protein_g;
        nullable_float input.carbs_g;
        nullable_float input.fat_g;
        Sqlite3.Data.TEXT input.source;
      ]
    in
    let sql, values =
      match id with
      | None ->
          ( "INSERT INTO products (" ^ product_columns
            ^ ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING "
            ^ product_columns,
            [
              Sqlite3.Data.TEXT (Product_id.to_string (Product_id.fresh ()));
              owner;
            ]
            @ fields @ [ current; current ] )
      | Some id ->
          ( "UPDATE products SET name = ?, basis = ?, calories_kcal = ?, \
             protein_g = ?, carbs_g = ?, fat_g = ?, source = ?, updated_at = ? \
             WHERE id = ? AND user_id = ? RETURNING " ^ product_columns,
            fields
            @ [ current; Sqlite3.Data.TEXT (Product_id.to_string id); owner ] )
    in
    with_statement db sql (fun statement ->
        match bind statement values with
        | Error _ as error -> error
        | Ok () -> row_or_not_found statement product_of_row)
  in
  match id with
  | None -> persist ()
  | Some id ->
      with_checked_numeric_update db
        ~check:(fun () ->
          Result.map
            ~f:(fun _ -> ())
            (with_statement db
               ("SELECT " ^ product_columns
              ^ " FROM products WHERE id = ? AND user_id = ?")
               (fun statement ->
                 match
                   bind statement
                     [
                       Sqlite3.Data.TEXT (Product_id.to_string id);
                       Sqlite3.Data.TEXT (User_id.to_string user.User.id);
                     ]
                 with
                 | Error _ as error -> error
                 | Ok () -> row_or_not_found statement product_of_row)))
        ~update:persist

let qualified_columns table columns =
  String.split columns ~on:','
  |> List.map ~f:(fun column -> table ^ "." ^ String.strip column)
  |> String.concat ~sep:", "

let search_products db ~user ~query ~limit =
  if limit < 1 || limit > 500 then Error (invalid_input ())
  else
    with_statement db
      ("SELECT "
      ^ qualified_columns "products" product_columns
      ^ " FROM products JOIN products_fts ON products.rowid = \
         products_fts.rowid WHERE products.user_id = ? AND products_fts MATCH \
         ? ORDER BY bm25(products_fts), products.name, products.id LIMIT ?")
      (fun statement ->
        match
          bind statement
            [
              Sqlite3.Data.TEXT (User_id.to_string user.User.id);
              Sqlite3.Data.TEXT (Search_query.fts5_match_expression query);
              Sqlite3.Data.INT (Int64.of_int limit);
            ]
        with
        | Error _ as error -> error
        | Ok () -> decode_all_rows statement product_of_row)

let query_meals ?search db ~user ~from ~to_ ~limit =
  if not (is_valid_query ~from ~to_ ~limit) then Error (invalid_input ())
  else
    let join, search_clause, search_values, ordering =
      match search with
      | None -> ("", "", [], "meals.eaten_at ASC, meals.id ASC")
      | Some query ->
          ( " JOIN meals_fts ON meals.rowid = meals_fts.rowid",
            " AND meals_fts MATCH ?",
            [ Sqlite3.Data.TEXT (Search_query.fts5_match_expression query) ],
            "bm25(meals_fts), meals.eaten_at ASC, meals.id ASC" )
    in
    with_statement db
      ("SELECT "
      ^ qualified_columns "meals" meal_columns
      ^ " FROM meals" ^ join
      ^ " WHERE meals.user_id = ? AND meals.deleted_at IS NULL" ^ search_clause
      ^ " AND (? IS NULL OR meals.eaten_at >= ?) AND (? IS NULL OR \
         meals.eaten_at <= ?) ORDER BY " ^ ordering ^ " LIMIT ?")
      (fun statement ->
        let from_value = nullable (Option.map ~f:timestamp from)
        and to_value = nullable (Option.map ~f:timestamp to_) in
        match
          bind statement
            ([ Sqlite3.Data.TEXT (User_id.to_string user.User.id) ]
            @ search_values
            @ [
                from_value;
                from_value;
                to_value;
                to_value;
                Sqlite3.Data.INT (Int64.of_int limit);
              ])
        with
        | Error _ as error -> error
        | Ok () -> decode_all_rows statement meal_of_row)

(* Every meal in [start, stop), unpaginated, for server-side aggregation. *)
let meals_between db ~user ~start ~stop =
  with_statement db
    ("SELECT " ^ meal_columns
   ^ " FROM meals WHERE user_id = ? AND deleted_at IS NULL AND eaten_at >= ? \
      AND eaten_at < ? ORDER BY eaten_at ASC")
    (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
            Sqlite3.Data.TEXT (timestamp start);
            Sqlite3.Data.TEXT (timestamp stop);
          ]
      with
      | Error _ as error -> error
      | Ok () -> decode_all_rows statement meal_of_row)

let delete_meal db ~user id =
  let current = timestamp (now ()) in
  with_statement db
    "UPDATE meals SET deleted_at = COALESCE(deleted_at, ?), updated_at = ? \
     WHERE id = ? AND user_id = ? AND deleted_at IS NULL" (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT current;
            Sqlite3.Data.TEXT current;
            Sqlite3.Data.TEXT (Meal_id.to_string id);
            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
          ]
      with
      | Error _ as error -> error
      | Ok () -> (
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE ->
              if Sqlite3.changes db = 1 then Ok ()
              else
                with_statement db
                  "SELECT 1 FROM meals WHERE id = ? AND user_id = ? AND \
                   deleted_at IS NOT NULL" (fun tombstone ->
                    match
                      bind tombstone
                        [
                          Sqlite3.Data.TEXT (Meal_id.to_string id);
                          Sqlite3.Data.TEXT (User_id.to_string user.User.id);
                        ]
                    with
                    | Error _ as error -> error
                    | Ok () -> tombstone_or_not_found tombstone)
          | _ -> Error (storage_error ())))

let weigh_in_columns =
  "id, user_id, measured_at, weight_kg, source, external_id, created_at, \
   updated_at, deleted_at"

let weigh_in_of_row statement =
  match
    ( Weigh_in_id.of_string (text statement 0),
      User_id.of_string (text statement 1),
      parse_time (text statement 2),
      parse_time (text statement 6),
      parse_time (text statement 7) )
  with
  | Some id, Some user_id, Some measured_at, Some created_at, Some updated_at
    when numeric_column statement 3 -> (
      let source =
        match text statement 4 with
        | "manual" -> Some Weigh_in.Manual
        | "withings" ->
            Option.bind (optional_text statement 5) ~f:(fun external_id ->
                Result.ok
                  (Result.map
                     ~f:(fun identity -> Weigh_in.Withings identity)
                     (Weigh_in.Imported_identity.of_external_id external_id)))
        | _ -> None
      in
      match source with
      | Some source ->
          Result.ok
            (Weigh_in.of_storage ~id ~user_id ~measured_at
               ~weight_kg:(float statement 3) ~source ~created_at ~updated_at
               ~deleted_at:
                 (Option.bind (optional_text statement 8) ~f:parse_time))
      | None -> None)
  | _ -> None

let create_manual_weigh_in db ~user input =
  let input = Weigh_in.Create.to_raw input in
  let current_time = now () in
  let measured_at =
    timestamp (Option.value input.measured_at ~default:current_time)
  in
  let current = timestamp current_time in
  with_statement db
    ("INSERT INTO weigh_ins (" ^ weigh_in_columns
   ^ ") VALUES (?, ?, ?, ?, 'manual', NULL, ?, ?, NULL) RETURNING "
   ^ weigh_in_columns)
    (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT (Weigh_in_id.to_string (Weigh_in_id.fresh ()));
            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
            Sqlite3.Data.TEXT measured_at;
            Sqlite3.Data.FLOAT input.weight_kg;
            Sqlite3.Data.TEXT current;
            Sqlite3.Data.TEXT current;
          ]
      with
      | Error _ as error -> error
      | Ok () ->
          if
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW -> true
            | _ -> false
          then
            match weigh_in_of_row statement with
            | Some weight -> Ok weight
            | None -> Error (Error.Storage_decoding "row")
          else Error (storage_error ()))

let get_weigh_in db ~user id =
  with_statement db
    ("SELECT " ^ weigh_in_columns
   ^ " FROM weigh_ins WHERE id = ? AND user_id = ? AND deleted_at IS NULL")
    (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT (Weigh_in_id.to_string id);
            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
          ]
      with
      | Error _ as error -> error
      | Ok () -> row_or_not_found statement weigh_in_of_row)

let update_manual_weigh_in db ~user id patch =
  let patch = Weigh_in.Patch.to_raw patch in
  with_checked_numeric_update db
    ~check:(fun () -> Result.map ~f:(fun _ -> ()) (get_weigh_in db ~user id))
    ~update:(fun () ->
      let current = timestamp (now ()) in
      with_statement db
        ("UPDATE weigh_ins SET measured_at = COALESCE(?, measured_at), \
          weight_kg = COALESCE(?, weight_kg), updated_at = ? WHERE id = ? AND \
          user_id = ? AND deleted_at IS NULL AND source = 'manual' RETURNING "
       ^ weigh_in_columns) (fun statement ->
          match
            bind statement
              [
                nullable (Option.map ~f:timestamp patch.measured_at);
                nullable_float patch.weight_kg;
                Sqlite3.Data.TEXT current;
                Sqlite3.Data.TEXT (Weigh_in_id.to_string id);
                Sqlite3.Data.TEXT (User_id.to_string user.User.id);
              ]
          with
          | Error _ as error -> error
          | Ok () -> row_or_not_found statement weigh_in_of_row))

let query_weigh_ins db ~user ~from ~to_ ~limit =
  if not (is_valid_query ~from ~to_ ~limit) then Error (invalid_input ())
  else
    with_statement db
      ("SELECT " ^ weigh_in_columns
     ^ " FROM weigh_ins WHERE user_id = ? AND deleted_at IS NULL AND (? IS \
        NULL OR measured_at >= ?) AND (? IS NULL OR measured_at <= ?) ORDER BY \
        measured_at ASC LIMIT ?")
      (fun statement ->
        let from_value = nullable (Option.map ~f:timestamp from)
        and to_value = nullable (Option.map ~f:timestamp to_) in
        match
          bind statement
            [
              Sqlite3.Data.TEXT (User_id.to_string user.User.id);
              from_value;
              from_value;
              to_value;
              to_value;
              Sqlite3.Data.INT (Int64.of_int limit);
            ]
        with
        | Error _ as error -> error
        | Ok () -> decode_all_rows statement weigh_in_of_row)

(* Every weigh-in in [start, stop), unpaginated, for server-side aggregation. *)
let weigh_ins_between db ~user ~start ~stop =
  with_statement db
    ("SELECT " ^ weigh_in_columns
   ^ " FROM weigh_ins WHERE user_id = ? AND deleted_at IS NULL AND measured_at \
      >= ? AND measured_at < ? ORDER BY measured_at ASC")
    (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
            Sqlite3.Data.TEXT (timestamp start);
            Sqlite3.Data.TEXT (timestamp stop);
          ]
      with
      | Error _ as error -> error
      | Ok () -> decode_all_rows statement weigh_in_of_row)

let latest_weigh_in db ~user =
  with_statement db
    ("SELECT " ^ weigh_in_columns
   ^ " FROM weigh_ins WHERE user_id = ? AND deleted_at IS NULL ORDER BY \
      measured_at DESC LIMIT 1")
    (fun statement ->
      match
        bind statement [ Sqlite3.Data.TEXT (User_id.to_string user.User.id) ]
      with
      | Error _ as error -> error
      | Ok () -> row_or_not_found statement weigh_in_of_row)

let withings_connection_of_row statement =
  match
    ( User_id.of_string (text statement 1),
      optional_text statement 2,
      Option.bind (optional_text statement 3) ~f:parse_time )
  with
  | Some user_id, Some withings_user_id, Some token_expires_at ->
      Result.ok
        (Withings_connection.decode ~id:(text statement 0) ~user_id
           ~withings_user_id ~token_expires_at
           ~sync_cursor:(optional_int64 statement 4))
  | _ -> None

let create_withings_oauth_state db ~user ~state_hash ~expires_at =
  let created_at = timestamp (now ()) in
  with_statement db
    "INSERT INTO withings_oauth_states (state_hash, user_id, expires_at, \
     consumed_at, created_at) VALUES (?, ?, ?, NULL, ?)" (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT state_hash;
            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
            Sqlite3.Data.TEXT (timestamp expires_at);
            Sqlite3.Data.TEXT created_at;
          ]
      with
      | Error _ as error -> error
      | Ok () ->
          if
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> true
            | _ -> false
          then Ok ()
          else Error (storage_error ()))

let consume_withings_oauth_state db ~user ~state_hash ~now:current_time =
  with_statement db
    "UPDATE withings_oauth_states SET consumed_at = ? WHERE state_hash = ? AND \
     user_id = ? AND consumed_at IS NULL AND expires_at > ?" (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT (timestamp current_time);
            Sqlite3.Data.TEXT state_hash;
            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
            Sqlite3.Data.TEXT (timestamp current_time);
          ]
      with
      | Error _ as error -> error
      | Ok () ->
          if
            (match Sqlite3.step statement with
              | Sqlite3.Rc.DONE -> true
              | _ -> false)
            && Sqlite3.changes db = 1
          then Ok ()
          else Error Error.Not_found)

let consume_withings_oauth_state_for_callback db ~state_hash ~now:current_time =
  with_statement db
    "UPDATE withings_oauth_states SET consumed_at = ? WHERE state_hash = ? AND \
     consumed_at IS NULL AND expires_at > ? RETURNING user_id" (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT (timestamp current_time);
            Sqlite3.Data.TEXT state_hash;
            Sqlite3.Data.TEXT (timestamp current_time);
          ]
      with
      | Error _ as error -> error
      | Ok () -> (
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW ->
              let user_id = text statement 0 in
              with_statement db
                "SELECT id, oidc_issuer, oidc_subject, created_at FROM users \
                 WHERE id = ?" (fun user ->
                  match bind user [ Sqlite3.Data.TEXT user_id ] with
                  | Error _ as error -> error
                  | Ok () -> row_or_not_found user user_of_row)
          | Sqlite3.Rc.DONE -> Error Error.Not_found
          | _ -> Error (storage_error ())))

let get_withings_status db ~user =
  with_statement db
    "SELECT id, user_id, withings_user_id, token_expires_at, sync_cursor, \
     requires_reauthorization, access_token_encrypted, refresh_token_encrypted \
     FROM withings_connections WHERE user_id = ?" (fun statement ->
      match
        bind statement [ Sqlite3.Data.TEXT (User_id.to_string user.User.id) ]
      with
      | Error _ as error -> error
      | Ok () -> (
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> Ok Withings_connection.Disconnected
          | Sqlite3.Rc.ROW -> (
              match withings_connection_of_row statement with
              | Some connection
                when not
                       (Sqlite3.column_is_null statement 6
                       || Sqlite3.column_is_null statement 7) ->
                  Ok
                    (if int statement 5 = 0 then
                       Withings_connection.Connected connection
                     else Reauthorization_required connection)
              | _ -> Error (Error.Storage_decoding "Withings connection"))
          | _ -> Error (storage_error ())))

let delete_withings_connection db ~user =
  with_statement db "DELETE FROM withings_connections WHERE user_id = ?"
    (fun statement ->
      match
        bind statement [ Sqlite3.Data.TEXT (User_id.to_string user.User.id) ]
      with
      | Error _ as error -> error
      | Ok () ->
          if
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> true
            | _ -> false
          then Ok ()
          else Error (storage_error ()))

let withings_connection_for_identity db ~withings_user_id =
  with_statement db
    "SELECT c.id, c.user_id, c.withings_user_id, CASE WHEN \
     c.access_token_encrypted IS NULL OR c.refresh_token_encrypted IS NULL \
     THEN NULL ELSE c.token_expires_at END, c.sync_cursor, \
     c.requires_reauthorization, u.oidc_issuer, u.oidc_subject, u.created_at \
     FROM withings_connections c JOIN users u ON u.id = c.user_id WHERE \
     c.withings_user_id = ?" (fun statement ->
      match bind statement [ Sqlite3.Data.TEXT withings_user_id ] with
      | Error _ as error -> error
      | Ok () -> (
          let step = Sqlite3.step statement in
          let connection =
            match step with
            | Sqlite3.Rc.ROW -> withings_connection_of_row statement
            | _ -> None
          in
          match (step, connection) with
          | Sqlite3.Rc.ROW, Some connection -> (
              match
                ( User_id.of_string (text statement 1),
                  parse_time (text statement 8) )
              with
              | Some id, Some created_at ->
                  Ok
                    ( {
                        User.id;
                        oidc_issuer = text statement 6;
                        oidc_subject = text statement 7;
                        created_at;
                      },
                      connection )
              | _ -> Error (storage_error ()))
          | Sqlite3.Rc.DONE, _ -> Error Error.Not_found
          | _ -> Error (storage_error ())))

let withings_connections db =
  with_statement db
    "SELECT c.id, c.user_id, c.withings_user_id, CASE WHEN \
     c.access_token_encrypted IS NULL OR c.refresh_token_encrypted IS NULL \
     THEN NULL ELSE c.token_expires_at END, c.sync_cursor, \
     c.requires_reauthorization, u.oidc_issuer, u.oidc_subject, u.created_at \
     FROM withings_connections c JOIN users u ON u.id = c.user_id WHERE \
     c.access_token_encrypted IS NOT NULL AND c.refresh_token_encrypted IS NOT \
     NULL" (fun statement ->
      let rec rows values =
        match Sqlite3.step statement with
        | Sqlite3.Rc.ROW -> (
            match
              ( withings_connection_of_row statement,
                User_id.of_string (text statement 1),
                parse_time (text statement 8) )
            with
            | Some connection, Some id, Some created_at ->
                rows
                  (( {
                       User.id;
                       oidc_issuer = text statement 6;
                       oidc_subject = text statement 7;
                       created_at;
                     },
                     connection )
                  :: values)
            | _ -> Error (storage_error ()))
        | Sqlite3.Rc.DONE -> Ok (List.rev values)
        | _ -> Error (storage_error ())
      in
      rows [])

(* A single UPSERT is SQLite-atomic, including trigger failures. Encryption is
   complete before entering that implicit write transaction. *)
let replace_withings_connection db ~user ~key credentials =
  match
    ( Secret.encrypt ~key credentials.Withings.access_token,
      Secret.encrypt ~key credentials.refresh_token )
  with
  | Ok access, Ok refresh ->
      if String.length credentials.withings_user_id = 0 then
        Error (Error.Invalid_input "Withings account identity required")
      else
        let current = timestamp (now ()) in
        with_statement db
          "INSERT INTO withings_connections \
           (id,user_id,withings_user_id,access_token_encrypted,refresh_token_encrypted,token_expires_at,sync_cursor,requires_reauthorization,created_at,updated_at) \
           VALUES (?,?,?,?,?,?,NULL,0,?,?) ON CONFLICT(user_id) DO UPDATE SET \
           sync_cursor = CASE WHEN withings_connections.withings_user_id IS \
           NOT excluded.withings_user_id THEN NULL ELSE \
           withings_connections.sync_cursor END, \
           withings_user_id=excluded.withings_user_id, \
           access_token_encrypted=excluded.access_token_encrypted, \
           refresh_token_encrypted=excluded.refresh_token_encrypted, \
           token_expires_at=excluded.token_expires_at, \
           requires_reauthorization=0, updated_at=excluded.updated_at \
           RETURNING \
           id,user_id,withings_user_id,token_expires_at,sync_cursor,requires_reauthorization"
          (fun statement ->
            match
              bind statement
                [
                  Sqlite3.Data.TEXT
                    (Withings_connection_id.to_string
                       (Withings_connection_id.fresh ()));
                  Sqlite3.Data.TEXT (User_id.to_string user.User.id);
                  Sqlite3.Data.TEXT credentials.withings_user_id;
                  Sqlite3.Data.TEXT access;
                  Sqlite3.Data.TEXT refresh;
                  Sqlite3.Data.TEXT (timestamp credentials.expires_at);
                  Sqlite3.Data.TEXT current;
                  Sqlite3.Data.TEXT current;
                ]
            with
            | Error _ as error -> error
            | Ok () -> row_or_not_found statement withings_connection_of_row)
  | Error error, _ | _, Error error -> Error error

let refresh_withings_credentials db ~user ~connection ~key credentials =
  if
    not
      (String.equal credentials.Withings.withings_user_id
         (Withings_connection.Account.to_string
            connection.Withings_connection.account))
  then Error (Error.Withings_failure Malformed_response)
  else
    match
      ( Secret.encrypt ~key credentials.access_token,
        Secret.encrypt ~key credentials.refresh_token )
    with
    | Ok access, Ok refresh ->
        with_statement db
          "UPDATE withings_connections SET \
           access_token_encrypted=?,refresh_token_encrypted=?,token_expires_at=?,requires_reauthorization=0,updated_at=? \
           WHERE id=? AND user_id=? AND withings_user_id=? RETURNING \
           id,user_id,withings_user_id,token_expires_at,sync_cursor,requires_reauthorization"
          (fun statement ->
            match
              bind statement
                [
                  Sqlite3.Data.TEXT access;
                  Sqlite3.Data.TEXT refresh;
                  Sqlite3.Data.TEXT (timestamp credentials.expires_at);
                  Sqlite3.Data.TEXT (timestamp (now ()));
                  Sqlite3.Data.TEXT connection.id;
                  Sqlite3.Data.TEXT (User_id.to_string user.User.id);
                  Sqlite3.Data.TEXT
                    (Withings_connection.Account.to_string connection.account);
                ]
            with
            | Error _ as error -> error
            | Ok () -> row_or_not_found statement withings_connection_of_row)
    | Error error, _ | _, Error error -> Error error

let withings_credentials db ~user ~connection ~key =
  with_statement db
    "SELECT access_token_encrypted, refresh_token_encrypted, token_expires_at, \
     withings_user_id FROM withings_connections WHERE id = ? AND user_id = ? \
     AND requires_reauthorization = 0 AND withings_user_id = ?"
    (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT connection.Withings_connection.id;
            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
            Sqlite3.Data.TEXT
              (Withings_connection.Account.to_string connection.account);
          ]
      with
      | Error _ as error -> error
      | Ok () -> (
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW -> (
              match
                ( optional_text statement 0,
                  optional_text statement 1,
                  Option.bind (optional_text statement 2) ~f:parse_time,
                  optional_text statement 3 )
              with
              | ( Some encrypted_access,
                  Some encrypted_refresh,
                  Some expires_at,
                  Some withings_user_id ) -> (
                  match
                    ( Secret.decrypt ~key encrypted_access,
                      Secret.decrypt ~key encrypted_refresh )
                  with
                  | Ok access_token, Ok refresh_token ->
                      Ok
                        Withings.
                          {
                            access_token;
                            refresh_token;
                            expires_at;
                            withings_user_id;
                          }
                  | (Error _ as error), _ -> error
                  | _, (Error _ as error) -> error)
              | _ -> Error (Error.Storage_decoding "Withings credentials"))
          | Sqlite3.Rc.DONE -> Error Error.Not_found
          | _ -> Error (storage_error ())))

let mark_withings_reauthorization db ~user ~connection =
  with_statement db
    "UPDATE withings_connections SET requires_reauthorization = 1, updated_at \
     = ? WHERE id = ? AND user_id = ? AND withings_user_id = ?"
    (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT (timestamp (now ()));
            Sqlite3.Data.TEXT connection.Withings_connection.id;
            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
            Sqlite3.Data.TEXT
              (Withings_connection.Account.to_string connection.account);
          ]
      with
      | Error _ as error -> error
      | Ok () ->
          if
            (match Sqlite3.step statement with
              | Sqlite3.Rc.DONE -> true
              | _ -> false)
            && Sqlite3.changes db = 1
          then Ok ()
          else Error Error.Not_found)

let persist_withings_import db ~user ~connection ~rows ~expected_cursor ~cursor
    =
  let rows = List.map ~f:Weigh_in.Import.to_raw rows in
  if
    not
      (match Sqlite3.exec db "BEGIN IMMEDIATE" with
      | Sqlite3.Rc.OK -> true
      | _ -> false)
  then Error (storage_error ())
  else
    let rollback () = ignore (Sqlite3.exec db "ROLLBACK") in
    let committed = ref false in
    Stdlib.Fun.protect
      ~finally:(fun () -> if not !committed then rollback ())
      (fun () ->
        let result =
          try
            let stale =
              with_statement db
                "SELECT sync_cursor, withings_user_id FROM \
                 withings_connections WHERE id = ? AND user_id = ?"
                (fun statement ->
                  match
                    bind statement
                      [
                        Sqlite3.Data.TEXT connection.Withings_connection.id;
                        Sqlite3.Data.TEXT (User_id.to_string user.User.id);
                      ]
                  with
                  | Error _ as error -> error
                  | Ok () -> (
                      match Sqlite3.step statement with
                      | Sqlite3.Rc.ROW ->
                          let current_cursor =
                            if Sqlite3.column_is_null statement 0 then None
                            else Some (Sqlite3.column_int64 statement 0)
                          in
                          if
                            not
                              (Option.equal String.equal
                                 (optional_text statement 1)
                                 (Some
                                    (Withings_connection.Account.to_string
                                       connection.account)))
                          then Error Error.Not_found
                          else
                            Ok
                              (match cursor with
                              | None ->
                                  not
                                    (Option.equal Int64.equal current_cursor
                                       expected_cursor)
                              | Some received -> (
                                  match current_cursor with
                                  | None -> false
                                  | Some current ->
                                      Int64.compare received current < 0))
                      | Sqlite3.Rc.DONE -> Error Error.Not_found
                      | _ -> Error (storage_error ())))
            in
            let current = timestamp (now ()) in
            let persist (row : Weigh_in.import) =
              let existing =
                with_statement db
                  ("SELECT " ^ weigh_in_columns
                 ^ " FROM weigh_ins WHERE source = 'withings' AND external_id \
                    = ? AND user_id = ?")
                  (fun statement ->
                    match
                      bind statement
                        [
                          Sqlite3.Data.TEXT row.external_id;
                          Sqlite3.Data.TEXT (User_id.to_string user.User.id);
                        ]
                    with
                    | Error _ as error -> error
                    | Ok () -> (
                        match Sqlite3.step statement with
                        | Sqlite3.Rc.DONE -> Ok ()
                        | Sqlite3.Rc.ROW -> (
                            match weigh_in_of_row statement with
                            | Some _ -> Ok ()
                            | None -> Error (Error.Storage_decoding "weigh-in"))
                        | _ -> Error (storage_error ())))
              in
              match existing with
              | Error _ as error -> error
              | Ok () ->
                  with_statement db
                    "INSERT INTO weigh_ins (id, user_id, measured_at, \
                     weight_kg, source, external_id, created_at, updated_at, \
                     deleted_at, withings_connection_id) VALUES (?, ?, ?, ?, \
                     'withings', ?, ?, ?, NULL, ?) ON CONFLICT(source, \
                     external_id) DO UPDATE SET weight_kg = \
                     excluded.weight_kg, withings_connection_id = \
                     excluded.withings_connection_id, updated_at = \
                     excluded.updated_at WHERE weigh_ins.deleted_at IS NULL \
                     AND weigh_ins.user_id = excluded.user_id" (fun statement ->
                      match
                        bind statement
                          [
                            Sqlite3.Data.TEXT
                              (Weigh_in_id.to_string (Weigh_in_id.fresh ()));
                            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
                            Sqlite3.Data.TEXT
                              (timestamp row.Weigh_in.measured_at);
                            Sqlite3.Data.FLOAT row.weight_kg;
                            Sqlite3.Data.TEXT row.external_id;
                            Sqlite3.Data.TEXT current;
                            Sqlite3.Data.TEXT current;
                            Sqlite3.Data.TEXT connection.Withings_connection.id;
                          ]
                      with
                      | Error _ as error -> error
                      | Ok () ->
                          if
                            match Sqlite3.step statement with
                            | Sqlite3.Rc.DONE -> true
                            | _ -> false
                          then Ok ()
                          else Error (storage_error ()))
            in
            match stale with
            | Error _ as error -> error
            | Ok true -> Ok ()
            | Ok false -> (
                match
                  List.fold rows ~init:(Ok ()) ~f:(fun result row ->
                      Result.bind result ~f:(fun () -> persist row))
                with
                | Error _ as error -> error
                | Ok () ->
                    with_statement db
                      "UPDATE withings_connections SET sync_cursor = CASE WHEN \
                       sync_cursor IS NULL OR ? >= sync_cursor THEN ? ELSE \
                       sync_cursor END, updated_at = ? WHERE id = ? AND \
                       user_id = ?" (fun statement ->
                        let cursor =
                          match cursor with
                          | None -> Sqlite3.Data.NULL
                          | Some value -> Sqlite3.Data.INT value
                        in
                        match
                          bind statement
                            [
                              cursor;
                              cursor;
                              Sqlite3.Data.TEXT current;
                              Sqlite3.Data.TEXT
                                connection.Withings_connection.id;
                              Sqlite3.Data.TEXT (User_id.to_string user.User.id);
                            ]
                        with
                        | Error _ as error -> error
                        | Ok () ->
                            if
                              (match Sqlite3.step statement with
                                | Sqlite3.Rc.DONE -> true
                                | _ -> false)
                              && Sqlite3.changes db = 1
                            then Ok ()
                            else Error Error.Not_found))
          with Sqlite3.Error _ | Sqlite3.SqliteError _ ->
            Error (storage_error ())
        in
        match result with
        | Ok () -> (
            match Sqlite3.exec db "COMMIT" with
            | Sqlite3.Rc.OK ->
                committed := true;
                Ok ()
            | _ -> Error (storage_error ()))
        | Error _ as error -> error)

let delete_weigh_in db ~user id =
  let current = timestamp (now ()) in
  with_statement db
    "UPDATE weigh_ins SET deleted_at = COALESCE(deleted_at, ?), updated_at = ? \
     WHERE id = ? AND user_id = ? AND deleted_at IS NULL" (fun statement ->
      match
        bind statement
          [
            Sqlite3.Data.TEXT current;
            Sqlite3.Data.TEXT current;
            Sqlite3.Data.TEXT (Weigh_in_id.to_string id);
            Sqlite3.Data.TEXT (User_id.to_string user.User.id);
          ]
      with
      | Error _ as error -> error
      | Ok () -> (
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE ->
              if Sqlite3.changes db = 1 then Ok ()
              else
                with_statement db
                  "SELECT 1 FROM weigh_ins WHERE id = ? AND user_id = ? AND \
                   deleted_at IS NOT NULL" (fun tombstone ->
                    match
                      bind tombstone
                        [
                          Sqlite3.Data.TEXT (Weigh_in_id.to_string id);
                          Sqlite3.Data.TEXT (User_id.to_string user.User.id);
                        ]
                    with
                    | Error _ as error -> error
                    | Ok () -> tombstone_or_not_found tombstone)
          | _ -> Error (storage_error ())))

let compile_search_query = Search_query.of_strings
