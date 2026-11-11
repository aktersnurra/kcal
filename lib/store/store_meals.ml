module Domain_error = Error
open Base
module Error = Domain_error
open Sqlite_db

type new_item = Meal_item.Create.t * Meal_item.product_link option

let meal_columns =
  "id, user_id, eaten_at, label, created_at, updated_at, deleted_at"

(* A meal before its items are attached. *)
type meal_row = {
  id : Meal_id.t;
  user_id : User_id.t;
  eaten_at : Ptime.t;
  label : string option;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  deleted_at : Ptime.t option;
}

let meal_row_of statement =
  match
    ( Meal_id.of_string (text statement 0),
      User_id.of_string (text statement 1),
      parse_time (text statement 2),
      parse_time (text statement 4),
      parse_time (text statement 5) )
  with
  | Some id, Some user_id, Some eaten_at, Some created_at, Some updated_at ->
      Some
        {
          id;
          user_id;
          eaten_at;
          label = optional_text statement 3;
          created_at;
          updated_at;
          deleted_at = Option.bind (optional_text statement 6) ~f:parse_time;
        }
  | _ -> None

let item_columns =
  "id, meal_id, description, calories_kcal, protein_g, carbs_g, fat_g, \
   confidence, estimate_source, notes, product_id, product_version, \
   product_quantity, product_unit, created_at, updated_at"

(* Columns [first] to [first + 3]; all four are set together or not at all.
   [None] is a decoding failure, [Some None] an item without a product. *)
let product_link_of_row statement ~first =
  let quantity =
    match Sqlite3.column statement (first + 2) with
    | Sqlite3.Data.FLOAT value -> Some value
    | Sqlite3.Data.INT value -> Some (Int64.to_float value)
    | _ -> None
  in
  match
    ( optional_text statement first,
      Sqlite3.column statement (first + 1),
      Sqlite3.column statement (first + 2),
      optional_text statement (first + 3) )
  with
  | None, Sqlite3.Data.NULL, Sqlite3.Data.NULL, None -> Some None
  | Some product_id, Sqlite3.Data.INT version, _, Some unit -> (
      match (Product_id.of_string product_id, quantity) with
      | Some product_id, Some quantity ->
          Some
            (Some
               Meal_item.
                 {
                   product_id;
                   product_version = Int64.to_int_exn version;
                   quantity;
                   unit;
                 })
      | _ -> None)
  | _ -> None

(* Columns 0-15 of [item_columns], with the owning meal's id. *)
let item_of_row statement =
  match
    ( Meal_item_id.of_string (text statement 0),
      Meal_id.of_string (text statement 1),
      parse_time (text statement 14),
      parse_time (text statement 15) )
  with
  | Some id, Some meal_id, Some created_at, Some updated_at
    when calorie_column statement 3 && numeric_column statement 4
         && List.for_all [ 5; 6; 7 ] ~f:(optional_numeric_column statement) ->
      Option.bind (product_link_of_row statement ~first:10) ~f:(fun product ->
          Result.ok
            (Meal_item.of_storage ~id ~description:(text statement 2)
               ~calories_kcal:(int statement 3) ~protein_g:(float statement 4)
               ~carbs_g:(optional_float statement 5)
               ~fat_g:(optional_float statement 6)
               ~confidence:(optional_float statement 7)
               ~estimate_source:(optional_text statement 8)
               ~notes:(optional_text statement 9)
               ~product ~created_at ~updated_at)
          |> Option.map ~f:(fun item -> (meal_id, item)))
  | _ -> None

let qualified_meal_columns = qualified_columns "meals" meal_columns
let qualified_item_columns = qualified_columns "meal_items" item_columns

(* Live items of [meals], in order, attached in one query. *)
let attach_items db rows =
  match rows with
  | [] -> Ok []
  | _ ->
      let* items =
        query db
          ("SELECT " ^ item_columns
         ^ " FROM meal_items WHERE deleted_at IS NULL AND meal_id IN (SELECT \
            value FROM json_each(?)) ORDER BY meal_id, position, id")
          [
            json_ids
              (List.map rows ~f:(fun (row : meal_row) ->
                   Meal_id.to_string row.id));
          ]
          item_of_row
      in
      let by_meal =
        Map.of_alist_multi
          (module String)
          (List.map items ~f:(fun (meal_id, item) ->
               (Meal_id.to_string meal_id, item)))
      in
      List.map rows ~f:(fun (row : meal_row) ->
          Meal.of_storage ~id:row.id ~user_id:row.user_id ~eaten_at:row.eaten_at
            ~label:row.label
            ~items:
              (Option.value ~default:[]
                 (Map.find by_meal (Meal_id.to_string row.id)))
            ~created_at:row.created_at ~updated_at:row.updated_at
            ~deleted_at:row.deleted_at)
      |> Result.all

let select_meals db sql values =
  let* rows = query db sql values meal_row_of in
  attach_items db rows

let single = function
  | [ meal ] -> Ok meal
  | [] -> Error Error.Not_found
  | _ -> Error (storage_error ())

let get_meal db ~user id =
  Result.bind ~f:single
    (select_meals db
       ("SELECT " ^ meal_columns
      ^ " FROM meals WHERE id = ? AND user_id = ? AND deleted_at IS NULL")
       [ text_value (Meal_id.to_string id); owner user ])

let product_link_values = function
  | None ->
      [
        Sqlite3.Data.NULL;
        Sqlite3.Data.NULL;
        Sqlite3.Data.NULL;
        Sqlite3.Data.NULL;
      ]
  | Some (link : Meal_item.product_link) ->
      [
        Sqlite3.Data.TEXT (Product_id.to_string link.product_id);
        Sqlite3.Data.INT (Int64.of_int link.product_version);
        Sqlite3.Data.FLOAT link.quantity;
        Sqlite3.Data.TEXT link.unit;
      ]

(* Appended after the meal's existing items. *)
let insert_item db ~meal_id ~current (item, product) =
  let item = Meal_item.Create.to_raw item in
  let meal_id = text_value (Meal_id.to_string meal_id) in
  execute db
    "INSERT INTO meal_items (id, meal_id, position, description, \
     calories_kcal, protein_g, carbs_g, fat_g, confidence, estimate_source, \
     notes, product_id, product_version, product_quantity, product_unit, \
     created_at, updated_at) SELECT ?, ?, COALESCE(MAX(position), -1) + 1, ?, \
     ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? FROM meal_items WHERE meal_id = ?"
    ([
       text_value (Meal_item_id.to_string (Meal_item_id.fresh ()));
       meal_id;
       text_value item.description;
       int_value item.calories_kcal;
       float_value item.protein_g;
       nullable_float item.carbs_g;
       nullable_float item.fat_g;
       nullable_float item.confidence;
       nullable item.estimate_source;
       nullable item.notes;
     ]
    @ product_link_values product
    @ [ text_value current; text_value current; meal_id ])

let touch_meal db meal_id ~current =
  execute db "UPDATE meals SET updated_at = ? WHERE id = ?"
    [ text_value current; text_value (Meal_id.to_string meal_id) ]

type idempotency = Meal.idempotency = { key : string; fingerprint : string }

(* [None] when the key is unused. A deleted meal keeps its key, so a retry
   cannot resurrect it, and a key reused for different content is an error
   rather than a silent answer with the first meal. *)
let idempotent_meal db ~user { key; fingerprint } =
  let* found =
    query db
      "SELECT id, idempotency_fingerprint, deleted_at IS NOT NULL FROM meals \
       WHERE user_id = ? AND idempotency_key = ?"
      [ owner user; text_value key ]
      (fun statement ->
        Option.map
          (Meal_id.of_string (text statement 0))
          ~f:(fun id -> (id, optional_text statement 1, int statement 2 = 1)))
  in
  match found with
  | [] -> Ok None
  | [ (_, _, true) ] ->
      Error
        (Error.Rejected
           "idempotency_key belongs to a meal that was deleted; use a new key \
            to record it again")
  | [ (id, stored, false) ]
    when Option.equal String.equal stored (Some fingerprint) ->
      Result.map (get_meal db ~user id) ~f:Option.some
  | [ _ ] ->
      Error
        (Error.Rejected
           "idempotency_key was already used for a different meal; use a new \
            key for each meal you log, and the same key only to retry it")
  | _ -> Error (storage_error ())

let insert_meal db ~user ~idempotency ~eaten_at ~label items =
  let current_time = now () in
  let current = timestamp current_time in
  let id = Meal_id.fresh () in
  let* () =
    execute db
      "INSERT INTO meals (id, user_id, eaten_at, label, created_at, \
       updated_at, deleted_at, idempotency_key, idempotency_fingerprint) \
       VALUES (?, ?, ?, ?, ?, ?, NULL, ?, ?)"
      [
        text_value (Meal_id.to_string id);
        owner user;
        time_value (Option.value eaten_at ~default:current_time);
        nullable label;
        text_value current;
        text_value current;
        nullable (Option.map idempotency ~f:(fun i -> i.key));
        nullable (Option.map idempotency ~f:(fun i -> i.fingerprint));
      ]
  in
  let* () =
    List.fold_result items ~init:() ~f:(fun () item ->
        insert_item db ~meal_id:id ~current item)
  in
  get_meal db ~user id

let create_meal ?idempotency db ~user ~eaten_at ~label items =
  let* () = Meal.validate_label label in
  if List.is_empty items then
    Error (Error.Rejected "a meal needs at least one item")
  else
    in_transaction db (fun () ->
        let* existing =
          match idempotency with
          | None -> Ok None
          | Some idempotency -> idempotent_meal db ~user idempotency
        in
        match existing with
        | Some meal -> Ok (meal, `Replayed)
        | None ->
            Result.map
              (insert_meal db ~user ~idempotency ~eaten_at ~label items)
              ~f:(fun meal -> (meal, `Recorded)))

let update_meal db ~user id patch =
  let patch = Meal.Patch.to_raw patch in
  let* rows =
    query db
      ("UPDATE meals SET eaten_at = COALESCE(?, eaten_at), label = COALESCE(?, \
        label), updated_at = ? WHERE id = ? AND user_id = ? AND deleted_at IS \
        NULL RETURNING " ^ meal_columns)
      [
        nullable (Option.map ~f:timestamp patch.eaten_at);
        nullable patch.label;
        time_value (now ());
        text_value (Meal_id.to_string id);
        owner user;
      ]
      meal_row_of
  in
  Result.bind (attach_items db rows) ~f:single

let add_item db ~user meal_id item =
  in_transaction db (fun () ->
      let* (_ : Meal.t) = get_meal db ~user meal_id in
      let current = timestamp (now ()) in
      let* () = insert_item db ~meal_id ~current item in
      let* () = touch_meal db meal_id ~current in
      get_meal db ~user meal_id)

(* A live item of one of the user's live meals. *)
let owned_item db ~user item_id =
  Result.bind ~f:single
    (query db
       ("SELECT " ^ qualified_item_columns
      ^ " FROM meal_items JOIN meals ON meals.id = meal_items.meal_id WHERE \
         meal_items.id = ? AND meals.user_id = ? AND meals.deleted_at IS NULL \
         AND meal_items.deleted_at IS NULL")
       [ text_value (Meal_item_id.to_string item_id); owner user ]
       item_of_row)

let revision_columns =
  "item_id, revision, revised_at, reason, description, calories_kcal, \
   protein_g, carbs_g, fat_g, confidence, estimate_source, notes, product_id, \
   product_version, product_quantity, product_unit"

(* Snapshot [item] as it stands, before the caller changes it, inside the
   caller's transaction. *)
let record_revision db (item : Meal_item.t) ~revised_at ~reason =
  let id = text_value (Meal_item_id.to_string item.id) in
  execute db
    ("INSERT INTO meal_item_revisions (" ^ revision_columns
   ^ ") SELECT ?, COALESCE(MAX(revision), 0) + 1, ?, ?, ?, ?, ?, ?, ?, ?, ?, \
      ?, ?, ?, ?, ? FROM meal_item_revisions WHERE item_id = ?")
    ([
       id;
       text_value revised_at;
       nullable reason;
       text_value item.description;
       int_value item.calories_kcal;
       float_value item.protein_g;
       nullable_float item.carbs_g;
       nullable_float item.fat_g;
       nullable_float item.confidence;
       nullable item.estimate_source;
       nullable item.notes;
     ]
    @ product_link_values item.product
    @ [ id ])

let revision_of_row statement =
  match
    (Meal_item_id.of_string (text statement 0), parse_time (text statement 2))
  with
  | Some item_id, Some revised_at
    when calorie_column statement 5 && numeric_column statement 6
         && List.for_all [ 7; 8; 9 ] ~f:(optional_numeric_column statement) ->
      Option.map (product_link_of_row statement ~first:12) ~f:(fun product ->
          ( item_id,
            Meal_item.
              {
                revision = int statement 1;
                revised_at;
                reason = optional_text statement 3;
                previous =
                  {
                    description = text statement 4;
                    calories_kcal = int statement 5;
                    protein_g = float statement 6;
                    carbs_g = optional_float statement 7;
                    fat_g = optional_float statement 8;
                    confidence = optional_float statement 9;
                    estimate_source = optional_text statement 10;
                    notes = optional_text statement 11;
                    product;
                  };
              } ))
  | _ -> None

let item_revisions db (meal : Meal.t) =
  query db
    ("SELECT " ^ revision_columns
   ^ " FROM meal_item_revisions WHERE item_id IN (SELECT value FROM \
      json_each(?)) ORDER BY item_id, revision")
    [
      json_ids
        (List.map meal.items ~f:(fun (item : Meal_item.t) ->
             Meal_item_id.to_string item.id));
    ]
    revision_of_row

let get_meal_with_revisions db ~user id =
  let* meal = get_meal db ~user id in
  let* revisions = item_revisions db meal in
  Ok (meal, revisions)

let set_item_nutrition db (item : Meal_item.t) ~current
    (portion : Product.portion) link =
  execute db
    "UPDATE meal_items SET calories_kcal = ?, protein_g = ?, carbs_g = ?, \
     fat_g = ?, product_id = ?, product_version = ?, product_quantity = ?, \
     product_unit = ?, updated_at = ? WHERE id = ?"
    ([
       int_value portion.calories_kcal;
       float_value portion.protein_g;
       nullable_float portion.carbs_g;
       nullable_float portion.fat_g;
     ]
    @ product_link_values (Some link)
    @ [ text_value current; text_value (Meal_item_id.to_string item.id) ])

let update_item ?reason ?quantity db ~user item_id patch =
  let changes_nutrition = Meal_item.changes_nutrition patch in
  let patch = Meal_item.Patch.to_raw patch in
  in_transaction db (fun () ->
      let* meal_id, item = owned_item db ~user item_id in
      (* A new quantity rescales the item and keeps its link; a hand-edited
         value no longer follows from the product, so it detaches. *)
      let* rescaled =
        match quantity with
        | None -> Ok None
        | Some quantity ->
            Result.map (Meal_item.rescale item ~quantity) ~f:Option.some
      in
      let calories_kcal, protein_g, carbs_g, fat_g, detach_product =
        match rescaled with
        | Some ((portion : Product.portion), _) ->
            ( Some portion.calories_kcal,
              Some portion.protein_g,
              portion.carbs_g,
              portion.fat_g,
              false )
        | None ->
            ( patch.calories_kcal,
              patch.protein_g,
              patch.carbs_g,
              patch.fat_g,
              changes_nutrition )
      in
      let rescaled_quantity =
        Option.map rescaled ~f:(fun (_, (link : Meal_item.product_link)) ->
            link.quantity)
      in
      let current = timestamp (now ()) in
      let* () = record_revision db item ~revised_at:current ~reason in
      let detach = Sqlite3.Data.INT (if detach_product then 1L else 0L) in
      let* () =
        execute db
          "UPDATE meal_items SET description = COALESCE(?, description), \
           calories_kcal = COALESCE(?, calories_kcal), protein_g = COALESCE(?, \
           protein_g), carbs_g = COALESCE(?, carbs_g), fat_g = COALESCE(?, \
           fat_g), confidence = COALESCE(?, confidence), estimate_source = \
           COALESCE(?, estimate_source), notes = COALESCE(?, notes), \
           product_id = CASE WHEN ? THEN NULL ELSE product_id END, \
           product_version = CASE WHEN ? THEN NULL ELSE product_version END, \
           product_quantity = CASE WHEN ? THEN NULL ELSE COALESCE(?, \
           product_quantity) END, product_unit = CASE WHEN ? THEN NULL ELSE \
           product_unit END, updated_at = ? WHERE id = ?"
          [
            nullable patch.description;
            nullable_int calories_kcal;
            nullable_float protein_g;
            nullable_float carbs_g;
            nullable_float fat_g;
            nullable_float patch.confidence;
            nullable patch.estimate_source;
            nullable patch.notes;
            detach;
            detach;
            detach;
            nullable_float rescaled_quantity;
            detach;
            text_value current;
            text_value (Meal_item_id.to_string item_id);
          ]
      in
      let* () = touch_meal db meal_id ~current in
      get_meal db ~user meal_id)

let delete_item db ~user item_id =
  in_transaction db (fun () ->
      let* meal_id, _ = owned_item db ~user item_id in
      let* meal = get_meal db ~user meal_id in
      if List.length meal.items <= 1 then
        Error
          (Error.Rejected
             "a meal keeps at least one item; delete the meal instead")
      else
        let current = timestamp (now ()) in
        let* () =
          execute db
            "UPDATE meal_items SET deleted_at = ?, updated_at = ? WHERE id = ?"
            [
              text_value current;
              text_value current;
              text_value (Meal_item_id.to_string item_id);
            ]
        in
        let* () = touch_meal db meal_id ~current in
        get_meal db ~user meal_id)

(* A meal matches when any live item does; it ranks by its best item. *)
let query_meals ?search db ~user ~from ~to_ ~limit =
  if not (is_valid_query ~from ~to_ ~limit) then Error (invalid_input ())
  else
    let with_clause, join, search_values, ordering =
      match search with
      | None -> ("", "", [], "meals.eaten_at ASC, meals.id ASC")
      | Some query ->
          ( "WITH matches AS MATERIALIZED (SELECT meal_items.meal_id AS \
             meal_id, bm25(meal_items_fts) AS score FROM meal_items_fts JOIN \
             meal_items ON meal_items.rowid = meal_items_fts.rowid WHERE \
             meal_items_fts MATCH ? AND meal_items.deleted_at IS NULL), best \
             AS (SELECT meal_id, MIN(score) AS score FROM matches GROUP BY \
             meal_id) ",
            " JOIN best ON best.meal_id = meals.id",
            [ text_value (Search_query.fts5_match_expression query) ],
            "best.score, meals.eaten_at ASC, meals.id ASC" )
    in
    let range, range_values = range_clause "meals.eaten_at" ~from ~to_ in
    select_meals db
      (with_clause ^ "SELECT " ^ qualified_meal_columns ^ " FROM meals" ^ join
     ^ " WHERE meals.user_id = ? AND meals.deleted_at IS NULL" ^ range
     ^ " ORDER BY " ^ ordering ^ " LIMIT ?")
      (search_values @ [ owner user ] @ range_values @ [ int_value limit ])

(* Every meal in [start, stop), unpaginated, for server-side aggregation. *)
let meals_between db ~user ~start ~stop =
  select_meals db
    ("SELECT " ^ meal_columns
   ^ " FROM meals WHERE user_id = ? AND deleted_at IS NULL AND eaten_at >= ? \
      AND eaten_at < ? ORDER BY eaten_at ASC, id ASC")
    [ owner user; time_value start; time_value stop ]

(* Live items of live meals logged from an earlier version of [product], with
   their meal and when it was eaten. *)
let items_from_earlier_versions db ~user (product : Product.t) =
  query db
    ("SELECT " ^ qualified_item_columns
   ^ ", meals.eaten_at FROM meal_items JOIN meals ON meals.id = \
      meal_items.meal_id WHERE meals.user_id = ? AND meals.deleted_at IS NULL \
      AND meal_items.deleted_at IS NULL AND meal_items.product_id = ? AND \
      meal_items.product_version < ? ORDER BY meals.eaten_at ASC, \
      meal_items.id ASC")
    [
      owner user;
      text_value (Product_id.to_string product.id);
      int_value product.version;
    ]
    (fun statement ->
      match (item_of_row statement, parse_time (text statement 16)) with
      | Some (meal_id, item), Some eaten_at -> Some (meal_id, eaten_at, item)
      | _ -> None)

(* All or none: each item gets a revision, then its new nutrition and link. *)
let rescale_items db ~reason changes =
  in_transaction db (fun () ->
      let current = timestamp (now ()) in
      List.fold_result changes ~init:()
        ~f:(fun () (meal_id, (item : Meal_item.t), portion, link) ->
          let* () =
            record_revision db item ~revised_at:current ~reason:(Some reason)
          in
          let* () = set_item_nutrition db item ~current portion link in
          touch_meal db meal_id ~current))

let delete_meal db ~user id =
  soft_delete db ~table:"meals" ~user (Meal_id.to_string id)
