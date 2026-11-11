open Base
open Sqlite_db

let product_columns =
  "id, user_id, name, basis, calories_kcal, protein_g, carbs_g, fat_g, source, \
   created_at, updated_at, version, basis_quantity, basis_unit"

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
      let serving =
        match (Sqlite3.column statement 12, optional_text statement 13) with
        | Sqlite3.Data.NULL, None -> Some None
        | Sqlite3.Data.FLOAT quantity, Some unit -> Some (Some (quantity, unit))
        | Sqlite3.Data.INT quantity, Some unit ->
            Some (Some (Int64.to_float quantity, unit))
        | _ -> None
      in
      Option.bind serving ~f:(fun serving ->
          Result.ok
            (Product.of_storage ~id ~user_id ~version:(int statement 11)
               ~name:(text statement 2) ~basis:(text statement 3) ~serving
               ~calories_kcal:(int statement 4) ~protein_g:(float statement 5)
               ~carbs_g:(optional_float statement 6)
               ~fat_g:(optional_float statement 7)
               ~source:(text statement 8) ~created_at ~updated_at))
  | _ -> None

let qualified_product_columns = qualified_columns "products" product_columns

let get_product db ~user id =
  query_one db
    ("SELECT " ^ product_columns ^ " FROM products WHERE id = ? AND user_id = ?")
    [ text_value (Product_id.to_string id); Store_rows.owner user ]
    product_of_row

let save_product db ~user ~id input =
  let input = Product.Create.to_raw input in
  let persist () =
    let current = time_value (now ()) in
    let fields =
      [
        text_value input.name;
        text_value input.basis;
        int_value input.calories_kcal;
        float_value input.protein_g;
        nullable_float input.carbs_g;
        nullable_float input.fat_g;
        text_value input.source;
        nullable_float (Option.map input.serving ~f:fst);
        nullable (Option.map input.serving ~f:snd);
      ]
    in
    let sql, values =
      match id with
      | None ->
          ( "INSERT INTO products (id, user_id, name, basis, calories_kcal, \
             protein_g, carbs_g, fat_g, source, basis_quantity, basis_unit, \
             created_at, updated_at, version) VALUES (?, ?, ?, ?, ?, ?, ?, ?, \
             ?, ?, ?, ?, ?, 1) RETURNING " ^ product_columns,
            [
              text_value (Product_id.to_string (Product_id.fresh ()));
              Store_rows.owner user;
            ]
            @ fields @ [ current; current ] )
      | Some id ->
          ( "UPDATE products SET name = ?, basis = ?, calories_kcal = ?, \
             protein_g = ?, carbs_g = ?, fat_g = ?, source = ?, basis_quantity \
             = ?, basis_unit = ?, updated_at = ?, version = version + 1 WHERE \
             id = ? AND user_id = ? RETURNING " ^ product_columns,
            fields
            @ [
                current;
                text_value (Product_id.to_string id);
                Store_rows.owner user;
              ] )
    in
    query_one db sql values product_of_row
  in
  match id with
  | None -> persist ()
  | Some id ->
      with_checked_numeric_update db
        ~check:(fun () ->
          Result.map (get_product db ~user id) ~f:(fun (_ : Product.t) -> ()))
        ~update:persist

(* Products behind the user's meal items since [since], most recently eaten
   first. *)
let recent_products db ~user ~since ~limit =
  query db
    ("SELECT " ^ qualified_product_columns
   ^ ", MAX(meals.eaten_at) AS last_eaten_at FROM products JOIN meal_items ON \
      meal_items.product_id = products.id JOIN meals ON meals.id = \
      meal_items.meal_id WHERE products.user_id = ? AND meals.user_id = ? AND \
      meals.deleted_at IS NULL AND meal_items.deleted_at IS NULL AND \
      meals.eaten_at >= ? GROUP BY products.id ORDER BY last_eaten_at DESC, \
      products.id LIMIT ?")
    [
      Store_rows.owner user;
      Store_rows.owner user;
      time_value since;
      int_value limit;
    ]
    (fun statement ->
      match (product_of_row statement, parse_time (text statement 14)) with
      | Some product, Some last_eaten_at -> Some (product, last_eaten_at)
      | _ -> None)

let search_products db ~user ~query:search ~limit =
  if limit < 1 || limit > 500 then Error (invalid_input ())
  else
    query db
      ("SELECT " ^ qualified_product_columns
     ^ " FROM products JOIN products_fts ON products.rowid = \
        products_fts.rowid WHERE products.user_id = ? AND products_fts MATCH ? \
        ORDER BY bm25(products_fts), products.name, products.id LIMIT ?")
      [
        Store_rows.owner user;
        text_value (Search_query.fts5_match_expression search);
        int_value limit;
      ]
      product_of_row
