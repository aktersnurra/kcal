let open_temporary_database () = Sqlite3.db_open ":memory:"

let table_exists_bool db name =
  let statement =
    Sqlite3.prepare db
      "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ? LIMIT 1"
  in
  ignore (Sqlite3.bind statement 1 (Sqlite3.Data.TEXT name));
  let exists = Sqlite3.step statement = Sqlite3.Rc.ROW in
  ignore (Sqlite3.finalize statement);
  exists

let table_exists db name =
  Alcotest.(check bool) ("table " ^ name ^ " exists") true (table_exists_bool db name)

let store_with_user () =
  let store = open_temporary_database () in
  Result.get_ok (Migration.apply_all store);
  let user =
    Result.get_ok
      (Store_sqlite.resolve_user store ~issuer:"https://issuer.example" ~subject:"alice")
  in
  (store, user)

let store_with_two_users () =
  let store, alice = store_with_user () in
  let bob =
    Result.get_ok
      (Store_sqlite.resolve_user store ~issuer:"https://issuer.example" ~subject:"bob")
  in
  (store, alice, bob)

let meal_input ?eaten_at () =
  Meal.
    {
      description = "Soup";
      calories_kcal = 250;
      protein_g = 12.0;
      carbs_g = Some 20.0;
      fat_g = Some 5.0;
      confidence = Some 0.8;
      estimate_source = Some "label";
      notes = Some "lunch";
      eaten_at;
    }

let create_meal store user =
  Result.get_ok (Store_sqlite.create_meal store ~user (meal_input ()))

let create_weight store user =
  Result.get_ok
    (Store_sqlite.create_manual_weigh_in store ~user
       Weigh_in.{ measured_at = None; weight_kg = 79.6 })
