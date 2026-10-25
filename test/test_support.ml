open Base

let close_database store =
  Alcotest.(check bool)
    "SQLite database closes without outstanding statements" true
    (Sqlite3.db_close store)

let with_database f =
  let store = Sqlite3.db_open ":memory:" in
  Stdlib.Fun.protect
    ~finally:(fun () -> close_database store)
    (fun () -> f store)

let table_exists_bool db name =
  let statement =
    Sqlite3.prepare db
      "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ? LIMIT 1"
  in
  ignore (Sqlite3.bind statement 1 (Sqlite3.Data.TEXT name));
  let exists =
    match Sqlite3.step statement with Sqlite3.Rc.ROW -> true | _ -> false
  in
  ignore (Sqlite3.finalize statement);
  exists

let table_exists db name =
  Alcotest.(check bool)
    ("table " ^ name ^ " exists")
    true
    (table_exists_bool db name)

let with_store (f : Store_sqlite.t -> User.t -> 'a) : 'a =
  let store =
    Option.value_exn (Result.ok (Store_sqlite.open_database ~path:":memory:"))
  in
  Stdlib.Fun.protect
    ~finally:(fun () -> Option.value_exn (Result.ok (Store_sqlite.close store)))
    (fun () ->
      let user =
        Option.value_exn
          (Result.ok
             (Store_sqlite.resolve_user store ~issuer:"https://issuer.example"
                ~subject:"alice"))
      in
      f store user)

let with_two_users (f : Store_sqlite.t -> User.t -> User.t -> 'a) : 'a =
  with_store (fun store alice ->
      let bob =
        Option.value_exn
          (Result.ok
             (Store_sqlite.resolve_user store ~issuer:"https://issuer.example"
                ~subject:"bob"))
      in
      f store alice bob)

let with_exclusive_lock setup f =
  let path = Stdlib.Filename.temp_file "kcal-store" ".sqlite" in
  let store = Option.value_exn (Result.ok (Store_sqlite.open_database ~path)) in
  Stdlib.Fun.protect
    ~finally:(fun () ->
      Option.value_exn (Result.ok (Store_sqlite.close store));
      Stdlib.Sys.remove path)
    (fun () ->
      let user =
        Option.value_exn
          (Result.ok
             (Store_sqlite.resolve_user store ~issuer:"https://issuer.example"
                ~subject:"alice"))
      in
      let value = setup store user in
      let locker = Sqlite3.db_open path in
      Stdlib.Fun.protect
        ~finally:(fun () ->
          ignore (Sqlite3.exec locker "ROLLBACK");
          close_database locker)
        (fun () ->
          Alcotest.(check bool)
            "exclusive lock acquired" true
            (match Sqlite3.exec locker "BEGIN EXCLUSIVE" with
            | Sqlite3.Rc.OK -> true
            | _ -> false);
          f store user value))

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

let create_meal_raw store ~user raw =
  Result.bind (Meal.Create.of_raw raw) ~f:(Store_sqlite.create_meal store ~user)

let update_meal_raw store ~user id raw =
  Result.bind (Meal.Patch.of_raw raw)
    ~f:(Store_sqlite.update_meal store ~user id)

let save_product_raw store ~user ~id raw =
  Result.bind
    (Product.Create.of_raw raw)
    ~f:(Store_sqlite.save_product store ~user ~id)

let create_manual_weigh_in_raw store ~user raw =
  Result.bind
    (Weigh_in.Create.of_raw raw)
    ~f:(Store_sqlite.create_manual_weigh_in store ~user)

let update_manual_weigh_in_raw store ~user id raw =
  Result.bind
    (Weigh_in.Patch.of_raw raw)
    ~f:(Store_sqlite.update_manual_weigh_in store ~user id)

let persist_withings_import_raw store ~user ~connection ~rows ~cursor =
  let rec parse = function
    | [] -> Ok []
    | row :: rest ->
        Result.bind (Weigh_in.Import.of_raw row) ~f:(fun row ->
            Result.map ~f:(fun rest -> row :: rest) (parse rest))
  in
  Result.bind (parse rows) ~f:(fun rows ->
      Store_sqlite.persist_withings_import store ~user ~connection ~rows
        ~expected_cursor:connection.Withings_connection.sync_cursor ~cursor)

let record_meal_raw service ~user raw =
  Result.bind (Meal.Create.of_raw raw) ~f:(Service.record_meal service ~user)

let record_manual_weigh_in_raw service ~user raw =
  Result.bind
    (Weigh_in.Create.of_raw raw)
    ~f:(Service.record_manual_weigh_in service ~user)

let create_meal store user =
  Option.value_exn (Result.ok (create_meal_raw store ~user (meal_input ())))

let create_weight store user =
  Option.value_exn
    (Result.ok
       (create_manual_weigh_in_raw store ~user
          Weigh_in.{ measured_at = None; weight_kg = 79.6 }))

let equal_product (left : Product.t) (right : Product.t) =
  Product_id.equal left.id right.id
  && User_id.equal left.user_id right.user_id
  && String.equal left.name right.name
  && String.equal left.basis right.basis
  && Int.equal left.calories_kcal right.calories_kcal
  && Float.equal left.protein_g right.protein_g
  && Option.equal Float.equal left.carbs_g right.carbs_g
  && Option.equal Float.equal left.fat_g right.fat_g
  && String.equal left.source right.source
  && Ptime.equal left.created_at right.created_at
  && Ptime.equal left.updated_at right.updated_at

let equal_meal (left : Meal.t) (right : Meal.t) =
  Meal_id.equal left.id right.id
  && User_id.equal left.user_id right.user_id
  && Ptime.equal left.eaten_at right.eaten_at
  && String.equal left.description right.description
  && Int.equal left.calories_kcal right.calories_kcal
  && Float.equal left.protein_g right.protein_g
  && Option.equal Float.equal left.carbs_g right.carbs_g
  && Option.equal Float.equal left.fat_g right.fat_g
  && Option.equal Float.equal left.confidence right.confidence
  && Option.equal String.equal left.estimate_source right.estimate_source
  && Option.equal String.equal left.notes right.notes
  && Ptime.equal left.created_at right.created_at
  && Ptime.equal left.updated_at right.updated_at
  && Option.equal Ptime.equal left.deleted_at right.deleted_at

let user_id =
  Alcotest.testable
    (fun fmt id -> Stdlib.Format.pp_print_string fmt (User_id.to_string id))
    User_id.equal

let product_id =
  Alcotest.testable
    (fun fmt id -> Stdlib.Format.pp_print_string fmt (Product_id.to_string id))
    Product_id.equal

let timestamp =
  Alcotest.testable
    (fun fmt time ->
      Stdlib.Format.pp_print_string fmt (Time.to_utc_string time))
    Ptime.equal

let product =
  Alcotest.testable
    (fun fmt value ->
      Yojson.Safe.pretty_print fmt (Mcp_json.product_json value))
    equal_product

let meal =
  Alcotest.testable
    (fun fmt value -> Yojson.Safe.pretty_print fmt (Mcp_json.meal_json value))
    equal_meal

let exact_float = Alcotest.testable Stdlib.Format.pp_print_float Float.equal

let ensure_withings_connection store ~user ~withings_user_id =
  let key =
    Option.value_exn (Result.ok (Secret.Key.of_bytes (Bytes.make 32 'k')))
  in
  Store_sqlite.replace_withings_connection store ~user ~key
    Withings.
      {
        withings_user_id;
        access_token = "access";
        refresh_token = "refresh";
        expires_at = Option.value_exn (Ptime.of_float_s 1_800_000_000.);
      }

let connection_with_cursor (connection : Withings_connection.connected)
    sync_cursor =
  Option.value_exn
    (Result.ok
       (Withings_connection.decode ~id:connection.id ~user_id:connection.user_id
          ~withings_user_id:
            (Withings_connection.Account.to_string connection.account)
          ~token_expires_at:connection.token_expires_at ~sync_cursor))
