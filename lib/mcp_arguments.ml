module Domain_error = Error
open Base
module Error = Domain_error

(* Every argument error names the field and what it expected, so the model
   can correct its call; Mcp returns these as tool results. *)
let rejected message = Error (Error.Rejected message)

let edit_distance a b =
  let la = String.length a and lb = String.length b in
  let previous = Array.init (lb + 1) ~f:Fn.id in
  for i = 1 to la do
    let diagonal = ref previous.(0) in
    previous.(0) <- i;
    for j = 1 to lb do
      let above = previous.(j) in
      let cost = if Char.equal a.[i - 1] b.[j - 1] then 0 else 1 in
      previous.(j) <-
        Int.min (Int.min (above + 1) (previous.(j - 1) + 1)) (!diagonal + cost);
      diagonal := above
    done
  done;
  previous.(lb)

(* A likely intended field: a near spelling, or one naming the same thing
   (kcal for calories_kcal). *)
let suggestion allowed name =
  List.filter allowed ~f:(fun candidate ->
      edit_distance candidate name <= 2
      || String.is_substring candidate ~substring:name
      || String.is_substring name ~substring:candidate)
  |> List.min_elt ~compare:(fun a b ->
      Int.compare (edit_distance a name) (edit_distance b name))

let only_fields allowed = function
  | `Assoc fields -> (
      match
        List.find fields ~f:(fun (name, _) ->
            not (List.mem allowed name ~equal:String.equal))
      with
      | None -> Ok fields
      | Some (name, _) ->
          rejected
            (Printf.sprintf "unknown field %S%s; allowed fields: %s" name
               (match suggestion allowed name with
               | Some candidate ->
                   Printf.sprintf " (did you mean %S?)" candidate
               | None -> "")
               (String.concat ~sep:", " allowed)))
  | _ -> rejected "arguments must be an object"

let field name fields = List.Assoc.find fields ~equal:String.equal name

let missing name kind =
  rejected (Printf.sprintf "%s is required (%s)" name kind)

let wrong_type name kind = rejected (Printf.sprintf "%s must be %s" name kind)

let required_string name fields : (string, Error.t) Result.t =
  match field name fields with
  | Some (`String value) -> Ok value
  | None -> missing name "a string"
  | Some _ -> wrong_type name "a string"

let required_int name fields : (int, Error.t) Result.t =
  match field name fields with
  | Some (`Int value) -> Ok value
  | None -> missing name "an integer"
  | Some _ -> wrong_type name "an integer"

let required_float name fields : (float, Error.t) Result.t =
  match field name fields with
  | Some (`Float value) -> Ok value
  | Some (`Int value) -> Ok (Float.of_int value)
  | None -> missing name "a number"
  | Some _ -> wrong_type name "a number"

let optional_string name fields =
  match field name fields with
  | None -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some _ -> wrong_type name "a string"

let optional_int name fields =
  match field name fields with
  | None -> Ok None
  | Some (`Int value) -> Ok (Some value)
  | Some _ -> wrong_type name "an integer"

let optional_float name fields =
  match field name fields with
  | None -> Ok None
  | Some (`Float value) -> Ok (Some value)
  | Some (`Int value) -> Ok (Some (Float.of_int value))
  | Some _ -> wrong_type name "a number"

let timestamp_kind =
  "an RFC 3339 timestamp with an offset, e.g. 2026-09-22T12:30:00Z or \
   2026-09-22T14:30:00+02:00"

let optional_time name fields =
  match field name fields with
  | None -> Ok None
  | Some (`String value) -> (
      match Time.parse_offset_datetime value with
      | Ok time -> Ok (Some time)
      | Error _ -> wrong_type name timestamp_kind)
  | Some _ -> wrong_type name timestamp_kind

let bind result f =
  match result with Ok value -> f value | Error _ as error -> error

let ( let* ) = bind

let id name value of_string =
  match of_string value with
  | Some id -> Ok id
  | None ->
      rejected
        (Printf.sprintf
           "%s %S is not a valid id; use one returned by an earlier call" name
           value)

let optional_bool name fields =
  match field name fields with
  | None -> Ok None
  | Some (`Bool value) -> Ok (Some value)
  | Some _ -> wrong_type name "true or false"

let has name fields = List.Assoc.mem fields ~equal:String.equal name
let nutrition_fields = [ "calories_kcal"; "protein_g"; "carbs_g"; "fat_g" ]
let provenance_fields = [ "confidence"; "estimate_source"; "notes" ]

let item_from_product fields =
  let* fields =
    only_fields
      ([ "product_id"; "quantity"; "description" ] @ provenance_fields)
      (`Assoc fields)
  in
  let* value = required_string "product_id" fields in
  let* product_id = id "product_id" value Product_id.of_string in
  let* quantity = required_float "quantity" fields in
  let* description = optional_string "description" fields in
  let* confidence = optional_float "confidence" fields in
  let* () =
    match confidence with
    | None -> Ok ()
    | Some value ->
        Result.map (Nutrition.Confidence.of_float value) ~f:(fun _ -> ())
  in
  let* estimate_source = optional_string "estimate_source" fields in
  let* notes = optional_string "notes" fields in
  Ok
    (Meal_item.From_product
       { product_id; quantity; description; confidence; estimate_source; notes })

let estimated_item fields =
  let* fields =
    only_fields
      (("description" :: nutrition_fields) @ provenance_fields)
      (`Assoc fields)
  in
  let* description = required_string "description" fields in
  let* calories_kcal = required_int "calories_kcal" fields in
  let* protein_g = required_float "protein_g" fields in
  let* carbs_g = optional_float "carbs_g" fields in
  let* fat_g = optional_float "fat_g" fields in
  let* confidence = optional_float "confidence" fields in
  let* estimate_source = optional_string "estimate_source" fields in
  let* notes = optional_string "notes" fields in
  Result.map
    (Meal_item.Create.of_raw
       {
         description;
         calories_kcal;
         protein_g;
         carbs_g;
         fat_g;
         confidence;
         estimate_source;
         notes;
       })
    ~f:(fun input -> Meal_item.Estimated input)

(* The two shapes are disjoint, so no tag is needed: product_id selects the
   product shape, and nutrition fields are then an error, not ignored. *)
let item_entry = function
  | `Assoc fields ->
      if has "product_id" fields then
        if List.exists nutrition_fields ~f:(fun name -> has name fields) then
          Error
            (Error.Rejected
               "with product_id, omit calories_kcal, protein_g, carbs_g and \
                fat_g: they are scaled from the product")
        else item_from_product fields
      else if has "quantity" fields then
        Error (Error.Rejected "quantity is only meaningful with product_id")
      else estimated_item fields
  | _ -> Error (Error.Rejected "each item must be an object")

let item_patch fields : (Meal_item.Patch.t, Error.t) Result.t =
  let* description = optional_string "description" fields in
  let* calories_kcal = optional_int "calories_kcal" fields in
  let* protein_g = optional_float "protein_g" fields in
  let* carbs_g = optional_float "carbs_g" fields in
  let* fat_g = optional_float "fat_g" fields in
  let* confidence = optional_float "confidence" fields in
  let* estimate_source = optional_string "estimate_source" fields in
  let* notes = optional_string "notes" fields in
  Meal_item.Patch.of_raw
    {
      description;
      calories_kcal;
      protein_g;
      carbs_g;
      fat_g;
      confidence;
      estimate_source;
      notes;
    }

let parse_limit fields =
  let* value = optional_int "limit" fields in
  let limit = Option.value value ~default:100 in
  if limit >= 1 && limit <= 500 then Ok limit
  else rejected "limit must be between 1 and 500"

let query fields =
  let* fields = only_fields [ "from"; "to"; "limit" ] (`Assoc fields) in
  let* from = optional_time "from" fields in
  let* to_ = optional_time "to" fields in
  let* limit = parse_limit fields in
  match (from, to_) with
  | Some lower, Some upper when Ptime.compare lower upper > 0 ->
      rejected "from must not be after to"
  | _ -> Ok (from, to_, limit)

let parse_zone_argument ~default_zone fields =
  let* name = optional_string "timezone" fields in
  match name with
  | None -> default_zone ()
  | Some value -> Time.parse_zone value

let parse_date_argument name fields =
  let* value = optional_string name fields in
  match value with
  | None -> Ok None
  | Some value -> Result.map ~f:Option.some (Date.of_string value)

type range_request = { zone : Time.zone; range : Date_range.t }

let parse_range ~now ~default_zone fields =
  let* zone = parse_zone_argument ~default_zone fields in
  let* first =
    Result.bind (required_string "from_date" fields) ~f:Date.of_string
  in
  let* last = parse_date_argument "to_date" fields in
  let* last =
    match last with Some date -> Ok date | None -> Time.local_date zone now
  in
  let* range = Date_range.make ~first ~last in
  Ok { zone; range }

let parse_range_request ~now ~default_zone fields =
  let* fields =
    only_fields [ "from_date"; "to_date"; "timezone" ] (`Assoc fields)
  in
  parse_range ~now ~default_zone fields

let parse_goals fields =
  let* fields =
    only_fields
      [
        "timezone";
        "weekday_calorie_target_kcal";
        "weekend_calorie_target_kcal";
        "protein_target_g";
        "eating_window_start";
        "eating_window_end";
        "target_weight_kg";
      ]
      (`Assoc fields)
  in
  let* timezone =
    Result.bind (required_string "timezone" fields) ~f:Time.parse_zone
  in
  let calories name =
    Result.bind (required_int name fields) ~f:(fun kcal ->
        Result.map_error (Goals.Calorie_target.of_kcal kcal) ~f:(fun _ ->
            Error.Rejected (name ^ " must be positive")))
  in
  let* weekday_calories = calories "weekday_calorie_target_kcal" in
  let* weekend_calories = calories "weekend_calorie_target_kcal" in
  let optional parse name of_value =
    let* value = parse name fields in
    match value with
    | None -> Ok None
    | Some value -> Result.map (of_value value) ~f:Option.some
  in
  let* protein =
    optional optional_float "protein_target_g" Goals.Protein_target.of_g
  in
  let* target_weight_kg =
    optional optional_float "target_weight_kg" Nutrition.Kilograms.of_float
  in
  let* start = optional_string "eating_window_start" fields in
  let* end_ = optional_string "eating_window_end" fields in
  let* eating_window =
    match (start, end_) with
    | None, None -> Ok None
    | Some start, Some end_ ->
        Result.map (Goals.Eating_window.of_strings ~start ~end_) ~f:Option.some
    | _ ->
        Error
          (Error.Rejected
             "give eating_window_start and eating_window_end together, or \
              neither")
  in
  Ok
    Goals.
      {
        timezone;
        weekday_calories;
        weekend_calories;
        protein;
        eating_window;
        target_weight_kg;
      }

let product_input fields : (Product.Create.t, Error.t) Result.t =
  let* name = required_string "name" fields in
  let* basis = required_string "basis" fields in
  let* calories_kcal = required_int "calories_kcal" fields in
  let* protein_g = required_float "protein_g" fields in
  let* source = required_string "source" fields in
  let* carbs_g = optional_float "carbs_g" fields in
  let* fat_g = optional_float "fat_g" fields in
  let* quantity = optional_float "basis_quantity" fields in
  let* unit = optional_string "basis_unit" fields in
  let* serving =
    match (quantity, unit) with
    | None, None -> Ok None
    | Some quantity, Some unit -> Ok (Some (quantity, unit))
    | _ ->
        Error
          (Error.Rejected
             "give basis_quantity and basis_unit together, or neither")
  in
  Product.Create.of_raw
    Product.
      { name; basis; serving; calories_kcal; protein_g; source; carbs_g; fat_g }

let parse_search ~compile_search = function
  | `String text -> compile_search [ text ]
  | `List values ->
      let rec strings = function
        | [] -> Ok []
        | `String text :: rest ->
            let* rest = strings rest in
            Ok (text :: rest)
        | _ -> Error (Error.Invalid_input "search alternatives must be strings")
      in
      let* alternatives = strings values in
      compile_search alternatives
  | _ ->
      Error
        (Error.Invalid_input
           "search must be a string or nonempty array of strings")

type query_request = {
  from : Ptime.t option;
  to_ : Ptime.t option;
  limit : int;
}

type product_correction = {
  product_id : Product_id.t;
  reason : string;
  dry_run : bool;
}

type request =
  | Begin_withings_connection
  | Get_withings_status
  | Disconnect_withings
  | Get_context
  | Set_goals of Goals.t
  | Save_product of Product_id.t option * Product.Create.t
  | Search_products of Search_query.t * int
  | Correct_meals_from_product of product_correction
  | Record_meal of {
      idempotency_key : string option;
      eaten_at : Ptime.t option;
      label : string option;
      items : Meal_item.entry list;
    }
  | Get_meal of Meal_id.t
  | Query_meals of Search_query.t option * query_request
  | Get_daily_series of range_request
  | Get_summary of range_request
  | Update_meal of Meal_id.t * Meal.Patch.t
  | Delete_meal of Meal_id.t
  | Add_meal_item of Meal_id.t * Meal_item.entry
  | Update_meal_item of {
      id : Meal_item_id.t;
      patch : Meal_item.Patch.t;
      reason : string option;
    }
  | Delete_meal_item of Meal_item_id.t
  | Record_weight of Weigh_in.Create.t
  | Get_weight of Weigh_in_id.t
  | Query_weights of query_request
  | Update_weight of Weigh_in_id.t * Weigh_in.Patch.t
  | Delete_weight of Weigh_in_id.t

let nonblank_string name fields =
  let* value = optional_string name fields in
  match value with
  | Some value when String.is_empty (String.strip value) ->
      Error (Error.Rejected (name ^ " must not be blank"))
  | value -> Ok value

let parse ~now ~default_zone ~compile_search ~name arguments =
  let* fields =
    match arguments with
    | `Assoc fields -> Ok fields
    | _ -> rejected "arguments must be an object"
  in
  match name with
  | "save_product" ->
      let* fields =
        only_fields
          ("id"
          :: [
               "name";
               "basis";
               "calories_kcal";
               "protein_g";
               "source";
               "carbs_g";
               "fat_g";
               "basis_quantity";
               "basis_unit";
             ])
          (`Assoc fields)
      in
      let* value = optional_string "id" fields in
      let* product_id =
        match value with
        | None -> Ok None
        | Some value ->
            Result.map ~f:Option.some (id "id" value Product_id.of_string)
      in
      let* input = product_input fields in
      Ok (Save_product (product_id, input))
  | "search_products" ->
      let* fields = only_fields [ "query"; "limit" ] (`Assoc fields) in
      let* query =
        match List.Assoc.find fields ~equal:String.equal "query" with
        | Some value -> parse_search ~compile_search value
        | None -> Error (Error.Invalid_input "query is required")
      in
      let* limit = parse_limit fields in
      Ok (Search_products (query, limit))
  | "correct_meals_from_product" ->
      let* fields =
        only_fields [ "product_id"; "reason"; "dry_run" ] (`Assoc fields)
      in
      let* value = required_string "product_id" fields in
      let* product_id = id "product_id" value Product_id.of_string in
      let* reason = nonblank_string "reason" fields in
      let* reason =
        Result.of_option reason ~error:(Error.Rejected "reason is required")
      in
      let* dry_run = optional_bool "dry_run" fields in
      Ok
        (Correct_meals_from_product
           { product_id; reason; dry_run = Option.value dry_run ~default:true })
  | "record_meal" ->
      let* fields =
        only_fields
          [ "items"; "eaten_at"; "label"; "idempotency_key" ]
          (`Assoc fields)
      in
      let* idempotency_key = nonblank_string "idempotency_key" fields in
      let* label = nonblank_string "label" fields in
      let* eaten_at = optional_time "eaten_at" fields in
      let* items =
        match List.Assoc.find fields ~equal:String.equal "items" with
        | Some (`List (_ :: _ as items)) ->
            Result.all (List.map items ~f:item_entry)
        | _ -> Error (Error.Rejected "items must be a nonempty array")
      in
      Ok (Record_meal { idempotency_key; eaten_at; label; items })
  | "get_meal" ->
      let* fields = only_fields [ "id" ] (`Assoc fields) in
      let* value = required_string "id" fields in
      let* meal_id = id "id" value Meal_id.of_string in
      Ok (Get_meal meal_id)
  | "query_meals" ->
      let* fields =
        only_fields [ "from"; "to"; "limit"; "search" ] (`Assoc fields)
      in
      let* search =
        match List.Assoc.find fields ~equal:String.equal "search" with
        | None -> Ok None
        | Some value ->
            Result.map ~f:Option.some (parse_search ~compile_search value)
      in
      let* from, to_, limit =
        query (List.Assoc.remove fields ~equal:String.equal "search")
      in
      Ok (Query_meals (search, { from; to_; limit }))
  | "update_meal" ->
      let* fields = only_fields [ "id"; "eaten_at"; "label" ] (`Assoc fields) in
      let* value = required_string "id" fields in
      let* meal_id = id "id" value Meal_id.of_string in
      let* eaten_at = optional_time "eaten_at" fields in
      let* label = nonblank_string "label" fields in
      let* patch = Meal.Patch.of_raw { eaten_at; label } in
      Ok (Update_meal (meal_id, patch))
  | "add_meal_item" ->
      let* fields = only_fields [ "meal_id"; "item" ] (`Assoc fields) in
      let* value = required_string "meal_id" fields in
      let* meal_id = id "meal_id" value Meal_id.of_string in
      let* entry =
        match List.Assoc.find fields ~equal:String.equal "item" with
        | Some item -> item_entry item
        | None -> Error (Error.Rejected "item is required")
      in
      Ok (Add_meal_item (meal_id, entry))
  | "update_meal_item" ->
      let* fields =
        only_fields
          (("id" :: "reason" :: "description" :: nutrition_fields)
          @ provenance_fields)
          (`Assoc fields)
      in
      let* value = required_string "id" fields in
      let* item_id = id "id" value Meal_item_id.of_string in
      let* reason = nonblank_string "reason" fields in
      let* patch = item_patch fields in
      Ok (Update_meal_item { id = item_id; patch; reason })
  | "delete_meal_item" ->
      let* fields = only_fields [ "id" ] (`Assoc fields) in
      let* value = required_string "id" fields in
      let* item_id = id "id" value Meal_item_id.of_string in
      Ok (Delete_meal_item item_id)
  | "delete_meal" ->
      let* fields = only_fields [ "id" ] (`Assoc fields) in
      let* value = required_string "id" fields in
      let* meal_id = id "id" value Meal_id.of_string in
      Ok (Delete_meal meal_id)
  | "record_weight" ->
      let* fields =
        only_fields [ "weight_kg"; "measured_at" ] (`Assoc fields)
      in
      let* weight_kg = required_float "weight_kg" fields in
      let* measured_at = optional_time "measured_at" fields in
      let* input = Weigh_in.Create.of_raw Weigh_in.{ measured_at; weight_kg } in
      Ok (Record_weight input)
  | "get_weight" ->
      let* fields = only_fields [ "id" ] (`Assoc fields) in
      let* value = required_string "id" fields in
      let* weight_id = id "id" value Weigh_in_id.of_string in
      Ok (Get_weight weight_id)
  | "query_weights" ->
      let* from, to_, limit = query fields in
      Ok (Query_weights { from; to_; limit })
  | "update_weight" ->
      let* fields =
        only_fields ("id" :: [ "weight_kg"; "measured_at" ]) (`Assoc fields)
      in
      let* value = required_string "id" fields in
      let* weight_id = id "id" value Weigh_in_id.of_string in
      let* measured_at = optional_time "measured_at" fields in
      let* weight_kg = optional_float "weight_kg" fields in
      let* patch = Weigh_in.Patch.of_raw Weigh_in.{ measured_at; weight_kg } in
      Ok (Update_weight (weight_id, patch))
  | "delete_weight" ->
      let* fields = only_fields [ "id" ] (`Assoc fields) in
      let* value = required_string "id" fields in
      let* weight_id = id "id" value Weigh_in_id.of_string in
      Ok (Delete_weight weight_id)
  | "begin_withings_connection" ->
      let* _ = only_fields [] arguments in
      Ok Begin_withings_connection
  | "get_withings_status" ->
      let* _ = only_fields [] arguments in
      Ok Get_withings_status
  | "disconnect_withings" ->
      let* _ = only_fields [] arguments in
      Ok Disconnect_withings
  | "get_context" ->
      let* _ = only_fields [] arguments in
      Ok Get_context
  | "set_goals" ->
      let* goals = parse_goals fields in
      Ok (Set_goals goals)
  | "get_daily_series" ->
      let* request = parse_range_request ~now ~default_zone fields in
      Ok (Get_daily_series request)
  | "get_summary" ->
      let* request = parse_range_request ~now ~default_zone fields in
      Ok (Get_summary request)
  | _ -> rejected ("unknown tool " ^ name)
