let invalid () = Error (Error.Invalid_input "invalid MCP parameters")

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let only_fields allowed = function
  | `Assoc fields
    when List.for_all (fun (name, _) -> List.mem name allowed) fields ->
      Ok fields
  | _ -> invalid ()

let required_string name fields : (string, Error.t) result =
  match List.assoc_opt name fields with
  | Some (`String value) -> Ok value
  | _ -> invalid ()

let required_int name fields : (int, Error.t) result =
  match List.assoc_opt name fields with
  | Some (`Int value) -> Ok value
  | _ -> invalid ()

let required_float name fields : (float, Error.t) result =
  match List.assoc_opt name fields with
  | Some (`Float value) -> Ok value
  | Some (`Int value) -> Ok (float_of_int value)
  | _ -> invalid ()

let optional_string name fields =
  match List.assoc_opt name fields with
  | None -> Ok None
  | Some (`String value) -> Ok (Some value)
  | _ -> invalid ()

let optional_int name fields =
  match List.assoc_opt name fields with
  | None -> Ok None
  | Some (`Int value) -> Ok (Some value)
  | _ -> invalid ()

let optional_float name fields =
  match List.assoc_opt name fields with
  | None -> Ok None
  | Some (`Float value) -> Ok (Some value)
  | Some (`Int value) -> Ok (Some (float_of_int value))
  | _ -> invalid ()

let optional_time name fields =
  match List.assoc_opt name fields with
  | None -> Ok None
  | Some (`String value) ->
      Result.map Option.some (Time.parse_offset_datetime value)
  | _ -> invalid ()

let bind result f =
  match result with Ok value -> f value | Error _ as error -> error

let ( let* ) = bind

let id value of_string =
  match of_string value with Some id -> Ok id | None -> invalid ()

let json_option f = function None -> `Null | Some value -> f value

let meal_json (meal : Meal.t) =
  `Assoc
    [
      ("id", `String (Meal_id.to_string meal.id));
      ("eaten_at", `String (Time.to_utc_string meal.eaten_at));
      ("description", `String meal.description);
      ("calories_kcal", `Int meal.calories_kcal);
      ("protein_g", `Float meal.protein_g);
      ("carbs_g", json_option (fun v -> `Float v) meal.carbs_g);
      ("fat_g", json_option (fun v -> `Float v) meal.fat_g);
      ("confidence", json_option (fun v -> `Float v) meal.confidence);
      ("estimate_source", json_option (fun v -> `String v) meal.estimate_source);
      ("notes", json_option (fun v -> `String v) meal.notes);
      ("created_at", `String (Time.to_utc_string meal.created_at));
      ("updated_at", `String (Time.to_utc_string meal.updated_at));
    ]

let product_json (product : Product.t) =
  `Assoc
    [
      ("id", `String (Product_id.to_string product.id));
      ("name", `String product.name);
      ("basis", `String product.basis);
      ("calories_kcal", `Int product.calories_kcal);
      ("protein_g", `Float product.protein_g);
      ("carbs_g", json_option (fun v -> `Float v) product.carbs_g);
      ("fat_g", json_option (fun v -> `Float v) product.fat_g);
      ("source", `String product.source);
      ("created_at", `String (Time.to_utc_string product.created_at));
      ("updated_at", `String (Time.to_utc_string product.updated_at));
    ]

let float_json value = `Float value

(* One row per local date.  Unlogged days keep meal_count 0 but report null
   intake, since nothing logged means unknown, not zero. *)
let day_json (day : Summary.day) =
  let meals = day.meals and weight = day.weight in
  `Assoc
    [
      ("date", `String (Date.to_string day.date));
      ( "meal_count",
        `Int
          (Option.fold ~none:0
             ~some:(fun (totals : Meal.totals) -> totals.meal_count)
             meals) );
      ( "calories_kcal",
        json_option
          (fun (totals : Meal.totals) -> `Int totals.calories_kcal)
          meals );
      ( "protein_g",
        json_option
          (fun (totals : Meal.totals) -> `Float totals.protein_g)
          meals );
      ( "carbs_g",
        json_option float_json
          (Option.bind meals (fun (totals : Meal.totals) -> totals.carbs_g)) );
      ( "fat_g",
        json_option float_json
          (Option.bind meals (fun (totals : Meal.totals) -> totals.fat_g)) );
      ( "weigh_in_count",
        `Int
          (Option.fold ~none:0
             ~some:(fun (w : Summary.daily_weight) -> w.weigh_in_count)
             weight) );
      ( "mean_weight_kg",
        json_option
          (fun (w : Summary.daily_weight) -> `Float w.mean_weight_kg)
          weight );
    ]

let range_json zone range =
  [
    ("from_date", `String (Date.to_string (Date_range.first range)));
    ("to_date", `String (Date.to_string (Date_range.last range)));
    ("timezone", `String (Time.zone_name zone));
  ]

let weight_trend_json = function
  | None -> `Null
  | Some (Summary.Single_reading { weight_kg; measured_at }) ->
      `Assoc
        [
          ("kind", `String "single_reading");
          ("weight_kg", `Float weight_kg);
          ("measured_at", `String (Time.to_utc_string measured_at));
        ]
  | Some (Summary.Trend trend) ->
      `Assoc
        [
          ("kind", `String "trend");
          ("weigh_in_count", `Int trend.weigh_in_count);
          ("first_kg", `Float trend.first_kg);
          ("last_kg", `Float trend.last_kg);
          ("change_kg", `Float trend.change_kg);
          ("span_days", `Float trend.span_days);
          ("slope_kg_per_week", `Float trend.slope_kg_per_week);
        ]

(* Always carries "kcal"; when it is null, "unavailable_reason" says why. *)
let implied_maintenance_json = function
  | Summary.Estimate { kcal } -> `Assoc [ ("kcal", `Float kcal) ]
  | Summary.Unavailable reason -> (
      let unavailable name details =
        `Assoc
          ([ ("kcal", `Null); ("unavailable_reason", `String name) ] @ details)
      in
      match reason with
      | Summary.No_intake_logged -> unavailable "no_intake_logged" []
      | Summary.No_weight_trend -> unavailable "no_weight_trend" []
      | Summary.Span_too_short { span_days; required_days } ->
          unavailable "weigh_in_span_too_short"
            [
              ("span_days", `Float span_days);
              ("required_days", `Float required_days);
            ])

let summary_json zone range (summary : Summary.t) =
  let logged = summary.logged_intake in
  `Assoc
    (range_json zone range
    @ [
        ("day_count", `Int summary.day_count);
        ( "logged_day_count",
          `Int
            (Option.fold ~none:0
               ~some:(fun (l : Summary.logged_intake) -> l.logged_day_count)
               logged) );
        ( "avg_calories_kcal",
          json_option
            (fun (l : Summary.logged_intake) -> `Float l.avg_calories_kcal)
            logged );
        ( "avg_protein_g",
          json_option
            (fun (l : Summary.logged_intake) -> `Float l.avg_protein_g)
            logged );
        ( "calorie_target",
          json_option
            (fun (result : Summary.Calorie_target.t Summary.target_result) ->
              `Assoc
                [
                  ("target_kcal", `Int (result.target :> int));
                  ("days_at_or_under", `Int result.days_met);
                ])
            summary.calorie_target );
        ( "protein_target",
          json_option
            (fun (result : Summary.Protein_target.t Summary.target_result) ->
              `Assoc
                [
                  ("target_g", `Float (result.target :> float));
                  ("days_at_or_over", `Int result.days_met);
                ])
            summary.protein_target );
        ("weight_trend", weight_trend_json summary.weight_trend);
        ( "implied_maintenance",
          implied_maintenance_json summary.implied_maintenance );
      ])

let weight_json (weight : Weigh_in.t) =
  `Assoc
    [
      ("id", `String (Weigh_in_id.to_string weight.id));
      ("measured_at", `String (Time.to_utc_string weight.measured_at));
      ("weight_kg", `Float weight.weight_kg);
      ("source", `String (Weigh_in.source_to_string weight.source));
      ("created_at", `String (Time.to_utc_string weight.created_at));
      ("updated_at", `String (Time.to_utc_string weight.updated_at));
    ]

let complete fields = `Assoc (("resultType", `String "complete") :: fields)

let cacheable fields =
  complete
    (("ttlMs", `Int 300_000) :: ("cacheScope", `String "public") :: fields)

let text_result value =
  complete
    [
      ( "content",
        `List
          [
            `Assoc
              [
                ("type", `String "text");
                ("text", `String (Yojson.Safe.to_string value));
              ];
          ] );
      ("structuredContent", value);
    ]

let tool_error message =
  complete
    [
      ( "content",
        `List [ `Assoc [ ("type", `String "text"); ("text", `String message) ] ]
      );
      ("isError", `Bool true);
    ]

let schema properties required =
  `Assoc
    [
      ("type", `String "object");
      ("properties", `Assoc properties);
      ("required", `List (List.map (fun name -> `String name) required));
      ("additionalProperties", `Bool false);
    ]

let string = `Assoc [ ("type", `String "string") ]
let integer = `Assoc [ ("type", `String "integer") ]
let number = `Assoc [ ("type", `String "number") ]

let describe text = function
  | `Assoc fields -> `Assoc (fields @ [ ("description", `String text) ])
  | other -> other

let property name text field_schema = (name, describe text field_schema)

let meal_properties =
  [
    property "description" "Free-text description of what was eaten." string;
    property "calories_kcal" "Energy in kilocalories." integer;
    property "protein_g" "Protein in grams." number;
    property "carbs_g" "Carbohydrates in grams, if known." number;
    property "fat_g" "Fat in grams, if known." number;
    property "confidence"
      "Estimate confidence from 0.0 to 1.0, if the macros were estimated \
       rather than measured."
      number;
    property "estimate_source"
      "Where the estimate came from (e.g. a nutrition label or an AI \
       estimate), if applicable."
      string;
    property "notes" "Freeform notes." string;
    property "eaten_at"
      "When the meal was eaten, as an RFC 3339 UTC timestamp (e.g. \
       2026-09-22T12:30:00Z). Defaults to now if omitted."
      string;
  ]

let weight_properties =
  [
    property "weight_kg" "Body weight in kilograms." number;
    property "measured_at"
      "When the weigh-in was taken, as an RFC 3339 UTC timestamp. Defaults to \
       now if omitted."
      string;
  ]

let id_properties =
  [ property "id" "The record's id, as returned by a previous call." string ]

let query_properties =
  [
    property "from"
      "Inclusive lower bound as an RFC 3339 UTC timestamp (e.g. \
       2026-09-22T00:00:00Z for the start of that day)."
      string;
    property "to" "Inclusive upper bound as an RFC 3339 UTC timestamp." string;
    property "limit"
      "Maximum number of records to return (default 100, max 500). Without \
       text search, results are ordered oldest first, so once you have more \
       history than limit, pass from/to to scope the query instead of relying \
       on limit alone. For a single day's totals, prefer get_daily_totals; for \
       the current weight, prefer get_latest_weight."
      integer;
  ]

let matching_description =
  "Plain-text string or nonempty array of alternatives. Words are OR-matched \
   in any order and ranked by lexical relevance (BM25), with Swedish case \
   folding and distinct diacritics. No translation, stemming or prefix \
   matching. Blank or ASCII-tokenless alternatives are rejected."

let search_input =
  `Assoc
    [
      ( "oneOf",
        `List
          [
            string;
            `Assoc
              [
                ("type", `String "array");
                ("items", string);
                ("minItems", `Int 1);
              ];
          ] );
    ]

let meal_query_properties =
  query_properties
  @ [
      property "search"
        ("Match description, estimate_source or notes before date bounds and \
          limit. " ^ matching_description)
        search_input;
    ]

let product_properties =
  [
    property "name" "Nonblank name of a packaged food or recurring dish." string;
    property "basis"
      "Nonblank text amount the nutrition describes, e.g. 40 g or one portion. \
       No unit conversion is performed."
      string;
    property "calories_kcal"
      "Nonnegative energy in kilocalories for the stated basis." integer;
    property "protein_g" "Nonnegative protein in grams for the stated basis."
      number;
    property "source"
      "Nonblank free-text provenance, e.g. label or estimated from strawberry \
       flavour."
      string;
    property "carbs_g"
      "Nonnegative carbohydrates for the basis, if known; omission on \
       replacement clears to null."
      number;
    property "fat_g"
      "Nonnegative fat for the basis, if known; omission on replacement clears \
       to null."
      number;
  ]

let product_search_properties =
  [
    property "query"
      ("Match name, basis or source. " ^ matching_description)
      search_input;
    property "limit"
      "Maximum references returned (default 100, range 1–500), ranked by \
       relevance, then name and id."
      integer;
  ]

let timezone_property =
  property "timezone"
    ("Time zone that defines where calendar days begin and end: a fixed UTC \
      offset such as +02:00 or -05:00, or one of these names (EU summer time \
      applied): "
    ^ String.concat ", " Time.supported_zone_names
    ^ ". Defaults to UTC; pass the user's zone so late-evening meals land on \
       the right day.")
    string

let daily_totals_properties =
  [
    property "date"
      "Calendar date as YYYY-MM-DD in the given timezone. Defaults to today \
       there."
      string;
    timezone_property;
  ]

let range_properties =
  [
    property "from_date" "First calendar date as YYYY-MM-DD, inclusive." string;
    property "to_date"
      (Printf.sprintf
         "Last calendar date as YYYY-MM-DD, inclusive. Defaults to today in \
          the given timezone. The range may cover at most %d days."
         Date_range.max_day_count)
      string;
    timezone_property;
  ]

let summary_properties =
  range_properties
  @ [
      property "calorie_target_kcal"
        "Daily calorie goal in kcal (positive). When given, the result counts \
         logged days at or under it."
        integer;
      property "protein_target_g"
        "Daily protein goal in grams (positive). When given, the result counts \
         logged days at or over it."
        number;
    ]

let tool ?description name input_schema =
  let fields = [ ("name", `String name); ("inputSchema", input_schema) ] in
  `Assoc
    (match description with
    | None -> fields
    | Some text -> fields @ [ ("description", `String text) ])

let withings_tools =
  [
    tool "begin_withings_connection"
      ~description:
        "Start linking a Withings account; returns an authorization_url to \
         open in a browser."
      (schema [] []);
    tool "get_withings_status"
      ~description:
        "Check whether a Withings account is connected and whether it needs \
         reauthorization."
      (schema [] []);
    tool "disconnect_withings"
      ~description:"Unlink the connected Withings account." (schema [] []);
  ]

let tools =
  [
    tool "save_product"
      ~description:
        "Save reusable nutrition for a text basis. Without id, create; with \
         id, full replacement of your reference, not a partial patch \
         (preserves id/created_at). Unknown or foreign ids are not found. The \
         agent scales values and records meals normally; saving never rewrites \
         meals."
      (schema
         (product_properties @ id_properties)
         [ "name"; "basis"; "calories_kcal"; "protein_g"; "source" ]);
    tool "search_products"
      ~description:
        ("Find your reusable nutrition references, ranked by relevance, then \
          name and id. Scale values in the agent; references are independent \
          of meals. " ^ matching_description)
      (schema product_search_properties [ "query" ]);
    tool "record_meal" ~description:"Record a new meal in the nutrition ledger."
      (schema meal_properties [ "description"; "calories_kcal"; "protein_g" ]);
    tool "get_meal" ~description:"Fetch a single meal by id."
      (schema id_properties [ "id" ]);
    tool "query_meals"
      ~description:
        "List meals, ranked by relevance when searching and oldest first \
         otherwise, optionally bounded by from/to and filtered by search. For \
         a day's totals, prefer get_daily_totals; for intake over several \
         days, get_daily_series or get_summary. Both aggregate server-side \
         instead of requiring client-side summation."
      (schema meal_query_properties []);
    tool "update_meal"
      ~description:
        "Update fields on an existing meal; omitted fields are left unchanged."
      (schema (id_properties @ meal_properties) [ "id" ]);
    tool "delete_meal" ~description:"Delete a meal."
      (schema id_properties [ "id" ]);
    tool "get_daily_totals"
      ~description:
        "One calendar day's intake and weight: date, meal_count, \
         calories_kcal, protein_g, carbs_g, fat_g, weigh_in_count, \
         mean_weight_kg. On a day with no meals, meal_count is 0 and the \
         intake fields are null (unknown, not zero); mean_weight_kg is null \
         without a weigh-in. Prefer this over query_meals plus client-side \
         summation."
      (schema daily_totals_properties []);
    tool "get_daily_series"
      ~description:
        "The get_daily_totals row (date, meal_count, calories_kcal, protein_g, \
         carbs_g, fat_g, weigh_in_count, mean_weight_kg) for every calendar \
         date in a range, oldest first, in one call, including days with no \
         meals or weigh-ins. Use it to see intake against weight over time, \
         instead of calling get_daily_totals per day or paging \
         query_meals/query_weights."
      (schema range_properties [ "from_date" ]);
    tool "get_summary"
      ~description:
        "Analytics over a date range in one call. avg_calories_kcal and \
         avg_protein_g average only the logged days (logged_day_count of \
         day_count). calorie_target/protein_target, when requested, count days \
         meeting the goal. weight_trend is null without weigh-ins, kind \
         single_reading for one, or kind trend with first_kg, last_kg, \
         change_kg, span_days and a least-squares slope_kg_per_week. \
         implied_maintenance.kcal is average intake minus the energy in the \
         weight trend (7700 kcal/kg); when it is null, unavailable_reason is \
         no_intake_logged, no_weight_trend or weigh_in_span_too_short (needs 7 \
         days between first and last weigh-in). It is only as reliable as the \
         logging is complete."
      (schema summary_properties [ "from_date" ]);
    tool "record_weight" ~description:"Record a manual weigh-in."
      (schema weight_properties [ "weight_kg" ]);
    tool "get_weight" ~description:"Fetch a single weigh-in by id."
      (schema id_properties [ "id" ]);
    tool "query_weights"
      ~description:
        "List weigh-ins, oldest first, optionally bounded by from/to. For the \
         current weight, prefer get_latest_weight; for weight over time, \
         get_daily_series (daily mean weight) or get_summary (trend and \
         slope)."
      (schema query_properties []);
    tool "update_weight"
      ~description:
        "Update a manual weigh-in; omitted fields are left unchanged."
      (schema (id_properties @ weight_properties) [ "id" ]);
    tool "delete_weight" ~description:"Delete a weigh-in."
      (schema id_properties [ "id" ]);
    tool "get_latest_weight"
      ~description:
        "Fetch the single most recent weigh-in (manual or Withings-imported), \
         if any."
      (schema [] []);
  ]
  @ withings_tools

let meal_create fields : (Meal.Create.t, Error.t) result =
  let* fields = only_fields (List.map fst meal_properties) (`Assoc fields) in
  let* description = required_string "description" fields in
  let* calories_kcal = required_int "calories_kcal" fields in
  let* protein_g = required_float "protein_g" fields in
  let* carbs_g = optional_float "carbs_g" fields in
  let* fat_g = optional_float "fat_g" fields in
  let* confidence = optional_float "confidence" fields in
  let* estimate_source = optional_string "estimate_source" fields in
  let* notes = optional_string "notes" fields in
  let* eaten_at = optional_time "eaten_at" fields in
  Meal.Create.of_raw
    ({
       description;
       calories_kcal;
       protein_g;
       carbs_g;
       fat_g;
       confidence;
       estimate_source;
       notes;
       eaten_at;
     }
      : Meal.create)

let meal_patch fields : (Meal.Patch.t, Error.t) result =
  let* description = optional_string "description" fields in
  let* calories_kcal = optional_int "calories_kcal" fields in
  let* protein_g = optional_float "protein_g" fields in
  let* carbs_g = optional_float "carbs_g" fields in
  let* fat_g = optional_float "fat_g" fields in
  let* confidence = optional_float "confidence" fields in
  let* estimate_source = optional_string "estimate_source" fields in
  let* notes = optional_string "notes" fields in
  let* eaten_at = optional_time "eaten_at" fields in
  Meal.Patch.of_raw
    ({
       description;
       calories_kcal;
       protein_g;
       carbs_g;
       fat_g;
       confidence;
       estimate_source;
       notes;
       eaten_at;
     }
      : Meal.patch)

let query fields =
  let* fields = only_fields (List.map fst query_properties) (`Assoc fields) in
  let* from = optional_time "from" fields in
  let* to_ = optional_time "to" fields in
  let* limit = optional_int "limit" fields in
  Ok (from, to_, Option.value limit ~default:100)

let now () = Option.get (Ptime.of_float_s (Unix.gettimeofday ()))

let parse_zone_argument fields =
  let* name = optional_string "timezone" fields in
  match name with None -> Ok Time.utc | Some value -> Time.parse_zone value

let parse_date_argument name fields =
  let* value = optional_string name fields in
  match value with
  | None -> Ok None
  | Some value -> Result.map Option.some (Date.of_string value)

type range_request = { zone : Time.zone; range : Date_range.t }

let parse_day_request fields =
  let* fields =
    only_fields (List.map fst daily_totals_properties) (`Assoc fields)
  in
  let* zone = parse_zone_argument fields in
  let* date = parse_date_argument "date" fields in
  let* date =
    match date with
    | Some date -> Ok date
    | None -> Time.local_date zone (now ())
  in
  let* range = Date_range.make ~first:date ~last:date in
  Ok { zone; range }

let parse_range fields =
  let* zone = parse_zone_argument fields in
  let* first =
    Result.bind (required_string "from_date" fields) Date.of_string
  in
  let* last = parse_date_argument "to_date" fields in
  let* last =
    match last with
    | Some date -> Ok date
    | None -> Time.local_date zone (now ())
  in
  let* range = Date_range.make ~first ~last in
  Ok { zone; range }

let parse_range_request fields =
  let* fields = only_fields (List.map fst range_properties) (`Assoc fields) in
  parse_range fields

type summary_request = {
  range_request : range_request;
  calorie_target : Summary.Calorie_target.t option;
  protein_target : Summary.Protein_target.t option;
}

let parse_target name of_value parse fields =
  let* value = parse name fields in
  match value with
  | None -> Ok None
  | Some value -> Result.map Option.some (of_value value)

let parse_summary_request fields =
  let* fields = only_fields (List.map fst summary_properties) (`Assoc fields) in
  let* range_request = parse_range fields in
  let* calorie_target =
    parse_target "calorie_target_kcal" Summary.Calorie_target.of_kcal
      optional_int fields
  in
  let* protein_target =
    parse_target "protein_target_g" Summary.Protein_target.of_g optional_float
      fields
  in
  Ok { range_request; calorie_target; protein_target }

type withings = {
  oauth : Withings_oauth.t;
  client_id : string;
  redirect_uri : string;
}

let withings_status_json = function
  | Withings_connection.Connected status ->
      `Assoc
        [
          ("connected", `Bool true);
          ("requires_reauthorization", `Bool status.requires_reauthorization);
          ( "token_expires_at",
            json_option
              (fun value -> `String (Time.to_utc_string value))
              status.token_expires_at );
        ]

let required_scope = function
  | "search_products" | "get_meal" | "query_meals" | "get_daily_totals"
  | "get_daily_series" | "get_summary" | "get_weight" | "query_weights"
  | "get_latest_weight" ->
      Some "ledger:read"
  | "save_product" | "record_meal" | "update_meal" | "delete_meal"
  | "record_weight" | "update_weight" | "delete_weight" ->
      Some "ledger:write"
  | "begin_withings_connection" | "get_withings_status" | "disconnect_withings"
    ->
      Some "withings:manage"
  | _ -> None

let withings_for_call = function
  | Some withings -> Ok withings
  | None -> Error (Error.Storage_error "Withings unavailable")

let call_begin_withings_connection withings ~user =
  let* withings = withings_for_call withings in
  Result.map
    (fun (_, url) ->
      text_result (`Assoc [ ("authorization_url", `String url) ]))
    (Withings_oauth.begin_authorization ~client_id:withings.client_id
       ~redirect_uri:withings.redirect_uri withings.oauth ~user)

let call_get_withings_status withings ~user =
  let* withings = withings_for_call withings in
  Result.map
    (fun status -> text_result (withings_status_json status))
    (Store_sqlite.get_withings_status withings.oauth.store ~user)

let call_disconnect_withings withings ~user =
  let* withings = withings_for_call withings in
  Result.map
    (fun () -> text_result (`Assoc [ ("disconnected", `Bool true) ]))
    (Store_sqlite.delete_withings_connection withings.oauth.store ~user)

let product_input fields : (Product.Create.t, Error.t) result =
  let* name = required_string "name" fields in
  let* basis = required_string "basis" fields in
  let* calories_kcal = required_int "calories_kcal" fields in
  let* protein_g = required_float "protein_g" fields in
  let* source = required_string "source" fields in
  let* carbs_g = optional_float "carbs_g" fields in
  let* fat_g = optional_float "fat_g" fields in
  Product.Create.of_raw
    Product.{ name; basis; calories_kcal; protein_g; source; carbs_g; fat_g }

let call_save_product service ~user fields =
  let* fields =
    only_fields ("id" :: List.map fst product_properties) (`Assoc fields)
  in
  let* value = optional_string "id" fields in
  let* product_id =
    match value with
    | None -> Ok None
    | Some value -> Result.map Option.some (id value Product_id.of_string)
  in
  let* input = product_input fields in
  Result.map
    (fun product -> text_result (product_json product))
    (Service.save_product service ~user ~id:product_id input)

let parse_search service = function
  | `String text -> Service.compile_search_query service [ text ]
  | `List values ->
      let rec strings = function
        | [] -> Ok []
        | `String text :: rest ->
            let* rest = strings rest in
            Ok (text :: rest)
        | _ -> Error (Error.Invalid_input "search alternatives must be strings")
      in
      let* alternatives = strings values in
      Service.compile_search_query service alternatives
  | _ ->
      Error
        (Error.Invalid_input
           "search must be a string or nonempty array of strings")

let call_search_products service ~user fields =
  let* fields =
    only_fields (List.map fst product_search_properties) (`Assoc fields)
  in
  let* query =
    match List.assoc_opt "query" fields with
    | Some value -> parse_search service value
    | None -> Error (Error.Invalid_input "query is required")
  in
  let* limit = optional_int "limit" fields in
  Result.map
    (fun products ->
      text_result
        (`Assoc [ ("products", `List (List.map product_json products)) ]))
    (Service.search_products service ~user ~query
       ~limit:(Option.value limit ~default:100))

let call_record_meal service ~user fields =
  let* input = meal_create fields in
  Result.map
    (fun meal -> text_result (meal_json meal))
    (Service.record_meal service ~user input)

let call_get_meal service ~user fields =
  let* fields = only_fields [ "id" ] (`Assoc fields) in
  let* value = required_string "id" fields in
  let* meal_id = id value Meal_id.of_string in
  Result.map
    (fun meal -> text_result (meal_json meal))
    (Service.get_meal service ~user meal_id)

let call_query_meals service ~user fields =
  let* fields =
    only_fields (List.map fst meal_query_properties) (`Assoc fields)
  in
  let* search =
    match List.assoc_opt "search" fields with
    | None -> Ok None
    | Some value -> Result.map Option.some (parse_search service value)
  in
  let* from, to_, limit = query (List.remove_assoc "search" fields) in
  Result.map
    (fun meals ->
      text_result (`Assoc [ ("meals", `List (List.map meal_json meals)) ]))
    (Service.query_meals ?search service ~user ~from ~to_ ~limit)

let records_in_range service ~user { zone; range } =
  let* start, stop = Date_range.utc_bounds zone range in
  let* meals = Service.meals_between service ~user ~start ~stop in
  let* weigh_ins = Service.weigh_ins_between service ~user ~start ~stop in
  Ok (meals, weigh_ins)

let daily_series service ~user ({ zone; range } as request) =
  let* meals, weigh_ins = records_in_range service ~user request in
  Summary.daily_series zone range ~meals ~weigh_ins

let call_get_daily_totals service ~user fields =
  let* request = parse_day_request fields in
  let* series = daily_series service ~user request in
  match series with
  | [ day ] -> Ok (text_result (day_json day))
  | _ ->
      invalid_arg
        "Mcp.call_get_daily_totals: a one-day range must yield exactly one day"

let call_get_daily_series service ~user fields =
  let* request = parse_range_request fields in
  let* series = daily_series service ~user request in
  Ok
    (text_result
       (`Assoc
          (range_json request.zone request.range
          @ [ ("days", `List (List.map day_json series)) ])))

let call_get_summary service ~user fields =
  let* { range_request; calorie_target; protein_target } =
    parse_summary_request fields
  in
  let* meals, weigh_ins = records_in_range service ~user range_request in
  let { zone; range } = range_request in
  let* summary =
    Summary.summarise ?calorie_target ?protein_target zone range ~meals
      ~weigh_ins
  in
  Ok (text_result (summary_json zone range summary))

let call_update_meal service ~user fields =
  let* fields =
    only_fields ("id" :: List.map fst meal_properties) (`Assoc fields)
  in
  let* value = required_string "id" fields in
  let* meal_id = id value Meal_id.of_string in
  let* patch = meal_patch fields in
  Result.map
    (fun meal -> text_result (meal_json meal))
    (Service.update_meal service ~user meal_id patch)

let call_delete_meal service ~user fields =
  let* fields = only_fields [ "id" ] (`Assoc fields) in
  let* value = required_string "id" fields in
  let* meal_id = id value Meal_id.of_string in
  Result.map
    (fun () -> text_result (`Assoc [ ("deleted", `Bool true) ]))
    (Service.delete_meal service ~user meal_id)

let call_record_weight service ~user fields =
  let* fields = only_fields (List.map fst weight_properties) (`Assoc fields) in
  let* weight_kg = required_float "weight_kg" fields in
  let* measured_at = optional_time "measured_at" fields in
  let* input = Weigh_in.Create.of_raw Weigh_in.{ measured_at; weight_kg } in
  Result.map
    (fun weight -> text_result (weight_json weight))
    (Service.record_manual_weigh_in service ~user input)

let call_get_weight service ~user fields =
  let* fields = only_fields [ "id" ] (`Assoc fields) in
  let* value = required_string "id" fields in
  let* weight_id = id value Weigh_in_id.of_string in
  Result.map
    (fun weight -> text_result (weight_json weight))
    (Service.get_weigh_in service ~user weight_id)

let call_query_weights service ~user fields =
  let* from, to_, limit = query fields in
  Result.map
    (fun weights ->
      text_result (`Assoc [ ("weights", `List (List.map weight_json weights)) ]))
    (Service.query_weigh_ins service ~user ~from ~to_ ~limit)

let call_get_latest_weight service ~user =
  Result.map
    (fun weight -> text_result (weight_json weight))
    (Service.latest_weigh_in service ~user)

let call_update_weight service ~user fields =
  let* fields =
    only_fields ("id" :: List.map fst weight_properties) (`Assoc fields)
  in
  let* value = required_string "id" fields in
  let* weight_id = id value Weigh_in_id.of_string in
  let* measured_at = optional_time "measured_at" fields in
  let* weight_kg = optional_float "weight_kg" fields in
  let* patch = Weigh_in.Patch.of_raw Weigh_in.{ measured_at; weight_kg } in
  Result.map
    (fun weight -> text_result (weight_json weight))
    (Service.update_manual_weigh_in service ~user weight_id patch)

let call_delete_weight service ~user fields =
  let* fields = only_fields [ "id" ] (`Assoc fields) in
  let* value = required_string "id" fields in
  let* weight_id = id value Weigh_in_id.of_string in
  Result.map
    (fun () -> text_result (`Assoc [ ("deleted", `Bool true) ]))
    (Service.delete_weigh_in service ~user weight_id)

let call ?withings service (identity : Auth.identity) name arguments =
  match arguments with
  | `Assoc fields -> (
      let user = identity.user in
      match name with
      | "begin_withings_connection" ->
          call_begin_withings_connection withings ~user
      | "get_withings_status" -> call_get_withings_status withings ~user
      | "disconnect_withings" -> call_disconnect_withings withings ~user
      | "save_product" -> call_save_product service ~user fields
      | "search_products" -> call_search_products service ~user fields
      | "record_meal" -> call_record_meal service ~user fields
      | "get_meal" -> call_get_meal service ~user fields
      | "query_meals" -> call_query_meals service ~user fields
      | "get_daily_totals" -> call_get_daily_totals service ~user fields
      | "get_daily_series" -> call_get_daily_series service ~user fields
      | "get_summary" -> call_get_summary service ~user fields
      | "update_meal" -> call_update_meal service ~user fields
      | "delete_meal" -> call_delete_meal service ~user fields
      | "record_weight" -> call_record_weight service ~user fields
      | "get_weight" -> call_get_weight service ~user fields
      | "query_weights" -> call_query_weights service ~user fields
      | "get_latest_weight" -> call_get_latest_weight service ~user
      | "update_weight" -> call_update_weight service ~user fields
      | "delete_weight" -> call_delete_weight service ~user fields
      | _ -> invalid ())
  | _ -> invalid ()

let response ?(id = `Null) result =
  `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ]

let rpc_error ?(id = `Null) code message =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", id);
      ("error", `Assoc [ ("code", `Int code); ("message", `String message) ]);
    ]

let stateless_protocol_version = "2026-07-28"
let legacy_protocol_version = "2025-03-26"

let discover_result =
  cacheable
    [
      ( "supportedVersions",
        `List
          [
            `String stateless_protocol_version; `String legacy_protocol_version;
          ] );
      ("capabilities", `Assoc [ ("tools", `Assoc []) ]);
      ( "_meta",
        `Assoc
          [
            ( "io.modelcontextprotocol/serverInfo",
              `Assoc [ ("name", `String "kcal"); ("version", `String "dev") ] );
          ] );
    ]

(* Argument errors the model can fix itself come back as tool results, which
   MCP clients show to the model, rather than as protocol errors. *)
let argument_error_message = function
  | Error.Unsupported_timezone _ as error ->
      Error.to_string error
      ^ ": use a fixed UTC offset such as +02:00, or one of "
      ^ String.concat ", " Time.supported_zone_names
  | error -> Error.to_string error

let call_response ~request_id = function
  | Ok result -> response ~id:request_id result
  | Error Error.Not_found ->
      response ~id:request_id (tool_error "record not found")
  | Error
      (( Error.Calendar_date_addition _ | Error.Calendar_day_bounds _
       | Error.Calendar_local_date _ | Error.Invalid_date _
       | Error.Unsupported_timezone _ | Error.Reversed_date_range _
       | Error.Date_range_too_long _ | Error.Nonpositive_argument _ ) as error)
    ->
      response ~id:request_id (tool_error (argument_error_message error))
  | Error
      (Error.Invalid_field
         { field = Numeric (Calorie_target_kcal | Protein_target_g); _ } as
       error) ->
      response ~id:request_id (tool_error (Error.to_string error))
  | Error (Error.Invalid_input _ | Error.Invalid_field _) ->
      rpc_error ~id:request_id (-32602) "Invalid params"
  | Error ((Error.Numeric_overflow _ | Error.Unrepresentable_result _) as error)
    ->
      response ~id:request_id (tool_error (Error.to_string error))
  | Error
      ( Error.Unauthorized | Error.Conflict _ | Error.Storage_error _
      | Error.Storage_decoding _ ) ->
      rpc_error ~id:request_id (-32603) "Internal error"

let handle ?withings ~service ~identity request =
  match request with
  | `Assoc fields -> (
      let request_id =
        Option.value (List.assoc_opt "id" fields) ~default:`Null
      in
      let valid_request =
        List.assoc_opt "jsonrpc" fields = Some (`String "2.0")
      in
      match List.assoc_opt "method" fields with
      | Some (`String "server/discover") when valid_request ->
          Ok (response ~id:request_id discover_result)
      | Some (`String "initialize") when valid_request -> (
          match List.assoc_opt "params" fields with
          | Some (`Assoc params)
            when List.assoc_opt "protocolVersion" params
                 = Some (`String legacy_protocol_version) ->
              Ok
                (response ~id:request_id
                   (`Assoc
                      [
                        ("protocolVersion", `String legacy_protocol_version);
                        ("capabilities", `Assoc [ ("tools", `Assoc []) ]);
                        ( "serverInfo",
                          `Assoc
                            [
                              ("name", `String "kcal");
                              ("version", `String "dev");
                            ] );
                      ]))
          | _ -> Ok (rpc_error ~id:request_id (-32602) "Invalid params"))
      | Some (`String "tools/list") when valid_request ->
          Ok (response ~id:request_id (cacheable [ ("tools", `List tools) ]))
      | Some (`String "tools/call") when valid_request -> (
          match List.assoc_opt "params" fields with
          | Some (`Assoc params) -> (
              match
                ( required_string "name" params,
                  List.assoc_opt "arguments" params )
              with
              | Ok name, Some arguments -> (
                  match required_scope name with
                  | Some scope when not (Auth.has_scope identity scope) ->
                      Error `Forbidden
                  | _ ->
                      Ok
                        (call_response ~request_id
                           (call ?withings service identity name arguments)))
              | ( Ok
                    (( "save_product" | "search_products" | "query_meals"
                     | "query_weights" | "get_daily_totals"
                     | "get_latest_weight" | "begin_withings_connection"
                     | "get_withings_status" | "disconnect_withings" ) as name),
                  None ) -> (
                  match required_scope name with
                  | Some scope when not (Auth.has_scope identity scope) ->
                      Error `Forbidden
                  | _ ->
                      Ok
                        (call_response ~request_id
                           (call ?withings service identity name (`Assoc []))))
              | _ -> Ok (rpc_error ~id:request_id (-32602) "Invalid params"))
          | _ -> Ok (rpc_error ~id:request_id (-32602) "Invalid params"))
      | Some (`String _) when valid_request ->
          Ok (rpc_error ~id:request_id (-32601) "Method not found")
      | _ -> Ok (rpc_error ~id:request_id (-32600) "Invalid Request"))
  | _ -> Ok (rpc_error (-32700) "Parse error")
