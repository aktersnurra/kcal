open Base

let schema properties required =
  `Assoc
    [
      ("type", `String "object");
      ("properties", `Assoc properties);
      ("required", `List (List.map ~f:(fun name -> `String name) required));
      ("additionalProperties", `Bool false);
    ]

let string = `Assoc [ ("type", `String "string") ]
let integer = `Assoc [ ("type", `String "integer") ]
let number = `Assoc [ ("type", `String "number") ]
let boolean = `Assoc [ ("type", `String "boolean") ]

let describe text = function
  | `Assoc fields -> `Assoc (fields @ [ ("description", `String text) ])
  | other -> other

let property name text field_schema = (name, describe text field_schema)

let nutrition_properties =
  [
    property "calories_kcal" "Energy in kilocalories." integer;
    property "protein_g" "Protein in grams." number;
    property "carbs_g" "Carbohydrates in grams, if known." number;
    property "fat_g" "Fat in grams, if known." number;
  ]

let provenance_properties =
  [
    property "confidence"
      "Estimate confidence from 0.0 to 1.0, if the macros were estimated \
       rather than measured."
      number;
    property "estimate_source"
      "Where the estimate came from (e.g. a nutrition label or an AI \
       estimate), if applicable."
      string;
    property "notes" "Freeform notes about this item." string;
  ]

let item_description_property =
  property "description" "Free-text description of what was eaten." string

(* Two disjoint shapes: product_id selects the second. *)
let item_schema =
  `Assoc
    [
      ( "oneOf",
        `List
          [
            schema
              ((item_description_property :: nutrition_properties)
              @ provenance_properties)
              [ "description"; "calories_kcal"; "protein_g" ];
            schema
              ([
                 property "product_id"
                   "A saved product: kcal and macros are scaled from its \
                    current version to quantity, and the item is linked to \
                    that version."
                   string;
                 property "quantity"
                   "The amount eaten in the product's basis_unit, e.g. 45 for \
                    45 g of a product saved per 40 g."
                   number;
                 property "description" "Defaults to the product name." string;
               ]
              @ provenance_properties)
              [ "product_id"; "quantity" ];
          ] );
    ]

let item_property =
  property "item"
    "One thing eaten: either description, calories_kcal and protein_g (plus \
     optional macros), or product_id and quantity."
    item_schema

let eaten_at_property =
  property "eaten_at"
    "When the meal was eaten, as an RFC 3339 UTC timestamp (e.g. \
     2026-09-22T12:30:00Z). Defaults to now if omitted."
    string

let label_property =
  property "label" "Optional name for the meal, e.g. lunch." string

let record_meal_properties =
  [
    property "items"
      "What was eaten, one entry per food: each either description, \
       calories_kcal and protein_g (plus optional macros), or product_id and \
       quantity. A plate estimated as a whole is one item."
      (`Assoc
         [
           ("type", `String "array");
           ("items", item_schema);
           ("minItems", `Int 1);
         ]);
    eaten_at_property;
    label_property;
    property "idempotency_key"
      "A new unique string for each meal you log. Retrying the same request \
       with the same key returns the meal already recorded, with replayed \
       true, instead of adding a duplicate; the same key with different \
       content is rejected."
      string;
  ]

let item_update_properties =
  [
    property "id" "The item's id, from the meal's items." string;
    item_description_property;
  ]
  @ nutrition_properties @ provenance_properties
  @ [
      property "quantity"
        "For an item logged from a product: the new amount in its unit. The \
         item is rescaled from its own values and stays linked; give it \
         instead of nutrition values."
        number;
      property "reason"
        "Why the estimate changed, e.g. portion was 300 g, not 200 g." string;
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
       on limit alone. For today, prefer get_context; for daily totals, \
       get_daily_series."
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
        ("Match meals with an item whose description, estimate_source or notes \
          match, before date bounds and limit; a meal ranks by its best item. "
       ^ matching_description)
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
    property "basis_quantity"
      "The basis as a positive number, e.g. 40 for 40 g; with basis_unit it \
       lets record_meal scale a quantity. Omission on replacement clears it."
      number;
    property "basis_unit" "The unit of basis_quantity, e.g. g, ml or portion."
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
    ^ String.concat ~sep:", " Time.supported_zone_names
    ^ ". Defaults to the timezone in set_goals, else UTC.")
    string

let range_properties =
  [
    property "from_date" "First calendar date as YYYY-MM-DD, inclusive." string;
    property "to_date"
      (Stdlib.Printf.sprintf
         "Last calendar date as YYYY-MM-DD, inclusive. Defaults to today in \
          the given timezone. The range may cover at most %d days."
         Date_range.max_day_count)
      string;
    timezone_property;
  ]

let goals_properties =
  [
    property "timezone"
      ("The user's time zone, used for every calendar day: a fixed UTC offset \
        or one of "
      ^ String.concat ~sep:", " Time.supported_zone_names
      ^ ".")
      string;
    property "weekday_calorie_target_kcal"
      "Daily calorie target Monday through Friday (positive)." integer;
    property "weekend_calorie_target_kcal"
      "Daily calorie target on Saturday and Sunday (positive)." integer;
    property "protein_target_g" "Daily protein target in grams, if any." number;
    property "eating_window_start"
      "Local HH:MM the eating window opens, e.g. 12:00 for 16:8. Give with \
       eating_window_end, or omit both for no window."
      string;
    property "eating_window_end"
      "Local HH:MM the eating window closes, e.g. 20:00. An end before the \
       start crosses midnight."
      string;
    property "target_weight_kg" "Goal body weight in kilograms, if any." number;
    property "notes"
      (Printf.sprintf
         "The user's own context for these goals, in their words: why, how \
          strict, what matters most (e.g. losing about 0.5 kg a week before \
          summer; weekends are flexible but protein matters every day). Read \
          it back from get_context before advising. At most %d characters."
         Goals.max_notes_length)
      string;
  ]

let correction_properties =
  [
    property "product_id" "The product whose current version is correct." string;
    property "reason"
      "Why the earlier values were wrong, e.g. label misread: 160 kcal per 40 \
       g, not 140. Stored on every changed meal's revision."
      string;
    property "dry_run"
      "Default true: only list the meals and their before and after values. \
       Pass false to apply."
      boolean;
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
    tool "get_context"
      ~description:
        "Start here. One call with the user's goals, today's intake against \
         today's weekday or weekend target (remaining_kcal, \
         remaining_protein_g), the eating window (open, closes_at or opens_at, \
         hours_since_last_meal), the latest weight, the last 7 complete days \
         against targets, the last 28 days' weight trend, progress to the \
         target weight and implied maintenance, and recently eaten products."
      (schema [] []);
    tool "set_goals"
      ~description:
        "Replace the user's goals; omitted optional fields are cleared. \
         get_context, get_summary and the calendar tools judge and bucket days \
         by them."
      (schema goals_properties
         [
           "timezone";
           "weekday_calorie_target_kcal";
           "weekend_calorie_target_kcal";
         ]);
    tool "save_product"
      ~description:
        "Save reusable nutrition for a basis. Without id, create at version 1; \
         with id, full replacement (not a partial patch) that increments \
         version and preserves id/created_at. Unknown or foreign ids are not \
         found. Saving never rewrites meals: meals keep the values they were \
         logged with. To fix meals logged from a wrong earlier version, use \
         correct_meals_from_product."
      (schema
         (product_properties @ id_properties)
         [ "name"; "basis"; "calories_kcal"; "protein_g"; "source" ]);
    tool "search_products"
      ~description:
        ("Find your reusable nutrition references, ranked by relevance, then \
          name and id. To log one, pass its id and the quantity eaten as a \
          record_meal item; the server scales it and links the item. "
       ^ matching_description)
      (schema product_search_properties [ "query" ]);
    tool "correct_meals_from_product"
      ~description:
        "After save_product fixed a product, rescale the meal items logged \
         from its earlier versions to the current one, by their logged \
         quantity. Each changed item keeps a revision with the old values and \
         reason. Items logged in another unit than the current version's are \
         listed under skipped and left alone. Use only when the earlier values \
         were wrong; a changed recipe should leave old meals alone."
      (schema correction_properties [ "product_id"; "reason" ]);
    tool "record_meal"
      ~description:
        "Record a meal: one occasion of eating with one or more items. The \
         meal's calories and macros are the sums of its items."
      (schema record_meal_properties [ "items" ]);
    tool "get_meal"
      ~description:
        "Fetch a meal by id, with each item's revisions: earlier versions of \
         its estimate, when it changed and why."
      (schema id_properties [ "id" ]);
    tool "query_meals"
      ~description:
        "List meals, ranked by relevance when searching and oldest first \
         otherwise, optionally bounded by from/to and filtered by search. For \
         today, prefer get_context; for intake over several days, \
         get_daily_series or get_summary. Both aggregate server-side instead \
         of requiring client-side summation."
      (schema meal_query_properties []);
    tool "update_meal"
      ~description:
        "Change when a meal was eaten or its label; give at least one, and \
         omitted fields are left unchanged. To change what was eaten, use the \
         item tools."
      (schema (id_properties @ [ eaten_at_property; label_property ]) [ "id" ]);
    tool "delete_meal" ~description:"Delete a meal and its items."
      (schema id_properties [ "id" ]);
    tool "add_meal_item" ~description:"Add something eaten to an existing meal."
      (schema
         [ property "meal_id" "The meal's id." string; item_property ]
         [ "meal_id"; "item" ]);
    tool "update_meal_item"
      ~description:
        "Correct an item's estimate; give at least one field, and omitted \
         fields are left unchanged. The previous values are kept as a revision \
         with the reason. For an item from a product, give quantity to change \
         the amount; changing calories or a macro by hand detaches the item \
         from its product."
      (schema item_update_properties [ "id" ]);
    tool "delete_meal_item"
      ~description:
        "Remove an item from its meal. A meal keeps at least one item; delete \
         the meal instead. A removed item and its revisions are no longer \
         returned."
      (schema
         [ property "id" "The item's id, from the meal's items." string ]
         [ "id" ]);
    tool "get_daily_series"
      ~description:
        "One row (date, meal_count, calories_kcal, protein_g, carbs_g, fat_g, \
         weigh_in_count, mean_weight_kg) for every calendar date in a range, \
         oldest first, including days with no meals or weigh-ins; on those, \
         intake fields are null (unknown, not zero). Use it to see intake \
         against weight over time instead of paging query_meals/query_weights."
      (schema range_properties [ "from_date" ]);
    tool "get_summary"
      ~description:
        "Analytics over a date range in one call, with the same metrics for \
         the equally long previous_period. avg_calories_kcal and avg_protein_g \
         average only the logged days (logged_day_count of day_count). With \
         goals set, calorie_target counts days_at_or_under each day's weekday \
         or weekend target and gives avg_vs_target_kcal; protein_target counts \
         days_at_or_over; eating_window counts days_within, whose meals all \
         fell in the window; target_weight gives remaining_kg and \
         weeks_to_target at the current slope. weight_trend is null without \
         weigh-ins, kind single_reading for one, or kind trend with first_kg, \
         last_kg, change_kg, span_days and a least-squares slope_kg_per_week. \
         implied_maintenance.kcal is average intake minus the energy in the \
         weight trend (7700 kcal/kg); when it is null, unavailable_reason is \
         no_intake_logged, no_weight_trend or weigh_in_span_too_short (needs 7 \
         days between first and last weigh-in). It is only as reliable as the \
         logging is complete."
      (schema range_properties [ "from_date" ]);
    tool "record_weight" ~description:"Record a manual weigh-in."
      (schema weight_properties [ "weight_kg" ]);
    tool "get_weight" ~description:"Fetch a single weigh-in by id."
      (schema id_properties [ "id" ]);
    tool "query_weights"
      ~description:
        "List weigh-ins, oldest first, optionally bounded by from/to. For the \
         current weight, prefer get_context; for weight over time, \
         get_daily_series (daily mean weight) or get_summary (trend and \
         slope)."
      (schema query_properties []);
    tool "update_weight"
      ~description:
        "Update a manual weigh-in; omitted fields are left unchanged."
      (schema (id_properties @ weight_properties) [ "id" ]);
    tool "delete_weight" ~description:"Delete a weigh-in."
      (schema id_properties [ "id" ]);
  ]
  @ withings_tools
