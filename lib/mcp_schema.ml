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
    ^ String.concat ~sep:", " Time.supported_zone_names
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
      (Stdlib.Printf.sprintf
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
