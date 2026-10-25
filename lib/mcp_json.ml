open Base

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
          (Option.value_map ~default:0
             ~f:(fun (totals : Meal.totals) -> totals.meal_count)
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
          (Option.bind meals ~f:(fun (totals : Meal.totals) -> totals.carbs_g))
      );
      ( "fat_g",
        json_option float_json
          (Option.bind meals ~f:(fun (totals : Meal.totals) -> totals.fat_g)) );
      ( "weigh_in_count",
        `Int
          (Option.value_map ~default:0
             ~f:(fun (w : Summary.daily_weight) -> w.weigh_in_count)
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
            (Option.value_map ~default:0
               ~f:(fun (l : Summary.logged_intake) -> l.logged_day_count)
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

let withings_status_json = function
  | Withings_connection.Disconnected ->
      `Assoc
        [
          ("connected", `Bool false);
          ("requires_reauthorization", `Bool false);
          ("token_expires_at", `Null);
        ]
  | ( Withings_connection.Connected connection
    | Reauthorization_required connection ) as status ->
      let requires_reauthorization =
        match status with
        | Withings_connection.Reauthorization_required _ -> true
        | _ -> false
      in
      `Assoc
        [
          ("connected", `Bool true);
          ("requires_reauthorization", `Bool requires_reauthorization);
          ( "token_expires_at",
            `String (Time.to_utc_string connection.token_expires_at) );
        ]
