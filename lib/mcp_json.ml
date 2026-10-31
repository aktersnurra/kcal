open Base

let json_option f = function None -> `Null | Some value -> f value

let product_link_json =
  json_option (fun (link : Meal.product_link) ->
      `Assoc
        [
          ("product_id", `String (Product_id.to_string link.product_id));
          ("product_version", `Int link.product_version);
          ("quantity", `Float link.quantity);
        ])

let meal_fields ~eaten_at ~description ~calories_kcal ~protein_g ~carbs_g ~fat_g
    ~confidence ~estimate_source ~notes ~product =
  [
    ("eaten_at", `String (Time.to_utc_string eaten_at));
    ("description", `String description);
    ("calories_kcal", `Int calories_kcal);
    ("protein_g", `Float protein_g);
    ("carbs_g", json_option (fun v -> `Float v) carbs_g);
    ("fat_g", json_option (fun v -> `Float v) fat_g);
    ("confidence", json_option (fun v -> `Float v) confidence);
    ("estimate_source", json_option (fun v -> `String v) estimate_source);
    ("notes", json_option (fun v -> `String v) notes);
    ("product", product_link_json product);
  ]

let meal_json (meal : Meal.t) =
  `Assoc
    (("id", `String (Meal_id.to_string meal.id))
     :: meal_fields ~eaten_at:meal.eaten_at ~description:meal.description
          ~calories_kcal:meal.calories_kcal ~protein_g:meal.protein_g
          ~carbs_g:meal.carbs_g ~fat_g:meal.fat_g ~confidence:meal.confidence
          ~estimate_source:meal.estimate_source ~notes:meal.notes
          ~product:meal.product
    @ [
        ("created_at", `String (Time.to_utc_string meal.created_at));
        ("updated_at", `String (Time.to_utc_string meal.updated_at));
      ])

let revision_json (revision : Meal.revision) =
  let previous = revision.previous in
  `Assoc
    [
      ("revision", `Int revision.revision);
      ("revised_at", `String (Time.to_utc_string revision.revised_at));
      ("reason", json_option (fun v -> `String v) revision.reason);
      ( "previous",
        `Assoc
          (meal_fields ~eaten_at:previous.eaten_at
             ~description:previous.description
             ~calories_kcal:previous.calories_kcal ~protein_g:previous.protein_g
             ~carbs_g:previous.carbs_g ~fat_g:previous.fat_g
             ~confidence:previous.confidence
             ~estimate_source:previous.estimate_source ~notes:previous.notes
             ~product:previous.product) );
    ]

let meal_with_revisions_json meal revisions =
  match meal_json meal with
  | `Assoc fields ->
      `Assoc
        (fields @ [ ("revisions", `List (List.map revisions ~f:revision_json)) ])
  | json -> json

let product_json (product : Product.t) =
  `Assoc
    [
      ("id", `String (Product_id.to_string product.id));
      ("version", `Int product.version);
      ("name", `String product.name);
      ("basis", `String product.basis);
      ( "basis_quantity",
        json_option
          (fun (serving : Product.serving) -> `Float serving.quantity)
          product.serving );
      ( "basis_unit",
        json_option
          (fun (serving : Product.serving) -> `String serving.unit)
          product.serving );
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

let target_weight_json =
  json_option (fun (target : Summary.target_weight) ->
      `Assoc
        [
          ("target_kg", `Float target.target_kg);
          ("remaining_kg", `Float target.remaining_kg);
          ("weeks_to_target", json_option float_json target.weeks_to_target);
        ])

let summary_fields (summary : Summary.t) =
  let logged = summary.logged_intake in
  [
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
        (fun (c : Summary.calorie_adherence) ->
          `Assoc
            [
              ("days_at_or_under", `Int c.days_at_or_under);
              ("avg_vs_target_kcal", `Float c.avg_vs_target_kcal);
            ])
        summary.calorie_target );
    ( "protein_target",
      json_option
        (fun (p : Summary.protein_adherence) ->
          `Assoc [ ("days_at_or_over", `Int p.days_at_or_over) ])
        summary.protein_target );
    ( "eating_window",
      json_option
        (fun (w : Summary.window_adherence) ->
          `Assoc [ ("days_within", `Int w.days_within) ])
        summary.eating_window );
    ("weight_trend", weight_trend_json summary.weight_trend);
    ("target_weight", target_weight_json summary.target_weight);
    ("implied_maintenance", implied_maintenance_json summary.implied_maintenance);
  ]

let summary_json zone range (summary : Service.summary) =
  `Assoc
    (range_json zone range
    @ summary_fields summary.current
    @ [
        ( "previous_period",
          json_option
            (fun (range, summary) ->
              `Assoc
                ( range_json zone range
                |> List.filter ~f:(fun (name, _) ->
                    not (String.equal name "timezone"))
                |> fun fields -> fields @ summary_fields summary ))
            summary.previous );
      ])

let goals_json (goals : Goals.t) =
  let window = goals.eating_window in
  `Assoc
    [
      ("timezone", `String (Time.zone_name goals.timezone));
      ("weekday_calorie_target_kcal", `Int (goals.weekday_calories :> int));
      ("weekend_calorie_target_kcal", `Int (goals.weekend_calories :> int));
      ( "protein_target_g",
        json_option
          (fun (g : Goals.Protein_target.t) -> `Float (g :> float))
          goals.protein );
      ( "eating_window_start",
        json_option
          (fun (w : Goals.Eating_window.t) ->
            `String (Goals.Eating_window.to_string w.start_minute))
          window );
      ( "eating_window_end",
        json_option
          (fun (w : Goals.Eating_window.t) ->
            `String (Goals.Eating_window.to_string w.end_minute))
          window );
      ( "fasting_hours",
        json_option
          (fun w ->
            `Float
              (Float.of_int ((24 * 60) - Goals.Eating_window.duration_minutes w)
              /. 60.))
          window );
      ( "target_weight_kg",
        json_option
          (fun (kg : Nutrition.Kilograms.t) -> `Float (kg :> float))
          goals.target_weight_kg );
    ]

let hours_between earlier later =
  Ptime.Span.to_float_s (Ptime.diff later earlier) /. 3600.

let context_json (context : Context.t) =
  let today = context.today in
  let week_range, week = context.last_week in
  let month_range, month = context.last_four_weeks in
  let range_dates range =
    [
      ("from_date", `String (Date.to_string (Date_range.first range)));
      ("to_date", `String (Date.to_string (Date_range.last range)));
    ]
  in
  let totals f = json_option f today.totals in
  `Assoc
    [
      ("now", `String (Time.to_utc_string context.now));
      ("timezone", `String (Time.zone_name context.zone));
      ("goals", json_option goals_json context.goals);
      ( "today",
        `Assoc
          [
            ("date", `String (Date.to_string today.date));
            ( "meal_count",
              `Int
                (Option.value_map today.totals ~default:0
                   ~f:(fun (t : Meal.totals) -> t.meal_count)) );
            ( "calories_kcal",
              totals (fun (t : Meal.totals) -> `Int t.calories_kcal) );
            ("protein_g", totals (fun (t : Meal.totals) -> `Float t.protein_g));
            ( "calorie_target_kcal",
              json_option
                (fun (t : Goals.Calorie_target.t) -> `Int (t :> int))
                today.calorie_target );
            ( "remaining_kcal",
              json_option (fun v -> `Int v) today.remaining_kcal );
            ( "remaining_protein_g",
              json_option float_json today.remaining_protein_g );
          ] );
      ( "eating_window",
        json_option
          (fun (fasting : Context.fasting) ->
            let state =
              match fasting.status with
              | Goals.Eating_window.Open { closes_at } ->
                  [
                    ("open", `Bool true);
                    ("closes_at", `String (Time.to_utc_string closes_at));
                  ]
              | Closed { opens_at } ->
                  [
                    ("open", `Bool false);
                    ("opens_at", `String (Time.to_utc_string opens_at));
                  ]
            in
            `Assoc
              (state
              @ [
                  ( "last_meal_at",
                    json_option
                      (fun at -> `String (Time.to_utc_string at))
                      fasting.last_meal_at );
                  ( "hours_since_last_meal",
                    json_option
                      (fun at -> `Float (hours_between at context.now))
                      fasting.last_meal_at );
                ]))
          context.fasting );
      ("latest_weight", json_option weight_json context.latest_weight);
      ("last_7_days", `Assoc (range_dates week_range @ summary_fields week));
      ( "last_28_days",
        `Assoc
          (range_dates month_range
          @ [
              ("weight_trend", weight_trend_json month.weight_trend);
              ("target_weight", target_weight_json month.target_weight);
              ( "implied_maintenance",
                implied_maintenance_json month.implied_maintenance );
            ]) );
      ( "recent_products",
        `List
          (List.map context.recent_products ~f:(fun (product, last_eaten_at) ->
               match product_json product with
               | `Assoc fields ->
                   `Assoc
                     (fields
                     @ [
                         ( "last_eaten_at",
                           `String (Time.to_utc_string last_eaten_at) );
                       ])
               | json -> json)) );
    ]

let correction_json (correction : Service.product_correction) =
  let product = correction.product in
  `Assoc
    [
      ("product_id", `String (Product_id.to_string product.id));
      ("product_version", `Int product.version);
      ("applied", `Bool correction.applied);
      ( "meals",
        `List
          (List.map correction.meals
             ~f:(fun ((meal : Meal.t), (portion : Product.portion)) ->
               `Assoc
                 [
                   ("id", `String (Meal_id.to_string meal.id));
                   ("eaten_at", `String (Time.to_utc_string meal.eaten_at));
                   ( "from_version",
                     json_option
                       (fun (link : Meal.product_link) ->
                         `Int link.product_version)
                       meal.product );
                   ("calories_kcal_before", `Int meal.calories_kcal);
                   ("calories_kcal_after", `Int portion.calories_kcal);
                   ("protein_g_before", `Float meal.protein_g);
                   ("protein_g_after", `Float portion.protein_g);
                 ])) );
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
