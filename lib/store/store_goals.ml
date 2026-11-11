module Domain_error = Error
open Base
module Error = Domain_error
open Sqlite_db

let goals_columns =
  "timezone, weekday_calorie_target_kcal, weekend_calorie_target_kcal, \
   protein_target_g, eating_window_start_minute, eating_window_end_minute, \
   target_weight_kg, notes"

let goals_of_row statement =
  let decode () =
    let* timezone = Time.parse_zone (text statement 0) in
    let* weekday_calories = Goals.Calorie_target.of_kcal (int statement 1) in
    let* weekend_calories = Goals.Calorie_target.of_kcal (int statement 2) in
    let optional f column =
      if Sqlite3.column_is_null statement column then Ok None
      else Result.map (f column) ~f:Option.some
    in
    let* protein =
      optional
        (fun column -> Goals.Protein_target.of_g (float statement column))
        3
    in
    let* eating_window =
      optional
        (fun column ->
          Goals.Eating_window.of_minutes ~start_minute:(int statement column)
            ~end_minute:(int statement (column + 1)))
        4
    in
    let* target_weight_kg =
      optional
        (fun column -> Nutrition.Kilograms.of_float (float statement column))
        6
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
          notes = optional_text statement 7;
        }
  in
  if
    calorie_column statement 1 && calorie_column statement 2
    && List.for_all [ 3; 4; 5; 6 ] ~f:(optional_numeric_column statement)
  then Result.ok (decode ())
  else None

let get_goals db ~user =
  query_one db
    ("SELECT " ^ goals_columns ^ " FROM goals WHERE user_id = ?")
    [ Store_rows.owner user ]
    goals_of_row

let set_goals db ~user (goals : Goals.t) =
  let minute f = nullable_int (Option.map goals.eating_window ~f) in
  query_one db
    ("INSERT INTO goals (user_id, " ^ goals_columns
   ^ ", updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(user_id) \
      DO UPDATE SET timezone = excluded.timezone, weekday_calorie_target_kcal \
      = excluded.weekday_calorie_target_kcal, weekend_calorie_target_kcal = \
      excluded.weekend_calorie_target_kcal, protein_target_g = \
      excluded.protein_target_g, eating_window_start_minute = \
      excluded.eating_window_start_minute, eating_window_end_minute = \
      excluded.eating_window_end_minute, target_weight_kg = \
      excluded.target_weight_kg, notes = excluded.notes, updated_at = \
      excluded.updated_at RETURNING " ^ goals_columns)
    [
      Store_rows.owner user;
      text_value (Time.zone_name goals.timezone);
      int_value (goals.weekday_calories :> int);
      int_value (goals.weekend_calories :> int);
      nullable_float (Option.map goals.protein ~f:(fun g -> (g :> float)));
      minute (fun (w : Goals.Eating_window.t) -> w.start_minute);
      minute (fun (w : Goals.Eating_window.t) -> w.end_minute);
      nullable_float
        (Option.map goals.target_weight_kg ~f:(fun kg -> (kg :> float)));
      nullable goals.notes;
      time_value (now ());
    ]
    goals_of_row

(* Read and write in one transaction, so a concurrent set_goals cannot land
   between them and be silently overwritten by the merge. *)
let update_goals db ~user f =
  in_transaction db (fun () ->
      let* current =
        match get_goals db ~user with
        | Ok goals -> Ok (Some goals)
        | Error Error.Not_found -> Ok None
        | Error _ as error -> error
      in
      let* goals = f current in
      set_goals db ~user goals)
