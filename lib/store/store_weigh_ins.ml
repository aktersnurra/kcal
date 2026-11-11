open Base
open Sqlite_db

let create_manual_weigh_in db ~user input =
  let input = Weigh_in.Create.to_raw input in
  let current_time = now () in
  let current = time_value current_time in
  query_one db
    ("INSERT INTO weigh_ins (" ^ Store_rows.weigh_in_columns
   ^ ") VALUES (?, ?, ?, ?, 'manual', NULL, ?, ?, NULL) RETURNING "
   ^ Store_rows.weigh_in_columns)
    [
      text_value (Weigh_in_id.to_string (Weigh_in_id.fresh ()));
      Store_rows.owner user;
      time_value (Option.value input.measured_at ~default:current_time);
      float_value input.weight_kg;
      current;
      current;
    ]
    Store_rows.weigh_in_of_row

let get_weigh_in db ~user id =
  query_one db
    ("SELECT " ^ Store_rows.weigh_in_columns
   ^ " FROM weigh_ins WHERE id = ? AND user_id = ? AND deleted_at IS NULL")
    [ text_value (Weigh_in_id.to_string id); Store_rows.owner user ]
    Store_rows.weigh_in_of_row

let update_manual_weigh_in db ~user id patch =
  let patch = Weigh_in.Patch.to_raw patch in
  with_checked_numeric_update db
    ~check:(fun () ->
      Result.map (get_weigh_in db ~user id) ~f:(fun (_ : Weigh_in.t) -> ()))
    ~update:(fun () ->
      query_one db
        ("UPDATE weigh_ins SET measured_at = COALESCE(?, measured_at), \
          weight_kg = COALESCE(?, weight_kg), updated_at = ? WHERE id = ? AND \
          user_id = ? AND deleted_at IS NULL AND source = 'manual' RETURNING "
       ^ Store_rows.weigh_in_columns)
        [
          nullable (Option.map ~f:timestamp patch.measured_at);
          nullable_float patch.weight_kg;
          time_value (now ());
          text_value (Weigh_in_id.to_string id);
          Store_rows.owner user;
        ]
        Store_rows.weigh_in_of_row)

let query_weigh_ins db ~user ~from ~to_ ~limit =
  if not (is_valid_query ~from ~to_ ~limit) then Error (invalid_input ())
  else
    let range, range_values = range_clause "measured_at" ~from ~to_ in
    query db
      ("SELECT " ^ Store_rows.weigh_in_columns
     ^ " FROM weigh_ins WHERE user_id = ? AND deleted_at IS NULL" ^ range
     ^ " ORDER BY measured_at ASC, rowid ASC LIMIT ?")
      ((Store_rows.owner user :: range_values) @ [ int_value limit ])
      Store_rows.weigh_in_of_row

(* Every weigh-in in [start, stop), unpaginated, for server-side aggregation. *)
let weigh_ins_between db ~user ~start ~stop =
  query db
    ("SELECT " ^ Store_rows.weigh_in_columns
   ^ " FROM weigh_ins WHERE user_id = ? AND deleted_at IS NULL AND measured_at \
      >= ? AND measured_at < ? ORDER BY measured_at ASC, rowid ASC")
    [ Store_rows.owner user; time_value start; time_value stop ]
    Store_rows.weigh_in_of_row

let latest_weigh_in db ~user =
  query_one db
    ("SELECT " ^ Store_rows.weigh_in_columns
   ^ " FROM weigh_ins WHERE user_id = ? AND deleted_at IS NULL ORDER BY \
      measured_at DESC, rowid DESC LIMIT 1")
    [ Store_rows.owner user ]
    Store_rows.weigh_in_of_row

let delete_weigh_in db ~user id =
  Store_rows.soft_delete db ~table:"weigh_ins" ~user (Weigh_in_id.to_string id)
