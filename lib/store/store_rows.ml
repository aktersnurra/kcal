module Domain_error = Error
open Base
module Error = Domain_error
open Sqlite_db

let owner user = text_value (User_id.to_string user.User.id)
let user_columns = "id, oidc_issuer, oidc_subject, created_at"

let user_of_row statement =
  match
    (User_id.of_string (text statement 0), parse_time (text statement 3))
  with
  | Some id, Some created_at ->
      Some
        User.
          {
            id;
            oidc_issuer = text statement 1;
            oidc_subject = text statement 2;
            created_at;
          }
  | _ -> None

let weigh_in_columns =
  "id, user_id, measured_at, weight_kg, source, external_id, created_at, \
   updated_at, deleted_at"

let weigh_in_of_row statement =
  match
    ( Weigh_in_id.of_string (text statement 0),
      User_id.of_string (text statement 1),
      parse_time (text statement 2),
      parse_time (text statement 6),
      parse_time (text statement 7) )
  with
  | Some id, Some user_id, Some measured_at, Some created_at, Some updated_at
    when numeric_column statement 3 -> (
      let source =
        match text statement 4 with
        | "manual" -> Some Weigh_in.Manual
        | "withings" ->
            Option.bind (optional_text statement 5) ~f:(fun external_id ->
                Result.ok
                  (Result.map
                     ~f:(fun identity -> Weigh_in.Withings identity)
                     (Weigh_in.Imported_identity.of_external_id external_id)))
        | _ -> None
      in
      match source with
      | Some source ->
          Result.ok
            (Weigh_in.of_storage ~id ~user_id ~measured_at
               ~weight_kg:(float statement 3) ~source ~created_at ~updated_at
               ~deleted_at:
                 (Option.bind (optional_text statement 8) ~f:parse_time))
      | None -> None)
  | _ -> None

(* Deleting a record that is already deleted succeeds; one that never existed
   for this user is not found. [table] is a literal from the store. *)
let soft_delete db ~table ~user id =
  let current = time_value (now ()) in
  let* changed =
    execute_changes db
      ("UPDATE " ^ table
     ^ " SET deleted_at = ?, updated_at = ? WHERE id = ? AND user_id = ? AND \
        deleted_at IS NULL")
      [ current; current; text_value id; owner user ]
  in
  if changed = 1 then Ok ()
  else
    first_row db
      ("SELECT 1 FROM " ^ table
     ^ " WHERE id = ? AND user_id = ? AND deleted_at IS NOT NULL")
      [ text_value id; owner user ]
      ~absent:(fun () -> Error Error.Not_found)
      ~present:(fun _ -> Ok ())
