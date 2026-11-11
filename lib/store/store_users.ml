open Base
open Sqlite_db

(* A read for a known user: only the first sight of an identity writes. *)
let resolve_user db ~issuer ~subject =
  let identity = [ text_value issuer; text_value subject ] in
  let find () =
    query_optional ~undecodable:(storage_error ()) db
      ("SELECT " ^ Store_rows.user_columns
     ^ " FROM users WHERE oidc_issuer = ? AND oidc_subject = ?")
      identity Store_rows.user_of_row
  in
  let* existing = find () in
  match existing with
  | Some user -> Ok user
  | None ->
      let* () =
        execute db
          "INSERT INTO users (id, oidc_issuer, oidc_subject, created_at) \
           VALUES (?, ?, ?, ?) ON CONFLICT(oidc_issuer, oidc_subject) DO \
           NOTHING"
          ((text_value (User_id.to_string (User_id.fresh ())) :: identity)
          @ [ time_value (now ()) ])
      in
      let* created = find () in
      Result.of_option created ~error:(storage_error ())
