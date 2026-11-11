module Domain_error = Error
open Base
module Error = Domain_error
open Sqlite_db
module Withings_connection_id = Id.Make ()

let withings_connection_of_row statement =
  match
    ( User_id.of_string (text statement 1),
      optional_text statement 2,
      Option.bind (optional_text statement 3) ~f:parse_time )
  with
  | Some user_id, Some withings_user_id, Some token_expires_at ->
      Result.ok
        (Withings_connection.decode ~id:(text statement 0) ~user_id
           ~withings_user_id ~token_expires_at
           ~sync_cursor:(optional_int64 statement 4))
  | _ -> None

(* States that expired before [now] can never be consumed, so each new one
   clears them out. *)
let create_withings_oauth_state db ~user ~state_hash ~expires_at
    ~now:current_time =
  let current = time_value current_time in
  in_transaction db (fun () ->
      let* () =
        execute db "DELETE FROM withings_oauth_states WHERE expires_at <= ?"
          [ current ]
      in
      execute db
        "INSERT INTO withings_oauth_states (state_hash, user_id, expires_at, \
         consumed_at, created_at) VALUES (?, ?, ?, NULL, ?)"
        [ text_value state_hash; owner user; time_value expires_at; current ])

let consume_withings_oauth_state db ~user ~state_hash ~now:current_time =
  let current = time_value current_time in
  let* changed =
    execute_changes db
      "UPDATE withings_oauth_states SET consumed_at = ? WHERE state_hash = ? \
       AND user_id = ? AND consumed_at IS NULL AND expires_at > ?"
      [ current; text_value state_hash; owner user; current ]
  in
  if changed = 1 then Ok () else Error Error.Not_found

let consume_withings_oauth_state_for_callback db ~state_hash ~now:current_time =
  let current = time_value current_time in
  let* user_id =
    query_one db
      "UPDATE withings_oauth_states SET consumed_at = ? WHERE state_hash = ? \
       AND consumed_at IS NULL AND expires_at > ? RETURNING user_id"
      [ current; text_value state_hash; current ]
      (fun statement -> Some (text statement 0))
  in
  query_one db
    ("SELECT " ^ Store_rows.user_columns ^ " FROM users WHERE id = ?")
    [ text_value user_id ]
    Store_rows.user_of_row

let get_withings_status db ~user =
  first_row db
    "SELECT id, user_id, withings_user_id, token_expires_at, sync_cursor, \
     requires_reauthorization, access_token_encrypted, refresh_token_encrypted \
     FROM withings_connections WHERE user_id = ?"
    [ owner user ]
    ~absent:(fun () -> Ok Withings_connection.Disconnected)
    ~present:(fun statement ->
      match withings_connection_of_row statement with
      | Some connection
        when not
               (Sqlite3.column_is_null statement 6
               || Sqlite3.column_is_null statement 7) ->
          Ok
            (if int statement 5 = 0 then
               Withings_connection.Connected connection
             else Reauthorization_required connection)
      | _ -> Error (Error.Storage_decoding "Withings connection"))

let delete_withings_connection db ~user =
  execute db "DELETE FROM withings_connections WHERE user_id = ?" [ owner user ]

(* A connection with its user. A connection without both tokens has no usable
   expiry, so it does not decode. *)
let connections_with_users =
  "SELECT c.id, c.user_id, c.withings_user_id, CASE WHEN \
   c.access_token_encrypted IS NULL OR c.refresh_token_encrypted IS NULL THEN \
   NULL ELSE c.token_expires_at END, c.sync_cursor, \
   c.requires_reauthorization, u.oidc_issuer, u.oidc_subject, u.created_at \
   FROM withings_connections c JOIN users u ON u.id = c.user_id"

let connection_with_user_of_row statement =
  match
    ( withings_connection_of_row statement,
      User_id.of_string (text statement 1),
      parse_time (text statement 8) )
  with
  | Some connection, Some id, Some created_at ->
      Some
        ( {
            User.id;
            oidc_issuer = text statement 6;
            oidc_subject = text statement 7;
            created_at;
          },
          connection )
  | _ -> None

let withings_connection_for_identity db ~withings_user_id =
  query_one ~undecodable:(storage_error ()) db
    (connections_with_users ^ " WHERE c.withings_user_id = ?")
    [ text_value withings_user_id ]
    connection_with_user_of_row

let withings_connections db =
  query ~undecodable:(storage_error ()) db
    (connections_with_users
   ^ " WHERE c.access_token_encrypted IS NOT NULL AND \
      c.refresh_token_encrypted IS NOT NULL")
    [] connection_with_user_of_row

let account_value (connection : Withings_connection.connected) =
  text_value (Withings_connection.Account.to_string connection.account)

let connection_columns =
  "id,user_id,withings_user_id,token_expires_at,sync_cursor,requires_reauthorization"

(* A single UPSERT is SQLite-atomic, including trigger failures. Encryption is
   complete before entering that implicit write transaction. *)
let replace_withings_connection db ~user ~key credentials =
  if String.is_empty credentials.Withings.withings_user_id then
    Error (Error.Invalid_input "Withings account identity required")
  else
    let current = time_value (now ()) in
    query_one db
      ("INSERT INTO withings_connections \
        (id,user_id,withings_user_id,access_token_encrypted,refresh_token_encrypted,token_expires_at,sync_cursor,requires_reauthorization,created_at,updated_at) \
        VALUES (?,?,?,?,?,?,NULL,0,?,?) ON CONFLICT(user_id) DO UPDATE SET \
        sync_cursor = CASE WHEN withings_connections.withings_user_id IS NOT \
        excluded.withings_user_id THEN NULL ELSE \
        withings_connections.sync_cursor END, \
        withings_user_id=excluded.withings_user_id, \
        access_token_encrypted=excluded.access_token_encrypted, \
        refresh_token_encrypted=excluded.refresh_token_encrypted, \
        token_expires_at=excluded.token_expires_at, \
        requires_reauthorization=0, updated_at=excluded.updated_at RETURNING "
     ^ connection_columns)
      [
        text_value
          (Withings_connection_id.to_string (Withings_connection_id.fresh ()));
        owner user;
        text_value credentials.withings_user_id;
        text_value (Secret.encrypt ~key credentials.access_token);
        text_value (Secret.encrypt ~key credentials.refresh_token);
        time_value credentials.expires_at;
        current;
        current;
      ]
      withings_connection_of_row

let refresh_withings_credentials db ~user ~connection ~key credentials =
  if
    not
      (String.equal credentials.Withings.withings_user_id
         (Withings_connection.Account.to_string
            connection.Withings_connection.account))
  then Error (Error.Withings_failure Malformed_response)
  else
    query_one db
      ("UPDATE withings_connections SET \
        access_token_encrypted=?,refresh_token_encrypted=?,token_expires_at=?,requires_reauthorization=0,updated_at=? \
        WHERE id=? AND user_id=? AND withings_user_id=? RETURNING "
     ^ connection_columns)
      [
        text_value (Secret.encrypt ~key credentials.access_token);
        text_value (Secret.encrypt ~key credentials.refresh_token);
        time_value credentials.expires_at;
        time_value (now ());
        text_value connection.id;
        owner user;
        account_value connection;
      ]
      withings_connection_of_row

let withings_credentials db ~user ~connection ~key =
  first_row db
    "SELECT access_token_encrypted, refresh_token_encrypted, token_expires_at, \
     withings_user_id FROM withings_connections WHERE id = ? AND user_id = ? \
     AND requires_reauthorization = 0 AND withings_user_id = ?"
    [
      text_value connection.Withings_connection.id;
      owner user;
      account_value connection;
    ]
    ~absent:(fun () -> Error Error.Not_found)
    ~present:(fun statement ->
      match
        ( optional_text statement 0,
          optional_text statement 1,
          Option.bind (optional_text statement 2) ~f:parse_time,
          optional_text statement 3 )
      with
      | ( Some encrypted_access,
          Some encrypted_refresh,
          Some expires_at,
          Some withings_user_id ) ->
          let* access_token = Secret.decrypt ~key encrypted_access in
          let* refresh_token = Secret.decrypt ~key encrypted_refresh in
          Ok
            Withings.
              { access_token; refresh_token; expires_at; withings_user_id }
      | _ -> Error (Error.Storage_decoding "Withings credentials"))

(* Only while the stored tokens are still the ones [connection] was read
   with: when another sync has refreshed them since, its rejection of the old
   refresh token says nothing about the new one, and the mark is not found. *)
let mark_withings_reauthorization db ~user ~connection =
  let* changed =
    execute_changes db
      "UPDATE withings_connections SET requires_reauthorization = 1, \
       updated_at = ? WHERE id = ? AND user_id = ? AND withings_user_id = ? \
       AND token_expires_at = ?"
      [
        time_value (now ());
        text_value connection.Withings_connection.id;
        owner user;
        account_value connection;
        time_value connection.token_expires_at;
      ]
  in
  if changed = 1 then Ok () else Error Error.Not_found

let persist_withings_import db ~user ~connection ~rows ~expected_cursor ~cursor
    =
  let rows = List.map ~f:Weigh_in.Import.to_raw rows in
  let connection_id = text_value connection.Withings_connection.id in
  let account =
    Withings_connection.Account.to_string connection.Withings_connection.account
  in
  (* A batch older than what is stored must not regress the cursor. *)
  let is_stale current_cursor =
    match cursor with
    | None -> not (Option.equal Int64.equal current_cursor expected_cursor)
    | Some received -> (
        match current_cursor with
        | None -> false
        | Some current -> Int64.compare received current < 0)
  in
  let persist ~current (row : Weigh_in.import) =
    (* A corrupt stored row is reported, never overwritten by the upsert. *)
    let* () =
      first_row db
        ("SELECT " ^ Store_rows.weigh_in_columns
       ^ " FROM weigh_ins WHERE source = 'withings' AND external_id = ? AND \
          user_id = ?")
        [ text_value row.external_id; owner user ]
        ~absent:(fun () -> Ok ())
        ~present:(fun statement ->
          match Store_rows.weigh_in_of_row statement with
          | Some (_ : Weigh_in.t) -> Ok ()
          | None -> Error (Error.Storage_decoding "weigh-in"))
    in
    execute db
      "INSERT INTO weigh_ins (id, user_id, measured_at, weight_kg, source, \
       external_id, created_at, updated_at, deleted_at, \
       withings_connection_id) VALUES (?, ?, ?, ?, 'withings', ?, ?, ?, NULL, \
       ?) ON CONFLICT(source, external_id) DO UPDATE SET weight_kg = \
       excluded.weight_kg, withings_connection_id = \
       excluded.withings_connection_id, updated_at = excluded.updated_at WHERE \
       weigh_ins.deleted_at IS NULL AND weigh_ins.user_id = excluded.user_id"
      [
        text_value (Weigh_in_id.to_string (Weigh_in_id.fresh ()));
        owner user;
        time_value row.measured_at;
        float_value row.weight_kg;
        text_value row.external_id;
        current;
        current;
        connection_id;
      ]
  in
  in_transaction db (fun () ->
      let* stale =
        first_row db
          "SELECT sync_cursor, withings_user_id FROM withings_connections \
           WHERE id = ? AND user_id = ?"
          [ connection_id; owner user ]
          ~absent:(fun () -> Error Error.Not_found)
          ~present:(fun statement ->
            if
              Option.equal String.equal
                (optional_text statement 1)
                (Some account)
            then Ok (is_stale (optional_int64 statement 0))
            else Error Error.Not_found)
      in
      if stale then Ok ()
      else
        let current = time_value (now ()) in
        let* () =
          List.fold_result rows ~init:() ~f:(fun () row -> persist ~current row)
        in
        let cursor =
          match cursor with
          | None -> Sqlite3.Data.NULL
          | Some value -> Sqlite3.Data.INT value
        in
        let* changed =
          execute_changes db
            "UPDATE withings_connections SET sync_cursor = CASE WHEN \
             sync_cursor IS NULL OR ? >= sync_cursor THEN ? ELSE sync_cursor \
             END, updated_at = ? WHERE id = ? AND user_id = ?"
            [ cursor; cursor; current; connection_id; owner user ]
        in
        if changed = 1 then Ok () else Error Error.Not_found)
