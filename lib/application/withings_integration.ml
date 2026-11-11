module Domain_error = Error
open Base
module Error = Domain_error

let log_src =
  Logs.Src.create "kcal.withings.integration"
    ~doc:"Withings account linking and notification-triggered syncs"

module Log = (val Logs.src_log log_src : Logs.LOG)

type t = {
  store : Store_sqlite.t;
  oauth : Withings_oauth.t;
  client : (module Withings.S);
  token_key : Secret.Key.t;
  sync : User.t -> Withings_connection.connected -> (unit, Error.t) Result.t;
  sync_gate : Sync_gate.t;
  callback_url : string;
  client_id : string;
  redirect_uri : string;
}

let ( let* ) result f = Result.bind result ~f

let begin_authorization t ~user =
  Result.map ~f:snd
    (Withings_oauth.begin_authorization ~client_id:t.client_id
       ~redirect_uri:t.redirect_uri t.oauth ~user)

type authorization_failure =
  | Invalid_state of Error.t
  | Exchange_failed of Error.t
  | Persistence_failed of Error.t
  | Sync_failed of Error.t
  | Subscription_failed of Error.t

let complete_authorization t ~state ~code =
  let module Client = (val t.client : Withings.S) in
  let* user =
    Result.map_error
      ~f:(fun error -> Invalid_state error)
      (Withings_oauth.consume_callback_state t.oauth ~state)
  in
  let* credentials =
    Result.map_error
      ~f:(fun error -> Exchange_failed error)
      (Client.exchange_code ~redirect_uri:t.redirect_uri ~code)
  in
  let* connection =
    Result.map_error
      ~f:(fun error -> Persistence_failed error)
      (Store_sqlite.replace_withings_connection t.store ~user ~key:t.token_key
         credentials)
  in
  let* () =
    Result.map_error
      ~f:(fun error -> Sync_failed error)
      (t.sync user connection)
  in
  Result.map_error
    ~f:(fun error -> Subscription_failed error)
    (Client.subscribe ~access_token:credentials.access_token
       ~callback_url:t.callback_url)

(* Runs outside any request, on the server's own switch, where an escaping
   exception would end the whole listener: a failed sync is only logged. The
   connection is read again for each run, so a rerun sees what the run before
   it stored. *)
let notified_sync t ~withings_user_id () =
  match
    let* user, connection =
      Store_sqlite.withings_connection_for_identity t.store ~withings_user_id
    in
    t.sync user connection
  with
  | Ok () -> ()
  | Error error ->
      Log.err (fun m ->
          m "notification-triggered sync for withings_user_id=%s failed: %s"
            withings_user_id (Error.to_string error))
  | exception (Eio.Cancel.Cancelled _ as cancelled) -> raise cancelled
  | exception exn ->
      Log.err (fun m ->
          m "notification-triggered sync for withings_user_id=%s raised %s"
            withings_user_id (Exn.to_string exn))

type notification = Sync_requested of User.t | Unknown_account

let notify t ~withings_user_id ~schedule =
  match
    Store_sqlite.withings_connection_for_identity t.store ~withings_user_id
  with
  | Ok (user, connection) ->
      Sync_gate.request t.sync_gate ~key:connection.id ~schedule
        (notified_sync t ~withings_user_id);
      Ok (Sync_requested user)
  | Error Error.Not_found -> Ok Unknown_account
  | Error _ as error -> error
