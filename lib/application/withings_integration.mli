(** Everything a configured Withings integration needs, and the flows that use
    it: starting and completing the OAuth link, and reacting to a Withings
    notification. The HTTP and MCP layers only call these; none of them reaches
    into OAuth state, tokens or the store directly. *)

type t = {
  store : Store_sqlite.t;
  oauth : Withings_oauth.t;
  client : (module Withings.S);
  token_key : Secret.Key.t;
  sync : User.t -> Withings_connection.connected -> (unit, Error.t) result;
  sync_gate : Sync_gate.t;
      (** One notification-triggered sync at a time per connection. *)
  callback_url : string;  (** Where Withings posts notifications. *)
  client_id : string;
  redirect_uri : string;
}

val begin_authorization : t -> user:User.t -> (string, Error.t) result
(** The URL where [user] authorizes kcal at Withings. *)

type authorization_failure =
  | Invalid_state of Error.t
  | Exchange_failed of Error.t
  | Persistence_failed of Error.t
  | Sync_failed of Error.t
  | Subscription_failed of Error.t

val complete_authorization :
  t -> state:string -> code:string -> (unit, authorization_failure) result
(** Exchanges the code, stores the connection, syncs it once and subscribes to
    notifications, stopping at the first step that fails. *)

type notification = Sync_requested of User.t | Unknown_account

val notify :
  t ->
  withings_user_id:string ->
  schedule:((unit -> unit) -> unit) ->
  (notification, Error.t) result
(** Requests a sync of that account's connection through [schedule], unless one
    is already under way; that one is then followed by one more. The sync reads
    the connection afresh each run. A failed or raising sync is logged and never
    escapes, since [schedule] runs it outside any request. *)
