type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

type protected_resource = {
  resource : string;
  authorization_server : string;
  metadata_url : string;
}

type withings_config = {
  oauth : Withings_oauth.t;
  client : (module Withings.S);
  token_key : Secret.Key.t;
  sync : User.t -> Withings_connection.connected -> (unit, Error.t) result;
  callback_url : string;
  client_id : string;
  redirect_uri : string;
}

val handle :
  ?protected_resource:protected_resource ->
  auth:Auth.t ->
  service:Service.t ->
  method_:[ `GET | `POST | `HEAD ] ->
  path:string ->
  headers:(string * string) list ->
  body:string ->
  unit ->
  response

val handle_withings :
  protected_resource:protected_resource option ->
  schedule_sync:((unit -> unit) -> unit) ->
  withings:withings_config option ->
  auth:Auth.t ->
  service:Service.t ->
  method_:[ `GET | `POST | `HEAD ] ->
  path:string ->
  headers:(string * string) list ->
  body:string ->
  response

val run :
  withings:withings_config option ->
  < net : _ Eio.Net.t ; .. > ->
  config:Config.server ->
  auth:Auth.t ->
  Service.t ->
  unit

module For_testing : sig
  val framed_response : response -> Httpun.Response.t
  (** Builds the exact response headers and status used by the HTTP writer. *)
end
