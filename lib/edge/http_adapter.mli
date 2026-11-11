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
  withings:Withings_integration.t option ->
  auth:Auth.t ->
  service:Service.t ->
  method_:[ `GET | `POST | `HEAD ] ->
  path:string ->
  headers:(string * string) list ->
  body:string ->
  response

val run :
  withings:Withings_integration.t option ->
  < net : _ Eio.Net.t ; .. > ->
  config:Config.server ->
  auth:Auth.t ->
  Service.t ->
  unit

module For_testing : sig
  val framed_response : response -> Httpun.Response.t
  (** Builds the exact response headers and status used by the HTTP writer. *)
end
