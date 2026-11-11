val make :
  < net : _ Eio.Net.t ; clock : _ Eio.Time.clock ; .. > -> Withings.request
(** Each request is abandoned after [Https_transport.request_timeout_s]. *)

module For_testing : sig
  val read_success_body : status:int -> read_body:(unit -> string) -> string
  (** The production response-body decision, evaluated inside its live switch.
  *)

  val of_post :
    (Uri.t -> (string * string) list -> int * string) -> Withings.request
  (** Runs the same HTTPS guard, status mapping and exception boundary as
      [make]. *)
end
