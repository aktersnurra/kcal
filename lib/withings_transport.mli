val make : < net : _ Eio.Net.t ; .. > -> Withings.request

module For_testing : sig
  val of_post :
    (Uri.t -> (string * string) list -> int * string) -> Withings.request
  (** Runs the same HTTPS guard, status mapping and exception boundary as
      [make]. *)
end
