type setup_error = No_trust_bundle | Invalid_tls_configuration

val endpoint_host : Uri.t -> ([ `host ] Domain_name.t, unit) result
(** Checks HTTPS scheme and TLS hostname suitability before network access. *)

val make_client : net:_ Eio.Net.t -> (Cohttp_eio.Client.t, setup_error) result
(** Shared system trust anchors and TLS authentication for provider clients.
    Check [endpoint_host] before issuing requests: Cohttp connects before its
    TLS callback runs. *)

val request_timeout_s : float
(** How long one request to a provider may take, connection included. *)

val with_timeout : _ Eio.Time.clock -> (unit -> 'a) -> 'a
(** Raises [Eio.Time.Timeout] when [f] outlasts [request_timeout_s]; a provider
    that stops answering must not hold a request open indefinitely. *)

val catch_transport_failure : (unit -> 'a) -> ('a, exn) result
(** Only dependency transport failures and timeouts become [Error]; cancellation
    and programming exceptions propagate to the caller. *)
