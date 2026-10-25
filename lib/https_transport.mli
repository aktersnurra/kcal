type setup_error = No_trust_bundle | Invalid_tls_configuration

val make_client : net:_ Eio.Net.t -> (Cohttp_eio.Client.t, setup_error) result
(** Shared system trust anchors and TLS authentication for provider clients. *)

val catch_transport_failure : (unit -> 'a) -> ('a, exn) result
(** Only dependency transport failures become [Error]; cancellation and
    programming exceptions propagate to the caller. *)
