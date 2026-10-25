open Base

type setup_error = No_trust_bundle | Invalid_tls_configuration

let trust_bundle () =
  let rec read = function
    | [] -> Error ()
    | path :: rest -> (
        try
          let channel = Stdlib.open_in_bin path in
          let pem =
            Stdlib.Fun.protect
              ~finally:(fun () -> Stdlib.close_in_noerr channel)
              (fun () ->
                Stdlib.really_input_string channel
                  (Stdlib.in_channel_length channel))
          in
          match X509.Certificate.decode_pem_multiple pem with
          | Ok anchors when not (List.is_empty anchors) -> Ok anchors
          | _ -> read rest
        with Sys_error _ -> read rest)
  in
  read [ "/etc/ssl/cert.pem"; "/etc/ssl/certs/ca-certificates.crt" ]

let make_client ~net =
  Mirage_crypto_rng_unix.use_default ();
  match trust_bundle () with
  | Error () -> Error No_trust_bundle
  | Ok anchors -> (
      let authenticator =
        X509.Authenticator.chain_of_trust
          ~time:(fun () -> Ptime.of_float_s (Unix.gettimeofday ()))
          anchors
      in
      match Tls.Config.client ~authenticator () with
      | Error _ -> Error Invalid_tls_configuration
      | Ok tls_config ->
          let https uri flow =
            match Uri.host uri with
            | None -> invalid_arg "missing HTTPS host"
            | Some host -> (
                match Domain_name.of_string host with
                | Error _ -> invalid_arg "invalid HTTPS host"
                | Ok host ->
                    Tls_eio.client_of_flow tls_config
                      ~host:(Domain_name.host_exn host)
                      flow)
          in
          Ok (Cohttp_eio.Client.make ~https:(Some https) net))

(* Cohttp_eio.Client reports closed/malformed HTTP responses and DNS failure
   using [Failure]; TLS and Eio expose their own transport exceptions. *)
let catch_transport_failure f =
  try Ok (f ())
  with
  | ( Eio.Io _ | Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _ | End_of_file
    | Failure _ ) as exn
  ->
    Error exn
