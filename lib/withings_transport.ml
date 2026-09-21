let log_src = Logs.Src.create "kcal.withings.transport" ~doc:"Withings HTTPS transport"
module Log = (val Logs.src_log log_src : Logs.LOG)

let error_of_status status =
  Error.Invalid_input
    (Printf.sprintf "Withings request failed (HTTP %d)" (Cohttp.Code.code_of_status status))

let error_of_exn exn =
  Error.Invalid_input (Printf.sprintf "Withings request failed (%s)" (Printexc.to_string exn))

let unavailable reason = Error.Invalid_input (Printf.sprintf "Withings transport unavailable (%s)" reason)

let trust_bundle () =
  let rec read = function
    | [] -> Error ()
    | path :: rest ->
        (try
           let channel = open_in_bin path in
           let pem =
             Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
                 really_input_string channel (in_channel_length channel))
           in
           match X509.Certificate.decode_pem_multiple pem with
           | Ok anchors when anchors <> [] -> Ok anchors
           | _ -> read rest
         with Sys_error _ -> read rest)
  in
  read [ "/etc/ssl/cert.pem"; "/etc/ssl/certs/ca-certificates.crt" ]

(* Reconciliation constructs this transport without visiting the OAuth flow. *)
let make env =
  Mirage_crypto_rng_unix.use_default ();
  let tls =
    match trust_bundle () with
    | Error () ->
        Log.err (fun m -> m "no usable CA trust bundle found; Withings requests will fail");
        Error "no CA trust bundle"
    | Ok anchors ->
        let authenticator =
          X509.Authenticator.chain_of_trust ~time:(fun () -> Ptime.of_float_s (Unix.gettimeofday ())) anchors
        in
        (match Tls.Config.client ~authenticator () with
        | Ok config -> Ok config
        | Error _ ->
            Log.err (fun m -> m "could not build TLS client configuration; Withings requests will fail");
            Error "no TLS configuration")
  in
  match tls with
  | Error reason -> { Withings.post_form = (fun ~uri:_ ~fields:_ -> Error (unavailable reason)) }
  | Ok tls_config ->
      let https uri flow =
        match Uri.host uri with
        | None -> failwith "missing Withings HTTPS host"
        | Some host ->
            (match Domain_name.of_string host with
            | Error _ -> failwith "invalid Withings HTTPS host"
            | Ok host -> Tls_eio.client_of_flow tls_config ~host:(Domain_name.host_exn host) flow)
      in
      let client = Cohttp_eio.Client.make ~https:(Some https) env#net in
      {
        Withings.post_form =
          (fun ~uri ~fields ->
            try
              let target = Uri.of_string uri in
              if Uri.scheme target <> Some "https" then (
                Log.err (fun m -> m "refusing to POST %s: only https is allowed" uri);
                Error (unavailable "non-https endpoint"))
              else
                Eio.Switch.run @@ fun sw ->
                let body =
                  Cohttp_eio.Body.of_string
                    (Uri.encoded_of_query (List.map (fun (key, value) -> (key, [ value ])) fields))
                in
                let headers = Cohttp.Header.init_with "content-type" "application/x-www-form-urlencoded" in
                let response, response_body = Cohttp_eio.Client.post client ~sw ~headers ~body target in
                let status = Cohttp.Response.status response in
                if Cohttp.Code.code_of_status status / 100 <> 2 then (
                  (* The response body may echo request fields: log the status only. *)
                  Log.warn (fun m -> m "POST %s returned %s" uri (Cohttp.Code.string_of_status status));
                  Error (error_of_status status))
                else Ok (Eio.Flow.read_all response_body)
            with exn ->
              Log.warn (fun m -> m "POST %s raised %s" uri (Printexc.to_string exn));
              Error (error_of_exn exn));
      }
