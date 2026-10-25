module Domain_error = Error
open Base
module Error = Domain_error

let unavailable reason = Error.Withings_failure (Transport_failure reason)

let of_post post =
  {
    Withings.post_form =
      (fun ~uri ~fields ->
        let target = Uri.of_string uri in
        if not (Option.equal String.equal (Uri.scheme target) (Some "https"))
        then Error (unavailable "non-https endpoint")
        else
          match
            Https_transport.catch_transport_failure (fun () ->
                post target fields)
          with
          | Error exn -> Error (unavailable (Stdlib.Printexc.to_string exn))
          | Ok (status, body) ->
              if status / 100 = 2 then Ok body
              else Error (Error.Withings_failure (Http_status status)));
  }

(* Reconciliation constructs this transport without visiting the OAuth flow. *)
let make env =
  match Https_transport.make_client ~net:env#net with
  | Error failure ->
      let reason =
        match failure with
        | Https_transport.No_trust_bundle -> "no CA trust bundle"
        | Invalid_tls_configuration -> "no TLS configuration"
      in
      {
        Withings.post_form =
          (fun ~uri:_ ~fields:_ -> Error (unavailable reason));
      }
  | Ok client ->
      of_post (fun target fields ->
          Eio.Switch.run @@ fun sw ->
          let body =
            Cohttp_eio.Body.of_string
              (Uri.encoded_of_query
                 (List.map ~f:(fun (key, value) -> (key, [ value ])) fields))
          in
          let headers =
            Cohttp.Header.init_with "content-type"
              "application/x-www-form-urlencoded"
          in
          let response, response_body =
            Cohttp_eio.Client.post client ~sw ~headers ~body target
          in
          ( Cohttp.Code.code_of_status (Cohttp.Response.status response),
            Eio.Flow.read_all response_body ))

module For_testing = struct
  let of_post = of_post
end
