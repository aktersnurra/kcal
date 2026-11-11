module Domain_error = Error
open Base
module Error = Domain_error

(* Runs [request] against a local endpoint that accepts the connection and
   never answers, then advances a mock clock to the next deadline. Returns the
   request's result and the time at which it was abandoned. *)
let against_stalled_endpoint request =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = env#net in
  (* Listen where the client will connect: the first address of localhost. *)
  let address =
    match Eio.Net.getaddrinfo_stream ~service:"0" net "localhost" with
    | address :: _ -> address
    | [] -> Alcotest.fail "localhost does not resolve"
  in
  let listener = Eio.Net.listen ~sw ~backlog:1 net address in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | `Unix _ -> Alcotest.fail "expected a TCP listener"
  in
  let accepted, accept = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      let (_ : _ Eio.Net.stream_socket), (_ : Eio.Net.Sockaddr.stream) =
        Eio.Net.accept ~sw listener
      in
      Eio.Promise.resolve accept ());
  let clock = Eio_mock.Clock.make () in
  let environment =
    object
      method net = net
      method clock = clock
    end
  in
  let uri = Printf.sprintf "https://localhost:%d/stalled" port in
  let pending =
    Eio.Fiber.fork_promise ~sw (fun () -> request environment uri)
  in
  Eio.Fiber.first
    (fun () -> Eio.Promise.await accepted)
    (fun () ->
      ignore (Eio.Promise.await_exn pending);
      Alcotest.fail "the request finished without connecting");
  Eio_mock.Clock.advance clock;
  let result = Eio.Promise.await_exn pending in
  (result, Eio.Time.now clock)

let test_oidc_request_to_a_stalled_endpoint_times_out () =
  let result, abandoned_at =
    against_stalled_endpoint (fun environment uri ->
        (Oidc.https_client environment).fetch_jwks ~uri)
  in
  Alcotest.(check bool) "fails" true (Result.is_error result);
  Alcotest.(check (float 0.))
    "abandoned at the timeout" Https_transport.request_timeout_s abandoned_at

let test_withings_request_to_a_stalled_endpoint_times_out () =
  let result, abandoned_at =
    against_stalled_endpoint (fun environment uri ->
        (Withings_transport.make environment).post_form ~uri ~fields:[])
  in
  Alcotest.(check bool)
    "transport failure" true
    (match result with
    | Error (Error.Withings_failure (Transport_failure _)) -> true
    | _ -> false);
  Alcotest.(check (float 0.))
    "abandoned at the timeout" Https_transport.request_timeout_s abandoned_at

let () =
  Alcotest.run "https_transport"
    [
      ( "timeouts",
        [
          Alcotest.test_case "OIDC request to a stalled endpoint" `Quick
            test_oidc_request_to_a_stalled_endpoint_times_out;
          Alcotest.test_case "Withings request to a stalled endpoint" `Quick
            test_withings_request_to_a_stalled_endpoint_times_out;
        ] );
    ]
