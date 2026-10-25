open Base

let executable = Stdlib.Sys.argv.(1)
let check = Alcotest.(check bool)

let env values =
  Array.of_list (List.map values ~f:(fun (name, value) -> name ^ "=" ^ value))

let with_fixture f =
  let path = Stdlib.Filename.temp_file "kcal-cli-" ".sqlite" in
  Stdlib.Sys.remove path;
  Stdlib.Fun.protect
    ~finally:(fun () ->
      List.iter
        [ path; path ^ "-wal"; path ^ "-shm" ]
        ~f:(fun path ->
          if Stdlib.Sys.file_exists path then Stdlib.Sys.remove path))
    (fun () -> f path)

let read_file path =
  let channel = Stdlib.open_in_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in channel)
    (fun () ->
      Stdlib.really_input_string channel (Stdlib.in_channel_length channel))

let with_process ~environment args f =
  let out = Stdlib.Filename.temp_file "kcal-cli-out-" ".log" in
  let err = Stdlib.Filename.temp_file "kcal-cli-err-" ".log" in
  let output = Unix.openfile out [ Unix.O_WRONLY ] 0o600 in
  let errors = Unix.openfile err [ Unix.O_WRONLY ] 0o600 in
  let input = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let pid =
    Unix.create_process_env executable
      (Array.of_list (executable :: args))
      (env environment) input output errors
  in
  List.iter [ input; output; errors ] ~f:Unix.close;
  Stdlib.Fun.protect
    ~finally:(fun () ->
      (try
         match Unix.waitpid [ Unix.WNOHANG ] pid with
         | 0, _ ->
             Unix.kill pid Stdlib.Sys.sigkill;
             ignore (Unix.waitpid [] pid)
         | _ -> ()
       with Unix.Unix_error (Unix.ECHILD, _, _) -> ());
      Stdlib.Sys.remove out;
      Stdlib.Sys.remove err)
    (fun () -> f pid (fun () -> (read_file out, read_file err)))

let run ~environment args =
  with_process ~environment args (fun pid output ->
      let rec wait attempts =
        match Unix.waitpid [ Unix.WNOHANG ] pid with
        | 0, _ when attempts > 0 ->
            Unix.sleepf 0.01;
            wait (attempts - 1)
        | 0, _ ->
            let _, stderr = output () in
            Alcotest.fail ("command did not exit: " ^ stderr)
        | _, status -> status
      in
      let status = wait 1000 in
      let stdout, stderr = output () in
      (status, stdout, stderr))

let success = function Unix.WEXITED 0 -> true | _ -> false

let help () =
  let status, stdout, _ = run ~environment:[] [ "--help" ] in
  check "help succeeds" true (success status);
  List.iter [ "migrate"; "serve"; "sync-withings" ] ~f:(fun command ->
      check "listed subcommand" true
        (String.is_substring stdout ~substring:command);
      let status, _, _ = run ~environment:[] [ command; "-help" ] in
      check "subcommand help succeeds" true (success status))

let migration () =
  with_fixture (fun path ->
      let status, stdout, _ =
        run ~environment:[ ("KCAL_DATABASE_PATH", path) ] [ "migrate" ]
      in
      check "database only" true (success status);
      Alcotest.(check string) "quiet stdout" "" stdout;
      check "database created" true (Stdlib.Sys.file_exists path);
      let status, _, _ =
        run
          ~environment:[ ("KCAL_DATABASE_PATH", "") ]
          [ "migrate"; "-database-path"; path ]
      in
      check "flag overrides invalid environment" true (success status))

let server_env path =
  [
    ("KCAL_DATABASE_PATH", path);
    ("KCAL_PUBLIC_BASE_URL", "https://kcal.example.com/api/");
    ("KCAL_OIDC_ISSUER", "https://id.example.com/issuer");
    ("KCAL_OIDC_AUDIENCE", "kcal");
  ]

let invalid_configuration () =
  with_fixture (fun path ->
      let cases =
        [
          ( [ "serve" ],
            [ ("WITHINGS_CLIENT_ID", "id") ],
            "WITHINGS_CLIENT_SECRET" );
          ( [ "serve"; "-public-base-url"; "SECRET_URL" ],
            [],
            "KCAL_PUBLIC_BASE_URL" );
          ( [ "serve"; "-listen-address"; "SECRET_LISTENER" ],
            [],
            "KCAL_LISTEN_ADDRESS" );
          ( [ "sync-withings" ],
            [
              ("WITHINGS_CLIENT_ID", "id");
              ("WITHINGS_CLIENT_SECRET", "SECRET_CLIENT");
              ("KCAL_TOKEN_ENCRYPTION_KEY", "SECRET_KEY");
            ],
            "KCAL_TOKEN_ENCRYPTION_KEY" );
          ( [
              "sync-withings";
              "-withings-client-id";
              "id";
              "-withings-client-secret";
              "SECRET_CLIENT";
              "-token-encryption-key";
              "SECRET_KEY";
            ],
            [],
            "KCAL_TOKEN_ENCRYPTION_KEY" );
          ([ "sync-withings" ], [], "WITHINGS_CLIENT_ID");
          ( [ "sync-withings"; "--withings-client-secret=SECRET_CLIENT" ],
            [],
            "WITHINGS_CLIENT_ID" );
          ( [ "sync-withings"; "-token-encryption-key=SECRET_KEY" ],
            [],
            "WITHINGS_CLIENT_ID" );
          ( [ "sync-withings"; "--withings-client-secret=" ],
            [ ("WITHINGS_CLIENT_ID", "id") ],
            "WITHINGS_CLIENT_SECRET" );
          ( [ "sync-withings"; "-token-encryption-key=" ],
            [
              ("WITHINGS_CLIENT_ID", "id");
              ("WITHINGS_CLIENT_SECRET", "SECRET_CLIENT");
            ],
            "KCAL_TOKEN_ENCRYPTION_KEY" );
          ([ "sync-withings"; "-token=SECRET_KEY" ], [], "WITHINGS_CLIENT_ID");
          ( [
              "sync-withings";
              "-withings-client-secret";
              "SECRET_CLIENT";
              "-withings-client-secret";
              "SECRET_SECOND";
            ],
            [],
            "withings-client-secret" );
          ( [ "sync-withings"; "-token-encryption-key" ],
            [],
            "token-encryption-key" );
        ]
      in
      List.iter cases ~f:(fun (args, extra, field) ->
          let status, stdout, stderr =
            run ~environment:(server_env path @ extra) args
          in
          check "configuration exits unsuccessfully" false (success status);
          List.iter
            [
              "SECRET_CLIENT";
              "SECRET_KEY";
              "SECRET_URL";
              "SECRET_LISTENER";
              "SECRET_SECOND";
            ] ~f:(fun secret ->
              check "stdout secret redaction" false
                (String.is_substring stdout ~substring:secret);
              check "stderr secret redaction" false
                (String.is_substring stderr ~substring:secret));
          check "diagnostic names field" true
            (String.is_substring stderr ~substring:field)))

let sync_precedence () =
  with_fixture (fun path ->
      let status, _, stderr =
        run
          ~environment:
            [
              ("KCAL_DATABASE_PATH", path);
              ("WITHINGS_CLIENT_ID", "");
              ("WITHINGS_CLIENT_SECRET", "");
              ("KCAL_TOKEN_ENCRYPTION_KEY", "invalid");
            ]
          [
            "sync-withings";
            "-withings-client-id";
            "id";
            "-withings-client-secret";
            "fixture-secret";
            "-token-encryption-key";
            String.make 64 'a';
          ]
      in
      if not (success status) then Alcotest.fail stderr)

let inline_flags () =
  with_fixture (fun path ->
      let status, _, stderr =
        run
          ~environment:[ ("KCAL_DATABASE_PATH", path) ]
          [
            "sync-withings";
            "--withings-client-id=id";
            "-withings-client-secret";
            "-token-encryption-key=SECRET_CLIENT";
            "--token-encryption-key=" ^ String.make 64 'a';
          ]
      in
      if not (success status) then Alcotest.fail stderr)

let unused_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Stdlib.Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname socket with
      | Unix.ADDR_INET (_, port) -> port
      | _ -> assert false)

let serve_metadata ~flag_overrides () =
  with_fixture (fun path ->
      let port = unused_port () in
      let listener = "127.0.0.1:" ^ Int.to_string port in
      let environment =
        server_env path @ [ ("KCAL_LISTEN_ADDRESS", listener) ]
      in
      let environment, args =
        if flag_overrides then
          ( List.map environment ~f:(fun (name, value) ->
                if String.equal name "KCAL_DATABASE_PATH" then (name, value)
                else (name, "")),
            [
              "serve";
              "-listen-address";
              listener;
              "-public-base-url";
              "https://kcal.example.com/api/";
              "-oidc-issuer";
              "https://id.example.com/issuer";
              "-oidc-audience";
              "kcal";
            ] )
        else (environment, [ "serve" ])
      in
      with_process ~environment args (fun pid output ->
          let rec connect attempts =
            let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
            try
              Unix.connect socket
                (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
              socket
            with
            | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
                Unix.close socket;
                let exited, _ = Unix.waitpid [ Unix.WNOHANG ] pid in
                if exited <> 0 || attempts = 0 then
                  let _, stderr = output () in
                  Alcotest.fail ("server did not start: " ^ stderr)
                else (
                  Unix.sleepf 0.02;
                  connect (attempts - 1))
            | exn ->
                Unix.close socket;
                raise exn
          in
          let request method_ path =
            let socket = connect 250 in
            Stdlib.Fun.protect
              ~finally:(fun () -> Unix.close socket)
              (fun () ->
                let request =
                  method_ ^ " " ^ path
                  ^ " HTTP/1.1\r\n\
                     Host: localhost\r\n\
                     Connection: close\r\n\
                     Content-Length: 0\r\n\
                     \r\n"
                in
                let rec write offset =
                  if offset < String.length request then
                    let count =
                      Unix.write_substring socket request offset
                        (String.length request - offset)
                    in
                    write (offset + count)
                in
                write 0;
                let buffer = Buffer.create 1024 in
                let bytes = Bytes.create 1024 in
                let rec read () =
                  let ready, _, _ = Unix.select [ socket ] [] [] 5.0 in
                  if List.is_empty ready then
                    Alcotest.fail "HTTP response timed out";
                  match Unix.read socket bytes 0 (Bytes.length bytes) with
                  | 0 -> Buffer.contents buffer
                  | count ->
                      Buffer.add_subbytes buffer bytes ~pos:0 ~len:count;
                      read ()
                in
                read ())
          in
          List.iter
            [
              "/.well-known/oauth-protected-resource/api/mcp";
              "/.well-known/oauth-protected-resource";
            ] ~f:(fun path ->
              let response = request "GET" path in
              check "HTTP success" true
                (String.is_substring response ~substring:"200 OK");
              check "public resource not opaque audience" true
                (String.is_substring response
                   ~substring:"https://kcal.example.com/api/mcp"));
          let response = request "POST" "/mcp" in
          check "unauthenticated" true
            (String.is_substring response ~substring:"401 Unauthorized");
          check "challenge derives metadata from resource" true
            (String.is_substring response
               ~substring:
                 "https://kcal.example.com/.well-known/oauth-protected-resource/api/mcp")))

let () =
  (* Remove the executable argument before Alcotest parses its own arguments. *)
  Alcotest.run
    ~argv:[| Stdlib.Sys.argv.(0) |]
    "cli"
    [
      ( "subprocess",
        [
          Alcotest.test_case "help and subcommands" `Quick help;
          Alcotest.test_case "environment-only migration / precedence" `Quick
            migration;
          Alcotest.test_case "invalid configuration and secret redaction" `Quick
            invalid_configuration;
          Alcotest.test_case "inline flags and flag-like secret value" `Quick
            inline_flags;
          Alcotest.test_case "typed key flags override invalid environment"
            `Quick sync_precedence;
          Alcotest.test_case
            "environment-only serve / disabled Withings / path metadata" `Quick
            (serve_metadata ~flag_overrides:false);
          Alcotest.test_case "server flags override invalid environment" `Quick
            (serve_metadata ~flag_overrides:true);
        ] );
    ]
