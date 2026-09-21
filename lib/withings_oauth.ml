let log_src = Logs.Src.create "kcal.withings.oauth" ~doc:"Withings OAuth state handling"
module Log = (val Logs.src_log log_src : Logs.LOG)

type t = { store : Store_sqlite.t; now : unit -> Ptime.t }

let make ~store ~now = { store; now }

let state_hash state = Digestif.SHA256.(to_hex (digest_string state))
let expires_after = Ptime.Span.of_int_s 600
let rng_initialized = lazy (Mirage_crypto_rng_unix.use_default ())

let base64url bytes =
  Base64.encode_string ~pad:false bytes |> String.map (function '+' -> '-' | '/' -> '_' | c -> c)

let authorization_url ~client_id ~redirect_uri state =
  Uri.encoded_of_query [ ("response_type", [ "code" ]); ("client_id", [ client_id ]);
                         ("redirect_uri", [ redirect_uri ]); ("scope", [ "user.metrics" ]);
                         ("state", [ state ]) ]
  |> fun query -> "https://account.withings.com/oauth2_user/authorize2?" ^ query

let begin_authorization ~client_id ~redirect_uri oauth ~user =
  try
    Lazy.force rng_initialized;
    let state = base64url (Mirage_crypto_rng.generate 32) in
    let expires_at = Ptime.add_span (oauth.now ()) expires_after |> Option.get in
    match
      Store_sqlite.create_withings_oauth_state oauth.store ~user
        ~state_hash:(state_hash state) ~expires_at
    with
    | Ok () -> Ok (state, authorization_url ~client_id ~redirect_uri state)
    | Error error as result ->
        Log.err (fun m -> m "user=%s failed to persist Withings OAuth state: %s" (User_id.to_string user.User.id) (Error.to_string error));
        result
  with exn ->
    Log.err (fun m -> m "user=%s could not begin Withings authorization: %s" (User_id.to_string user.User.id) (Printexc.to_string exn));
    Error (Error.Storage_error "unable to create authorization state")

let consume_state oauth ~user ~state =
  match Store_sqlite.consume_withings_oauth_state oauth.store ~user ~state_hash:(state_hash state) ~now:(oauth.now ()) with
  | Ok () as result -> result
  | Error error as result ->
      Log.warn (fun m -> m "user=%s rejected Withings OAuth state: %s" (User_id.to_string user.User.id) (Error.to_string error));
      result

(* Browser callbacks have no bearer token: the single-use state is the binding. *)
let consume_callback_state oauth ~state =
  Store_sqlite.consume_withings_oauth_state_for_callback oauth.store
    ~state_hash:(state_hash state) ~now:(oauth.now ())
