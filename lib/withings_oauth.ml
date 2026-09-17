type t = { store : Store_sqlite.t; now : unit -> Ptime.t }

let make ~store ~now = { store; now }

let state_hash state = Digestif.SHA256.(to_hex (digest_string state))
let expires_after = Ptime.Span.of_int_s 600
let rng_initialized = lazy (Mirage_crypto_rng_unix.use_default ())

let authorization_url state =
  "https://account.withings.com/oauth2_user/authorize?response_type=code&state="
  ^ state ^ "&scope=user.metrics"

let begin_authorization oauth ~user =
  try
    Lazy.force rng_initialized;
    let state = Base64.encode_string ~pad:false (Mirage_crypto_rng.generate 32) in
    let expires_at = Ptime.add_span (oauth.now ()) expires_after |> Option.get in
    match
      Store_sqlite.create_withings_oauth_state oauth.store ~user
        ~state_hash:(state_hash state) ~expires_at
    with
    | Ok () -> Ok (state, authorization_url state)
    | Error _ as error -> error
  with _ -> Error (Error.Storage_error "unable to create authorization state")

let consume_state oauth ~user ~state =
  Store_sqlite.consume_withings_oauth_state oauth.store ~user
    ~state_hash:(state_hash state) ~now:(oauth.now ())
