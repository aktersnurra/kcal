# Changelog

## 2026-11-11 — Follow-up fixes: outages, races, health, clearing fields

- While the identity provider is down, the "using JWKS that expired" warning is logged once per outage instead of on every request; recovery is logged at info.
- Bearer tokens are accepted until a minute past `exp`, so a small clock difference with the provider no longer rejects valid tokens.
- A Withings sync that loses a refresh race (its refresh token was rotated by another sync) reloads the winner's tokens and continues instead of failing with "not found", so a cron run no longer exits 1 for it.
- The ten-second outbound timeout is tested: OIDC and Withings requests to a local endpoint that accepts and never answers are abandoned at the deadline, on a mock clock.
- `/health` reads the database and answers `503 {"status":"unavailable"}` when it cannot.
- `Secret` and `Withings` move from `domain/` to a new `infrastructure/` library between domain and store; the domain no longer depends on crypto, base64 or JSON.
- `Sqlite_db` no longer knows `User`: `owner` and `soft_delete` move to `Store_rows`.
- `update_meal_item` and `update_meal` tell absent, null and a value apart: null clears `carbs_g`, `fat_g`, `confidence`, `estimate_source`, `notes` and a meal's `label`, and is refused for required fields. The schema marks those fields nullable. The `Keep | Clear | Set` change type moves from `Goals` to the domain's `Change` module.

## 2026-11-11 — Review fixes: storage, OIDC, webhook, scopes

- SQLite opens with write-ahead logging, `synchronous = NORMAL` and a five-second busy timeout, so `kcal sync-withings` beside `kcal serve` waits for a lock instead of failing. Back up with `.backup`, not by copying the main file alone.
- Resolving a known user is a read; only the first sight of an identity writes.
- Every SQLite failure is logged with its result code and message; callers still see the sanitized error.
- The store is split by aggregate: `Sqlite_db` (connection, shared query helpers, failure logging) under `Store_users`, `Store_products`, `Store_meals`, `Store_weigh_ins`, `Store_withings` and `Store_goals`, with `Store_sqlite` gathering them so callers are unchanged. Prepared statements are cached per connection and reused. Range queries leave absent bounds out of the SQL so the timestamp index is used; weigh-in queries have a stable tiebreak; expired Withings OAuth states are purged; migration 007 drops a redundant index.
- Outbound HTTPS requests (OIDC discovery, JWKS, Withings) time out after ten seconds.
- JWKS cache: an unknown key id triggers one refetch (at most once a minute), expired keys stay trusted for up to a day while the provider is unreachable, a failed fetch is retried after thirty seconds, and concurrent requests share one fetch.
- Withings webhook: a failing or raising sync is logged instead of ending the listener, one sync runs per connection at a time, and unknown accounts get the same `200` as known ones. A rejected stale refresh token can no longer mark a connection another sync just refreshed as needing reauthorization.
- MCP tools are a variant (`Mcp_arguments.Tool.t`); the scope each requires is an exhaustive match, so a new tool cannot ship without one.
- Dependencies now point one way, kernel → domain → store → application → MCP → HTTP: the idempotency type moved to `Meal`, parsed MCP requests are generic over the compiled search so `Mcp_arguments` no longer names the store, `Identity` sits below `Auth` so MCP does not pull in OIDC, and the Withings linking and notification flows moved out of `Service`, `Mcp` and `Http_adapter` into `Withings_integration`.
- `Secret.decrypt` rejects non-canonical base64: an envelope whose unused trailing bits were changed decoded to the same ciphertext and was accepted, which the "any changed byte is rejected" property caught intermittently.
- `lib/` is split into six dune libraries (`kernel`, `domain`, `store`, `application`, `mcp`, `edge`) with transitive dependencies hidden, so the compiler rejects a module that uses a layer above it, and MCP or edge code that uses the store.
- The `Bearer` scheme is matched case-insensitively; `Secret.encrypt` returns a string; warnings are errors in every dune profile.

## 2026-10-26 — FreeBSD-buildable dependencies

- The CLI moves from Core's `Command` to Cmdliner, and `ppx_jane` is replaced by `ppx_sexp_conv`, `ppx_compare` and `ppx_let`. The pinned opam repository marks `bin_prot` and `spawn` unavailable on FreeBSD, so `core`, `core_unix` and `ppx_jane` could not build in the NUC's builder jail.
- The flag syntax is unchanged: `-name`/`--name`, unique `-prefix`es, inline `=value` and `-help` are rewritten to Cmdliner's `--name=VALUE` before parsing. Parse errors still name the flag, never a value.

## 2026-10-26 — Quality hardening (docs/superpowers/plans/2026-10-25-quality-hardening.md)

- Typed configuration and Command-based CLI (`migrate`, `serve`, `sync-withings`); flags override `KCAL_*`/`WITHINGS_*` environment variables, and each command requires only its own settings.
- Hegel properties for secret encryption (round trip, tamper detection), search-index lifecycle against an independent model, and meal patches; the catch-all properties executable is removed.
- README consolidated (duplicate Logging/MCP sections removed; configuration, audience versus resource, migrations documented).
- CI runs `dune build @all`, `dune runtest` and `dune build @fmt`; `hegel`/`ppx_hegel_test` require >= 0.25.0.
- Verified on Linux with OCaml 5.5.1 and 5.2.0. FreeBSD and live Withings were not run.
