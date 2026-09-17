# kcal Withings Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add secure per-user Withings OAuth and idempotent weight synchronization without expanding kcal beyond durable observations.

**Architecture:** SQLite migrations add encrypted connection/state storage and imported-weight uniqueness. A fakeable Withings client feeds one `Withings_sync.sync` function used by OAuth completion, webhooks, and the reconciliation CLI. HTTP/MCP remain thin adapters over authenticated service operations.

**Tech Stack:** OCaml 5.5.1, Eio/eio_posix, sqlite3, jose/mirage-crypto AES-GCM, Cohttp-eio/tls-eio, httpun-eio, Alcotest.

## Global Constraints

- Request only Withings `user.metrics`; do not import activity, sleep, or body composition.
- Encrypt OAuth tokens with the 32-byte `KCAL_TOKEN_ENCRYPTION_KEY`; never log or expose tokens.
- Every connection, OAuth state, and weigh-in remains scoped to its kcal user.
- Imported rows use `source = withings` and stable external identity; callers cannot edit imported measurements.
- Local deletion of an imported row is a durable tombstone.
- Advance the Withings cursor only in the transaction that persists successful imports.
- Use one sync function for initial import, webhooks, and reconciliation.
- Keep FreeBSD/Caddy/system scheduling configuration in `nuc-setup`, not this repository.

---

### Task 1: Add migration and encrypted credential primitives

**Files:**

- Create: `migrations/002_withings.sql`
- Create: `lib/secret.ml`
- Modify: `lib/store.ml`
- Modify: `lib/store_sqlite.ml`
- Create: `test/test_secret.ml`
- Modify: `test/test_migration.ml`

**Interfaces:**

```ocaml
val Secret.encrypt : key:bytes -> string -> (string, Error.t) result
val Secret.decrypt : key:bytes -> string -> (string, Error.t) result
```

- [ ] Write tests that decrypt a round trip, reject a changed ciphertext/key, and migrate an empty database through version 2.
- [ ] Run `opam exec -- dune exec test/test_secret.exe`; verify RED because `Secret`/migration 002 are absent.
- [ ] Create connection and hashed-state tables; add nullable connection metadata and allow `manual`/`withings` sources with unique `(source, external_id)`. Implement AES-GCM authenticated encryption with a random nonce and versioned ciphertext envelope.
- [ ] Run focused tests and `opam exec -- dune test`; verify GREEN.
- [ ] Commit: `jj describe -m "feat(withings): store encrypted connections" && jj new`.

### Task 2: Add OAuth state and connection ownership service

**Files:**

- Create: `lib/withings_oauth.ml`
- Create: `lib/withings_connection.ml`
- Modify: `lib/service.ml`
- Modify: `lib/store.ml`
- Modify: `lib/store_sqlite.ml`
- Create: `test/test_withings_oauth.ml`

**Interfaces:**

```ocaml
val begin_authorization : user:User.t -> (string * string, Error.t) result
val consume_state : user:User.t -> state:string -> (unit, Error.t) result
val get_withings_status : user:User.t -> (Withings_connection.status, Error.t) result
```

- [ ] Write tests for unique state, wrong-user rejection, expiry, replay, and a user not seeing another user’s connection.
- [ ] Run `opam exec -- dune exec test/test_withings_oauth.exe`; verify RED.
- [ ] Store only a SHA-256 state hash with expiry/consumed timestamp. Consume with one conditional update. Define connection status without credential fields and service methods with authenticated `User.t` only.
- [ ] Run focused tests and full suite; verify GREEN.
- [ ] Commit: `jj describe -m "feat(withings): add owned OAuth state" && jj new`.

### Task 3: Implement fakeable Withings API client and credential refresh

**Files:**

- Create: `lib/withings.ml`
- Modify: `lib/withings_connection.ml`
- Create: `test/test_withings_client.ml`

**Interfaces:**

```ocaml
module type S = sig
  val exchange_code : code:string -> (credentials, Error.t) result
  val refresh : refresh_token:string -> (credentials, Error.t) result
  val get_measurements : access_token:string -> lastupdate:int64 option -> (measurement_batch, Error.t) result
  val subscribe : access_token:string -> callback_url:string -> (unit, Error.t) result
end
```

- [ ] Write fake-client tests for immediate code exchange, refresh-token rotation, and permanent refresh failure marking reauthorization while retaining imported data.
- [ ] Run the focused test; verify RED.
- [ ] Implement HTTPS-only Withings endpoints, response decoding, sanitized upstream errors, and atomically encrypted credential replacement. Never include token text in errors.
- [ ] Run focused tests and full suite; verify GREEN.
- [ ] Commit: `jj describe -m "feat(withings): add OAuth client" && jj new`.

### Task 4: Implement transactional idempotent synchronization

**Files:**

- Create: `lib/withings_sync.ml`
- Modify: `lib/store.ml`
- Modify: `lib/store_sqlite.ml`
- Modify: `lib/weigh_in.ml`
- Create: `test/test_withings_sync.ml`

**Interfaces:**

```ocaml
val sync : user:User.t -> connection:Withings_connection.t -> (unit, Error.t) result
```

- [ ] Write failing tests for kg normalization, duplicate external IDs, changed imported value, preserved imported timestamp, tombstone retention, failed transaction cursor retention, successful cursor advance, duplicate/out-of-order sync convergence.
- [ ] Run `opam exec -- dune exec test/test_withings_sync.exe`; verify RED.
- [ ] Implement the sole import path: fetch incrementally, validate only weight measures, derive stable external IDs, upsert active rows, preserve tombstones, and commit rows/cursor together. Reject manual update of imported rows.
- [ ] Run focused sync tests and full suite; verify GREEN.
- [ ] Commit: `jj describe -m "feat(withings): synchronize imported weights" && jj new`.

### Task 5: Add OAuth/webhook HTTP routes and MCP management tools

**Files:**

- Modify: `lib/http.ml`
- Modify: `lib/http_adapter.ml`
- Modify: `lib/mcp.ml`
- Modify: `bin/kcal.ml`
- Create: `test/test_withings_http.ml`
- Modify: `test/test_mcp.ml`

**Interfaces:**

```ocaml
GET  /withings/connect
GET  /withings/callback
HEAD /withings/webhook
POST /withings/webhook
```

- [ ] Write failing tests for authenticated connect, callback state failures, HEAD success, malformed webhook rejection, unknown Withings identity rejection, known identity sync trigger, and MCP responses that omit credentials.
- [ ] Run focused HTTP/MCP tests; verify RED.
- [ ] Wire callback to immediate exchange → save → sync → subscribe. Keep webhook bounded, validate its content type/fields, resolve only known upstream identity, and return quickly. Add exactly `begin_withings_connection`, `get_withings_status`, and `disconnect_withings` MCP tools.
- [ ] Run focused tests and full suite; verify GREEN.
- [ ] Commit: `jj describe -m "feat(withings): expose OAuth and webhooks" && jj new`.

### Task 6: Add reconciliation command and application documentation

**Files:**

- Modify: `bin/kcal.ml`
- Modify: `README.md`
- Create: `test/test_withings_reconciliation.ml`

- [ ] Write a failing test proving the reconciliation service uses the same `Withings_sync.sync` function and continues safely across independent connected users.
- [ ] Run the focused test; verify RED.
- [ ] Add `kcal sync-withings`, document Withings credential variables, callback/webhook URLs, encryption-key handling, and the handoff to `nuc-setup` for periodic invocation. Do not add FreeBSD service/Caddy configuration here.
- [ ] Run `opam exec -- dune test` and `opam exec -- dune build @all`; verify GREEN.
- [ ] Commit: `jj describe -m "feat(withings): add reconciliation command" && jj new`.

## Final verification

- [ ] Run `opam exec -- dune test` and `opam exec -- dune build @all`.
- [ ] Confirm no MCP schema or response contains an access token, refresh token, encryption key, or arbitrary user ID.
- [ ] Confirm imported tombstones survive resynchronization and cursor failure tests pass.
- [ ] Provide `nuc-setup` the required environment variable names and periodic `kcal sync-withings` command; do not add infrastructure files to kcal.
