# kcal Ledger MCP Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Deliver a small, authenticated, multi-user SQLite nutrition ledger exposed through synchronous Streamable HTTP MCP.

**Architecture:** A single OCaml 5.5.1/Eio executable serves `GET /health` and standard MCP Streamable HTTP `POST /mcp` through Caddy. The HTTP adapter authenticates the bearer token through Pocket ID OIDC, then gives only a resolved `User.t` to the service layer; the service delegates persistence to a direct SQLite store. HTTP requests receive ordinary JSON responses—the transport has no SSE, server push, or long-lived connection in this slice.

**Tech Stack:** OCaml 5.5.1, Dune, Eio/eio_posix/eio_main, Cohttp-eio plus tls-eio for HTTPS OIDC discovery, httpun-eio for the portable HTTP listener, jose for JWT/JWKS verification, sqlite3, yojson, ptime, uuidm, cmdliner, Alcotest.

## Global Constraints

- Target a native FreeBSD jail; use `eio_posix`, never `eio_linux` or io_uring.
- Use only local SQLite storage on a ZFS dataset, never NFS.
- Keep all timestamps as RFC 3339 UTC text and generate UUID IDs.
- Every external record operation is scoped to the authenticated local user; no request schema includes `user_id`.
- SQL exists only in `store_sqlite.ml`; MCP and HTTP modules contain no SQL.
- Implement no Withings support, images, LLM calls, analysis, dashboards, recommendations, or PBT framework in this slice.
- Use Alcotest plus real temporary SQLite databases; do not add Hegel.
- Never write OIDC tokens, claims, health data, SQL text, or stack traces to normal logs.

---

### Task 1: Create the portable OCaml project and migration runner

**Files:**

- Create: `dune-project`
- Create: `kcal.opam`
- Create: `dune`
- Create: `lib/dune`
- Create: `bin/dune`
- Create: `test/dune`
- Create: `migrations/001_ledger.sql`
- Create: `lib/error.ml`
- Create: `lib/migration.ml`
- Create: `test/test_migration.ml`

**Interfaces:**

- Produces `Error.t`:

  ```ocaml
  type t = Unauthorized | Not_found | Invalid_input of string
         | Conflict of string | Storage_error of string
  ```

- Produces `Migration.apply_all : Sqlite3.db -> (unit, Error.t) result`.

- [ ] **Step 1: Write the failing migration test**

  ```ocaml
  let test_creates_ledger_tables () =
    let db = Test_support.open_temporary_database () in
    Alcotest.(check bool) "migration succeeds" true
      (Result.is_ok (Migration.apply_all db));
    List.iter (Test_support.table_exists db)
      [ "users"; "meals"; "weigh_ins" ]
  ```

- [ ] **Step 2: Run the test to verify it fails**

  Run: `dune exec test/test_migration.exe`

  Expected: compilation fails because `Migration` and `Test_support` do not exist.

- [ ] **Step 3: Add the Dune/opam files, test support, and schema**

  Put the following tables and indexes in `migrations/001_ledger.sql`:

  ```sql
  CREATE TABLE schema_migrations (version INTEGER PRIMARY KEY, applied_at TEXT NOT NULL);
  CREATE TABLE users (
    id TEXT PRIMARY KEY, oidc_issuer TEXT NOT NULL, oidc_subject TEXT NOT NULL,
    created_at TEXT NOT NULL, UNIQUE (oidc_issuer, oidc_subject)
  );
  CREATE TABLE meals (
    id TEXT PRIMARY KEY, user_id TEXT NOT NULL REFERENCES users(id), eaten_at TEXT NOT NULL,
    description TEXT NOT NULL, calories_kcal INTEGER NOT NULL, protein_g REAL NOT NULL,
    carbs_g REAL, fat_g REAL, confidence REAL, estimate_source TEXT, notes TEXT,
    created_at TEXT NOT NULL, updated_at TEXT NOT NULL, deleted_at TEXT
  );
  CREATE INDEX meals_user_eaten_at_idx ON meals(user_id, eaten_at);
  CREATE TABLE weigh_ins (
    id TEXT PRIMARY KEY, user_id TEXT NOT NULL REFERENCES users(id), measured_at TEXT NOT NULL,
    weight_kg REAL NOT NULL, source TEXT NOT NULL CHECK (source IN ('manual', 'withings')),
    external_id TEXT, created_at TEXT NOT NULL, updated_at TEXT NOT NULL, deleted_at TEXT,
    UNIQUE(source, external_id)
  );
  CREATE INDEX weigh_ins_user_measured_at_idx ON weigh_ins(user_id, measured_at);
  ```

  Make `Migration.apply_all` execute unapplied numbered SQL files in one transaction and record each version only after its SQL succeeds. Include the FreeBSD-safe runtime dependencies: `eio`, `eio_main`, `eio_posix`, `cohttp-eio`, `tls-eio`, `httpun-eio`, `jose`, `sqlite3`, `yojson`, `ptime`, `uuidm`, and `cmdliner`.

- [ ] **Step 4: Run the test to verify it passes**

  Run: `dune exec test/test_migration.exe`

  Expected: one passing Alcotest case.

- [ ] **Step 5: Commit**

  ```sh
  jj describe -m "build: initialize portable kcal project"
  jj new
  ```

### Task 2: Define domain values and validation

**Files:**

- Create: `lib/id.ml`
- Create: `lib/time.ml`
- Create: `lib/user.ml`
- Create: `lib/meal.ml`
- Create: `lib/weigh_in.ml`
- Create: `test/test_domain.ml`

**Interfaces:**

- Produces `Meal.create`, `Meal.patch`, `Meal.t`, and `Meal.validate_create`.
- Produces `Weigh_in.manual_create`, `Weigh_in.patch`, `Weigh_in.t`, and `Weigh_in.validate_manual_create`.
- Produces `Time.parse_offset_datetime : string -> (Ptime.t, Error.t) result` and `Time.to_utc_string : Ptime.t -> string`.

- [ ] **Step 1: Write failing value-validation tests**

  ```ocaml
  let test_rejects_negative_meal_calories () =
    let input = Meal.{ description = "Soup"; calories_kcal = -1; protein_g = 1.0;
                       carbs_g = None; fat_g = None; confidence = None;
                       estimate_source = None; notes = None; eaten_at = None } in
    Alcotest.(check bool) "invalid" true
      (Result.is_error (Meal.validate_create input))

  let test_rejects_nonpositive_manual_weight () =
    Alcotest.(check bool) "invalid" true
      (Result.is_error (Weigh_in.validate_manual_create None 0.0))
  ```

- [ ] **Step 2: Run the test to verify it fails**

  Run: `dune exec test/test_domain.exe`

  Expected: compilation fails because the domain modules do not exist.

- [ ] **Step 3: Implement only the typed values needed by the ledger**

  Use string UUID wrappers for `User_id`, `Meal_id`, and `Weigh_in_id`; hide constructors behind `fresh` and `of_string`. Model source as `Manual | Withings`, while only the SQLite manual constructor is available now. Require nonempty trimmed descriptions, `calories_kcal >= 0`, nonnegative macros, `weight_kg > 0`, and `0.0 <= confidence <= 1.0`. Use option-valued patch fields so `None` means unchanged.

- [ ] **Step 4: Run the domain test suite**

  Run: `dune exec test/test_domain.exe`

  Expected: validation tests pass, including empty descriptions, negative macros, invalid confidence, and malformed/no-offset timestamps.

- [ ] **Step 5: Commit**

  ```sh
  jj describe -m "feat(domain): validate meals and weigh-ins"
  jj new
  ```

### Task 3: Implement users and isolation-safe meal persistence

**Files:**

- Create: `lib/store.ml`
- Create: `lib/store_sqlite.ml`
- Create: `test/test_meal_store.ml`
- Create: `test/test_support.ml`

**Interfaces:**

```ocaml
module type S = sig
  val resolve_user : issuer:string -> subject:string -> (User.t, Error.t) result
  val create_meal : user:User.t -> Meal.create -> (Meal.t, Error.t) result
  val get_meal : user:User.t -> Meal_id.t -> (Meal.t, Error.t) result
  val query_meals : user:User.t -> from:Ptime.t option -> to_:Ptime.t option -> limit:int -> (Meal.t list, Error.t) result
  val update_meal : user:User.t -> Meal_id.t -> Meal.patch -> (Meal.t, Error.t) result
  val delete_meal : user:User.t -> Meal_id.t -> (unit, Error.t) result
end
```

- [ ] **Step 1: Write failing isolation and soft-delete tests**

  ```ocaml
  let test_cannot_read_another_users_meal () =
    let store, alice, bob = Test_support.store_with_two_users () in
    let meal = Test_support.create_meal store alice in
    Alcotest.(check bool) "hidden" true
      (match Store_sqlite.get_meal store ~user:bob meal.id with
       | Error Not_found -> true | _ -> false)

  let test_deleted_meal_is_excluded_from_query () =
    let store, user = Test_support.store_with_user () in
    let meal = Test_support.create_meal store user in
    let () = Result.get_ok (Store_sqlite.delete_meal store ~user meal.id) in
    Alcotest.(check int) "visible meals" 0
      (List.length (Result.get_ok (Store_sqlite.query_meals store ~user ~from:None ~to_:None ~limit:100)))
  ```

- [ ] **Step 2: Run the tests to verify they fail**

  Run: `dune exec test/test_meal_store.exe`

  Expected: compilation fails because the store API does not exist.

- [ ] **Step 3: Implement prepared-statement SQLite operations**

  Resolve users through `INSERT ... ON CONFLICT(oidc_issuer, oidc_subject) DO UPDATE ... RETURNING`. Bind every value; do not interpolate request data into SQL. Add `user_id = ?` and `deleted_at IS NULL` to meal reads and patches. Soft delete with `UPDATE meals SET deleted_at = COALESCE(deleted_at, ?), updated_at = ? WHERE id = ? AND user_id = ?`; if no active row was changed, first accept an owned tombstone as successful, otherwise return `Not_found`. Query ordered `eaten_at ASC` with validated limit `1..500`.

- [ ] **Step 4: Run all meal-store tests**

  Run: `dune exec test/test_meal_store.exe`

  Expected: user resolution is stable, users are isolated, CRUD works, repeat delete succeeds, and deleted rows are invisible.

- [ ] **Step 5: Commit**

  ```sh
  jj describe -m "feat(store): add isolated meal ledger"
  jj new
  ```

### Task 4: Implement manual weigh-in persistence

**Files:**

- Modify: `lib/store.ml`
- Modify: `lib/store_sqlite.ml`
- Create: `test/test_weigh_in_store.ml`

**Interfaces:**

```ocaml
val create_manual_weigh_in : t -> user:User.t -> Weigh_in.manual_create -> (Weigh_in.t, Error.t) result
val get_weigh_in : t -> user:User.t -> Weigh_in_id.t -> (Weigh_in.t, Error.t) result
val query_weigh_ins : t -> user:User.t -> from:Ptime.t option -> to_:Ptime.t option -> limit:int -> (Weigh_in.t list, Error.t) result
val update_manual_weigh_in : t -> user:User.t -> Weigh_in_id.t -> Weigh_in.patch -> (Weigh_in.t, Error.t) result
val delete_weigh_in : t -> user:User.t -> Weigh_in_id.t -> (unit, Error.t) result
```

- [ ] **Step 1: Write failing manual provenance and isolation tests**

  ```ocaml
  let test_recorded_weight_is_manual () =
    let store, user = Test_support.store_with_user () in
    let weight = Result.get_ok (Store_sqlite.create_manual_weigh_in store ~user
      Weigh_in.{ measured_at = None; weight_kg = 79.6 }) in
    Alcotest.(check string) "source" "manual" (Weigh_in.source_to_string weight.source)

  let test_cannot_update_another_users_weight () =
    let store, alice, bob = Test_support.store_with_two_users () in
    let weight = Test_support.create_weight store alice in
    Alcotest.(check bool) "hidden" true
      (match Store_sqlite.update_manual_weigh_in store ~user:bob weight.id Weigh_in.empty_patch with
       | Error Not_found -> true | _ -> false)
  ```

- [ ] **Step 2: Run the tests to verify they fail**

  Run: `dune exec test/test_weigh_in_store.exe`

  Expected: compilation fails because the manual weigh-in store operations do not exist.

- [ ] **Step 3: Implement the manual-only operations**

  Insert `source = 'manual'` and `external_id = NULL` unconditionally. Scope all get, query, update, and delete predicates to the local user. Update timestamp/value only when supplied; do not add a source-changing API. Query `measured_at ASC`, exclude tombstones, and use the same bounded range/limit rules as meals.

- [ ] **Step 4: Run the weigh-in tests**

  Run: `dune exec test/test_weigh_in_store.exe`

  Expected: manual provenance, CRUD, deletion, query order, validation, and user isolation pass.

- [ ] **Step 5: Commit**

  ```sh
  jj describe -m "feat(store): add manual weigh-ins"
  jj new
  ```

### Task 5: Add OIDC token verification and authenticated service calls

**Files:**

- Create: `lib/oidc.ml`
- Create: `lib/auth.ml`
- Create: `lib/service.ml`
- Create: `test/test_auth.ml`
- Create: `test/test_service_isolation.ml`

**Interfaces:**

```ocaml
type Oidc.claims = { issuer : string; subject : string; audience : string list; expires_at : Ptime.t }
val Auth.authenticate_bearer : string -> (User.t, Error.t) result
val Service.record_meal : user:User.t -> Meal.create -> (Meal.t, Error.t) result
```

- [ ] **Step 1: Write failing authentication and service-isolation tests**

  ```ocaml
  let test_rejects_expired_token () =
    Alcotest.(check bool) "unauthorized" true
      (match Auth.authenticate_bearer "expired-test-token" with Error Unauthorized -> true | _ -> false)

  let test_service_hides_foreign_meal () =
    let service, alice, bob = Test_support.service_with_two_users () in
    let meal = Result.get_ok (Service.record_meal service ~user:alice (Test_support.meal_create ())) in
    Alcotest.(check bool) "not found" true
      (match Service.get_meal service ~user:bob meal.id with Error Not_found -> true | _ -> false)
  ```

- [ ] **Step 2: Run the tests to verify they fail**

  Run: `dune exec test/test_auth.exe && dune exec test/test_service_isolation.exe`

  Expected: compilation fails because `Auth` and `Service` do not exist.

- [ ] **Step 3: Implement verification behind a fakeable OIDC client**

  Define an OIDC client interface for discovery/JWKS retrieval and a clock interface for expiry checks. Production code retrieves discovery/JWKS over TLS, caches keys respecting cache headers with a bounded fallback TTL, and uses `jose` to verify the JWT signature, exact configured issuer, configured client-ID audience, and expiration. `Auth` converts only verified `(iss, sub)` claims into `Store.resolve_user`. Tests use a fake verifier and never real network calls. Map all malformed, expired, unknown-key, invalid-signature, issuer, and audience failures to `Unauthorized` without logging token text.

- [ ] **Step 4: Run authentication and service tests**

  Run: `dune exec test/test_auth.exe && dune exec test/test_service_isolation.exe`

  Expected: valid identity resolves one stable user, distinct subjects isolate data, and invalid tokens fail as `Unauthorized`.

- [ ] **Step 5: Commit**

  ```sh
  jj describe -m "feat(auth): authenticate Pocket ID users"
  jj new
  ```

### Task 6: Implement MCP JSON-RPC tool dispatch

**Files:**

- Create: `lib/mcp.ml`
- Create: `test/test_mcp.ml`

**Interfaces:**

```ocaml
val Mcp.handle : service:Service.t -> user:User.t -> Yojson.Safe.t -> Yojson.Safe.t
```

- [ ] **Step 1: Write failing MCP boundary tests**

  ```ocaml
  let test_record_meal_rejects_user_id () =
    let request = `Assoc [
      ("method", `String "tools/call");
      ("params", `Assoc [ ("name", `String "record_meal");
        ("arguments", `Assoc [ ("user_id", `String "other-user");
          ("description", `String "Soup"); ("calories_kcal", `Int 250); ("protein_g", `Float 12.) ]) ]) ] in
    Alcotest.(check bool) "invalid arguments" true
      (Test_support.is_invalid_params (Mcp.handle ~service:(Test_support.service ()) ~user:(Test_support.user ()) request))
  ```

- [ ] **Step 2: Run the test to verify it fails**

  Run: `dune exec test/test_mcp.exe`

  Expected: compilation fails because `Mcp.handle` does not exist.

- [ ] **Step 3: Implement the smallest MCP surface**

  Support `initialize`, `tools/list`, and `tools/call`. Advertise exactly the ten approved meal/manual-weight tools. Parse each arguments object strictly, reject unknown ownership/provenance fields, require correct JSON types, and call `Service` with the authenticated user. Serialize timestamps as UTC RFC 3339 and provenance as lowercase `manual`. Map `Invalid_input` to JSON-RPC invalid-params errors, `Not_found` to a sanitized tool error, and unexpected failures to an internal error without details.

- [ ] **Step 4: Run the MCP tests**

  Run: `dune exec test/test_mcp.exe`

  Expected: tool list is exact, valid commands return records, malformed JSON is rejected, and callers cannot select a different user.

- [ ] **Step 5: Commit**

  ```sh
  jj describe -m "feat(mcp): expose ledger tools"
  jj new
  ```

### Task 7: Serve authenticated Streamable HTTP MCP and operational commands

**Files:**

- Create: `lib/config.ml`
- Create: `lib/http.ml`
- Create: `bin/kcal.ml`
- Create: `test/test_http.ml`
- Create: `.env.example`
- Create: `deploy/kcal.rc.d`
- Create: `deploy/Caddyfile`
- Create: `README.md`

**Interfaces:**

```ocaml
type Config.t
val Config.load_from_environment : unit -> (Config.t, Error.t) result
val Http.run : Eio.Stdenv.t -> config:Config.t -> Service.t -> unit
```

- [ ] **Step 1: Write failing HTTP behavior tests**

  ```ocaml
  let test_health_is_public () =
    let response = Test_support.request `GET "/health" [] "" in
    Alcotest.(check int) "status" 200 response.status

  let test_mcp_requires_bearer_token () =
    let response = Test_support.request `POST "/mcp" [] "{}" in
    Alcotest.(check int) "status" 401 response.status
  ```

- [ ] **Step 2: Run the tests to verify they fail**

  Run: `dune exec test/test_http.exe`

  Expected: compilation fails because the HTTP application does not exist.

- [ ] **Step 3: Implement the portable listener and commands**

  Use `httpun-eio` over Eio POSIX sockets. Route only `GET /health` and `POST /mcp`; return 404 elsewhere, reject non-JSON MCP content types, cap request bodies, and require an `Authorization: Bearer <JWT>` header before dispatch. Return one JSON-RPC response per successful MCP POST; do not implement SSE, GET `/mcp`, or application UI routes. Implement `kcal serve` and `kcal migrate` with Cmdliner. Document required environment values: database path, listen address, public base URL, OIDC issuer, OIDC audience/client ID, and a non-secret Caddy configuration. The FreeBSD rc.d script runs the unprivileged jail user; Caddy is the only public TLS listener.

- [ ] **Step 4: Run HTTP tests and the complete suite**

  Run: `dune test && dune build @all`

  Expected: all tests pass and the executable builds.

- [ ] **Step 5: Run a target-jail smoke test**

  Run inside the FreeBSD jail: `kcal migrate && kcal serve`

  Expected: migrations complete; `curl -fsS http://127.0.0.1:8080/health` returns a health JSON document; unauthenticated `POST /mcp` returns HTTP 401.

- [ ] **Step 6: Commit**

  ```sh
  jj describe -m "feat(http): serve authenticated MCP ledger"
  jj new
  ```

## Final verification

- [ ] Run `dune fmt --check`, `dune test`, and `dune build @all`.
- [ ] Run `kcal migrate` twice against an empty temporary database; verify the second run is a no-op.
- [ ] Verify manually that every MCP tool schema lacks `user_id`, `source`, and `external_id` inputs.
- [ ] Re-read `docs/superpowers/specs/2026-09-16-kcal-ledger-mcp-design.md` and confirm no Withings or analytics implementation was introduced.
- [ ] On the FreeBSD jail, run the target smoke test before deployment.
