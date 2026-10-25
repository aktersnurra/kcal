# Quality Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans or, if the user authorizes delegation, superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix all review findings and migrate to the user's OCaml/system-design/Tiger Style/Hegel conventions without losing data or breaking successful ledger API contracts.

**Architecture:** Validated domain constructors feed an application service and an abstract SQLite store. Pure calculations and decoding return structured errors; external effects and atomic state changes live at explicit boundaries. Withings imports preserve original batch provenance and freshness through pagination and persistence.

**Tech Stack:** OCaml >= 5.2, Dune, Base/Core, Command/Command_unix, ppx_jane, Eio, SQLite, Yojson, Ptime, Alcotest, Hegel, ocamlformat.

**Spec:** `docs/superpowers/specs/2026-10-25-quality-hardening-design.md`

## Global Constraints

- Keep `kcal migrate`, `kcal serve`, and `kcal sync-withings`.
- Keep Eio, SQLite, Yojson, Ptime, HTTP libraries, and the existing injected Withings transport. No Lwt, custom effect framework, speculative GADTs, or new storage abstraction framework.
- Retain OCaml >= 5.2 unless dependency verification demonstrates a necessary change; align CI, opam, and README. Local verification currently uses OCaml 5.5.1.
- Native FreeBSD remains a deployment target. Hegel runs on supported Linux CI; do not claim FreeBSD verification without running it.
- No network calls to live accounts and no production database changes during development.
- Do not add arbitrary nutritional upper bounds merely to avoid arithmetic edge cases.
- Preserve intentional Withings behavior: existing measurement timestamps and deletion tombstones survive subsequent imports.
- Do not change the partial-patch contract to add clearing nullable meal fields in this work.
- No implementation begins before the user approves this design/plan and execution approach.

## Review Focus

- A valid JSON number beyond float range must fail before a durable write, not become a misleading response-serialization parse error: Task 2.
- A calendar endpoint can parse successfully but lack a representable UTC midnight/next day: Task 3.
- A later Withings page can fail after earlier pages succeeded, and a connection can change while fetching: Tasks 4 and 5.
- Existing databases already contain dangling connection references; simply enabling foreign keys breaks disconnect: Task 5.
- Existing deployments use an opaque audience and environment-only startup, while commands have different required settings: Task 7.

## File and responsibility map

- `lib/nutrition.{ml,mli}`: validated scalar types and checked arithmetic.
- Existing domain modules plus new `.mli` files: validated inputs, private records, provenance, explicit state.
- `lib/error.{ml,mli}`: error ADTs and rendering; no message-based decisions.
- `lib/store_sqlite.{ml,mli}`: abstract connection lifecycle and persistence operations.
- `lib/migration.{ml,mli}`, `lib/embedded_migrations.ml` (generated), `migrations/005_connection_history.sql`: migration execution, single-source embedded schemas, retained-history references.
- `lib/service.{ml,mli}`: application operations for ledger, aggregation, and Withings lifecycle.
- `lib/https_transport.{ml,mli}`: shared trust-bundle/TLS construction.
- `lib/withings*.{ml,mli}`: provider decoding/pagination, OAuth, sync, connection states, reconciliation.
- `lib/mcp.ml`, `lib/mcp_arguments.{ml,mli}`, `lib/mcp_json.{ml,mli}`, `lib/mcp_schema.{ml,mli}`: dispatch, boundary decoding, JSON projections, tool schemas.
- `lib/config.{ml,mli}`, `bin/kcal.ml`: typed configuration and Command entry point.
- Existing test files: regression examples and colocated Hegel properties; `test/test_support.ml` owns bracketed fixtures.
- `dune`, `lib/dune`, `bin/dune`, `test/dune`, `kcal.opam`, `.ocamlformat`, `.github/workflows/ci.yml`, `README.md`, `.env.example`: build/style/deployment contracts.

## Task 1: Establish the Base/tooling and test-resource foundation

**Files:** `kcal.opam`, all Dune files, `.ocamlformat`, `lib/id.ml` and new interfaces for IDs, `test/test_support.ml`, existing test files.

**Interfaces:**
- Add explicit `equal`, `compare`, and safe sexp support to `Id.S` while preserving distinct abstract ID types.
- Replace open-ended test fixtures with `with_store : (Store_sqlite.t -> User.t -> 'a) -> 'a` and `with_two_users : (Store_sqlite.t -> User.t -> User.t -> 'a) -> 'a` (initially with the existing store representation).

- [x] Add lifecycle tests proving fixtures close SQLite handles on success and exceptions. Run the focused test and observe failure before implementing brackets.
- [x] Add Base/Core/Core_unix/ppx_jane dependencies, check the installed Command_unix package/API and supported compiler constraints, and configure formatting and development-only warning policy.
- [x] Implement bracketed test fixtures; convert existing test call sites without weakening assertions. Every Hegel case must release its database before replay/shrink.
- [x] Add explicit ID comparison/printing and replace generic record/ID/JSON test equality with typed comparators, derived equality, or field assertions. Retain explicit float tolerance where appropriate.
- [x] Run `dune build @all` and `dune runtest --force`; both must succeed. Check all new fixture exception paths.
- [x] Review the diff and create a focused jj change/checkpoint only under the selected execution workflow.

## Task 2: Enforce numeric/domain invariants and safe summaries

**Files:** `lib/nutrition.{ml,mli}`, `lib/meal.{ml,mli}`, `lib/product.{ml,mli}`, `lib/weigh_in.{ml,mli}`, `lib/error.{ml,mli}`, `lib/summary.{ml,mli}`, `lib/store_sqlite.ml`, `lib/mcp.ml`, `lib/http_adapter.ml`; domain/store/summary/HTTP/MCP tests.

**Interfaces:**
- `Nutrition.Calories.of_int : int -> (t, Error.t) result`, private `t = int`.
- `Nutrition.Grams.of_float`, `Nutrition.Kilograms.of_float`, `Nutrition.Confidence.of_float : float -> (t, Error.t) result`, each with private float representation and its domain-specific invariant.
- `Meal.Create.of_raw : raw -> (t, Error.t) result`, `Meal.Patch.of_raw : raw -> (t, Error.t) result`; equivalent input constructors for Product and Weigh_in. Raw record types name external fields; validated `t` values are abstract.
- Persistence consumes validated inputs and reconstructs private durable records through checked domain constructors.
- `Summary.daily_series : Time.zone -> Date_range.t -> meals:Meal.t list -> weigh_ins:Weigh_in.t list -> (day list, Error.t) result`; `summarise` similarly returns `(t, Error.t) result`.
- Error ADTs include structured invalid-field, numeric-overflow, and unrepresentable-result conditions; rendering remains at boundaries.

- [x] Add failing HTTP tests for `1e309` meal macros and weight: error response, zero rows persisted, subsequent reads succeed. Add nonfinite create/patch/target tests and `max_int + 1` summary regression.
- [x] Add Hegel properties in domain/summary test files for arbitrary integers/floats, optional macros, and checked totals. Do not cap generators at normal meal sizes.
- [x] Run focused tests and record expected failures against current production functions.
- [x] Implement scalar/input constructors, immutable private record reconstruction, shared create/patch invariants, checked aggregation, and typed error mapping. Update store/MCP/service call sites.
- [x] Restrict JSON parse-error handling to `Yojson.Safe.from_string`; response errors must not be presented as request parse failures.
- [x] Verify valid nutrition cases preserve existing successful JSON payloads and patch semantics; run all tests.
- [x] Review the diff/checkpoint.

## Task 3: Make calendar boundaries total and restore broad time properties

**Files:** `lib/date.{ml,mli}`, `lib/time.{ml,mli}`, `lib/date_range.{ml,mli}`, `lib/mcp.ml`, `lib/error.{ml,mli}`, `test/test_summary.ml`, `test/test_domain.ml`, `test/test_properties.ml`, `test/test_mcp.ml`.

**Interfaces:**
- `Date.add_days : t -> int -> (t, Error.t) result`.
- `Time.local_day_bounds : zone -> Date.t -> (Ptime.t * Ptime.t, Error.t) result`.
- `Date_range.utc_bounds : Time.zone -> t -> (Ptime.t * Ptime.t, Error.t) result`.
- Where local-date conversion cannot represent a result, expose an explicit result rather than an undocumented exception.

- [x] Add failing tool regressions for `9999-12-31` in UTC and `0000-01-01` in `+02:00`; require actionable tool errors without uncaught exceptions.
- [x] Add full-calendar date parsing/roundtrip and date/zone bounds properties, including leap days, month ends, DST changes, both calendar endpoints, and fixed offsets through +/-14 hours.
- [x] Run focused tests, then implement fallible arithmetic/bounds and propagate errors before queries.
- [x] Move time/summary properties from the catch-all properties file into their owning test files; remove the incorrect year/formatting restriction comment and register the PPX where needed.
- [x] Verify ordinary DST 23/24/25-hour behavior, inclusive ranges, local bucketing, and endpoint errors; run all tests.
- [x] Review the diff/checkpoint.

## Task 4: Preserve upstream pagination, errors, and batch freshness

**Files:** `lib/withings.{ml,mli}`, `lib/withings_sync.{ml,mli}`, `lib/store_sqlite.ml`, `lib/weigh_in.{ml,mli}`, `lib/error.{ml,mli}`, Withings client/sync tests.

**Interfaces:**
- Page decoding returns an explicit continuation and received watermark; `Withings.S.get_measurements` still returns a complete `measurement_batch` only after every page succeeds.
- `Withings.credentials_of_response ~now` is pure; `Withings.make` receives a clock for token lifetimes.
- `Store_sqlite.persist_withings_import` receives `~expected_cursor:int64 option` separately from `~cursor:int64 option`, plus the expected account via the validated connection.
- `Withings_sync.imports` accepts a connected account identity rather than optional identity; manual/imported origin uses a variant carrying the imported identity.
- Provider failures use variants, including credential rejection, API status, transport HTTP status, and malformed response; remove exact-message matching.

- [x] Add a full-sync regression: stored cursor 100/weight 80, received cursor 99/weight 70, expected state remains 100/80. Keep the existing store-level stale test.
- [x] Add a multi-page request fixture (`more=1`, returned offset) and a failure on page two; assert all pages are requested and no partial batch/cursor is persisted on failure.
- [x] Add repeated-offset/missing-offset malformed-page tests, raw missing-watermark tests, and equal-watermark correction tests.
- [x] Run failing tests, then implement continuation decoding/fetching, explicit upstream errors, injected time, and raw watermark forwarding.
- [x] In the persistence transaction check account identity and current watermark; for missing watermarks require equality with the fetch baseline. Preserve tombstones and existing timestamps.
- [x] Add Hegel page-partition equivalence and sync-state properties against a simple independent map/cursor model. Cover duplicate observations and out-of-order batches.
- [x] Verify all Withings suites and the full suite; review the diff/checkpoint.

## Task 5: Make connection transitions and database integrity explicit

**Files:** `lib/store_sqlite.{ml,mli}`, `lib/withings_connection.{ml,mli}`, `lib/service.{ml,mli}`, `lib/withings_sync.ml`, `lib/withings_oauth.ml`, `lib/http_adapter.ml`, `lib/mcp.ml`, `lib/migration.{ml,mli}`, `lib/dune`, `migrations/005_connection_history.sql`, migration and Withings tests.

**Interfaces:**
- `Store_sqlite.open_database : path:string -> (t, Error.t) result`, `close : t -> (unit, Error.t) result`; opaque `t`, centralized foreign-key initialization and migrations.
- `Store_sqlite.replace_withings_connection : t -> user:User.t -> key:Secret.Key.t -> Withings.credentials -> (Withings_connection.connected, Error.t) result` encrypts before transactional state replacement.
- Refresh persistence receives the expected connected identity; stale refreshes cannot overwrite a different linked account.
- `Withings_connection.status = Disconnected | Connected of ... | Reauthorization_required of ...`; connected type requires actual account identity and expiry.
- `Service.connect_withings`, `disconnect_withings`, and `withings_status` own application operations; adapters no longer access `oauth.store`.
- `Secret.Key.of_bytes` validates once and returns an opaque immutable key; encryption/decryption consume this key.

- [x] Add failing relink-save regression using a failing SQL trigger: old identity/tokens/cursor remain intact. Test refresh scoping after account replacement.
- [x] Add a v4 database fixture with imported rows and dangling connection references. After migration/disconnect assert preserved observation IDs/values/timestamps/tombstones, null linkage, and empty `foreign_key_check`.
- [x] Add explicit disconnected/status and incomplete-connection decoding tests.
- [x] Run the regressions, then generate embedded migrations from numbered SQL files and remove the duplicated string definitions from Store_sqlite.
- [x] Implement the forward-only reference migration, centralized connection lifecycle, explicit migration bookkeeping bootstrap, typed connection/key boundaries, and atomic lifecycle operations. Never hold a database transaction across upstream HTTP calls.
- [x] Update all application entry points and fixtures to use initialized stores; retain deliberate `For_testing` access for SQL fault injection/migration assertions.
- [x] Add a Hegel connection/import state machine covering failed relink, disconnect/reconnect, deletion tombstones, and refresh/account changes. Assert invariants after each successful step.
- [x] Run migration fixtures, connection/sync suites, and all tests; review the diff/checkpoint.

## Task 6: Finish protocol fixes and cohesive application boundaries

**Files:** `lib/http_adapter.{ml,mli}`, `lib/mcp*.{ml,mli}`, `lib/oidc.{ml,mli}`, `lib/https_transport.{ml,mli}`, `lib/withings_transport.{ml,mli}`, `lib/service.{ml,mli}`, remaining library interfaces, `lib/store.ml`, HTTP/MCP/auth tests.

**Interfaces:**
- `Mcp_arguments` parses raw JSON inputs into validated domain requests.
- `Mcp_json` provides wire projections; `Mcp_schema` owns tool schemas; `Mcp` owns scoped dispatch and JSON-RPC envelopes.
- `Service.daily_series` and `Service.summary` own fetch/aggregate operations instead of MCP reaching into persistence.
- Shared HTTPS setup constructs trust/authentication configuration for the existing clients; HTTP transport keeps its current injected contract.
- OIDC discovery URI construction appends `/.well-known/openid-configuration` to the issuer path without dropping that path.

- [x] Add failing wire-framing test asserting a 502 response stays 502. Add unknown-field rejection tests for every zero-argument tool and path-based issuer discovery tests.
- [x] Strengthen existing MCP tests to assert successful tool results, not merely outer `Ok`; test both omitted arguments and explicit empty objects.
- [x] Implement protocol corrections and shared HTTPS setup. Restrict dependency exception catches and preserve Eio cancellation; include failing-transport/malformed-document tests.
- [x] Extract cohesive MCP responsibilities and move aggregate/Withings application operations into Service. Remove unused Store.S and dead forwarding/duplicate OAuth entry points after checking references.
- [x] Add narrow `.mli` files for remaining modules; remove accidental public helpers and keep only needed, named test seams. Compile after each boundary change.
- [x] Finish Base migration across library/test code, explicit comparators/derived equality, meaningful predicate/action naming, and labeled ambiguous arguments. Format changed files.
- [x] Run protocol/auth/HTTP suites and full tests; review the diff/checkpoint.

## Task 7: Migrate CLI and configuration without breaking deployment

**Files:** `lib/config.{ml,mli}`, `bin/kcal.ml`, `bin/dune`, `kcal.opam`, `lib/logging.{ml,mli}`, `.env.example`, configuration tests and a CLI subprocess test.

**Interfaces:**
- Typed database/server/Withings configuration records, safe defaults for optional fields, pure validated constructors, and command-specific `Command.Param.t` builders.
- Command subcommands keep names `migrate`, `serve`, `sync-withings`; use `Command_unix.run` at the entry point.
- Keep existing `KCAL_*` and `WITHINGS_*` names; typed flags override environment inputs. Required-field errors identify names but not values.
- Public protected resource is validated `public_base_url + /mcp`; token audience remains independent. Metadata URL derives from resource, not audience.

- [x] Add failing tests for opaque audience `kcal`, malformed URL/listener/key, migrate with database-only settings, serve with no integration settings, and partially configured Withings.
- [x] Add CLI subprocess tests for help/subcommands, invalid configuration exit status, environment-only migration, flag overrides, and secret redaction. Execute without live network.
- [x] Implement typed configuration and Command wiring. Keep stdout/stderr and exit behavior deliberate; no nested `exit` inside command term construction.
- [x] Use secret-safe sexp printers and test that token/key/client-secret values cannot appear in serialized configuration/errors.
- [x] Verify command help and environment compatibility; run all tests and build executable.
- [x] Review the diff/checkpoint.

## Task 8: Complete PBT coverage, documentation, and release verification

**Files:** existing domain/store/search/summary/secret tests, `test/dune`, `.github/workflows/ci.yml`, `README.md`, `.env.example`, `.ocamlformat`, plan/spec checklists.

- [x] Add colocated Hegel properties for arbitrary valid secret plaintext roundtrip/tampering, generated patch field subsets, and stateful product/meal search-index lifecycle against an independent record model.
- [x] Ensure no property calls the production computation to produce its expected answer. Use full valid numeric/calendar domains and explicit boundary examples; keep exact JSON/schema examples as ordinary tests.
- [x] Remove the catch-all properties executable only after all its existing properties are preserved alongside their owners. Check that failed generated cases leave no SQLite resources open.
- [x] Consolidate README into single authoritative Logging/MCP sections; document actual status/error/configuration behavior, field names, migration/backups, CLI commands, and the distinction between audience and resource.
- [x] Align README/opam/CI compiler requirements and dependency installation. Linux CI must run both `dune build @all` and `dune runtest`; add formatting verification with the pinned formatter.
- [x] Run `dune build @all`, `dune runtest --force`, formatting verification, CLI subprocess checks, and migration fixtures. Probe changed paths with LSP when available and report tool coverage honestly. (LSP probe not run.)
- [x] Review every final diff against the spec's acceptance criteria; confirm no secrets, accidental behavior changes, unrelated edits, or untested interface assumptions.
- [x] Report pass/fail/not-run evidence, explicitly distinguishing local Linux checks from unrun FreeBSD/live-provider checks. Final review follows the user-selected execution approach.

## Self-review and execution notes

- Every original correctness finding maps to Tasks 2–7; interfaces/style/error simplification map to Tasks 1, 2, 5–7; Hegel improvements map to Tasks 2–5 and 8.
- Task order deliberately stabilizes domain signatures before extracting MCP boundaries and replacing CLI wiring.
- Task 5 introduces the opaque store; earlier fixture/persistence signatures must be updated then rather than keeping an unsafe public raw connection escape hatch.
- Any dependency/compiler-platform incompatibility must be reported before silently changing the deployment target.
- This is a broad change. The recommended execution is task-by-task implementation with independent review gates if the user authorizes subagents; otherwise execute sequentially in the current session and report the absence of independent review.
