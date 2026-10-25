# Quality hardening and OCaml style migration

## Intent and authorization

The user requested fixes for every finding in the whole-source review and explicitly selected the full Base/Core and Jane Street Command migration. Preserve the nutrition ledger's observable contracts while making validation, state transitions, error handling, and tests match the OCaml, system-design, Tiger Style, and Hegel skills.

This document proposes the detailed design for approval. Implementation has not started. The decisions below are the assistant's proposed defaults, not additional user requirements.

## Goals

- Fix every reproduced correctness issue and the smaller protocol/configuration issues from the review.
- Make domain validation produce typed values, with narrow `.mli` boundaries.
- Make Withings pagination, freshness, relinking, disconnect, and provenance explicit.
- Adopt Base in application/library code, Core/Command for the CLI, explicit equality/comparison, deriving where useful, and consistent formatting.
- Strengthen existing tests with Hegel boundary and stateful properties, proper resource lifetimes, and assertions about actual outcomes.
- Preserve production data, existing command names, environment-variable deployment, tool names, successful JSON output shapes, and documented query/patch semantics.

## Constraints and non-goals

- Keep Eio, SQLite, Yojson, Ptime, HTTP libraries, and the existing injected Withings transport. No Lwt, custom effect framework, speculative GADTs, or new storage abstraction framework.
- Keep `kcal migrate`, `kcal serve`, and `kcal sync-withings`.
- Retain OCaml >= 5.2 unless dependency verification demonstrates a necessary change; align CI, opam, and README. Local verification currently uses OCaml 5.5.1.
- Native FreeBSD remains a deployment target. Hegel runs on supported Linux CI; do not claim FreeBSD verification without running it.
- No network calls to live accounts and no production database changes during development.
- Do not add arbitrary nutritional upper bounds merely to avoid arithmetic edge cases.
- Preserve intentional Withings behavior: existing measurement timestamps and deletion tombstones survive subsequent imports.
- Do not change the partial-patch contract to add clearing nullable meal fields in this work.

## Domain boundaries

### Nutrition and aggregation

Introduce small private scalar types for nonnegative finite grams, positive finite kilograms, nonnegative calories, and confidence in [0,1]. Constructors return structured errors. Meal/product create inputs and patches become abstract values constructed once from raw boundary inputs. Share scalar constructors across creation and updates.

Domain records are immutable and privately constructible; persistence reconstruction uses a dedicated checked constructor. Invalid durable values become storage-decoding failures, not valid domain objects. Existing corrupt infinity/NaN rows are not silently altered or deleted.

Preserve integer calories on the wire. Checked integer addition returns an arithmetic-overflow error instead of wrapping. Floating aggregations must not produce NaN/infinity in successful results; use numerically stable calculations where appropriate and return a structured unrepresentable-result error otherwise. Change summary functions to return `result` rather than conceal failed arithmetic with defaults.

### Dates and zones

Keep the full date parser domain, including years 0000 and 9999. Calendar operations whose result can leave Ptime's range return `result`. Zoned interval construction validates both UTC bounds before database queries. User-provided endpoint dates produce actionable tool errors, not exceptions. Do not silently clamp dates.

### Weigh-in provenance and connection state

Encode manual and imported origin as variants carrying the data required by that origin. Imported identities require an account identity and group identity; remove the `unknown` fallback. Keep source strings and public weigh-in JSON unchanged.

Expose connection status as `Disconnected | Connected ... | Reauthorization_required ...`, with credentials-bearing connections distinguishable from an absent/incomplete connection. Successful linked state requires identity, encrypted credentials, and expiry. MCP status can explicitly report disconnected rather than turning ordinary absence into a missing-record error.

## Withings synchronization

### Pagination

Decode a page as measurements, optional received watermark, and an explicit continuation (`Complete | Continue of offset`). Follow all continuations using the original incremental-query watermark. Retain the raw received watermark for freshness checks. Repeated/non-progressing continuations are malformed upstream responses rather than successful partial batches. No cursor is persisted until all pages have been fetched and normalized successfully.

### Freshness and atomic import

The persistence transaction owns stale-batch rejection and cursor advancement. The sync layer must not replace an old received watermark with the prior watermark. Batches older than the stored watermark change neither measurements nor cursor. Preserve existing equal-watermark correction behavior and tombstone semantics. When the response omits a watermark, retain the existing cursor and apply only if the transaction's current cursor still equals the cursor used to fetch the batch; otherwise reject the stale operation. Pass this expected cursor separately from the received watermark. In all cases, verify that the linked account still matches the account used to fetch the batch.

### Linking and disconnect

Replace separate identity-upsert and credential-save operations with one application operation: encrypt credentials first, then atomically update identity, ciphertexts, expiry, authorization state, and reset the cursor only if account identity changes. Failed persistence leaves the previous complete connection untouched. Refresh updates must be scoped to the expected connection/account, not merely user id.

Add a forward-only migration rebuilding the weigh-in connection reference with `ON DELETE SET NULL`. Preserve all observations, IDs, timestamps, provenance, uniqueness, and tombstones. Normalize existing dangling connection references to NULL while preserving external identities. Enable foreign keys outside migration transactions on every application connection. Verify `foreign_key_check` after migration. Disconnect removes credentials/linkage without deleting historical measurements.

## Architecture and errors

- `Service` becomes the application-operation boundary, including linking/disconnect and aggregate orchestration. HTTP and MCP do not reach through OAuth records into SQLite.
- `Store_sqlite.t` becomes abstract and owns database initialization/close. Move migration definitions out of Store_sqlite to avoid dependency cycles.
- Generate embedded migrations from the SQL source files at build time, eliminating duplicate maintained schemas.
- Remove unused `Store.S` if there is no real implementation substitution; do not introduce a functor solely to retain it.
- Add focused interfaces for every library module. Keep useful test seams deliberately named `For_testing`, not accidentally public implementation details.
- Use ADTs for invalid domain fields, upstream HTTP/API statuses, malformed responses, credential rejection, arithmetic/calendar limits, and configuration failures. Never branch on human-readable error strings.
- Catch only documented dependency failures at their boundary, preserve cancellation, and distinguish storage failure from absence. Bootstrap migration bookkeeping explicitly rather than translating arbitrary query failures into “not applied.”
- Extract common HTTPS trust/configuration setup used by OIDC and Withings. Supply time to pure credential decoding. Keep RNG and clock effects at entry points/adapters.

## HTTP, MCP, and configuration

- Preserve upstream-failure HTTP 502 through actual wire framing.
- Catch malformed request JSON only while parsing the request, not around database operations or response serialization.
- Enforce empty argument schemas for all zero-argument tools, including omitted-argument behavior.
- Append OIDC discovery to the configured issuer path instead of replacing it.
- Separate token audience from the public protected-resource URL. Derive the resource as the validated public base URL plus `/mcp`; derive metadata from that resource. Audience may remain an opaque identifier such as `kcal`.
- Parse and validate URLs, listener host/port, and encryption keys once in configuration. Return field-specific errors without credential values.
- Use typed Command subcommands and typed configuration records. Environment variables remain supported defaults/inputs; flags override them. `migrate` requires only a database path. `serve` permits Withings to be disabled when all integration settings are absent and rejects partial integration configuration. `sync-withings` requires integration settings.
- Derive sexp for safe configuration types; redact secrets and do not expose plaintext tokens through printers. Provide defaults only for genuinely optional fields, never fake credentials or invalid required values.

## Style migration

Use Base rather than Stdlib implicitly in domain/application modules; Core/Command/Command_unix at the executable boundary. Replace polymorphic equality/comparison with type-specific functions or derived equality/comparison. Use labeled arguments for ambiguous same-type parameters. Keep statements and result-bindings readable with ocamlformat. Enable the skill's explicit development warning policy without imposing development warnings-as-errors on distributed builds.

Move JSON/schema/argument code out of the dispatch implementation only where doing so yields cohesive modules; no generic tool-registration framework is required. Update names that misstate behavior, remove dead forwarding functions, and preserve specific domain names already in use.

## Testing and verification

Regression tests must fail against the current implementation before fixes. Test actual service/sync and HTTP framing paths, not only lower-level helpers. Strengthen MCP assertions to distinguish successful results from `Ok` containing JSON-RPC/tool errors.

Hegel properties belong in corresponding module test files. Cover full accepted scalar ranges, nonfinite rejection, all calendar days including leap/calendar endpoints, generated patch subsets, stateful CRUD/search lifecycle, and Withings import/relink/disconnect models. Generate page partitions and failure points. Use independent models rather than reimplementing production algorithms. Resource-bracket SQLite handles per test case, including shrinking/replay.

Run focused tests per task, all Hegel properties, `dune build @all`, `dune runtest --force`, formatting checks, CLI subprocess checks, and migration fixtures with existing data. Review the final diff and explicitly report unverified FreeBSD/live-provider behavior.

## Acceptance

Every numbered correctness finding, smaller issue, and architectural/style/PBT recommendation from the review has either a tested implementation or a documented, user-approved alternative. Successful MCP payloads remain compatible, old databases upgrade without losing ledger history, existing deployment variables continue to work, and no live credentials appear in logs/tests/printed configuration.
