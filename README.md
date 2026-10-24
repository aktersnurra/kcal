# kcal

`kcal` is an authenticated [MCP](https://modelcontextprotocol.io) nutrition ledger for meals, manual weigh-ins, and optional imported Withings weight measurements.

It is a single OCaml binary serving JSON-RPC over HTTP, backed by SQLite. Bearer tokens are verified against an OIDC issuer; every tool call is scoped and isolated per user.

## Status

Personal project, running in production for a single user. The design is documented under `docs/superpowers/`, but there is no stability guarantee and no release process — treat it as reference rather than something to depend on.

## Building

Requires OCaml 5.5 and [opam](https://opam.ocaml.org). The build targets a native FreeBSD jail using `eio_posix`; Linux works for development and tests.

```sh
opam install --deps-only .
dune build
dune build @test/runtest
```

## Running

```sh
kcal migrate   # apply SQL migrations
kcal serve     # start the HTTP listener
```

Configuration is environment-based; copy `.env.example` and fill it in. `KCAL_LISTEN_ADDRESS` is a private listener — a reverse proxy such as Caddy is expected to own public TLS, and the service is expected to run under a supervisor such as FreeBSD `rc.d`. Provisioning and deployment live outside this repository.

## Architecture

- `lib/` — the library: domain types, SQLite store, MCP dispatch, OIDC verification, Withings client and transport
- `bin/kcal.ml` — CLI entry points (`serve`, `migrate`, `sync-withings`)
- `migrations/` — forward-only SQL migrations
- `test/` — Alcotest suites; the store and HTTP layers are tested against real SQLite

Domain logic is kept free of I/O: the Withings HTTP transport is injected as a record of functions (`Withings.request`), so the client is tested without a network.

## OAuth discovery

`GET $KCAL_PUBLIC_BASE_URL/.well-known/oauth-protected-resource` publishes RFC 9728 protected-resource metadata. It advertises the `ledger:read`, `ledger:write`, and `withings:manage` scopes. MCP clients use this metadata to discover the authorization server.

## Logging

`kcal` logs to stderr through [`logs`](https://erratique.ch/software/logs), with one line per event: a UTC timestamp, level, source name (e.g. `kcal.http`, `kcal.mcp`, `kcal.oidc`, `kcal.withings.sync`, `kcal.withings.transport`, `kcal.store`), and message. HTTP request lines and MCP call outcomes are always logged; OIDC discovery/verification failures, Withings OAuth, transport and sync failures, and unexpected SQLite errors are logged with enough detail to diagnose them without ever including tokens, secrets, or credentials.

Set `KCAL_LOG_LEVEL` to `debug`, `info` (default), `warning`, `error`, or `quiet` to control verbosity.

## MCP tools

`query_meals` and `query_weights` accept `from`/`to` (RFC 3339 UTC timestamps, inclusive) and `limit` (default 100, max 500) to scope a query instead of relying on `limit` alone; results are ordered oldest first. Every tool's JSON schema (via `tools/list`) now carries a description of its parameters, including this.

Reusable nutrition references (independent of meal history):

- `save_product(name, basis, calories_kcal, protein_g, source, carbs_g?, fat_g?, id?)` — requires `ledger:write`. Omit `id` to create; supply it to replace your existing reference **in full**, preserving its id and creation time. Omitted optional macros become `null`, not unchanged. Unknown or foreign ids return not found. Ownership and timestamps are server-managed.
- `search_products(query, limit?)` — requires `ledger:read`; returns `{ "products": [...] }` with full references, ranked by relevance then name and id. Default limit 100, range 1–500.
- `query_meals(from?, to?, limit?, search?)` also accepts string or nonempty string-array search over description, estimate source, and notes before date bounds and limit; its output remains `{ "meals": [...] }`. `query_weights` does not accept search.

Search accepts a string or nonempty array of plain-text alternatives, scoped to the authenticated user. For example, `search_products({query: ["vassleprotein", "whey protein"], limit: 5})` or `query_meals({search: ["kyckling ris", "chicken rice"]})`. Words from all alternatives are OR-matched, independent of order; partial overlap is allowed. Duplicate words do not add weight or duplicate records. Results use FTS5 BM25 ascending, with stable ties by product name/id or meal eaten_at/id. Owner/date/deleted filters apply before the final limit. Product search matches name, basis, and source; meal search matches description, estimate source, and notes.

Queries use SQLite's actual unicode61 tokenizer, matching the document indexes, through private connection-local TEMP tables on the existing service database connection. Query rows are cleared after compilation; the tables are reused until the connection owner closes it. Unicode spaces and dashes separate independent OR terms. SQLite unicode61 folds Swedish case while preserving diacritics: `BRÖD` matches `bröd`, not `brod` or `Polarbröd`. There is no automatic translation, stemming, synonym, typo, or prefix matching. Quotes, apostrophes, hyphens, FTS operators, and wildcard characters are plain text, not query syntax. Null, mixed arrays, empty arrays, blank alternatives, and ASCII punctuation-only alternatives are invalid; non-ASCII symbol-only input may simply return no matches. Omitting meal search keeps oldest-first behavior.

**Deployment requirement:** SQLite must include FTS5 with unicode61. Migration 004 creates and backfills external-content indexes and transactional maintenance triggers; there is no substring fallback. Local SQLite 3.53.4 supports FTS5; the production FreeBSD build has not been verified.

For example, save `Whey 100` with `basis: "40 g"`, `calories_kcal: 150`, `protein_g: 31`, and `source: "label"`. Next time, call `search_products(query: "Whey 100")`. For 60 g, the agent scales by 1.5 to 225 kcal and 46.5 g protein, then calls `record_meal` normally with those values and appropriate provenance. The basis is free text: the server performs no quantity arithmetic or unit conversion. Saving/replacing a reference never links to or rewrites past meals or their provenance, and logging meals does not automatically populate the reference library.

Aggregate tools avoid client-side arithmetic over raw records:

- `get_daily_totals(date?, timezone?)` — one day's row: `date`, `meal_count`, summed `calories_kcal`, `protein_g`, `carbs_g`, `fat_g`, plus `weigh_in_count` and `mean_weight_kg` (`date` as `YYYY-MM-DD`; defaults to today in `timezone`). On a day with no meals, `meal_count` is 0 and the intake fields are `null`: nothing logged means unknown, not zero. `carbs_g`/`fat_g` are also `null` when no meal that day recorded that macro, and `mean_weight_kg` is `null` without a weigh-in.
- `get_daily_series(from_date, to_date?, timezone?)` — that row for every calendar date in a range (up to 366 days, oldest first), with the range and timezone echoed back. This is the time series of intake against weight.
- `get_summary(from_date, to_date?, timezone?, calorie_target_kcal?, protein_target_g?)` — over a range:
  - `avg_calories_kcal` / `avg_protein_g` over the `logged_day_count` days with a meal (of `day_count`);
  - `calorie_target` / `protein_target`, when requested, echo the target with `days_at_or_under` / `days_at_or_over`;
  - `weight_trend`: `null` without weigh-ins, `kind: "single_reading"` for one, or `kind: "trend"` with `first_kg`, `last_kg`, `change_kg`, `span_days` and a least-squares `slope_kg_per_week`;
  - `implied_maintenance`: `{"kcal": …}` (average intake minus the energy in the weight trend, at 7700 kcal/kg), or `{"kcal": null, "unavailable_reason": …}` with `no_intake_logged`, `no_weight_trend` or `weigh_in_span_too_short` (first and last weigh-in less than 7 days apart). It is only as reliable as the logging is complete.

These three tools take an optional `timezone` that defines where calendar days begin and end: a fixed UTC offset such as `+02:00`, or an IANA name from a built-in table of European zones with EU summer time (e.g. `Europe/Stockholm`). It defaults to UTC. There is no tz database dependency, so other IANA names are rejected.

Argument mistakes the model can fix — a malformed date, an unsupported timezone (the message lists the supported ones), a reversed or over-long range, a non-positive target — come back as a tool result with `isError: true` and an explanation, rather than a bare JSON-RPC "Invalid params".

- `get_latest_weight()` — the single most recent weigh-in (manual or Withings-imported), instead of paging through `query_weights` to find it.

## Logging

`kcal` logs to stderr through [`logs`](https://erratique.ch/software/logs), with one line per event: a UTC timestamp, level, source name (e.g. `kcal.http`, `kcal.mcp`, `kcal.oidc`, `kcal.withings.sync`, `kcal.withings.transport`, `kcal.store`), and message. HTTP request lines and MCP call outcomes are always logged; OIDC discovery/verification failures, Withings OAuth, transport and sync failures, and unexpected SQLite errors are logged with enough detail to diagnose them without ever including tokens, secrets, or credentials.

Set `KCAL_LOG_LEVEL` to `debug`, `info` (default), `warning`, `error`, or `quiet` to control verbosity.

## MCP tools

`query_meals` and `query_weights` accept `from`/`to` (RFC 3339 UTC timestamps, inclusive) and `limit` (default 100, max 500) to scope a query instead of relying on `limit` alone; results are ordered oldest first. Every tool's JSON schema (via `tools/list`) now carries a description of its parameters, including this.

Two aggregate tools avoid client-side arithmetic over raw records:

- `get_daily_totals(date?)` — summed `calories_kcal`, `protein_g`, `carbs_g`, `fat_g`, and a `meal_count` for all meals on a given calendar day (`date` as `YYYY-MM-DD`; defaults to today in `timezone`, UTC if none). `carbs_g`/`fat_g` are `null` when no matching meal recorded that macro.
- `get_daily_series(from_date, to_date?, timezone?)` — the same daily totals for every calendar date in a range (up to 366 days) in one call; days without meals appear with `meal_count` 0.
- `get_summary(from_date, to_date?, timezone?, calorie_target?, protein_target?)` — average calories and protein over days with at least one meal logged (unlogged days are unknown, not zero), counts of days at or under the calorie target and at or over the protein target, the weight trend (first, last, change, least-squares slope in kg/week), and `implied_maintenance_kcal` (average intake minus the energy in the weight trend, only with at least 7 days of weigh-ins; it is only as reliable as the logging is complete).

`get_daily_totals`, `get_daily_series` and `get_summary` take an optional `timezone` that defines where calendar days begin and end: a fixed offset such as `+02:00`, or an IANA name from a built-in table of European zones with EU summer time (e.g. `Europe/Stockholm`). It defaults to UTC. There is no tz database dependency, so other IANA names are rejected.

- `get_latest_weight()` — the single most recent weigh-in (manual or Withings-imported), instead of paging through `query_weights` to find it.

## Logging

`kcal` logs to stderr through [`logs`](https://erratique.ch/software/logs), with one line per event: a UTC timestamp, level, source name (e.g. `kcal.http`, `kcal.mcp`, `kcal.oidc`, `kcal.withings.sync`, `kcal.store`), and message. HTTP request lines and MCP call outcomes are always logged; OIDC discovery/verification failures, Withings OAuth and sync failures, and unexpected SQLite errors are logged with enough detail to diagnose them without ever including tokens, secrets, or credentials.

Set `KCAL_LOG_LEVEL` to `debug`, `info` (default), `warning`, `error`, or `quiet` to control verbosity.

## Withings

Set `WITHINGS_CLIENT_ID`, `WITHINGS_CLIENT_SECRET`, and `KCAL_TOKEN_ENCRYPTION_KEY` before `kcal serve` or `kcal sync-withings`. The encryption key is exactly 64 hexadecimal characters (32 bytes); keep it secret and stable, since it encrypts access and refresh tokens at rest.

Configure the Withings callback URL as `$KCAL_PUBLIC_BASE_URL/withings/callback` and webhook URL as `$KCAL_PUBLIC_BASE_URL/withings/webhook`. OAuth requests only `user.metrics`. Credentials are never returned in MCP or HTTP responses.

Invoke this command periodically to reconcile connected accounts:

```sh
kcal sync-withings
```

The command uses the same synchronization path as callback and webhook flows; it imports only weights and preserves provenance, external identity, and tombstones.
