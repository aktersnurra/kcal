# kcal

`kcal` is an authenticated [MCP](https://modelcontextprotocol.io) nutrition ledger for meals, manual weigh-ins, and optional imported Withings weight measurements.

It is a single OCaml binary serving JSON-RPC over HTTP, backed by SQLite. Bearer tokens are verified against an OIDC issuer; every tool call is scoped and isolated per user.

## Status

Personal project, running in production for a single user. The design is documented under `docs/superpowers/`, but there is no stability guarantee and no release process — treat it as reference rather than something to depend on.

## Building

Requires OCaml 5.2 or newer (developed on 5.5) and [opam](https://opam.ocaml.org). The build targets a native FreeBSD jail using `eio_posix`; Linux works for development and tests and is the only platform CI verifies.

```sh
opam install --deps-only --with-test .
dune build @all
dune runtest
dune build @fmt   # needs ocamlformat 0.29.0 (opam install --deps-only --with-dev-setup .)
```

## Running

```sh
kcal migrate        # apply SQL migrations
kcal serve          # start the HTTP listener
kcal sync-withings  # reconcile connected Withings accounts
```

Settings come from the environment (copy `.env.example`); a command-line flag of the same name (e.g. `--database-path`) overrides its environment variable. Each command requires only what it uses:

- `migrate`: `KCAL_DATABASE_PATH`.
- `serve`: additionally `KCAL_PUBLIC_BASE_URL`, `KCAL_OIDC_ISSUER`, `KCAL_OIDC_AUDIENCE`; `KCAL_LISTEN_ADDRESS` defaults to `127.0.0.1:8080`. Withings is disabled when all three Withings settings are empty; a partial set is rejected.
- `sync-withings`: the database path plus all three Withings settings.

Configuration errors name the offending setting but never print its value.

`KCAL_PUBLIC_BASE_URL` is an absolute HTTPS URL, optionally with a deployment path; the protected MCP resource is `BASE/mcp`. `KCAL_OIDC_AUDIENCE` is the expected token audience and is independent of that URL, so an opaque ID such as `kcal` is valid. `KCAL_LISTEN_ADDRESS` is a private listener — a reverse proxy such as Caddy is expected to own public TLS, and the service is expected to run under a supervisor such as FreeBSD `rc.d`. Provisioning and deployment live outside this repository.

Migrations are forward-only and run when the database is opened. Back up the SQLite file before upgrading.

## Architecture

- `lib/` — the library: domain types, SQLite store, MCP dispatch, OIDC verification, Withings client and transport
- `bin/kcal.ml` — CLI entry points (`serve`, `migrate`, `sync-withings`)
- `migrations/` — forward-only SQL migrations
- `test/` — Alcotest suites; the store and HTTP layers are tested against real SQLite

Domain logic is kept free of I/O: the Withings HTTP transport is injected as a record of functions (`Withings.request`), so the client is tested without a network.

## OAuth discovery

`GET /.well-known/oauth-protected-resource` (and, for a deployment path, `/.well-known/oauth-protected-resource/<path>/mcp`) publishes RFC 9728 protected-resource metadata for the resource `$KCAL_PUBLIC_BASE_URL/mcp`. It advertises the `ledger:read`, `ledger:write`, and `withings:manage` scopes. MCP clients use this metadata to discover the authorization server.

## Logging

`kcal` logs to stderr through [`logs`](https://erratique.ch/software/logs), with one line per event: a UTC timestamp, level, source name (e.g. `kcal.http`, `kcal.mcp`, `kcal.oidc`, `kcal.withings.sync`, `kcal.withings.transport`, `kcal.store`), and message. HTTP request lines and MCP call outcomes are always logged; OIDC discovery/verification failures, Withings OAuth, transport and sync failures, and unexpected SQLite errors are logged with enough detail to diagnose them without ever including tokens, secrets, or credentials.

Set `KCAL_LOG_LEVEL` to `debug`, `info` (default), `warning`, `error`, or `quiet` to control verbosity.

## MCP tools

`query_meals` and `query_weights` accept `from`/`to` (RFC 3339 UTC timestamps, inclusive) and `limit` (default 100, max 500) to scope a query instead of relying on `limit` alone; results are ordered oldest first. Every tool's JSON schema (via `tools/list`) now carries a description of its parameters, including this.

Responsibilities are split: the LLM interprets food and estimates nutrition; kcal stores meals, products, goals and history, and does only deterministic arithmetic (scaling a product to a quantity, aggregation).

Start a conversation with `get_context()`: goals, today's intake against today's target (`remaining_kcal`, `remaining_protein_g`), the eating window (`open`, `closes_at`/`opens_at`, `hours_since_last_meal`), the latest weight, the last 7 complete days against targets, the last 28 days' weight trend, progress to the target weight and implied maintenance, and up to five recently eaten products.

Goals:

- `set_goals(timezone, weekday_calorie_target_kcal, weekend_calorie_target_kcal, protein_target_g?, eating_window_start?, eating_window_end?, target_weight_kg?)` — requires `ledger:write`; full replacement, so omitted optional goals are cleared. Weekend is Saturday and Sunday. The eating window is two local `HH:MM` times (12:00–20:00 for 16:8; an end before the start crosses midnight). The goals' `timezone` becomes the default for every calendar-day tool.

Reusable nutrition references:

- `save_product(name, basis, calories_kcal, protein_g, source, basis_quantity?, basis_unit?, carbs_g?, fat_g?, id?)` — requires `ledger:write`. Omit `id` to create at `version` 1; supply it to replace your existing reference **in full**, preserving its id and creation time and incrementing `version`. Omitted optional fields become `null`, not unchanged. `basis_quantity` and `basis_unit` (together, e.g. 40 and `g`) make the basis structured so meals can be logged by quantity. Unknown or foreign ids return not found. Saving never rewrites meals.
- `correct_meals_from_product(product_id, reason, dry_run?)` — requires `ledger:write`. When an earlier version was wrong (a misread label, say), rescales the live meals logged from earlier versions to the current one by their logged quantity, recording a revision with `reason` on each. `dry_run` defaults to true and only lists the meals with before/after values. A changed recipe should leave old meals alone, so don't run it then.
- `search_products(query, limit?)` — requires `ledger:read`; returns `{ "products": [...] }` with full references, ranked by relevance then name and id. Default limit 100, range 1–500.
Meals:

- `record_meal` takes either `description`, `calories_kcal`, `protein_g` (plus optional macros), or `product_id` and `quantity` in the product's `basis_unit`: kcal (rounded) and macros are scaled from its current version, `description` defaults to its name and `estimate_source` to its source, and the meal records `product: {product_id, product_version, quantity}`. A meal always stores its own values, so later product changes never alter it. An optional `idempotency_key` makes retries safe: a repeated key returns the meal already recorded, and the key of a deleted meal is rejected.
- `update_meal(id, …, reason?)` keeps the previous values as a revision, with the optional reason. Changing calories or a macro detaches the product link, since the values no longer follow from the product.
- `get_meal(id)` returns the meal with `revisions`, oldest first: each has `revision`, `revised_at`, `reason` and the `previous` values, including the original estimate and its `estimate_source`, `confidence` and `notes`.
- `query_meals(from?, to?, limit?, search?)` also accepts string or nonempty string-array search over description, estimate source, and notes before date bounds and limit; its output remains `{ "meals": [...] }`. `query_weights` does not accept search.

Search accepts a string or nonempty array of plain-text alternatives, scoped to the authenticated user. For example, `search_products({query: ["vassleprotein", "whey protein"], limit: 5})` or `query_meals({search: ["kyckling ris", "chicken rice"]})`. Words from all alternatives are OR-matched, independent of order; partial overlap is allowed. Duplicate words do not add weight or duplicate records. Results use FTS5 BM25 ascending, with stable ties by product name/id or meal eaten_at/id. Owner/date/deleted filters apply before the final limit. Product search matches name, basis, and source; meal search matches description, estimate source, and notes.

Queries use SQLite's actual unicode61 tokenizer, matching the document indexes, through private connection-local TEMP tables on the existing service database connection. Query rows are cleared after compilation; the tables are reused until the connection owner closes it. Unicode spaces and dashes separate independent OR terms. SQLite unicode61 folds Swedish case while preserving diacritics: `BRÖD` matches `bröd`, not `brod` or `Polarbröd`. There is no automatic translation, stemming, synonym, typo, or prefix matching. Quotes, apostrophes, hyphens, FTS operators, and wildcard characters are plain text, not query syntax. Null, mixed arrays, empty arrays, blank alternatives, and ASCII punctuation-only alternatives are invalid; non-ASCII symbol-only input may simply return no matches. Omitting meal search keeps oldest-first behavior.

**Deployment requirement:** SQLite must include FTS5 with unicode61. Migration 004 creates and backfills external-content indexes and transactional maintenance triggers; there is no substring fallback. Local SQLite 3.53.4 supports FTS5; the production FreeBSD build has not been verified.

For example, save `Whey 100` with `basis: "40 g"`, `basis_quantity: 40`, `basis_unit: "g"`, `calories_kcal: 150`, `protein_g: 31`, and `source: "label"`. For "my usual shake, 45 g instead of 40", call `search_products(query: "Whey 100")`, then `record_meal(product_id, quantity: 45)`. A product without `basis_quantity`/`basis_unit` (e.g. "one portion") can still be found and scaled by the agent, which then records the values itself.

Aggregate tools avoid client-side arithmetic over raw records:

- `get_daily_series(from_date, to_date?, timezone?)` — one row per calendar date in a range (up to 366 days, oldest first): `date`, `meal_count`, summed `calories_kcal`, `protein_g`, `carbs_g`, `fat_g`, plus `weigh_in_count` and `mean_weight_kg`, with the range and timezone echoed back. On a day with no meals, `meal_count` is 0 and the intake fields are `null`: nothing logged means unknown, not zero. `carbs_g`/`fat_g` are also `null` when no meal that day recorded that macro, and `mean_weight_kg` is `null` without a weigh-in. This is the time series of intake against weight.
- `get_summary(from_date, to_date?, timezone?)` — over a range, with the same fields for the equally long `previous_period` just before it:
  - `avg_calories_kcal` / `avg_protein_g` over the `logged_day_count` days with a meal (of `day_count`);
  - with goals set: `calorie_target` gives `days_at_or_under` each day's weekday or weekend target and `avg_vs_target_kcal` (negative is under); `protein_target` gives `days_at_or_over`; `eating_window` gives `days_within`, the logged days whose meals all fell in the window; `target_weight` gives `target_kg`, `remaining_kg` and `weeks_to_target` at the current slope (`null` unless the trend heads toward it);
  - `weight_trend`: `null` without weigh-ins, `kind: "single_reading"` for one, or `kind: "trend"` with `first_kg`, `last_kg`, `change_kg`, `span_days` and a least-squares `slope_kg_per_week`;
  - `implied_maintenance`: `{"kcal": …}` (average intake minus the energy in the weight trend, at 7700 kcal/kg), or `{"kcal": null, "unavailable_reason": …}` with `no_intake_logged`, `no_weight_trend` or `weigh_in_span_too_short` (first and last weigh-in less than 7 days apart). It is only as reliable as the logging is complete.

Both take an optional `timezone` that defines where calendar days begin and end: a fixed UTC offset such as `+02:00`, or an IANA name from a built-in table of European zones with EU summer time (e.g. `Europe/Stockholm`). It defaults to the goals' timezone, else UTC. There is no tz database dependency, so other IANA names are rejected.

Argument mistakes the model can fix — a malformed date, an unsupported timezone (the message lists the supported ones), a reversed or over-long range, a non-positive target, a product logged by quantity without a structured basis — come back as a tool result with `isError: true` and an explanation, rather than a bare JSON-RPC "Invalid params".

## Withings

Set `WITHINGS_CLIENT_ID`, `WITHINGS_CLIENT_SECRET`, and `KCAL_TOKEN_ENCRYPTION_KEY` to enable Withings in `kcal serve`; `kcal sync-withings` requires all three. The encryption key is exactly 64 hexadecimal characters (32 bytes); keep it secret and stable, since it encrypts access and refresh tokens at rest.

Configure the Withings callback URL as `$KCAL_PUBLIC_BASE_URL/withings/callback` and webhook URL as `$KCAL_PUBLIC_BASE_URL/withings/webhook`. OAuth requests only `user.metrics`. Credentials are never returned in MCP or HTTP responses.

Invoke this command periodically to reconcile connected accounts:

```sh
kcal sync-withings
```

The command uses the same synchronization path as callback and webhook flows; it imports only weights and preserves provenance, external identity, and tombstones.
