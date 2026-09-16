# kcal Ledger and MCP Design

## Scope

This first delivery builds the durable nutrition ledger and authenticated remote MCP surface. It covers users, meals, manual weigh-ins, SQLite migrations, Pocket ID OIDC authentication, and deployment basics. Withings OAuth, webhook handling, and synchronization are deferred to the next delivery; no Withings table or incomplete integration is added now.

The product boundary remains: ChatGPT interprets; `kcal` remembers. The service does not estimate nutrition, analyze history, call an LLM, or expose derived analytics.

## Architecture

`kcal` is a single OCaml 5.5.1 executable using Eio and effect handlers. It depends on the portable `eio`, `eio_main`, and `eio_posix` packages, never `eio_linux` or io_uring. Caddy terminates TLS and proxies a small HTTP surface:

- `GET /health` returns a non-sensitive health response.
- `POST /mcp` implements synchronous Streamable HTTP MCP JSON-RPC.

Each MCP request supplies a Bearer token. The OIDC adapter uses Pocket ID discovery and cached JWKS material to validate issuer, audience, signature, and expiration. It maps the validated `(iss, sub)` pair to a local user, creating that user transactionally on first access. The MCP payload cannot select a `user_id`.

MCP and HTTP adapters translate protocol input into typed service commands. The service receives an authenticated `User.t`; it owns authorization, validation, and error translation. The SQLite store owns SQL and migrations. Adapters do not contain SQL.

## Data model

Migration `001_ledger.sql` creates:

- `users`, uniquely identified by `(oidc_issuer, oidc_subject)`;
- `meals`, belonging to one user and soft-deletable;
- `weigh_ins`, belonging to one user, soft-deletable, and retaining a `source` value.

This slice only creates manual weigh-ins, so callers cannot set `source` or `external_id`. A future migration will add the Withings connection schema and an idempotent import path.

Timestamps are stored as RFC 3339 UTC text. The server defaults omitted record timestamps to its current UTC time. Explicit timestamps must be offset-bearing RFC 3339 values and are normalized before persistence. IDs are UUIDs.

## Service operations

The MCP surface contains only observations:

- `record_meal`, `get_meal`, `query_meals`, `update_meal`, `delete_meal`;
- `record_weight`, `get_weight`, `query_weights`, `update_weight`, `delete_weight`.

Queries order records ascending by measurement time and exclude soft-deleted rows. Updates are explicit patches: omitted fields remain unchanged. Get, update, and delete SQL predicates always include both the record ID and authenticated user ID. A missing record and another user's record both produce `Not_found`. Delete is idempotent.

## Validation and errors

The domain rejects empty meal descriptions, negative calories or macros, non-positive weight, and confidence values outside `[0, 1]`. It does not impose lifestyle or health judgments.

Expected failures are typed: unauthorized authentication failures, invalid input, not found, conflict, and sanitized storage failures. Error responses never include SQL details, token values, stack traces, or another user's record existence.

## Testing

Alcotest runs against temporary SQLite databases. Tests cover migration from an empty database, OIDC identity-to-user resolution, user isolation for each record operation, meal CRUD and soft deletion, manual-weight CRUD and provenance, timestamp/value validation, and MCP input rejection for caller-selected ownership. Store and service tests use real SQLite; protocol tests only exercise adapter boundaries.

## Operational shape

The production target is a native FreeBSD jail, not a bhyve Linux VM. The service and Caddy run from FreeBSD packages/opam in the jail and are supervised with `rc.d`, not systemd. The database lives on a local ZFS dataset mounted into the jail; it must not reside on NFS. Configuration is environment-based and documented through an example environment file. The initial command set is `kcal serve` and `kcal migrate`. Caddy owns public TLS; SQLite and the internal listening address are not exposed directly. Target-jail smoke tests cover the Eio HTTP listener and SQLite migration before deployment.

## Deferred follow-up

The next design/plan adds Withings OAuth state handling, encrypted credential storage, historical/incremental imports, locally durable tombstones, webhook-triggered synchronization, a reconciliation command, and its dedicated tests. It will use the same authenticated user and store boundaries defined here.
