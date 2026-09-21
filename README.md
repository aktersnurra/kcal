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

## Withings

Set `WITHINGS_CLIENT_ID`, `WITHINGS_CLIENT_SECRET`, and `KCAL_TOKEN_ENCRYPTION_KEY` before `kcal serve` or `kcal sync-withings`. The encryption key is exactly 64 hexadecimal characters (32 bytes); keep it secret and stable, since it encrypts access and refresh tokens at rest.

Configure the Withings callback URL as `$KCAL_PUBLIC_BASE_URL/withings/callback` and webhook URL as `$KCAL_PUBLIC_BASE_URL/withings/webhook`. OAuth requests only `user.metrics`. Credentials are never returned in MCP or HTTP responses.

Invoke this command periodically to reconcile connected accounts:

```sh
kcal sync-withings
```

The command uses the same synchronization path as callback and webhook flows; it imports only weights and preserves provenance, external identity, and tombstones.
