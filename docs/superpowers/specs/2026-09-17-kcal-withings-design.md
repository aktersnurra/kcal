# kcal Withings Integration Design

## Scope

This delivery adds per-user Withings OAuth, encrypted credentials, weight import, webhook-triggered synchronization, reconciliation, and connection management. It does not add nutrition analysis, Withings activity/sleep/body-composition data, or bidirectional writes.

## Credentials and OAuth

`withings_connections` stores one connection per kcal user, keyed by unique Withings user identity. Access and refresh tokens are AES-GCM encrypted with a 32-byte `KCAL_TOKEN_ENCRYPTION_KEY` supplied outside the database. Tokens are decrypted only for Withings calls and are never logged or returned through MCP.

`withings_oauth_states` stores a hash of a cryptographically random state, its kcal user, expiry, and consumed timestamp. State validation is atomic, bound to the authenticated user, expiring, and single-use. The connect route starts browser OAuth with only `user.metrics`. The callback immediately exchanges the short-lived code, saves the encrypted credentials and upstream identity, runs the initial sync, and subscribes to `appli=1` notifications.

## Synchronization

`Withings_sync.sync` is the only import path. Initial connection, webhook handling, and `kcal sync-withings` call it. It refreshes credentials when necessary, requests incremental measurements with `lastupdate`, normalizes weight to kilograms, and derives a stable external ID from upstream identifiers. It upserts imported rows transactionally: missing rows insert; active rows update; locally deleted rows remain tombstoned. The cursor advances only after all persistence succeeds. Permanent refresh failure marks the connection as requiring reauthorization while retaining imported history.

## HTTP and MCP

`GET /withings/connect` uses the existing authenticated user context. `GET /withings/callback` validates state before code exchange. `HEAD /withings/webhook` succeeds for Withings validation; `POST /withings/webhook` validates content and known Withings identity, then treats the payload as a sync trigger only. It never derives local ownership from request input.

MCP exposes `begin_withings_connection`, `get_withings_status`, and `disconnect_withings`. It never exposes credential values. Existing weight query/read tools return source provenance; imported weights may only be read or locally soft-deleted.

## Testing

Use fake Withings and clock interfaces. Cover OAuth-state uniqueness/expiry/replay, connection ownership, encrypted credential round trips, refresh rotation/permanent failure, idempotent import, changed upstream values, tombstone retention, cursor transactionality, duplicate/out-of-order webhooks, malformed/unknown webhook identities, and multi-user isolation.

## Operations

`kcal sync-withings` reconciles every connected user and is invoked periodically by infrastructure owned in `nuc-setup`. FreeBSD jail/Caddy configuration remains outside this repository.
