# kcal

`kcal` is an authenticated MCP nutrition ledger. It records meals and manual
weigh-ins; it does not provide nutritional analysis or recommendations.

## FreeBSD jail deployment

Run it in a native FreeBSD jail, not a bhyve Linux VM. Create a local ZFS
dataset for `/var/db/kcal` and mount it into the jail. SQLite must remain on
that local dataset: do **not** use NFS. Create an unprivileged `kcal` user and
install the executable, `deploy/kcal.rc.d`, and an environment file based on
`.env.example`. Keep the environment file readable only by that service user.

Configure Pocket ID with a client whose redirect/origin configuration matches
`KCAL_PUBLIC_BASE_URL`; set its HTTPS issuer URL and client ID as
`KCAL_OIDC_ISSUER` and `KCAL_OIDC_AUDIENCE`. The service fetches OIDC discovery
and JWKS only over HTTPS.

Caddy is the only public TLS listener. Bind kcal to `127.0.0.1:8080` and use
`deploy/Caddyfile` (with your public hostname). Never expose SQLite or the
internal listener directly.

```sh
install -m 0555 deploy/kcal.rc.d /usr/local/etc/rc.d/kcal
install -d -o kcal -g kcal /var/db/kcal
cp .env.example /usr/local/etc/kcal.env
# edit the environment values, then:
kcal migrate
service kcal onestart
curl -fsS http://127.0.0.1:8080/health
```

`kcal migrate` is safe to run repeatedly; applied versions are not rerun.

## Deferred work

Withings OAuth, credential storage, webhooks, synchronization, reconciliation,
and all related imports are intentionally deferred. No Withings integration is
present in this release.
