# Changelog

## 2026-10-26 — Quality hardening (docs/superpowers/plans/2026-10-25-quality-hardening.md)

- Typed configuration and Command-based CLI (`migrate`, `serve`, `sync-withings`); flags override `KCAL_*`/`WITHINGS_*` environment variables, and each command requires only its own settings.
- Hegel properties for secret encryption (round trip, tamper detection), search-index lifecycle against an independent model, and meal patches; the catch-all properties executable is removed.
- README consolidated (duplicate Logging/MCP sections removed; configuration, audience versus resource, migrations documented).
- CI runs `dune build @all`, `dune runtest` and `dune build @fmt`; `hegel`/`ppx_hegel_test` require >= 0.25.0.
- Verified on Linux with OCaml 5.5.1 and 5.2.0. FreeBSD and live Withings were not run.
