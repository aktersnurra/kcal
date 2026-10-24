# Lean nutrition-reference lookup

> Search follow-up (2026-10-24): the historical substring-search decisions below
> are superseded. Product `query` and optional meal `search` accept a string or
> nonempty array of plain-text alternatives, compiled once to `Search_query.t`.
> Distinct words are OR-matched through SQLite FTS5 unicode61 (diacritics kept),
> ranked by BM25 ascending, then product name/id or meal eaten_at/id. Owner,
> date and deleted filters precede the final limit; no-search meals stay oldest
> first. Migration 004 backfills indexes and maintains them with triggers.
> No translation, stemming, prefix expansion, raw FTS syntax or new dependencies.
> Empty/blank and ASCII-tokenless alternatives are invalid; non-ASCII symbol-only
> text may return no matches rather than duplicating Unicode tokenization.
> See README's current API and deployment requirement. All unrelated decisions
> below remain unchanged.

## Intent and approved scope

Help an MCP nutrition agent find and reuse food values without scanning the whole
meal log. The user approved reusable references (name, nutrition for a stated
amount, source), save/update/search, and meal-history text search. Nutrition is
approximate: the agent handles arithmetic and records meals exactly as today.

## Constraints and non-goals

- Keep the existing OCaml/SQLite architecture and authentication model.
- No new dependencies.
- No server-side quantity arithmetic, unit conversion, tags, recipes engine,
  mandatory product links, historical rewrites, or automatic library population.
- Existing meal logging and queries without search remain compatible.
- Every reference and search is scoped to the authenticated user.

## Proposed implementation choices (assistant defaults)

These settle small interface details, rather than adding user requirements.

A product represents either a packaged food or a recurring dish. Store `id`,
`user_id`, `name`, `basis` (free text such as `40 g` or `one portion`),
`calories_kcal` (integer), `protein_g` (float), optional `carbs_g`/`fat_g`,
`source` (free-text provenance), `created_at`, and `updated_at`.
Name, basis, and source must not be blank; nutritional values must be finite and
nonnegative. Source can say `label`, or explain `estimated from strawberry flavour`;
no structured verification workflow or confidence field is needed here.

Two new tools:

- `save_product(name, basis, calories_kcal, protein_g, source, carbs_g?, fat_g?, id?)`:
  no id creates a new reference; an id replaces the owner's existing reference in
  full while preserving id/created_at. Optional macros omitted on replacement
  become unknown/null. Unknown or foreign ids return not found, never create.
- `search_products(query, limit?)`: searches name, basis, and source; returns full
  references ordered by name, then id. Default limit 100, accepted range 1–500.

Add optional `search` to `query_meals`; match description, estimate_source, or notes
before applying the existing date bounds, oldest-first ordering, and limit.
Neither `query_weights` nor aggregate tools gain search parameters.

Search is literal substring matching using SQLite `instr(lower(...), lower(?))`.
This avoids wildcard escaping, FTS indexes, and another dependency. ASCII letters
are case-insensitive; Swedish/non-ASCII characters match literally, but SQLite
without extensions does not fold their case (`bröd` matches `Polarbröd`, whereas
`BRÖD` need not). Blank search text is rejected; no matches is an empty list.

Products are independent of meals. Saving or replacing a reference cannot change
meal totals or provenance. Search is a read under `ledger:read`; save requires
`ledger:write`. Clients cannot supply ownership or timestamps.

## Verification

Use real SQLite tests for persistence, full replacement, user isolation,
migration from version 2, repeat migrations, literal searches, date/limit
composition, and deleted-meal exclusion. MCP tests pin schemas, responses,
argument rejection, and read/write scope enforcement. Existing logging and
aggregate tests remain regression coverage.
