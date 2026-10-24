# Nutrition Reference Lookup Implementation Plan

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

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add reusable food-reference lookup and meal-history text search without changing how meals are logged.

**Architecture:** A small Product record lives independently of meals in SQLite. Two MCP tools save/search references; an optional search parameter extends query_meals. The agent interprets the reference amount and calculates meal nutrition.

**Tech Stack:** OCaml 5.5, SQLite, Yojson, Alcotest, dune.

**Spec:** `docs/superpowers/specs/2026-10-24-nutrition-reference-lookup-design.md` (approved scope plus explicitly marked assistant defaults).

## Global Constraints

- Keep the existing OCaml/SQLite architecture and authentication model.
- No new dependencies.
- No server-side quantity arithmetic, unit conversion, tags, recipes engine, mandatory product links, historical rewrites, or automatic library population.
- Existing meal logging and queries without search remain compatible.
- Every reference and search is scoped to the authenticated user.

## Review Focus

- Updating a nonexistent or foreign product id must not insert or expose a record (Task 1).
- Literal `%`, `_`, apostrophes, and Swedish characters must not become SQL patterns or SQL syntax (Tasks 1–2).
- Nullable source/notes on old meals must not prevent a description match (Task 2).
- Adding meal search must not accidentally add it to query_weights or alter no-search/date/limit behavior (Tasks 2–3).
- Tool dispatch must enforce scopes even when arguments are omitted or unexpected ownership fields are supplied (Task 3).

## File map

- Create `lib/product.ml`, `lib/product_id.ml`: input validation, stored record, typed id following Meal/Meal_id conventions.
- Create `migrations/003_products.sql`: forward-only products table, indexed by user/name.
- Modify `lib/store_sqlite.ml`: matching embedded migration, product save/search, meal search predicate.
- Modify `lib/store.ml`, `lib/service.ml`: matching persistence contract and forwarding methods.
- Modify `lib/mcp.ml`: two schemas/serializers/dispatchers and meal-only search argument.
- Create `test/test_product_store.ml`; modify `test/test_migration.ml`, `test/test_meal_store.ml`, `test/test_mcp.ml`, `test/dune`: real implementation coverage.
- Modify the authoritative MCP section in `README.md`; leave unrelated duplicated sections untouched.

### Task 1: Persist and search reusable references

**Interfaces:**
- `Product.input = { name : string; basis : string; calories_kcal : int; protein_g : float; carbs_g : float option; fat_g : float option; source : string }`.
- `Product.t` has the same food fields plus `id : Product_id.t`, `user_id : User_id.t`, `created_at : Ptime.t`, `updated_at : Ptime.t`.
- `Product.validate_input : Product.input -> (unit, Error.t) result`.
- `Product_id` follows `include Id.Make ()`.
- `Store.S.save_product : t -> user:User.t -> id:Product_id.t option -> Product.input -> (Product.t, Error.t) result`.
- `Store.S.search_products : t -> user:User.t -> query:string -> limit:int -> (Product.t list, Error.t) result`.
- Store_sqlite and Service expose these same two operations with their own `t`.

- [ ] Write/register `test/test_product_store.ml` in `test/dune`. Test `Whey 100`, basis `40 g`, 150 kcal/31 g protein, source `label`: save then search `whey` returns its full record. Replacement by id changes it to 155 kcal, preserves id/created_at, and clears omitted optional macros. Other products and an existing meal remain unchanged.
- [ ] Add tests for blank name/basis/source; negative nutrition; non-finite float input; unknown/foreign update ids; foreign search isolation; blank query; empty results; limits 0/501 rejected; limit 1 obeyed; stable name/id ordering. Pin literal `%`, `_`, apostrophe and `bröd` matching, and ASCII case-insensitivity. Allow same-name references: no implicit merge.
- [ ] Extend migration tests: products table exists on a fresh database; running migration twice succeeds; a database bootstrapped with migrations 1/2 and a meal keeps that meal after applying migration 3. Run `dune exec test/test_product_store.exe` and `dune exec test/test_migration.exe`; confirm new expectations fail before implementation.
- [ ] Implement Product/Product_id and migration 3 (identical standalone and embedded schema). Implement save with INSERT or user-scoped UPDATE RETURNING depending on id; do not upsert on unknown id. Preserve current error sanitization. Implement literal substring search across name/basis/source before ordered LIMIT. Add Store.S contracts and Service forwarding methods.
- [ ] Run `dune exec test/test_product_store.exe` and `dune exec test/test_migration.exe`; expect all tests to pass.
- [ ] Review `jj diff`; record the tested change with `jj commit -m "feat(products): add reusable nutrition references"`.

### Task 2: Search meal history without changing existing queries

**Interfaces:**
- Extend `Store.S.query_meals`, `Store_sqlite.query_meals`, and `Service.query_meals` with leading optional `?search:string`, before their existing positional store/service argument. All existing callers still compile unchanged.
- `search=None` retains the existing query; `Some text` adds a literal substring match on description, estimate_source, or notes. Blank text yields `Error.Invalid_input`.

- [ ] Add meal-store tests with matches spread across all three searchable fields, including records with null optional text. Querying `whey` returns only the expected records; querying `bröd`, `%`, `_`, or an apostrophe treats characters literally. Deleted and foreign records never appear.
- [ ] Add composition tests: matches are filtered before limit; inclusive from/to bounds still work; matching results remain oldest first; omitted search preserves existing results; whitespace-only search and invalid limits fail. Keep existing no-search tests unchanged.
- [ ] Run `dune exec test/test_meal_store.exe`; confirm tests fail for missing search support.
- [ ] Add the optional argument and parameterized `instr(lower(...), lower(?))` predicate in the owning store query. Forward the argument through Service. Do not change `query_weigh_ins`, aggregation, or meal creation.
- [ ] Run `dune exec test/test_meal_store.exe`; expect all tests to pass.
- [ ] Review `jj diff`; record the tested change with `jj commit -m "feat(meals): add literal history search"`.

### Task 3: Expose the lean MCP workflow and document it

**Interfaces:**
- `save_product`: schema requires name/basis/calories_kcal/protein_g/source, optionally carbs_g/fat_g/id; returns Product fields including id and timestamps, without user_id. id means full replacement, not a partial patch.
- `search_products`: requires `query`, optional `limit` defaults 100; returns `{ "products": [...] }`.
- `query_meals`: adds optional string `search`, unchanged `{ "meals": [...] }` output.
- Extend `Mcp.required_scope`: save_product → ledger:write; search_products → ledger:read.

- [ ] Add MCP tests that discover tool schemas and perform save → search → replacement through `Mcp.handle`. Assert the example fields and source appear in structuredContent, optional macros serialize as null, and unknown/foreign ids produce the existing not-found tool error.
- [ ] Add scope tests for both new tools; reject user_id/timestamps/unknown arguments and malformed ids. Reject missing search query and missing save fields; test default/invalid limits. Ensure omitted arguments cannot bypass scopes or create data. Pin existing Invalid_input error mapping rather than broadening error handling.
- [ ] Add MCP tests for query_meals(search) with date/limit composition, unchanged omitted-search behavior, and rejection of search on query_weights. Run `dune exec test/test_mcp.exe`; confirm new tests fail before wiring the tools.
- [ ] Add product serialization, property descriptions, strict argument parsers, handlers, scope mapping and dispatch entries in `lib/mcp.ml`. Use meal-specific query properties; leave shared weight query parsing unchanged. Tool descriptions explain full replacement, text basis, agent-side arithmetic, no automatic meal rewrites, and SQLite case-matching limitations.
- [ ] Update `README.md` with two tool signatures, meal search, and the example workflow: save Whey 100 at 150 kcal/31 g per `40 g`, search it, scale values in the agent, record_meal normally. Include ownership, replacement and matching semantics without duplicating implementation details.
- [ ] Run `dune exec test/test_mcp.exe`, `dune runtest --force`, and `dune build`; expect zero failures and exit 0. Probe changed OCaml paths using active LSP diagnostics when available; distinguish unavailable LSP from a clean result.
- [ ] Review the full diff against the spec: no new dependencies, no quantity system, no product/meal links or historical mutations, no unrelated cleanup. Record the tested change with `jj commit -m "feat(mcp): expose product lookup and meal search"`.

## Execution handoff

Recommended: native execution, since all three tasks extend the same small store/service/MCP path and share the existing tests. Implementation starts after the user reviews this plan and chooses the execution approach. No implementation or tests have been run as part of writing this plan.
