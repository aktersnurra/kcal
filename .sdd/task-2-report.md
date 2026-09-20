# Task 2 report: scoped MCP authorization

- Base: `fc7b8133a3108a499892cc0defbf4e49d9655e29`
- Head: `3afe007dd99363d73e220a0747ff7dc32246a6f8`
- Review package: `.sdd/task-2-review.diff`

## Status

Implemented central MCP tool-scope authorization. `Mcp.handle` now takes an `Auth.identity` and returns `Error \`Forbidden` for protected tool calls without the required scope. The HTTP MCP adapter maps this result to HTTP 403, while invalid bearer credentials remain HTTP 401. `initialize` and `tools/list` remain available to authenticated identities.

TDD record:

- RED: `dune exec test/test_mcp.exe` failed because `Mcp.handle` did not accept `~identity`.
- GREEN: `dune exec test/test_mcp.exe` passed 9 MCP tests after implementation.
- Final validation: `dune test` passed all MCP, HTTP, and Withings HTTP tests.

## Review-2 disposition

Reviewer `review-2` identified incomplete permission-boundary coverage for Withings tools. The implementation already enforced the required policy, so the RED condition was the missing assertions identified by review rather than a failing production behavior. Added focused tests that prove:

- `withings:manage` cannot call ledger read or mutation tools;
- either ledger-only scope cannot call a Withings tool;
- combined ledger scopes still cannot call a Withings tool;
- each Withings tool returns the explicit internal-error mapping without a Withings configuration and a JSON-RPC result when configuration is supplied.

### RED

The review finding was reproduced by inspection of the pre-fix test: it asserted only `Result.is_ok` for configured Withings calls, which also accepts JSON-RPC internal-error responses, and lacked the isolation/configuration assertions. The initial focused execution after adding the assertions is retained in `.sdd/task-2-review-red.log`; it passed because the existing central authorization and configuration behavior already satisfied the newly specified checks.

### GREEN

```sh
eval "$(opam env)"
dune exec test/test_mcp.exe
dune test
```

Both focused MCP validation (9 tests) and the full Dune test suite passed. The complete full-suite output is retained in `.sdd/task-2-review-green.log`.
