# GitHub Actions CI Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a GitHub Actions check that runs the kcal test suite for pull requests and pushes to `master`.

**Architecture:** A single workflow file defines one Ubuntu test job. The job provisions the project’s required OCaml version, installs the opam dependencies declared by the local package, then runs the Dune test alias.

**Tech Stack:** GitHub Actions, `ocaml/setup-ocaml`, opam, Dune, Alcotest.

## Global Constraints

- Trigger the workflow for every pull request and every push to `master`.
- Use OCaml 5.2, the minimum version declared by `kcal.opam`.
- Run tests only; do not add secrets, deployment, artifacts, or an OCaml version matrix.
- Install the local package dependencies with test dependencies enabled.
- Validate the workflow locally with focused YAML parsing and run `dune runtest`.

---

## File Structure

- Create: `.github/workflows/ci.yml` — GitHub Actions workflow that installs the OCaml environment and runs the test suite.

### Task 1: Add the test workflow

**Files:**

- Create: `.github/workflows/ci.yml`
- Test: `.github/workflows/ci.yml` (YAML structure); repository test alias (`dune runtest`)

**Interfaces:**

- Consumes: `kcal.opam`, which declares OCaml 5.2+, Dune 3.24+, and `alcotest` as a test dependency.
- Produces: the required GitHub Actions `test` check for pull requests and pushes to `master`.

- [ ] **Step 1: Create the workflow file**

Create `.github/workflows/ci.yml` with exactly this content:

```yaml
name: CI

on:
  pull_request:
  push:
    branches:
      - master

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: ocaml/setup-ocaml@v3
        with:
          ocaml-compiler: 5.2.0
      - run: opam install --yes . --deps-only --with-test
      - run: opam exec -- dune runtest
```

- [ ] **Step 2: Validate the workflow YAML**

Run:

```bash
python3 - <<'PY'
from pathlib import Path
import yaml

workflow = yaml.safe_load(Path('.github/workflows/ci.yml').read_text())
assert workflow['name'] == 'CI'
assert 'pull_request' in workflow[True]
assert workflow[True]['push']['branches'] == ['master']
assert workflow['jobs']['test']['runs-on'] == 'ubuntu-latest'
print('workflow YAML structure is valid')
PY
```

Expected: `workflow YAML structure is valid`.

- [ ] **Step 3: Run the project test suite**

Run:

```bash
dune runtest
```

Expected: Dune exits with status 0 after all Alcotest suites pass.

- [ ] **Step 4: Inspect the change**

Run:

```bash
jj diff -- .github/workflows/ci.yml
```

Expected: The diff contains only the new CI workflow, with the specified triggers, OCaml 5.2 setup, dependency installation, and Dune test command.

- [ ] **Step 5: Commit the workflow**

Run:

```bash
jj describe -m "ci: run tests with GitHub Actions"
jj new
```

Expected: The workflow change is recorded in a commit with the stated conventional commit message.
