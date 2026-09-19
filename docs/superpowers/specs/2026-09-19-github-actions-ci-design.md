# GitHub Actions CI Design

## Purpose

Add a GitHub Actions workflow that verifies the OCaml project's test suite for pull requests and changes merged to `master`.

## Workflow

Create `.github/workflows/ci.yml` with one `test` job running on `ubuntu-latest`.

The workflow runs on:

- every pull request;
- every push to `master`.

The job uses `ocaml/setup-ocaml` to provision OCaml 5.2 and install dependencies declared in `kcal.opam`. It then runs `dune runtest`.

## Scope

The workflow performs tests only. It does not publish artifacts, deploy software, require secrets, or use a version matrix.

## Failure behavior

GitHub Actions reports a failed check whenever dependency installation or `dune runtest` exits non-zero.

## Verification

Validate the workflow YAML structure locally and confirm the repository test command succeeds with `dune runtest`.
