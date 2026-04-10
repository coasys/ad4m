# AGENTS.md — tests/

## Overview

Integration tests (Mocha + ts-node) that start a real executor binary and run operations.

## Run

```bash
# From repo root:
pnpm run test-integration
pnpm run test-integration -- --grep "simple"  # specific test

# Requires: executor binary built first (pnpm build-libs)
# Kill lingering processes first: pkill -9 ad4m-executor
```

## Structure

- `tests/js/tests/*.test.ts` — Test suites (Mocha)
- `tests/js/utils/` — Test utilities
- `test-runner/` — Orchestrates executor lifecycle

## Key Details

- Each test suite gets a fresh executor instance on a unique port
- Port allocation: base 12000, incremented per suite (conflicts in 12000-12100 range)
- `wipePerspective()` removes all links AND clears SHACL cache between tests
- Runtime helper: `tests/js/tests/runtime.ts` — `apolloClient(port)` creates GraphQL client
- CI order: build-and-test must pass first (CI builds executor binary before integration tests)
