# Test Suite Tasks

---

## 1. Install VS Code Mocha Test Explorer

Install the Mocha Test Explorer extension to get an interactive test tree in the VS Code sidebar — click ▶ next to any `describe` or `it` block to run it individually, jump to failures, see pass/fail inline.

**Steps:**
1. Install extension: `hbenl.vscode-mocha-test-adapter`
2. Add a `.vscode/settings.json` config pointing it at `tests/js`:
   ```json
   {
     "mochaExplorer.cwd": "tests/js",
     "mochaExplorer.require": ["tests/setup.ts"],
     "mochaExplorer.spec": "tests/**/*.test.ts",
     "mochaExplorer.loader": "ts-node/esm"
   }
   ```

---

## 2. Shared executor architecture (future)

Currently each suite spawns and kills a fresh executor process. This is the dominant cost per run and also prevents building a browser-based interactive test harness (like the we test app).

**The opportunity:** AD4M's multi-user mode already supports multiple agent identities on a single executor. The `startAgent` helper already allocates unique ports and data dirs per agent — the missing piece is a persistent executor that hosts all of them.

**Suites that could share an executor** (stateless beyond their own perspective/agent):
- `test-smoke`
- `test-auth` (auth-app, auth-core)
- `test-sdna`
- `test-model`
- Most `test-multi-user-*` suites

**Suites that must keep their own executor** (test lifecycle or network topology):
- `auth-email-verification` — needs specific proxy/bootstrap URLs at startup
- `multi-user-neighbourhood` — needs `runHcLocalServices()`
- `multi-user-multi-node` — tests inter-executor DHT connectivity
- `integration` — Alice/Bob/Jim need separate Holochain nodes

**Why it matters:** Shared executor would make the full suite dramatically faster, and would unlock a browser-based test harness using the same pattern as the we test app (connect once, run scenarios against live perspectives).
