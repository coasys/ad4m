# Test Reorganisation

---

## 1. Shared executor architecture (future)

Currently each suite spawns and kills a fresh executor process. This is the dominant cost per run and also prevents building a browser-based interactive test harness (like the We test app).

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

**Why it matters:** Shared executor would make the full suite dramatically faster, and would unlock a browser-based test harness using the same pattern as the We test app (connect once, run scenarios against live perspectives).
