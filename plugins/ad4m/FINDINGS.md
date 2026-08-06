# OpenClaw AD4M plugin — findings from the agent-harness wind tunnel

Issues found while exercising the plugin end to end against a containerised,
multi-user AD4M node with a real OpenClaw runtime (`ghcr.io/openclaw/openclaw`,
2026.7.x). Tracked on branch `fix/agent-harness-findings`.

## 1. npm package ships no compiled output — FIXED on this branch

`@coasys/openclaw-ad4m@0.0.2` set `main: "index.ts"` and
`openclaw.extensions: ["./index.ts"]` with no build step, so the published
package carried only TypeScript source. `openclaw plugins install
@coasys/openclaw-ad4m` rejected it:

> package install requires compiled runtime output for TypeScript entry
> `./index.ts`: expected `./dist/index.js`, `./dist/index.mjs`,
> `./dist/index.cjs`, `./index.js`, `./index.mjs`, `./index.cjs`.

OpenClaw accepts a TypeScript entry only from a local dev checkout, not from an
installed npm package — so the documented install path (`openclaw plugins install
@coasys/openclaw-ad4m`) fails for every real user.

**Fix (this branch):** an esbuild bundle → `dist/index.cjs`; `main` and
`openclaw.extensions` point at it; `prepack` and `prepublishOnly` run the build;
`files` ships `dist`. Re-publishing `0.0.3` from this branch restores the npm
install path.

## 2. External mode does not create a distinct user on a multi-user node — OPEN

`setup.ts` external mode authenticates with `request_capability` →
`generate_jwt` (the single-user / admin capability flow). On a node started with
`--enable-multi-user true`, an assistant should instead provision its **own**
identity — `signup(email,password)` then `login_email(...)` — so it holds a
distinct DID and a `user_email`-scoped JWT rather than riding the node's base
agent.

Verified independently that `signup` → `login_email` over the MCP surface yields
a distinct `did:key` with gated access, so the executor already supports this;
the gap sits in the plugin's external-mode flow. **Proposed:** detect multi-user
mode and add a signup/login branch to external setup.
