# AD4M Test Harness

Reusable automation toolkit for agent-driven AD4M testing. Thin CLI runner + composable action modules.

## Quick Start

```bash
cd test-harness
pnpm install
pnpm build

# List available actions
npx tsx harness.ts --list

# Run a single action
npx tsx harness.ts executor/start --port 12000

# Chain actions
npx tsx harness.ts --run executor/start:port=12000 agent/generate:executorId=exec-1
```

## Architecture

```
harness.ts (CLI) → discovers actions/ → runs with context → JSON output
```

Every action returns `{ ok: boolean, data?: {}, error?: string, duration_ms: number }`.

Session state persists to `session.json` between invocations.

## Available Actions

| Category | Actions |
|----------|---------|
| `executor/` | `init`, `start`, `stop`, `status` |
| `agent/` | `generate`, `unlock`, `jwt` |
| `neighbourhood/` | `create`, `join`, `configure` |
| `app/` | `dev-server`, `connect`, `navigate`, `screenshot` |
| `webrtc/` | `call`, `verify`, `configure` |
| `build/` | `sdk`, `executor`, `app`, `full-stack` |
| `util/` | `query`, `wait`, `cleanup` |

## Build Actions

The build actions encode the full AD4M dependency chain:

### `build/sdk`
Builds core TS SDK → ad4m-connect → hooks in correct order. Handles schema.gql regeneration.

**Key gotcha:** ad4m-connect bundles core via esbuild (`bundle: true`, no `external`). You MUST rebuild connect after any core change.

### `build/executor`
Builds JS executor bundle → Deno snapshot → Rust binary with features.

### `build/app`
Links local AD4M packages into Flux/WE using `link:` (not `file:` — `file:` copies, `link:` symlinks). Clears Vite pre-bundle cache, Turborepo cache, and `.turbo` outputs.

### `build/full-stack`
Orchestrates SDK → Executor → App in correct dependency order.

## Session Context

The context is a dynamic key-value store with typed accessors:

```typescript
const exec = ctx.executor('exec-1');     // ExecutorResource | undefined
const allExecs = ctx.executors();         // ExecutorResource[]
const id = ctx.nextId('exec');            // 'exec-1', 'exec-2', ...
const port = ctx.nextPort(12000);         // Sequential port allocation
```

Actions can compose via `ctx.run()`:

```typescript
const exec = await ctx.run('executor/start', { port: 12000 });
const agent = await ctx.run('agent/generate', { executorId: exec.data.id });
```

## Writing Actions

Create a file in `actions/<category>/<name>.ts`:

```typescript
import type { Action } from '../../lib/types.js';

const action: Action = {
  name: 'category/name',
  description: 'What this action does',
  params: {
    myParam: { type: 'string', description: 'A parameter', required: true },
  },
  async run(params, ctx) {
    const start = Date.now();
    // ... do work ...
    return { ok: true, data: { result: 'value' }, duration_ms: Date.now() - start };
  },
};

export default action;
```

## Library Modules

- `lib/context.ts` — Session state store with persistence
- `lib/api.ts` — Transport-agnostic executor API client (GraphQL)
- `lib/browser.ts` — Playwright wrapper with WebRTC stats
- `lib/process.ts` — Process lifecycle management
- `lib/ssh.ts` — Remote execution via SSH
- `lib/retry.ts` — Backoff, timeout, condition polling
- `lib/types.ts` — Shared TypeScript types

## Testing

```bash
pnpm test
```

## CLI Reference

```bash
# Single action with params
npx tsx harness.ts executor/start --port 12000 --adminCredential my-cred

# Complex params via JSON
npx tsx harness.ts webrtc/verify --params '{"browserIds": ["b-1", "b-2"], "expectedCount": 2}'

# Chain multiple actions
npx tsx harness.ts --run executor/start:port=12000 agent/generate:executorId=exec-1

# List all actions
npx tsx harness.ts --list

# Cleanup
npx tsx harness.ts util/cleanup
npx tsx harness.ts util/cleanup --all --removeData
```
