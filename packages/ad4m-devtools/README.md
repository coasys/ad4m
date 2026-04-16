# AD4M DevTools — Chrome Extension for Real-Time Debugging

A Chrome DevTools panel for debugging AD4M applications. Provides real-time visibility into REST requests, generated SPARQL traces, subscriptions, notifications, perspectives, and performance — the observability layer that was missing during AD4M development.

## Quick Start

1. **Build the extension:**
   ```bash
   cd packages/ad4m-devtools && pnpm install && pnpm build
   ```

2. **Load in Chrome:**
   - Open `chrome://extensions`
   - Enable "Developer mode"
   - Click "Load unpacked" → select `packages/ad4m-devtools/dist`

3. **Open on any page using AD4M:**
   - Open Chrome DevTools (Cmd+Opt+I)
   - Click the **"AD4M"** tab
   - Data flows once `Ad4mClient` connects to an executor

## What's Included

### Performance Bar (always visible)
Live counters across the top of every tab:
- **Requests** — total REST requests since page load
- **Errors** — total failed operations
- **Avg RTT** — rolling average round-trip time
- **Peak RTT** — highest single request time
- **Req/s** — requests per second
- **Subs** — active subscription count
- **Evt/s** — unified event-stream message rate
- **REST** — REST bridge detection state
- **Mem** — estimated memory usage of tracked data

### Connection Tab
- REST bridge detection and auth token state
- Unified event-stream activity + active stream count
- REST request / SPARQL trace / Prolog request breakdown
- Subscription update rate
- Executor URL

### Requests Tab (split panel)
**Left panel** — chronological request/trace list with:
- HH:MM:SS.mmm timestamps
- HTTP method badges (GET/POST/PUT/PATCH/DELETE) or trace badges (SPQ/SSE)
- Endpoint / label
- Duration in ms
- Error badge (❌) on failures

**Right panel** — full request details:
- Timestamp, type, transport, method, endpoint, status, duration, payload size
- **REST request** line + request body
- **SPARQL query** text (when available)
- Response body and headers
- **Full call stack** — trace exactly where each request originates
- Error details with type, message, stack trace, nested errors

**Sub-tabs:**
- **Subscriptions** — active subscriptions with model name, perspective UUID, fingerprint hit/miss stats, recent update stream
- **Getters** — getter evaluation traces with property name, query type (sparql/legacy), SPARQL text, duration, result

### Perspectives Tab
- List of all perspectives with UUID and name
- **SPARQL Editor** — type and execute queries against any perspective
- **Link Browser** — paginated table of all links with source/predicate/target filters
- **Subject Classes** — registered SHACL shapes in tree view

### Notifications Tab
- All registered notifications with trigger query, app name, granted status
- **Test Trigger** button — execute the trigger query and see results inline
- **Error highlighting** — red warning badge on notifications with SurrealDB syntax or parse errors
- Red badge on tab label when any notification has issues

### Agent Tab
- Agent DID, connection state, auth state
- **Language status** — installed languages with name, address, load status, timing

### Export
**⬇ Export** button in the top bar downloads the full DevTools state as JSON — requests/traces, subscriptions, notifications, performance metrics, getter traces. Share with team for debugging.

## Architecture

```
┌─────────────────────────────────────┐
│ Chrome DevTools Panel ("AD4M" tab)  │
│ Preact UI • Polls every 1 second    │
└──────────────┬──────────────────────┘
               │ chrome.devtools.inspectedWindow.eval()
┌──────────────▼──────────────────────┐
│ Page Context                        │
│ window.__AD4M_DEVTOOLS__ = {        │
│   getState(), logOperation(),       │
│   trackSubscription(), ...          │
│ }                                   │
└──────────────┬──────────────────────┘
               │ Populated by SDK bridge
┌──────────────▼──────────────────────┐
│ @coasys/ad4m SDK                    │
│ core/src/devtools/bridge.ts         │
│ RestClient • SubscriptionTracker    │
│ PerformanceTracker • NotifMonitor   │
└──────────────┬──────────────────────┘
               │ REST over HTTP + unified SSE
┌──────────────▼──────────────────────┐
│ AD4M Executor                       │
└─────────────────────────────────────┘
```

The SDK bridge activates automatically in non-production environments. Zero runtime overhead when the DevTools panel is not open (bridge only populates data when the `__AD4M_DEVTOOLS__` global is accessed).

## SDK Integration

The bridge is initialized from two places for reliability:
1. `Ad4mClient` constructor (core SDK)
2. `Ad4mConnect.buildClient()` (ad4m-connect)

DevTools hooks are added in:
- `RestClient` — wraps all executor HTTP requests with timing, status codes, payload snapshots, and stack traces
- `ModelQueryBuilder.subscribe()` — tracks subscriptions
- `Ad4mModel.queryToSPARQL()` — logs generated SPARQL traces
- `hydration.ts` evaluateCustomGettersForInstance — traces getter evaluation

## Development

```bash
# Watch mode (rebuilds on save)
cd packages/ad4m-devtools && pnpm dev

# Production build
pnpm build
```

The extension uses Preact for the panel UI (lightweight, ~38KB gzipped). No heavy dependencies.

## What It Catches

Based on real debugging sessions during the SPARQL migration:

| Issue | How DevTools Helps |
|-------|-------------------|
| SurrealDB getter syntax in SPARQL executor | Getter trace shows "unsupported" + the offending query |
| Stale JWT causing REST/SSE auth failures | Connection tab shows auth token state + request errors |
| Duplicate Preact instances from bundling | Stack traces show which module the hooks resolve from |
| Notification triggers with invalid SPARQL | Notification tab highlights SurrealDB syntax with red badge |
| N+1 request patterns in subscriptions | Request list shows duplicate endpoint traffic alongside the underlying SPARQL trace |
| SHACL registration taking 500ms | Performance bar shows request timing spikes on startup |
| Subscription firing for unrelated predicates | Subscription tab shows fingerprint hit/miss ratio |
