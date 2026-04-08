# AD4M DevTools Chrome Extension

Real-time debugging panel for AD4M applications, integrated into Chrome DevTools.

## Features

- **Connection Health** — WebSocket state, authentication status
- **Perspective Browser** — Browse perspectives with inline SPARQL query editor
- **GraphQL Operation Log** — Real-time capture of all queries, mutations, subscriptions with timing and errors
- **Notification Inspector** — View registered notifications, trigger queries, match history
- **Agent Info** — DID, initialization and unlock status
- **Performance Bar** — Always-visible counters: queries/s, errors, avg RTT, peak RTT, active subscriptions, memory

## How It Works

The AD4M SDK (`@coasys/ad4m`) automatically initializes a bridge on `window.__AD4M_DEVTOOLS__` in non-production browser environments. The Chrome extension panel reads this data via `chrome.devtools.inspectedWindow.eval()` polling every 1 second.

**Zero runtime overhead** when the DevTools panel is not open — the bridge stores data in a circular buffer (last 500 operations) and all tracking code is guarded behind `window.__AD4M_DEVTOOLS__` checks.

## Build

```bash
cd packages/ad4m-devtools
pnpm install
pnpm build
```

## Install

1. Open `chrome://extensions`
2. Enable "Developer mode"
3. Click "Load unpacked"
4. Select the `dist/` directory

## Usage

1. Open a page running an AD4M application
2. Open Chrome DevTools (F12)
3. Click the "AD4M" tab in the DevTools panel
4. The performance bar shows live counters; use tabs to explore operations, perspectives, and more

## SDK Integration

The bridge is automatically initialized when `Ad4mClient` is constructed in a browser environment (non-production). No manual setup needed.

For manual initialization:

```typescript
import { initDevToolsBridge } from '@coasys/ad4m/devtools/bridge';
initDevToolsBridge(myAd4mClient);
```
