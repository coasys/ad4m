# @coasys/connect

The app's side of Coasys Connect on the web. It replaces `@coasys/ad4m-connect`: no dialog, no
host browser, no token in storage.

## Flow (`src/core.ts`)

`Ad4mConnect.connect()`:

1. Make `state` (16 random bytes) and `codeVerifier` (32 random bytes, `src/pkce.ts`);
   `codeChallenge` = base64url SHA-256 of the verifier (RFC 7636 S256).
2. Open the Connect frame at `<authDomain>/connect`: an iframe overlay (default) or a popup.
   Wait for its `ready` message.
3. Post `ConnectRequest` to the auth origin (never `*`). Accept only a message whose origin is
   the auth origin, whose `source` is the frame's window and whose `state` is this one
   (`isFrameMessage` in `src/messages.ts`). `denied` rejects.
4. `user.redeemCode {code, codeVerifier}` on an anonymous client at the executor the frame named
   (http, https, ws, wss only). Build the `Ad4mClient` with the token.
5. Renew `RENEW_LEAD_S` before expiry: `agent.sessionChallenge`, `ConnectRenew` to the frame,
   `ConnectRenewed` back, `agent.renewSession`, `client.setToken`. `disconnect()` calls
   `agent.logout` and posts `ConnectSignOut`.

## Rules

- The token lives in the object's memory. Never write it to storage. `removeLegacyTokens`
  (`src/utils.ts`) removes the `<version>/ad4m-token` keys the old package left, at start.
- The verifier is in no message to the frame: the frame cannot redeem a code it was given.
- The frame is on another origin: trust nothing in a message beyond its shape. A message that
  fails `isFrameMessage` or names another `state` is ignored, not an error.
- `connectAsGuest(hostUrl)` is the one path without a frame (hosted multi-user nodes): a random
  account whose credentials, not the token, stay in `localStorage`.
- Message types (`ConnectRequest`, `ConnectAnswer`, `ConnectRenew`, `ConnectRenewed`,
  `ConnectDenied`, `ConnectReady`, `ConnectSignOut`) are exported for the frame to import. Change
  one only together with the frame (WE `apps/we-web` Connect page).
- Entry points `@coasys/connect`, `/core` (default export `Ad4mConnect`) and `/utils`
  (`getLocal`, `setLocal`) are used by WE. Keep them.

## Commands

- `pnpm run typecheck`, `pnpm run build` (`tsc` declarations, then esbuild into `dist/`).
- `pnpm test` (vitest + happy-dom). Needs Node 24: under Node 26 `localStorage.clear` is
  undefined.
- Tests fake the frame (a window object that answers `postMessage`) and `Ad4mClient`; the
  executor side is tested in `rust-executor/src/identity/key_login.rs`.
