# ad4m-connect

`@coasys/ad4m-connect` (directory `connect/`) is the browser and Node library that gives an app a connected, authenticated `Ad4mClient`. It finds the executor (local port 12000 by default, or a remote URL), runs a login flow, keeps the token in `localStorage`, and shows the connection UI. In **embedded mode** a host page sends the executor URL and the token by `postMessage`, and can proxy the socket.

**Why:** Every app needs the same steps: find the executor, get a grant, keep it, and react when the executor refuses it. One library does them. Embedded mode lets a host page own the credential, so an app inside it does not ask the person again.

**Parent:** [TypeScript SDK](README.md)

<details><summary>How it works</summary>

| Mode | Steps | Token kind |
|---|---|---|
| Local executor | `requestCapability()` → person approves in the launcher → `verifyLocalAd4mCode(code)`: the capability-request flow of [App grants](../../access/app-grants.md) | app grant on the main agent |
| Remote, multi-user | `isMultiUser()`; `loginWithPassword`, or `submitEmail` + `verifyEmailCode`, or `createAccount` | user token (managed agent) |
| Guest | `connectAsGuest(hostUrl)`: random `guest-<uuid>@flux.demo` email and password in `localStorage`, `user.login`, and `user.create` + `user.login` when that fails | user token |
| Embedded | wait for `AD4M_CONFIG` from `window.parent`; the origin is checked against `allowedOrigins` when the app sets it, and proxy mode requires it | the parent's token |

1. `connect()` runs the health check and the first authenticated read together. Before the health check passes, the token goes only to the URL where it was last accepted; a new URL gets it after its health check passes.
2. When the executor later refuses the token (`onAuthFailed`), the library discards the token and the state becomes `unauthenticated`.
3. A locked executor (`Executor is locked…`, close 1013) keeps the token.

</details>

<details><summary>Where in the code</summary>

- [connect/src/core.ts](../../../../connect/src/core.ts): `Ad4mConnect` (`connect`, `requestCapability`, `verifyLocalAd4mCode`, `loginWithPassword`, `submitEmail`, `verifyEmailCode`, `createAccount`, `connectAsGuest`, embedded mode).
- [connect/src/PostMessageWebSocket.ts](../../../../connect/src/PostMessageWebSocket.ts), [connect/src/PostMessageFetch.ts](../../../../connect/src/PostMessageFetch.ts): the proxy transport.
- [connect/src/web.ts](../../../../connect/src/web.ts), [connect/src/components/](../../../../connect/src/components): the UI.
- [connect/src/services/](../../../../connect/src/services): the hosting index (`fetchUserInfo`, `requestPayment`).
- [docs-src/pages/auth.mdx](../../../../docs-src/pages/auth.mdx), [docs-src/pages/multi-user.mdx](../../../../docs-src/pages/multi-user.mdx).

</details>
