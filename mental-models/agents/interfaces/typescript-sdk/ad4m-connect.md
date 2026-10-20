# ad4m-connect

`@coasys/ad4m-connect` (directory `connect/`) is the browser and Node library that gives an app a connected, authenticated `Ad4mClient`. Its main route is **Coasys Connect**: the library opens a frame of the **auth domain** (`coasys.id`, outside this repository), where the person picks one of their devices and approves the app; the frame signs a key login with its **browser device key** and passes back a one-time code, which the library redeems at the chosen executor for a token. The token stays in memory only. The library keeps two older routes (a six-digit code for a local executor, email and password for a host), guests, and an embedded mode.

**Why:** No key may share memory or a process with an app, so the key that signs sits in the auth domain's own storage, where a script on the app's page cannot read it. The browser device key, not the master key, signs web logins: it serves every person, including one who made their identity on the desktop with no email, and a lost browser costs one device-key revocation, not a master-key rotation. Only the executor issues tokens; the auth domain only signs, and the code exchange carries PKCE, so a code that leaks cannot be redeemed. The frame shows the approval card for a third-party origin only; the person's own Coasys app skips it. The picker lists the person's own devices, not a list of hosts.

**Parent:** [TypeScript SDK](README.md)

<details><summary>How it works</summary>

```
app page (library)                     auth-domain frame                 platform registry      chosen executor
 state, codeVerifier, codeChallenge,
 reply key ── ConnectRequest ───────►  check origin against ─────────►  GET /v1/connect/apps/{id}
                                        the app's registered origins ◄──  origins, firstParty
                                        no browser key yet: sign-in popup or "Link this browser"
                                        pick a device; approval card (third-party only)
                                        user.sessionChallenge ─────────────────────────────────►
                                        sign KeyStatement (client web, origin, codeChallenge)
                                        user.loginWithKey ─────────────────────────────────────►  code, 60 s
 ◄── ConnectAnswer (sealed to the reply key): { executor, code, did, role }
 user.redeemCode { code, codeVerifier } ───────────────────────────────────────────────────────►
 ◄──────────────────────────────────────────────────────────────────── token (1 h), grantId, role
 renewal: challenge → ConnectRenew → frame signs "renew" → user.renewSession (no screen)
```

| Route | Steps | Token |
|---|---|---|
| Coasys Connect (default for web apps) | the flow above; Switch device sends `ConnectRequest` with `device` and `prompt: "none"` | key session, 1 h, memory only |
| Local executor, no Coasys app | `requestCapability()` → approval on the executor → `verifyLocalAd4mCode(code)` ([App approval](../../access/grants/app-approval.md)) | app grant on the main agent, never the operator role |
| Remote host, password (`hosting: true`) | `loginWithPassword`, `submitEmail` + `verifyEmailCode`, `createAccount`, unchanged | user token |
| Guest | `guest.create` on a host that admits guests | guest grant; the token in `localStorage` under the executor's URL |
| Embedded | wait for `AD4M_CONFIG` from `window.parent`; the origin is checked against `allowedOrigins` when the app sets it, and proxy mode requires it | the parent's token |

1. `connect()` runs the health check and the first authenticated read together. The token goes only to the URL where it was accepted.
2. When the executor later refuses the token (`onAuthFailed`), the library drops it and the state becomes `unauthenticated`. A locked or sealed executor (close 1013) keeps the token.
3. `ConnectSignOut` tells the frame to forget the app's approval on this browser; it never touches the browser key. Log out keeps the browser key valid.
4. At its first start, the library deletes the old `ad4m-token` and `ad4m-url` from `localStorage`.
5. The browser device key lives 30 days and is one device in the person's key log; each embedding site gets its own key, because browsers partition a frame's storage per site.

</details>

<details><summary>Where in the code</summary>

- [connect/src/core.ts](../../../../connect/src/core.ts): `Ad4mConnect` (`connect`, Coasys Connect's request, redeem and renewal, `requestCapability`, `verifyLocalAd4mCode`, `loginWithPassword`, `submitEmail`, `verifyEmailCode`, `createAccount`, the guest route, embedded mode).
- [connect/src/types.ts](../../../../connect/src/types.ts): `ConnectRequest`, `ConnectRenew`, `ConnectSignOut`, `ConnectAnswer`, `ConnectError`, `ConnectGrant`.
- [connect/src/PostMessageWebSocket.ts](../../../../connect/src/PostMessageWebSocket.ts), [connect/src/PostMessageFetch.ts](../../../../connect/src/PostMessageFetch.ts): the proxy transport.
- [connect/src/web.ts](../../../../connect/src/web.ts), [connect/src/components/](../../../../connect/src/components): the UI.
- [connect/src/services/hostIndex.ts](../../../../connect/src/services/hostIndex.ts): the host list, outside the default flow, pointed at the hosting index.
- [docs-src/pages/auth.mdx](../../../../docs-src/pages/auth.mdx), [docs-src/pages/multi-user.mdx](../../../../docs-src/pages/multi-user.mdx).

</details>
