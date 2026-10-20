# Server link language

The **server link language** (`@coasys/server-link-language`) syncs a neighbourhood's links through one [link server](link-server.md), with end-to-end (E2E) encryption under a **room key**. Each neighbourhood is a **link-server room**, named by the template variable `UID`. The server URL is the template variable `SERVER_URL`. The language uses HTTP for auth, commit and catch-up, and a WebSocket for live diffs and telepresence.

**Why:** A small server that a community runs itself gives fast, predictable sync and telepresence. The server stores only ciphertext, so the link-server operator cannot read the links. The older server-backed link language, `centralized-p-diff-sync`, talks to one fixed host and does not encrypt.

**Parent:** [Link languages](README.md)

<details><summary>How it works</summary>

1. `init()`: if `SERVER_URL` or `UID` is still a placeholder, the language stays inert. Otherwise it authenticates: `POST /rooms/:id/auth` for a challenge, signs it with `agentSignStringHex`, sends the signature and its X25519 public key, and gets a JWT.
   A link server admits only DIDs on the link-server room's ACL, unless it runs with `--auto-admit`. The executor and the SDK have no call to add a DID to an ACL: the room's admin uses the link-server operator's page or the HTTP ACL route ([Link server](link-server.md)). The integration tests run the server with auto-admit.
2. E2E setup (`setupKeyRing`): the link-server room's admin creates the first AES-256-GCM room key and seals it to each member's X25519 key. Members fetch their sealed copies with `/keys`. Key rotation is an explicit admin action (`rotateKeys` in `src/api.ts`). A member refreshes its key ring when sync finds a key version it does not hold.
3. `perspectiveCommit`: store the diff locally, encrypt each link at the wire boundary (`toWireLink`), `POST /commit` with up to 3 attempts per segment, then emit the diff.
4. `perspectiveSyncSync`: `GET /sync?since=<seq>`; the WebSocket also pushes `diff` messages. Both go through `applyInboundWireDiff`.
5. Telepresence: online roster, signals and broadcasts over the WebSocket.

The local store in the language is always plaintext; only the wire carries ciphertext.

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/server-link-language/AGENTS.md](../../../bootstrap-languages/server-link-language/AGENTS.md), [README.md](../../../bootstrap-languages/server-link-language/README.md): design notes.
- [bootstrap-languages/server-link-language/index.ts](../../../bootstrap-languages/server-link-language/index.ts): `defineLanguage`, template variables, module state, `setupKeyRing`.
- [src/sync.ts](../../../bootstrap-languages/server-link-language/src/sync.ts): commit, catch-up, `applyInboundWireDiff`.
- [src/encryption.ts](../../../bootstrap-languages/server-link-language/src/encryption.ts): X25519 derivation, key sealing, link encryption.
- [src/api.ts](../../../bootstrap-languages/server-link-language/src/api.ts): `rotateKeys`.
- [tests/js/utils/linkServer.ts](../../../tests/js/utils/linkServer.ts): the link server the integration tests start, with auto-admit.
- [src/auth.ts](../../../bootstrap-languages/server-link-language/src/auth.ts), [src/ws-client.ts](../../../bootstrap-languages/server-link-language/src/ws-client.ts), [src/telepresence.ts](../../../bootstrap-languages/server-link-language/src/telepresence.ts), [src/adapters-deno.ts](../../../bootstrap-languages/server-link-language/src/adapters-deno.ts).

</details>