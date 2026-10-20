# Server link language

The **server link language** (`@coasys/server-link-language`) is the default link language of a new neighbourhood. It syncs a neighbourhood's links through one [link server](link-server.md), with end-to-end (E2E) encryption under a [room key](room-keys.md), and sends the neighbourhood's **membership links** in plaintext so that the link server can admit members by person. Each neighbourhood is a **link-server room**, fixed by the template variables `SERVER_URL`, `UID` and `CREATOR`. The language uses HTTP for auth, commit and catch-up, and a WebSocket for live diffs and telepresence.

**Why:** A small server that a community runs itself gives fast, predictable sync and telepresence. The server stores links only as ciphertext, so the link-server operator cannot read them. The older server-backed link language, `centralized-p-diff-sync`, talks to one fixed host and does not encrypt.

**Parent:** [Link languages](../README.md)

## Parts
- [Room keys](room-keys.md): the versioned AES-256-GCM keys, their seal to each member, the narrow seed call, and who grants and rotates.
- [Link server](link-server.md): the Node.js server that stores rooms, replays each room's membership graph and admits keys by person.
- [Link-server directory](link-server-directory.md): where the executor gets `SERVER_URL` when it creates a neighbourhood.

<details><summary>How it works</summary>

1. `neighbourhood.create` fills `SERVER_URL` from the [link-server directory](link-server-directory.md), `UID` with a fresh random salt and `CREATOR` with the creator's `did:scid`; the room id is `base58(sha256(CREATOR ‖ 0x00 ‖ UID))`. If `UID` is still a placeholder, the language stays inert.
2. `init()` authenticates: `POST /rooms/:id/auth` for a challenge, signs it with the device key, sends the signature and its X25519 public key with an ownership signature, and gets `{ token, expiresAt, admitted }`.
3. The creator sends the room's genesis (`rules` and `admin` links) through `POST /rooms/:id/membership` and makes room key version 1. A joiner sends its own `member` link the same way; once the link server counts it, any member grants it the room keys.
4. `perspectiveCommit`: encrypt each default-graph link at the wire boundary (`toWireLink`) and `POST /commit`; membership links go only through `/membership`. Unsent commits persist and retry on `sync()`.
5. `perspectiveSyncSync`: `GET /sync?since=<seq>`; the WebSocket also pushes `diff` and `membership` messages. Diffs go through `applyInboundWireDiff`.
6. Telepresence: online roster, signals and broadcasts over the WebSocket.
7. The language reads the membership graph through the `membership*` host imports and gets `handleMembershipChange` calls ([Membership](../../../perspectives/neighbourhoods/membership/README.md)).

The language's own store holds plaintext, sealed at rest by the host; only the wire carries ciphertext.

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/server-link-language/AGENTS.md](../../../../bootstrap-languages/server-link-language/AGENTS.md), [README.md](../../../../bootstrap-languages/server-link-language/README.md): design notes.
- [bootstrap-languages/server-link-language/index.ts](../../../../bootstrap-languages/server-link-language/index.ts): `defineLanguage`, template variables, the room id, module state.
- [src/sync.ts](../../../../bootstrap-languages/server-link-language/src/sync.ts): commit, the pending queue, catch-up, `applyInboundWireDiff`.
- [src/auth.ts](../../../../bootstrap-languages/server-link-language/src/auth.ts), [src/ws-client.ts](../../../../bootstrap-languages/server-link-language/src/ws-client.ts), [src/telepresence.ts](../../../../bootstrap-languages/server-link-language/src/telepresence.ts), [src/adapters-deno.ts](../../../../bootstrap-languages/server-link-language/src/adapters-deno.ts).
- [rust-executor/src/neighbourhoods/create.rs](../../../../rust-executor/src/neighbourhoods/create.rs) (new): `neighbourhood.create`, which fills the template.

</details>
