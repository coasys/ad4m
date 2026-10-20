# Server link language

The **server link language** (`@coasys/server-link-language`) is the default link language of a new neighbourhood. It syncs the neighbourhood's links through one **link-server room** on one [link server](link-server.md). It encrypts every link of the default graph on the wire under a [room key](room-keys.md), and sends the neighbourhood's **membership links** in plaintext on a separate **membership channel**, so that the link server can admit members by person. Three template variables fix the room for the life of the neighbourhood: `SERVER_URL`, `UID` and `CREATOR`.

**Why:** It admits people by identity, encrypts every link on the wire, and keeps syncing while a member's computer sleeps, because the link server holds the room. The executor, not the app, fills the template, so an app needs no link-server address and no template knowledge. The option that lost for new neighbourhoods was p-diff-sync: it reads no membership graph, so a Holochain neighbourhood admits anyone with its URL ([Perspective diff sync](../perspective-diff-sync.md)).

**Parent:** [Link languages](../README.md)

## Parts
- [Room keys](room-keys.md): the versioned AES-256-GCM keys, their seal to each member, the narrow seed call, and who grants and rotates.
- [Link server](link-server.md): the Node.js server that stores rooms, replays each room's membership graph and admits keys by person.
- [Link-server directory](link-server-directory.md): where the executor gets `SERVER_URL` when it creates a neighbourhood.

<details><summary>How it works</summary>

1. **Template.** `neighbourhood.create` fills `SERVER_URL` from the [link-server directory](link-server-directory.md), `UID` with a fresh 32-byte random salt and `CREATOR` with the creator's `did:scid`. The room id is `base58(sha256(CREATOR ‖ 0x00 ‖ UID))`. A joiner reads all three from the bundle it installs, so it needs no directory. With `UID` unfilled, the language stays inert.
2. **Auth.** `init()` runs `POST /rooms/:id/auth`: a challenge, then a signature by the device key over it, plus the X25519 public key and the key's ownership signature under the signing domain `coasys:room-key`. The answer is `{ token, expiresAt, admitted }`.
3. **Genesis** (creator only). The language sends the `rules` and `admin` links with `{ creator, salt }` through `POST /rooms/:id/membership`. The server checks the room id and creates the room. The creator's instance makes room key version 1.
4. **Join.** A joiner with `admitted: false` sends its own self-join `member` link (with an invite proof when the neighbourhood is invite-only) through `/membership`. When the server's replay counts it, the key is admitted and any member grants it the room keys.
5. **Commit.** `perspectiveCommit` sends only encrypted default-graph links (`POST /commit`). Membership links go only through `/membership`; the executor ignores a membership link that arrives inside an encrypted diff. Unsent commits persist in the language's key-value store and retry on `sync()`.
6. **Sync and telepresence.** `GET /sync?since=<seq>` and WebSocket `diff` messages go through `applyInboundWireDiff`. The WebSocket also carries `membership`, `rotate-needed`, `keys-granted`, telepresence and signals, and closes with `4005` when the key is removed or revoked.
7. **Membership reads.** The language asks the executor through the `membership*` host imports and gets `handleMembershipChange` calls ([Language isolates](../../runtime/isolates.md)). The graph itself: [Membership](../../../perspectives/neighbourhoods/membership/README.md); open joins and invites: [Open joins and invites](../../../perspectives/neighbourhoods/membership/invites.md).

Key-ring states: `none` → `ready` (genesis), `pending` → `ready` (a grant), `error` (an envelope does not open; retry after 10 s), `removed` (close `4005`). `commit()` sends nothing outside `ready`; the executor keeps the diff and reports `LinkLanguageInstalledButNotSynced`.

Neighbourhoods made before protocol 2 have no `CREATOR` in their template. The executor builds no membership replay for them and marks them `legacy`; their link-server room admits by its ACL ([Link server](link-server.md)).

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/server-link-language/AGENTS.md](../../../../bootstrap-languages/server-link-language/AGENTS.md), [README.md](../../../../bootstrap-languages/server-link-language/README.md): design notes.
- [bootstrap-languages/server-link-language/index.ts](../../../../bootstrap-languages/server-link-language/index.ts): `defineLanguage`, the template variables `SERVER_URL`, `UID`, `CREATOR`, the room-id derivation, module state.
- [src/sync.ts](../../../../bootstrap-languages/server-link-language/src/sync.ts): commit, the persisted pending queue, catch-up, `applyInboundWireDiff`.
- [src/auth.ts](../../../../bootstrap-languages/server-link-language/src/auth.ts): the two-step auth with the signed X25519 key; [src/ws-client.ts](../../../../bootstrap-languages/server-link-language/src/ws-client.ts), [src/telepresence.ts](../../../../bootstrap-languages/server-link-language/src/telepresence.ts), [src/adapters-deno.ts](../../../../bootstrap-languages/server-link-language/src/adapters-deno.ts).
- [rust-executor/src/neighbourhoods/create.rs](../../../../rust-executor/src/neighbourhoods/create.rs) (new): `SyncChoice`, `LinkLanguageChoice`, `neighbourhood.create`, which fills the template.
- [rust-executor/src/membership.rs](../../../../rust-executor/src/membership.rs) (new): the executor's replay per neighbourhood behind the `membership*` imports.

</details>
