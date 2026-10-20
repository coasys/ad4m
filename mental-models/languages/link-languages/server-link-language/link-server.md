# Link server

The **link server** is a Node.js process, separate from the executor, that stores and relays the links of **link-server rooms** for the [server link language](README.md). It runs on Fastify, `ws` and `better-sqlite3`, with one SQLite file. It **admits a key only when the room's [membership graph](../../../perspectives/neighbourhoods/membership/README.md) names the key's person**: it replays the plaintext membership links with the WASM build of the identity core, and asks the shared store's key index which person a `did:key` belongs to (`author_of`). It stores ciphertext and sealed room keys, never a room key or link content.

**Why:** Who may join a neighbourhood must not differ per link language, so the link server reads the same membership graph as every executor, and roles live only in that graph; the link server's own access list lost ([Membership](../../../perspectives/neighbourhoods/membership/README.md)). The server never parses link data, so encrypted rooms and legacy plaintext rooms share one storage path.

**Parent:** [Server link language](README.md)

<details><summary>How it works</summary>

| Route (protocol 2) | Does |
|---|---|
| `GET /.well-known/coasys-link-server` | `{ protocol: 2, name, membership: true }` |
| `POST /rooms/:id/auth` | challenge, then the signature, the X25519 key and its `coasys:room-key` ownership signature (all required); resolves the key with `author_of`; returns `{ token, expiresAt, admitted }`. An unknown room creates nothing. |
| `POST /rooms/:id/membership` | genesis `{ creator, salt }` (room id must match; the rules link must come from `CREATOR`), or membership links: a counted link is stored and broadcast; any other gets `403`. A non-admitted token may send only its own self-join link. |
| `GET /rooms/:id/membership?since=`, `GET /rooms/:id/members` | the plaintext membership links; the replay's members with key, person and X25519 status |
| `POST /rooms/:id/commit` | encrypted default-graph links only; a room with keys refuses plaintext links |
| `GET …/sync`, `…/render`, `…/revision`, `…/peers` | catch-up, full state, revision, online agents |
| `…/keys`, `…/keys/rotate`, `…/keys/missing`, `…/keys/grant` | sealed envelopes; any admitted member ([Room keys](room-keys.md)) |

1. At auth and on every request the server checks the key's admission from its cache. It refreshes a key's log head at each auth and at most every 10 minutes for a key that holds a socket. On a revocation or a counted `removed` link it deletes the key's sessions, closes its sockets with `4005`, and sends `rotate-needed`. When `author_of` is unavailable, it admits no new key and keeps the admitted ones.
2. The store client reads the shared store's address from `STORE_URL` and refuses a key-index answer whose head no key in `STORE_HEAD_KEYS` signed.
3. **Legacy rooms**, made before protocol 2 with no creator binding, keep the ACL table and its admin role ([db.ts](../../../../link-server/src/db.ts)); rooms with a creator binding admit from the membership graph. `AUTO_ADMIT` works per room for tests; the server refuses to start with it and `NODE_ENV=production`.
4. `--operator-port` starts a loopback-only listener for the **link-server operator**: rooms, members as the replay names them, key versions, sockets, health, and `POST /api/rooms/:id/suspend` and `…/resume`. No route admits or removes a person. The link-server operator is whoever runs the server; in the rest of ADAM, the operator is the identity in an executor's operator record.
5. `/membership` takes at most 30 self-join attempts per room per hour per source address.

</details>

<details><summary>Where in the code</summary>

- [link-server/AGENTS.md](../../../../link-server/AGENTS.md), [link-server/README.md](../../../../link-server/README.md): design and invariants.
- [link-server/src/server.ts](../../../../link-server/src/server.ts): `buildServer`, wiring; [link-server/src/index.ts](../../../../link-server/src/index.ts): flags, `AUTO_ADMIT` refusal in production.
- [link-server/src/routes.ts](../../../../link-server/src/routes.ts): HTTP routes, the membership channel, `removeMember`.
- [link-server/src/db.ts](../../../../link-server/src/db.ts): schema (rooms with creator binding, membership links, legacy ACL), `applyDiffAndAppend`.
- [link-server/src/auth.ts](../../../../link-server/src/auth.ts): `did:key` decoding, challenges, the ownership check, JWT sessions.
- `ad4m-identity-core` (WASM build, `membership.rs`): the replay; the `author_of` store client with its cache.
- [link-server/src/operator.ts](../../../../link-server/src/operator.ts), [ws.ts](../../../../link-server/src/ws.ts), [telepresence.ts](../../../../link-server/src/telepresence.ts), [encryption.ts](../../../../link-server/src/encryption.ts), [rate-limit.ts](../../../../link-server/src/rate-limit.ts).
- [link-server/tests/](../../../../link-server/tests): every test boots a real server.

</details>
