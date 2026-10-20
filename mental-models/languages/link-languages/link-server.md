# Link server

The **link server** is a self-hosted Node.js process, separate from the executor, that stores the links of **link-server rooms** for the [server link language](server-link-language.md). It runs on Fastify, `ws` and `better-sqlite3`, with one SQLite file. A room's active links are a set keyed by link hash; every commit also goes into an append-only diff log. Agents authenticate with their `did:key` and get a JWT backed by a session row.

**Why:** A community can run it on small hardware with no other services. The revision is a content hash of the sorted active link hashes, so two rooms with the same links have the same revision. The server never parses link data, so encrypted and plaintext rooms use one code path.

**Parent:** [Link languages](README.md)

<details><summary>How it works</summary>

| Route | Does |
|---|---|
| `POST /rooms/:id/auth` | step 1: challenge; step 2: verify the Ed25519 signature over `sha256(challenge)`, store the X25519 key, issue a JWT |
| `POST /rooms/:id/commit` | apply additions and removals in one transaction; push to WebSocket peers |
| `GET /rooms/:id/sync`, `/render`, `/revision` | catch-up since a sequence number; full state; current revision |
| `GET /rooms/:id/peers` | online agents |
| `GET` / `POST /rooms/:id/acl` | room members and admin |
| `/rooms/:id/keys`, `/keys/rotate`, `/keys/missing`, `/keys/grant` | sealed room keys |
| `POST /rooms/:id/admin/transfer` | hand over the admin role |

1. The first DID that authenticates to an unknown room id creates the room and becomes its admin.
2. Other DIDs need an ACL entry (or `--auto-admit`). Removing a DID from the ACL deletes its sessions and closes its sockets.
3. A removal resends the original `LinkExpression`, so its hash equals the addition's. For plaintext links the server requires `author` = session DID. For encrypted links the hash is the client-supplied `link_hash`.
4. `--operator-port` with `--operator-token-file` starts a second, loopback-only app for the **link-server operator** (list rooms, change ACLs, audit log). The link-server operator is the person who runs the server; in the rest of ADAM, "operator" is the executor's administrator.

</details>

<details><summary>Where in the code</summary>

- [link-server/AGENTS.md](../../../link-server/AGENTS.md), [link-server/README.md](../../../link-server/README.md): design and invariants.
- [link-server/src/server.ts](../../../link-server/src/server.ts): `buildServer`, wiring.
- [link-server/src/routes.ts](../../../link-server/src/routes.ts): HTTP routes, `removeMember`.
- [link-server/src/db.ts](../../../link-server/src/db.ts): schema, `applyDiffAndAppend`.
- [link-server/src/auth.ts](../../../link-server/src/auth.ts): `did:key` decoding, challenges, `hashMessageForVerify`, JWT sessions.
- [link-server/src/encryption.ts](../../../link-server/src/encryption.ts), [ws.ts](../../../link-server/src/ws.ts), [telepresence.ts](../../../link-server/src/telepresence.ts), [operator.ts](../../../link-server/src/operator.ts).
- [link-server/tests/](../../../link-server/tests): every test boots a real server.

</details>