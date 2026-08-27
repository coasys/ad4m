# Server Link Language

An [AD4M](https://ad4m.dev) link language that syncs a perspective through a
self-hosted [**link-server**](../../link-server/README.md)
instance — HTTP for auth, commit, and catch-up sync; a native WebSocket for
real-time diff push, presence, and telepresence. Every AD4M link-language capability is implemented:
`perspective-commit`, `perspective-sync`, `perspective-query`, `peers`, and
`telepresence`. Optional end-to-end encryption protects link payloads when
the room has E2E enabled.

Built on the modern [ALDK](https://github.com/coasys/ad4m/tree/dev/ad4m-ldk)
(`@coasys/ad4m-ldk`) pattern — see
[`ad4m-link-language-template`](https://github.com/coasys/ad4m-link-language-template)
for the skeleton this project extends.

![Setup & Join Guide](guide.svg)

## Prerequisites

- **[Deno](https://deno.land/)** (v1.32+) — the executor runtime and the build script
- **[Node.js](https://nodejs.org/)** (v20+) + npm/pnpm — dev dependencies and tests
- **`@coasys/ad4m-ldk`** cloned at a sibling path (`../ad4m/ad4m-ldk/js/`), or set `AD4M_LDK_ENTRY` to the compiled `lib/index.js`

## Quick start

```bash
NODE_ENV=development pnpm install     # or npm install
deno run --allow-all esbuild.ts       # -> build/bundle.js
node --experimental-vm-modules --import tsx --test tests/*.test.ts
npx tsc --noEmit
```

## Usage

### For neighbourhood creators

#### 1. Run a server

Deploy a [**link-server**](../../link-server/README.md) instance on your own hardware. One command gets you started:

```bash
npx @coasys/link-server --port 3457 --data ./my-data
```

See the [link-server README](../../link-server/README.md) for Docker setup, access control, federation, and encryption options.

#### 2. Publish the language template (one time)

Register the language code on the AD4M network. This only needs to happen once — after that, everyone reuses the same template address.

```bash
ad4m languages publish ./build/bundle.js \
  --name server-link-language \
  --description "Link language syncing through a self-hosted link-server" \
  --possible-template-params SERVER_URL,ROOM_ID
```

This returns a **template address** (content hash). Save it — you need it in the next step.

If someone else already published the template, skip this step and use their template address.

#### 3. Create an instance pointing at your server

Fill in the template with your server's URL and a room name of your choice:

```bash
ad4m languages apply-template-and-publish <template-address> \
  '{"SERVER_URL": "https://your-server.example.com:3457", "ROOM_ID": "my-room"}'
```

This returns an **instantiated language address** — the template code with your server details baked in.

#### 4. Create a neighbourhood

```bash
ad4m perspectives create                              # → perspective UUID
ad4m neighbourhoods create <perspective-UUID> <instantiated-language-address>
```

This returns a **neighbourhood URL** (`neighbourhood://Qm...`). Share it with anyone you want to invite.

Your agent — the first one to connect — automatically becomes the room's admin on the server.

### For users joining a neighbourhood

One command:

```bash
ad4m neighbourhoods join neighbourhood://Qm...
```

Everything else happens automatically:

- The language code downloads (with the server URL and room ID already filled in)
- Your agent authenticates with the server using your DID
- All existing links pull down to your local store
- A live connection opens for real-time updates

No server address to type, no account to create, no configuration needed. The neighbourhood URL contains everything.

Once joined, the neighbourhood works like any other. Links you create sync to the server and push to every connected agent in real time. Links from other agents appear as they arrive. If your connection drops, the language reconnects and catches up on anything it missed.

## The server this language talks to

[`link-server`](../../link-server/README.md) — a
self-hosted HTTP+WebSocket server. One "room" = one AD4M neighbourhood.

### HTTP

| Method & path | Purpose |
|---|---|
| `POST /rooms/:roomId/auth` | DID challenge-response. Step 1: `{did}` → `{challenge}`. Step 2: `{did, challenge, signature, x25519PublicKey?}` → `{token}` |
| `POST /rooms/:roomId/commit` | Push a `{additions, removals}` diff |
| `GET /rooms/:roomId/sync?since=<sequence>` | Pull diffs newer than `sequence` → `{diffs[], revision, sequence}` |
| `GET /rooms/:roomId/render` | Full current snapshot → `{links[], revision}` |
| `GET /rooms/:roomId/peers` | Connected agent DIDs → `{peers: string[]}` |
| `GET /rooms/:roomId/revision` | `{revision, sequence}` |
| `GET /rooms/:roomId/acl` | `{admin, members}` |
| `GET /rooms/:roomId/keys` | This agent's sealed room key → `{encryptedKey, version}` (E2E rooms only) |

### WebSocket — `/rooms/:roomId/ws`

Server → client: `diff`, `telepresence-signal`, `telepresence-broadcast`,
`online-agents`, `peer-joined`, `peer-left`.
Client → server: `telepresence-signal`, `telepresence-broadcast`,
`set-online-status`.

This language uses the **native `WebSocket` global** — no socket.io. The
executor's language sandbox runs a full `deno_runtime` worker (not a bare
`deno_core` isolate), so `WebSocket`/`fetch`/`crypto` are present; see
`src/adapters-deno.ts` `DenoWebSocketFactory`.

## Architecture

### Pure / impure separation

- **Pure** (`src/types.ts`, `src/store.ts`, `src/adapters.ts`, `src/api.ts`,
  `src/auth.ts`, `src/sync.ts`, `src/ws-client.ts`, `src/telepresence.ts`,
  `src/encryption.ts`) — no `ad4m:host` imports. Everything talks to the
  outside world through the `Transport` / `StorageAdapter` / `AgentAdapter`
  / `RuntimeAdapter` / `WebSocketFactory` interfaces in `src/adapters.ts`,
  swapped for mocks in tests.
- **Impure** (`src/adapters-deno.ts`) — wraps `ad4m:host` imports and the
  native `WebSocket` global. Only imported from `index.ts` during `init()`.

### Module map

| Module | Responsibility |
|---|---|
| `src/store.ts` | Local link store (indexed by source/target/predicate) + sync cursor (revision, sequence) |
| `src/api.ts` | Typed HTTP client for every server endpoint |
| `src/auth.ts` | DID challenge-response flow + token expiry tracking/refresh |
| `src/ws-client.ts` | WebSocket lifecycle: connect, typed message dispatch, send queue, exponential-backoff reconnect |
| `src/sync.ts` | Commit / catch-up sync / cold-start bootstrap; the single inbound-diff choke point |
| `src/telepresence.ts` | Online-agent roster + outbound signal/broadcast/status |
| `src/encryption.ts` | X25519 key derivation, sealed room-key exchange, AES-256-GCM link encryption |
| `index.ts` | Wires it all together behind `defineLanguage()` |

### The `emitPerspectiveDiff` rule

The AD4M executor **discards the return value of `perspectiveSyncSync()`**.
The only way an inbound link becomes queryable is calling
`emitPerspectiveDiff(diff)`. Every inbound diff in this language — HTTP
catch-up batch entries and WebSocket `"diff"` pushes alike — funnels through
one function, `applyInboundWireDiff` in `src/sync.ts`, which is the only
place that calls it. See the module doc comment there before adding a second
inbound-diff path.

### Sync strategy

- `init()` runs a one-shot `bootstrap()`: fetch the full `/render` snapshot
  + `/revision`, populate the local store, and record the sequence cursor —
  far cheaper than replaying the room's entire diff history on every fresh
  instance.
- The WebSocket pushes live diffs. On every (re)connect, `ws-client.ts`'s
  `onOpen` handler also triggers an HTTP catch-up (`GET /sync?since=<cursor>`)
  as a belt-and-braces gap-fill for whatever happened while offline.
- `sync.sync()` (called by the runtime on its own timer) always performs the
  same HTTP catch-up. This is deliberately unconditional rather than
  branching on "is the WebSocket connected": because `since` only advances
  after a diff is actually applied, a WebSocket that already delivered
  everything live simply gets back an empty `diffs[]` — there's no separate
  "skip" branch to get wrong.

### End-to-end encryption

See the doc comment at the top of `src/encryption.ts` for the full design;
summary:

- **No raw private key in the sandbox.** The Deno language sandbox never
  exposes the agent's Ed25519 private key — only a black-box
  `agentSignStringHex(payload)` signing call. Since EdDSA signatures are
  deterministic (RFC 8032), this language derives a stable X25519 keypair
  as `x25519(sha256(sign(FIXED_DOMAIN_SEPARATED_MESSAGE)))`. Nobody without
  the agent's cooperation (via the host's signing service) can reproduce it.
- **Public key exchange.** There's no separate "register my E2E public key"
  endpoint in the server API this language targets, so the derived X25519
  public key rides along as an additive `x25519PublicKey` field on step 2 of
  the auth flow — the one point the agent already proves DID ownership to
  the server. A server that doesn't care about E2E just ignores the field.
- **Room key distribution.** `GET /rooms/:roomId/keys` returns a
  `SealedRoomKeyEnvelope` object (JSON with `ephemeralPublicKey`, `nonce`,
  and `ciphertext` as hex strings) as the `encryptedKey` field — the same
  shape `libsodium`'s `crypto_box_seal` produces. Only the intended
  recipient's derived private key can open it.
- **Link confidentiality.** Once a room key is available,
  `encryptLinkForWire` / `decryptLinkFromWire` protect a link's
  `{source, predicate, target}` payload with AES-256-GCM (fresh nonce per
  link). `author` / `timestamp` / `proof` stay in the clear — the runtime's
  signature was computed over plaintext data before this language ever saw
  the diff, and this metadata is needed for verification and routing. **The
  local store always holds decrypted links; only the wire representation is
  ciphertext.**
- **Fail closed.** If the room key can't be fetched/decrypted for a reason
  other than "this room has no E2E key" (network error, corrupt envelope,
  wrong key), `commit()` refuses to send plaintext rather than risk leaking
  data into a room that expects encryption.

The `encryptedKey` wire framing (JSON `SealedRoomKeyEnvelope`) and the
`x25519PublicKey` auth field are shared conventions between this client and
`link-server`. Both are isolated in `src/encryption.ts` / `src/api.ts` /
`src/auth.ts`.

## Template variables

```typescript
//!@ad4m-template-variable
const SERVER_URL = "<to-be-filled>";  // e.g. "https://my-server.example.com"

//!@ad4m-template-variable
const ROOM_ID = "<to-be-filled>";     // UUID, set at neighbourhood creation
```

Filled in by the executor at publish time. Until then, `init()` runs in an
inert mode (logs and returns without attempting any network I/O).

## Publishing

```json
{
    "languagePath": "./build/bundle.js",
    "languageMeta": {
        "name": "server-link-language",
        "description": "AD4M link language syncing through a self-hosted link-server",
        "possibleTemplateParams": ["SERVER_URL", "ROOM_ID"],
        "sourceCodeLink": "https://github.com/coasys/ad4m/tree/dev/bootstrap-languages/server-link-language"
    }
}
```

## License

[Cryptographic Autonomy License v1.0 (CAL-1.0)](LICENSE)
