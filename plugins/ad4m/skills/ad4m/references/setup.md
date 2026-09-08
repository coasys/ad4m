# AD4M Plugin Setup & Authentication for AI Agents

> **Scope.** This file covers what has to happen *before* the MCP tools work: installing the plugin, connecting to an executor, and authenticating. Once you are connected, the executor documents itself — `ad4m_get_documentation(topic="overview")` (tool surface, workflow, authentication over MCP, data rules) and `topic="architecture"` (data model, SHACL format) need no authentication and are compiled into the binary, so they always describe the node in front of you. Setup is deliberately *not* served there: an agent that can call the tool is already past it, which is why this file lives in the skill.

If you need to stand up your own executor — downloading, initializing, running, unlocking — see `references/running-an-executor.md`.

## Installing the plugin itself

From a packed tarball:

```bash
openclaw plugins install /path/to/coasys-openclaw-ad4m-<version>.tgz \
  --accept-capabilities --force
```

**Both flags, and the error only names one at a time.** A local archive is outside
ClawHub's trust metadata, so the install stops twice: without `--accept-capabilities`
it asks for capability consent, and without `--force` it prints *"Install cancelled;
rerun with --force after reviewing the source."* Passing only the flag the first error
named leaves you stuck on the second.

Restart the gateway after installing — plugins load at start.

**Install into the profile whose gateway you actually run.** `ad4m_*` tools are served
by the gateway that loaded the plugin, so installing into a side profile
(`openclaw --profile foo plugins install …`) does *not* give your session those tools.
Running that profile's own gateway and driving it from outside is an OpenClaw
concern this skill does not cover, and agents reliably lose time there: `openclaw agent`
targets the default gateway regardless of `--profile`, and pointing it elsewhere needs
credentials that a freshly started gateway has not got.

If you were told to leave an existing profile untouched, that constraint governs the
whole session, not just the install command — sort out which gateway you will be running
*before* you install. A missing `ad4m_*` tool means you are talking to the wrong gateway.
It is not a reason to hand-roll an MCP client.

## Deployment Scenarios & Networking

### Scenario 1: Single-user, local (simplest)

Agent and executor on the same machine. No TLS needed.

```bash
export AD4M_ADMIN_CREDENTIAL="$(cat ~/.ad4m/admin-credential)"
ad4m-executor run --app-data-path ~/.ad4m --port 12000 --enable-mcp true
# MCP at http://localhost:3001/mcp
# API at http://localhost:12000
```

### Scenario 2: Agent connects to remote executor

The operator networking for Scenario 2 — SSH tunnels, Caddy reverse proxy, Cloudflare Tunnel — is in `references/running-an-executor.md` → "Operator Networking for Remote Executors". What matters on the agent side is the TLS guard below.

#### Where TLS actually comes from, and what `allowInsecureHttp` is for

Two different layers, easy to confuse:

- **The executor has no TLS of its own on the MCP port.** It serves `/mcp` as plain HTTP, so encryption comes from the front the operator puts in front of it — Caddy, Cloudflare Tunnel, or an SSH tunnel (see `references/running-an-executor.md` → "Operator Networking for Remote Executors"). That is the recommended path for anything off-LAN. (`--tls-cert-file` / `--tls-key-file` cover the API port, not MCP; executor-native TLS for MCP is a separate piece of work.)
- **`allowInsecureHttp` is a client-side guard in the plugin**, not a transport setting. Every MCP call carries the plugin's JWT or admin credential in an `Authorization` header, so the plugin refuses to talk to a non-loopback plaintext `http://` `mcpEndpoint` unless you set the flag. Turning it on does not weaken the executor; it only stops the plugin from refusing.

`https://` endpoints, `http://localhost…`, and anything reached through an SSH tunnel are all allowed with the flag off — so the only case that needs it is a plaintext endpoint on a network path you trust end to end, e.g. `http://marvin.fritz.box:3002/mcp` on your own LAN. For anything leaving that LAN, put a TLS front in front of the executor and use `https://` rather than setting the flag.

### Scenario 3: Multi-user (humans via Flux + agents via MCP)

Requires `--enable-multi-user true`. Each user (human or agent) authenticates as their own account.

**⚠️ Flux (browser) REQUIRES TLS for non-localhost.** Browsers block mixed content and WebSocket connections to insecure origins. You MUST use one of:

- Caddy/nginx reverse proxy with TLS cert
- Cloudflare Tunnel
- SSH tunnel (makes it appear as localhost on the client)
- Self-signed cert via `mkcert` (install CA on all client devices)

```bash
export AD4M_ADMIN_CREDENTIAL="$(cat ~/.ad4m/admin-credential)"
ad4m-executor run --app-data-path ~/.ad4m --port 12000 \
  --enable-mcp true --enable-multi-user true
```

**Agent provisioning + auth flow (recommended, one command):**

If you're running the OpenClaw AD4M plugin, don't hand-roll this. Set `multiUser: true` and `email` in `plugins.entries.ad4m.config`, export `AD4M_PASSWORD`, and run `openclaw ad4m-setup` — it resolves the password (env var → `config.password` → interactive prompt), calls `signup`, and calls `login_email`.

**It does not edit `openclaw.json` for you.** It writes the finished config — token included — to `ad4m-setup-config.json` beside the config file of the profile it ran against, mode `0600`, and prints that path. Copy the contents into `plugins.entries.ad4m.config`, restart the gateway, then delete the file. The token in the *log output* is elided (`"eyJ0eX…kf94"`), because OpenClaw redacts credentials in logs — copying the snippet out of the terminal gives you a broken token. That is the single most common reason people end up hand-rolling MCP calls they never needed. The plugin auto-retries login on every subsequent restart using the same password resolution. Full detail and password-hygiene rules in the main skill, Rules 3b/3c.

**Agent provisioning + auth flow (manual, MCP tool calls — only if you're not using the OpenClaw plugin or `ad4m-setup` can't run):**

1. `signup(email, password)` → creates the account, returns a DID, and emails a **`signup`**-typed verification code. Some nodes' `signup` response mentions that email even when the node doesn't actually enforce verification (seen on a test executor with SMTP disabled) — try `login_email` next rather than assuming.
2. `login_email(email, password)` → JWT token. Done, if the node doesn't enforce verification.
3. **If `login_email` returns no token, the node does enforce verification and this step is interactive** — a human has to read the code out of the inbox:
   - Straight after signup, verify the code signup already sent:
     `verify_email_code(email, code, verification_type="signup")` → JWT.
   - For an existing, already-verified account, ask for a fresh login code first:
     `request_login_verification(email)`, then
     `verify_email_code(email, code, verification_type="login")` → JWT.
   - The parameter is `verification_type` (`"signup"` or `"login"`), not `type`, and it has to match the code the executor issued — `verify_and_login` looks the code up by (email, type). `openclaw ad4m-setup` does exactly this, prompting for the code.
4. Include the JWT on subsequent requests (for the OpenClaw plugin: `plugins.entries.ad4m.config.token`).

**Legacy capability flow (single shared node identity, not a distinct per-agent account):**

1. `request_capability` → get `request_id` + `code`
2. Admin approves (or auto-approve with admin credential)
3. `generate_jwt` with `request_id` + `code` → get JWT token
4. All subsequent requests include the JWT

**Both auth paths require the node's operator to have unlocked the wallet** — a freshly-restarted multi-user node with no admin credential configured is fully deadlocked until someone runs `agent.unlock` (see `references/running-an-executor.md` → "Step 4: Unlock Agent"), since both `login_email` and the capability bootstrap fail with a locked wallet.

**Human auth flow (Flux):**

1. Open Flux UI → enter executor URL (must be HTTPS)
2. Email verification or admin approval
3. Flux stores JWT in browser

### Quick Decision Guide

| Who connects?   | Where?       | Encryption                          | Recommended setup                    |
| --------------- | ------------ | ----------------------------------- | ------------------------------------- |
| Just your agent | Same machine | None needed (loopback only)         | Scenario 1 (local)                   |
| Just your agent | Remote       | **Yes** — SSH tunnel (or TLS proxy) | SSH tunnel                           |
| Agent + Flux UI | Same machine | None needed (loopback only)         | Scenario 1                           |
| Agent + Flux UI | Remote/LAN   | **Yes** — TLS (Flux needs a cert)   | Caddy + domain, or Cloudflare Tunnel |
| Multiple users  | Remote       | **Yes** — TLS                       | Caddy + domain + multi-user flag     |

Every remote row above encrypts the whole connection, so admin credentials, JWTs and passwords never cross a network in the clear. Plain HTTP is acceptable on loopback, and — at your own risk, with `allowInsecureHttp` — on a LAN path you fully control; never anywhere else. See "Where TLS actually comes from" above.

## Security Considerations

### Credential Handling

The plugin manages MCP authentication internally — credentials are not sent in wake messages. Wake messages only contain event metadata (perspective UUID, parent, event type, agent DID).

- The plugin's background service maintains an authenticated MCP session
- Wake messages are sent over HTTP to your local OpenClaw hooks endpoint (`localhost` by default)
- If running the waker on a remote machine, ensure the wake endpoint uses HTTPS

### Executor Security

- **Never expose the admin credential** in logs, chat messages, or shared config files
- Pass the admin credential through `AD4M_ADMIN_CREDENTIAL` (exported from a mode-600 file), not `--admin-credential`: command-line arguments are readable by every user on the host via `ps` and end up in shell history
- The executor's API endpoint (`--port`, default 12000) should only be accessible to trusted agents
- Anything that is not loopback goes through an SSH tunnel or TLS (`--tls-cert-file` / `--tls-key-file`, or a reverse proxy as in Scenario 2). There is no case where auth traffic travels over plain HTTP across a network
- Same rule for multi-user passwords and JWTs: never in logs, chat messages, shared config, or command arguments. Use the file-based mechanics below rather than shell interpolation.

## Calling MCP tools without the plugin

**Read this only if you are actually stuck.** The plugin bridges the executor's tools as
your own (`ad4m_signup`, `ad4m_login_email`, `ad4m_verify_email_code`, the `instance_*`
surface, …), and `openclaw ad4m-setup` performs the capability handshake
(`request_capability` → `generate_jwt`) and writes the token for you. Reaching for an
external MCP client is a last resort for exactly two situations: an executor
`ad4m-setup` cannot reach, or a plugin build older than the static tool surface.
`request_capability` and `generate_jwt` are the only tools with no native equivalent.

Do not use raw `curl`: the MCP server speaks Streamable HTTP and answers with
`text/event-stream`.

```bash
mcporter call <mcpEndpoint>.<tool_name> --allow-http key=value ...
# e.g. the capability handshake ad4m-setup would otherwise do for you
mcporter call http://host:3001/mcp.request_capability --allow-http \
  app_name=my-agent app_desc="my agent"
```

The capability response carries both `request_id` and `code`. Read them from that
response — the executor redacts raw MCP capability codes from its own logs unless it was
started with `AD4M_LOG_SECRETS=1`.

**Password and token hygiene — the naive approach is NOT safe:**
- Generate the password into a file with `chmod 600` (e.g. `openssl rand -base64 24 | tr -d '\n' > ~/.mypw && chmod 600 ~/.mypw`), never as a literal string in a command you type.
- **Use mcporter's `key=@path` argument syntax** (`password=@~/.mypw`) — mcporter reads the file's content directly as the value. Only the *path* appears in the command and in process argv, never the plaintext password. Verified working (mcporter ≥ 0.13; a globally-installed 0.7.3 does *not* support `@path` — use `npx -y mcporter@0.13.10` if your installed version's `--help` doesn't list `key=@path` under Arguments — pin the version rather than tracking `@latest`, which executes whatever was published most recently).
- **Do NOT use shell substitution like `"$(cat ~/.mypw)"` for this.** That expands the plaintext into the process's actual argv before exec — `ps` and any process listing on the machine can read it. If a tool genuinely has no file/stdin-reading option for a required secret argument, say so as a known limitation rather than presenting shell substitution as a safe workaround.
- The same applies to the JWT you get back — capture it straight to a `chmod 600` file, don't echo it to verify.
- Write the resulting JWT into `plugins.entries.ad4m.config.token` — check your config tool's own file-reading support first; if it only accepts a literal argument, name that as a limitation too.

**Authenticated calls need the header, not just the endpoint.** `signup` and
`login_email` are unauthenticated, so the plain call above works for them. Everything
perspective-scoped (`add_model`, `list_perspectives`, `describe_perspective`, …) runs as
*whoever the call is authenticated as* — a bare call carries no identity and fails with
misleading errors like `Perspective not found` on a perspective you just created. Pass
the JWT as an `Authorization` header, referencing an environment variable by name so the
token never enters argv:

```bash
export AD4M_JWT="$(cat ~/.ad4m-token)"   # 0600 file, never echoed
npx -y mcporter@0.13.10 call http://host:3001/mcp.list_perspectives \
  --allow-http --header "Authorization=\$env:AD4M_JWT"
```

The JWT goes in **bare — no `Bearer ` prefix**. The `$env:NAME` indirection (and
`--header`) exist only in newer mcporter; verified against mcporter 0.13.10 via `npx`,
absent from 0.7.3.

## WebSocket RPC API (Fallback)

**Use MCP tools first.** The WebSocket RPC API is for low-level operations not exposed via MCP (language management, direct queries, debugging, and unlocking a wallet when you lack CLI access — see `references/running-an-executor.md` → "Step 4: Unlock Agent").

Connect to `ws://localhost:12000/api/v1/ws` (loopback or through an SSH tunnel; `wss://` behind your TLS proxy when remote) and send JSON-RPC messages:

The envelope field is **`type`**, not `method` — the dispatcher rejects a
message without it (`{"error":{"code":400,"message":"Missing 'type' field"}}`):

```json
{"type": "agent.status", "params": {}, "id": "1"}

{"type": "perspective.addLink", "params": {"uuid": "<perspective-uuid>", "link": {"source": "ad4m://self", "predicate": "has_name", "target": "literal:string:Data"}}, "id": "2"}

{"type": "agent.unlock", "params": {"passphrase": "<agent-passphrase>"}, "id": "3"}
```

Method names are the registered handler names (`agent.status`,
`agent.generate`, `agent.unlock`, `perspective.addLink`,
`perspective.queryLinks`, …) — camelCase after the dot, and `perspective.`
singular. There is no `perspectives.add_link`.

**Auth is a connection-upgrade query parameter, not a first message.** Pass
the admin credential (single-user) or the JWT (multi-user) as `?token=…` on
the WebSocket URL:

```
ws://localhost:12000/api/v1/ws?token=<admin-credential-or-jwt>
```

Remember the test-only behavior from `references/running-an-executor.md` → "Step 4: Unlock Agent": an empty token resolves to full access when no admin credential is configured — this is intentional for local/test setups, and it's exactly why a node without an admin credential must never be exposed beyond loopback.
**Endpoint:** `ws://localhost:12000/api/v1/ws` (port configurable via `--port`)

## Troubleshooting

| Symptom | Cause | Fix |
|---------|-------|-----|
| `App data path not set` panic | Missing `--app-data-path` | Always pass the flag |
| `mainnet_seed.seed` not found | Skipped `init` | Run `ad4m-executor init` first |
| `Failed to spawn Lair keystore` | Stale lair socket/pid | Delete `h/c/ks/pid_file` and `h/c/ks/socket` |
| Holochain conductor `IoError(internal)` | Corrupted conductor DB | Nuke `h/c/` directory, re-generate agent |
| Port already in use | Previous instance running | Kill old process, clean lair files |
| 404 on neighbourhood join | Version mismatch or expired link | Ensure same AD4M version as neighbourhood creator |
| Cannot connect to executor | Executor not running or wrong port | `curl http://localhost:12000/health` to verify (`/health` and `/` are the only general-purpose HTTP routes; everything else is WS-RPC) |
| Waker not firing | WS not accessible or bad query | Check `ws://localhost:12000/api/v1/ws/events` and waker logs |
| Messages "uninitialized" | Property set after creation (race) | Pass all initial values at creation — `instance_create(..., properties={...})` (static tools) or `{class}_create` with every property up front (legacy dynamic tools). Never a create followed by a separate set call. |
| Channel query returns empty | SHACL still syncing | Wait 3-5 min for Holochain gossip, then retry |
| `User key not found on executor` (login) or `main key not found` (capability flow), right after a restart | **Expected, by design** — see `references/running-an-executor.md` → "Step 4: Unlock Agent". The node hasn't been unlocked by its operator yet; the error message is misleading (reads like a bad credential) but the lockout itself is intentional. | If you're the operator: unlock with the agent's passphrase — `ad4m agent unlock` on the CLI, or `agent.unlock` over WS-RPC (there is no REST route for it). If you're a third party: this needs the node's operator, not a client-side retry. |
| `subscribe_to_mentions`/`subscribe_to_children` returns an honest "not listening yet, retrying" response, or the subscription shows under a **Pending** section in `list_waker_subscriptions` instead of active | Normal during node startup; also correct if the node genuinely hasn't been unlocked yet (row above). The plugin retries the registration every 30s automatically rather than pretending to have succeeded. | Nothing to do if Pending clears within a minute or two. If it doesn't clear, the cause is almost always the node not being unlocked — an operator problem, not a client-side one. |
