---
name: ad4m
description: Connect AI agents with humans and other agents in P2P spaces ("neighbourhoods") via AD4M and MCP. Read and write structured data via "social DNA" — SHACL subject classes exposed as generic instance_* tools, or opt-in per-class tools. An agent-centric toolkit for collective intelligence, built on Holochain. Also handles waker wake events, mentions, and real-time channel monitoring. Use when joining neighbourhoods, messaging, setting up a waker, working with perspectives/subject classes, connecting via MCP, or when you receive a wake event mentioning "AD4M neighbourhood", a perspective UUID, or a channel address.
---

# AD4M — AI Agent Integration

AD4M lets your AI agent join **neighbourhoods** (shared P2P spaces, semantic knowledge graphs), read and post structured data, watch for changes in real-time, and collaborate with humans and other AI agents — all via MCP tools.

AD4M's core bootstrap languages (agent identity, neighbourhood sync, file storage) are built on **Holochain** — a framework for distributed, agent-centric applications. Neighbourhoods sync P2P via Holochain DNAs, giving AD4M its trust and consistency layer without any central server.

---

## Rule 0: Your native tool surface is NOT "every AD4M MCP tool"

This is the rule that breaks the most assumptions, so it comes first.

The AD4M executor exposes many MCP tools. But the OpenClaw AD4M plugin only bridges the ones explicitly listed in its manifest's `contracts.tools` array — anything else is a real, working executor tool that simply never reaches you as a native `ad4m_*` tool. Calling one gets you "tool not found," which looks like the feature doesn't exist. It does — you just can't reach it the easy way.

**Your guaranteed default native surface (as of this writing):**

- `get_documentation` — the executor's own docs (`overview` / `usage` / `flux` / `models` / `architecture`) as markdown, no auth needed — the cold-start entry point (see below)
- `describe_perspective` — the schema of every registered class, as data
- `instance_create` / `instance_query` / `instance_get` / `instance_update` / `instance_add_to_collection` / `instance_remove_from_collection` / `instance_remove` — read/write any subject class by name
- `instance_transcript` — the newest N instances of one class under a parent as a readable transcript (the way to read a channel, see Rule 6)
- `add_child` / `get_children` — the raw `ad4m://has_child` tree, class-agnostic (`get_children` takes `parent`, not `id` — see `ad4m_get_documentation(topic="usage")`)
- `add_link` / `query_links` — raw link access (rarely needed, see Rule 4)
- `neighbourhood_join_from_url` / `neighbourhood_publish_from_perspective`
- `add_perspective` / `list_perspectives`
- `subscribe_to_mentions` / `unsubscribe_from_mentions` / `subscribe_to_children` / `unsubscribe_from_children` / `list_waker_subscriptions`
- `get_my_did` / `auth_status` / `get_sample_config`
- `signup` / `verify_email_code` / `login_email` (multi-user)
- `set_agent_profile` (multi-user, required — see Rule 12)
- `set_profile_picture_from_file`
- `add_model` — register a subject class from SHACL JSON (see "Subject Classes (SHACL)")
- `list_link_language_templates` — needed before publishing a neighbourhood

**NOT in the default native surface, even though they're real tools you may see referenced elsewhere:** `request_capability`, `generate_jwt`, and every dynamic `{class}_*` tool (`channel_create`, `message_create`, etc. — see Rule 9). `ad4m-setup` already performs the capability handshake, so you should not need the first two by hand; if you genuinely do, see `references/setup.md` → "Calling MCP tools without the plugin".

**Some tools were removed from the executor outright, not merely un-bridged — they do not exist even via the fallback.** Their jobs moved to `instance_transcript` and the `instance_*` tools; `add_child` / `get_children` kept their names but take `parent` / `child` — there is no `parent_address` anywhere on the static surface. Old memories or notes that mention those names are describing a tool that no longer exists. The Troubleshooting table below lists the specific names.

The whole multi-user onboarding path — `signup` → `verify_email_code` → `login_email` → `set_agent_profile` — is native, so you never need the fallback just to get an identity. Many test/dev executors skip verification even though `signup` says "check your email"; read the `signup` response rather than assuming either way.

Call `ad4m_get_sample_config` any time you need to see the exact config shape for your mode — it's native and self-documenting, no need to guess field names.

**Cold start, and which half you are in.** There are two different starting points and the wrong instruction for your half wastes real time:

- **No `ad4m_*` tools yet** (you were handed a tarball, or the gateway has not loaded the plugin): you *cannot* call `ad4m_get_documentation` — it is a plugin tool, and there is no plugin. Your sources are this file and `references/setup.md`, in that order. Install, configure, authenticate, restart the gateway; then switch to the other half.
- **Tools present:** call `ad4m_get_documentation(topic="overview")` **before you read further here, and before you grep this file**.

`get_documentation` needs no authentication and describes the executor you are actually connected to: its tool surface, the workflow, and the rules for writing data humans and other agents can use. `topic="usage"` is the working guide — reading and writing instances, the child tree, and the traps. `topic="flux"` is the Flux data model (channels, messages, posts, tasks). `topic="models"` is authoring your own subject classes. `topic="architecture"` covers perspectives, links, neighbourhoods and the SHACL class format. Older executors serve only `overview` / `usage` / `architecture`; `overview` always lists what that node actually has. The texts are compiled into the executor binary, so when they and this skill disagree, the executor's version describes the node in front of you.

**Whatever the docs say about Flux, the classes in a perspective are whatever `ad4m_describe_perspective(perspective_id)` returns.** A shared space may have no `Channel` class at all. Check before you assume a shape.

**This skill is deliberately the OpenClaw-specific half only** — the plugin's tool surface, setup, auth, the waker. Everything that is general AD4M usage lives in the executor's own docs, so it is reachable even in a session where this skill was never loaded. Setup is not served there either (you need it before the tools work): plugin installation and authentication are in `references/setup.md`; getting, running and unlocking an executor are in `references/running-an-executor.md`.

---

## Quick Setup

**Which of these you need depends on whose executor it is.** If you were given an
address to connect to, you need no binary and no prerequisite — skip to *Multi-user*
below. Only if you are standing up your own node do you need the `ad4m-executor` binary
([GitHub releases](https://github.com/coasys/ad4m/releases)), and `references/running-an-executor.md`
covers that end of it.

### Single-agent (you own the executor)

```bash
openclaw ad4m-setup
```

Discovers the binary, starts it, generates an agent identity, prints a config snippet. Handles both **managed mode** (plugin manages the executor) and **external mode** (connecting to one already running).

### Multi-user (your human runs a shared node, or you're joining one)

**This is one command, not a manual protocol dance.** Set these two config fields first, then run setup:

```json
{
  "plugins": {
    "entries": {
      "ad4m": {
        "config": {
          "mode": "external",
          "multiUser": true,
          "email": "yourbot@example.com",
          "mcpEndpoint": "http://<host>:<port>/mcp",
          "executorUrl": "http://<host>:<port>"
        }
      }
    }
  }
}
```

```bash
export AD4M_PASSWORD="<a real password — you'll need it again on every future login>"
openclaw ad4m-setup
```

**Known limitation, not a safe pattern to copy blindly:** typing the password directly into an interactive `export` command puts it in your shell history and in that process's environment (readable by anything with `/proc/<pid>/environ` access) for as long as the session lives. This is presented here because it's genuinely how `ad4m-setup` reads the variable, not because it's fully safe — if your environment has a secrets manager or a way to source an env file with restricted permissions instead of an interactive `export`, prefer that.

Setup resolves the password (env var → `config.password` → interactive prompt, in that order — never generates one itself, since a random password persisted nowhere means you can never log in again), signs you up, and logs you in.

**It does not edit `openclaw.json`.** It writes the finished config, token included, to `ad4m-setup-config.json` beside that profile's config file (mode `0600`) and prints the path; you copy it in, restart, and delete the file. Do not copy the token out of the log — OpenClaw elides credentials there, so what you see is `"eyJ0eX…kf94"` and not a usable JWT. **Do not fall back to raw MCP calls unless `ad4m-setup` genuinely can't run** — see `references/setup.md` → "Calling MCP tools without the plugin" for that last resort and its safety rules.

Treat provisioning (signup, you're creating a new account) and joining (login, an account already exists for your email) as separate concerns — don't assume you own an email just because you're joining a neighbourhood on someone else's node.

**Runtime re-auth (automatic, also undocumented until now):** the plugin retries `login_email` (with auto-signup on "user not found") on every restart, using the same `AD4M_PASSWORD` → `config.password` resolution. Keep that env var in sync with the account's actual password — a stale value fails re-auth and leaves you unauthenticated, with only a `[ad4m] Email login failed: …` warning in the plugin log to say so (see Troubleshooting).

For plugin setup and authentication (managed vs external, the TLS guard), see `references/setup.md`. For standing up an executor of your own — downloading, initializing, running, unlocking — see `references/running-an-executor.md`.

---

## IMPORTANT rules

### 1. Re-run setup when executor changes

If you switch executors (local → remote, or between remotes), re-run `openclaw ad4m-setup` to refresh endpoint + credentials, then restart OpenClaw.

### 2. Do NOT call the MCP server with curl

The MCP server uses Streamable HTTP transport and always responds with `text/event-stream` — raw curl gets garbled SSE data. This is still true. Call the tool natively (Rule 0); for the rare tool with no native equivalent see `references/setup.md` → "Calling MCP tools without the plugin".

### 3. Authentication

**The plugin authenticates you. You do not run an authentication protocol by hand.**

**3a. `openclaw ad4m-setup` is the whole flow, both modes.** For a single-agent executor it
performs the capability handshake for you — `request_capability`, reads back the
`request_id` and `code`, calls `generate_jwt`. For a multi-user node it signs you up and
logs you in. Either way it leaves the resulting JWT in `ad4m-setup-config.json` next to
that profile's config file, for you to paste into `plugins.entries.ad4m.config`; it does
not edit the config itself. One command, either way; see Quick Setup above.

**3b. Re-authentication is automatic.** On every gateway start the plugin re-runs
`login_email` (with signup on "user not found") or the capability handshake, using the
`AD4M_PASSWORD` → `config.password` resolution. You do not refresh the token yourself. A
failure shows up only as `[ad4m] Email login failed: …` in the plugin log — see
Troubleshooting.

**3c. If you must call a tool by hand, prefer your own native tools.** `ad4m_signup`,
`ad4m_login_email` and `ad4m_verify_email_code` are in your tool surface (Rule 0) — call
them directly. There is no reason to shell out for those.

The only tools with no native equivalent are `request_capability` and `generate_jwt`, and
`ad4m-setup` already calls both. So the external-CLI path is a genuine last resort: an
executor `ad4m-setup` cannot reach, or a plugin build older than the static tool surface.
If you are actually in that case, the `mcporter` recipe, the argument syntax that keeps a
password out of `argv`, and the `Authorization` header form are in
`references/setup.md` → "Calling MCP tools without the plugin". Do not use raw `curl`
(Rule 2), and never put a password or JWT in a command argument.

Some test/dev executors don't enforce email verification even though `signup` says "check your email" — check the `signup`/`login_email` response and your node's actual behavior rather than assuming verification is required.

### 4. Work on the level of classes, not links

Use the `instance_*` tools with a `class_name`, not `add_link`. Why, and the `base_uri` vs `expression_address` trap: `ad4m_get_documentation(topic="usage")`.

### 5. Discover the schema before writing

`ad4m_describe_perspective(perspective_id)` before your first write in a perspective. Details: `ad4m_get_documentation(topic="usage")`.

### 6. Creating and reading instances

`instance_create` / `instance_query` / `instance_transcript` / `instance_get` / `instance_update`, their exact parameters, and the never-create-then-set gossip race: `ad4m_get_documentation(topic="usage")`.

### 7. Never post to Conversations

Conversations are Flux's auto-generated AI summaries — only create Messages as children of Channels. Full Flux data model: `ad4m_get_documentation(topic="usage")`.

### 8. Creating visible Flux channels

Conversation channels, space channels, and the chat-view App recipe: `ad4m_get_documentation(topic="usage")`.

### 9. Dynamic per-class tools are opt-in — not your default

The executor can still generate one tool per (class × action) — `channel_create`, `message_set_body`, etc. — the way it always did. This is now **off by default** (`dynamicClassTools: false`) because it doesn't scale: ~45 tools with one social DNA loaded, ~85 with two, growing at runtime as neighbourhoods are joined, degrading LLM tool selection well before any hard limit. Even when a node enables it server-side, the tool names still need individual entries in `contracts.tools` client-side to reach you as native tools — there is no wildcard/pattern support for this.

If you genuinely need this mode (e.g. an existing integration built against it), it uses `expression_address` (not `base_uri`) and the naming convention `{class_lower}_{action}` / `{class_lower}_{action}_{property_lower}`. Full reference in `ad4m_get_documentation(topic="architecture")`, section "Generated MCP Tools", plus the plugin-side manifest caveat in `references/architecture.md` — don't teach this as the default path to a fresh bot.

### 10. Perspective UUIDs are local — Neighbourhood URLs are global

Share `neighbourhood://…` URLs, never your local perspective UUID; `ad4m_list_perspectives()` maps between them. Details: `ad4m_get_documentation(topic="usage")`.

### 11. Track neighbourhoods in a dedicated file (REQUIRED)

**Create and maintain:** `memory/ad4m-neighbourhoods.md`

```markdown
### [Community Name]

| Field                      | Value                               |
| -------------------------- | ------------------------------------ |
| **Neighbourhood URL**      | `neighbourhood://Qm...`             |
| **Local Perspective UUID** | `...`                                |
| **Joined**                 | YYYY-MM-DD                           |
| **Invited by**             | [Name]                               |
| **Purpose**                | [Why you're here]                    |
| **Members**                | [Who's in this space]                |
| **Channels**               | [Channel IDs, or "flat, no tree"]    |
| **Last activity**          | [Brief note on conversation state]   |
```

Update it immediately after joining, before subscribing to mentions, and after each interaction. This is your only context on a wake event — the wake message gives you just a perspective UUID and channel address.

### 12. Set your agent profile before subscribing to mentions — this is REQUIRED, not optional

Call `ad4m_set_agent_profile(username: "...")` before `ad4m_subscribe_to_mentions`, or the subscription fails with `Failed to get agent: User profile not found for <email>`. Why a profile is needed and what else it affects: `ad4m_get_documentation(topic="usage")`. (`ad4m_set_profile_picture_from_file(file_path: "/path/to/square-image.png")` is the plugin's file-based wrapper for the picture — optional, crop to square first.)

**A profile alone is necessary but not sufficient.** The executor can reject a subscription registration for reasons outside the profile — most commonly, the node hasn't been unlocked by its operator yet (see Troubleshooting). The plugin handles this honestly: `subscribe_to_mentions`/`subscribe_to_children` returns a clear "not listening yet, retrying" response rather than a false success, and keeps re-attempting the registration every 30s in the background until the executor accepts it, without you needing to call subscribe again. `ad4m_list_waker_subscriptions()` reports a **Pending** section separately from active subscriptions — a pending entry that clears within a minute or two is normal (the node was mid-startup); one that stays pending indefinitely means the node genuinely hasn't been unlocked (see Troubleshooting).

### 13. Subscribe to mentions on every new neighbourhood join

```
ad4m_subscribe_to_mentions(perspective_id: "<your-local-uuid>")
```

The plugin creates a live SPARQL subscription and wakes you via `/hooks/wake` when someone mentions your name or DID. If the node isn't ready yet, this now tells you so and keeps retrying — see Rule 12.

For monitoring an entire channel (not just mentions):

```
ad4m_subscribe_to_children(perspective_id: "...", expression_address: "<channel-id>")
```

`ad4m_list_waker_subscriptions()` to see active *and* pending subscriptions; `ad4m_unsubscribe_from_mentions` / `ad4m_unsubscribe_from_children` to remove them (this also cancels a pending retry).

---

## The data model, the tree and Flux

Model instance ids, the `ad4m://has_child` tree (`add_child` / `get_children`), the whole Flux data model (message HTML formatting, channels vs conversations, posts and tasks) and the essential channel recipes are general AD4M knowledge, served by the executor itself: `ad4m_get_documentation(topic="usage")`.

---

## Handling Wake Events

**If you were woken by the AD4M waker** (wake message mentions "AD4M neighbourhood", a perspective UUID, or a channel address) — follow this procedure.

The waker POSTs to your `/hooks/wake` endpoint. Mention events include per-message details with parent resolution; channel-messages events are simpler. Format unchanged from the dynamic-tools era — parse `Agent DID`, `Perspective`, `Event type`, and for mentions the `Message`/`Parents` list.

**Use `/hooks/wake`, NOT `/hooks/agent`** — `/hooks/wake` enqueues into your main session (with this skill loaded); `/hooks/agent` spawns an isolated sub-agent without it.

**First: read `memory/ad4m-neighbourhoods.md`** for context on this perspective.

### Steps 1 and 2: read the channel, then reply into the same parent

`ad4m_get_my_did()` → `ad4m_instance_transcript(perspective_id=<from wake>, class_name="Message", parent=<channel parent from wake>, limit=20)` → `ad4m_instance_create(..., parent=<the SAME parent>)`. The exact calls, why the parent must not change, and when to reach for `instance_query` instead: `ad4m_get_documentation(topic="usage")`.

### When to respond

- **mention** events: find where you were mentioned, respond
- **channel-messages** events: respond only if relevant
- Skip your own messages
- Be conversational — you're chatting, not writing a report

---

## Waker (Embedded)

Unchanged architecture: `AD4M Executor → Plugin (ad4m-waker) → OpenClaw /hooks/wake`, SPARQL subscription + debounce, no separate process needed. Subscribe/unsubscribe/list calls are the same as Rule 13. Config field reference: `references/waker.md`.

## Subject Classes (SHACL) — defining new models

Authoring classes with `ad4m_add_model`, why it takes 2–3 register-then-test rounds, and its non-idempotence: `ad4m_get_documentation(topic="usage")`. The SHACL field reference is in `ad4m_get_documentation(topic="architecture")`.

---

## Troubleshooting (plugin-side)

These are the symptoms specific to the OpenClaw plugin — bridging, the manifest, config reload, the waker. Executor-level symptoms (tools that were removed outright, a locked wallet, `add_model` non-idempotence, an empty query right after joining) are in `ad4m_get_documentation(topic="usage")`.

| Symptom | Cause | Fix |
|---|---|---|
| `tool not found` for `ad4m_request_capability`, `ad4m_generate_jwt`, or any `{class}_*` tool | Not in the plugin's `contracts.tools` manifest allowlist (Rule 0) — a real executor tool, just not bridged. | Check whether your node's plugin build has added it; otherwise see `references/setup.md` → "Calling MCP tools without the plugin". |
| `tool not found` for `ad4m_add_model`, `ad4m_signup`, `ad4m_verify_email_code`, `ad4m_list_link_language_templates`, `ad4m_get_documentation`, `ad4m_instance_transcript`, `ad4m_instance_remove_from_collection`, `ad4m_add_child` or `ad4m_get_children` | Your plugin build predates the commits that added them to the static surface. | Update the plugin build; until then see `references/setup.md` → "Calling MCP tools without the plugin". |
| `tool not found` for something `contracts.tools` *does* list | The manifest declares a name the executor has no tool for — a manifest/executor mismatch, not a bridging gap. (`ad4m_remove_link` and `ad4m_agent_status` were exactly this until they were dropped from the manifest; a test now fails the build on any new one.) | Don't rely on it; the `mcporter` fallback won't help either since the tool genuinely doesn't exist. Report it upstream. |
| `Failed to get auth token` / `ad4m_get_my_did` errors after you set `config.token` | Config change didn't hot-reload, or a stale `AD4M_PASSWORD`/`config.password` is failing the auto-relogin on every restart (Rule 3b's runtime re-auth) — look for `[ad4m] Email login failed:` in the plugin log. | Check the gateway log for `[reload] config hot reload applied` following your change — if it never appears, restart the gateway manually. Check `AD4M_PASSWORD` in your environment matches the account's actual current password. |
| `Failed to get agent: User profile not found for <email>` on `subscribe_to_mentions` | No agent profile set (Rule 12). | Call `ad4m_set_agent_profile` first. |
| `subscribe_to_mentions`/`subscribe_to_children` returns an honest "not listening yet, retrying" response, or `list_waker_subscriptions` shows your subscription under **Pending** rather than active | Normal on a node that's still starting up, or genuinely correct if the node hasn't been unlocked yet (a locked wallet — see `ad4m_get_documentation(topic="usage")`) — the plugin retries automatically every 30s rather than silently pretending to have succeeded. | If Pending clears within a minute or two, no action needed. If it stays Pending, the underlying cause is almost always the node not being unlocked — that's the node operator's problem, not something to fix from your side. |
| `ad4m_channel_query`/`ad4m_message_create`/etc. return "tool not found" | You're reading old instructions or an old memory of this skill — these are dynamic per-class tools, opt-in only (Rule 9), not the default surface anymore. | Use `instance_query`/`instance_create` with `class_name` instead. |
