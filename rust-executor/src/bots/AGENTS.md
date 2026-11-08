# bots/ — agents that act for a person under the person's grant

A bot is a managed agent (a `users` row and a wallet key) whose key its owner's key log
lists with the bot mark (`KeyLife::Bot`: the actions it may ever take, and until when). The
owner's grant (`db/bots.rs`) says where and what it may do now.

| File | What |
|---|---|
| `mod.rs` | The registry (in memory, loaded by `init` at startup, written through), `reach` (what a method does for a grant), `decide_grant` / `decide_key` / `decide_reason`, `admit` (run by `HandlerMap::admit_entry` before the guard of every call from a bot's session), `MCP_REASON`, `BOT_MCP_TOOLS` |
| `log.rs` | The activity log: writes, joins and refusals with the reason; 10,000 per bot, 30 days, hourly sweep. Revoking keeps it |
| `service.rs` | `bot.createAgent`, `bot.setGrant`, `bot.grant`, `bot.list`, `bot.log` and their types (registered in `api/bots_ws.rs`) |
| `tests.rs` | The decision's unit tests. The walk over every method is in `api/tests/method_matrix_tests.rs` |

Rules:

- **Order of a bot's call:** revoked (`bot-revoked`), outside every grant (`bot-grant`), its
  own session (passes), paused (`bot-paused`), grant expired (`bot-expired`), action not in
  the grant (`not-in-can`), space not in the grant (`bot-grant`), then the owner's log
  (`bot-unbound`, `bot-revoked`, `bot-expired`, `not-in-can`, 503 when unreadable), then a
  write or join without `meta.reason` (400 `reason-required`). Messages start with the code.
- **What acts in a space:** a `Guard::Owns { owner: Perspective }` method whose capability
  reads, subscribes, creates or updates (the perspective's UUID, or its neighbourhood URL,
  must be in the grant), and `neighbourhood.join` (its URL). Methods that change or run work on
  a perspective (`PERSPECTIVE_ADMIN`) and every other guarded method are outside.
- **A new method** is outside every grant unless its guard is `Owns { Perspective }`. Adding a
  method a bot should call means adding it to `reach` and to the matrix walk's lists.
- **Only dispatch records** (`record: true`); `HandlerMap::admit` (MCP's pre-check) does not.
- **Who enforces what.** The grant (spaces, `can`, expiry, pause, revocation) is the hosting
  executor's: it is not signed by the owner's master key, and only the executor that runs the
  bot enforces it (a trusted-host restriction). What every other reader verifies is what the
  owner's key log says about the bot key: the bot mark, its `can` and its `expires`. A reader
  judges a bot's link `invalid:notInScope` when the scope in force at the link's anchor lacks
  `write` (`identity::verdict::judge`), `invalid:expired` past `expires`. Per-space, portable
  delegation (owner-signed, bound to stable space ids, checked by every reader) belongs to
  `neighbourhood.addBot`/`removeBot` (slice 3). The log's scope is the ceiling of the grant:
  `bot.setGrant` refuses a `can` outside it when the log is readable.
- **Known limit:** a grant scopes by space and action only. It names no class, predicate or
  subject: a bot that may write in a space may write any link there.
- `bot.createAgent` claims through `ClaimService::claim_for_agent`, the path a hosted
  tenant's claim will reuse. It runs beside `--custody-channel` and without multi-user mode
  (`config::may_add_managed_user`).
