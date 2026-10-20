# Bots

A **bot** is a device key in its owner's key log with the **bot mark**: `kind: "bot"`, its allowed actions (`can`: `read`, `write`, `join`) and an expiry, 30 days by default. It has no log of its own and runs as an agent on some executor: as that executor's main agent, or as a managed agent there. A **bot grant**, which the owner's master key signs, names the neighbourhoods where it may act ([Bot grants](../../access/grants/bot-grants.md)). The owner can pause a bot without a log event. The executor that runs the bot keeps a **bot activity log**: each call of the bot, its stated reason and the outcome, refusals included.

**Why:** One rule makes every bot belong to one person: it is a key in that person's log, so it always shows as linked to its owner, and revoking the key retires the bot and ends every grant at once. The log marks only what changes a signature's meaning: a bot never counts as the person in votes. The neighbourhoods stay out of the public log, in the grant. The executor that runs the bot sees every call of its session, so it is the one place that can record what the bot did and why.

**Parent:** [Devices](README.md)

<details><summary>How it works</summary>

1. The owner opens a `bot` offer on an app with the master key, with the actions and expiry. The bot's executor claims in the bot's agent context: `claim_for_tenant` for a managed agent, `identity.claimOffer` for a bot that is its executor's main agent (that executor's operator record must name the owner). The holder confirms with the match code and appends `add` with the bot mark ([Device linking](linking.md)). No master key and no inbox message reach a bot.
2. `scope` events change `can` and `expires`. A key past its `expires` reads `Expired`.
3. `identity.pauseBot { keyId, paused }` signs the next `PauseRecord` with a non-bot device key of the owner and stores it beside the log. A link by a paused bot key, first seen during the pause, reads `Invalid("bot-paused")`.
4. A bot key never opens a person's session by key login. A bot program on the desktop gets an app grant on the bot's agent through a one-time pairing code, never the admin credential.
5. For every call of a session that acts as a bot agent, the guard layer calls `BotLog::record` after it decides and before the handler answers. Read-only calls write no entry. The reason comes from the request's `meta.reason` (RPC) or the MCP tool call's `reason` argument, cut at 280 characters; an entry never holds link content.

| `BotOutcome` | Meaning |
|---|---|
| `Allowed` | the guard and the bot grant passed |
| `Refused("not-in-can")`, `("no-grant")`, `("paused")`, `("expired")`, `("revoked")` | the reason the call stopped |

A bot's limits sit in three places, each for one question: the log (`can`, `expires`) sets the ceiling, the pause record is the switch, and the bot grant on the bot's executor names the neighbourhoods. The refusal reasons name all three.

`bots.activity { keyId, before?, limit? }` returns the entries of one bot key, newest first, at most 200, only to a session whose identity owns that key. The event `bot-activity` reaches the owner's sessions on this executor. Entries live 30 days or 10,000 per bot key; a sweep runs each hour.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/identity/bot_log.rs](../../../../rust-executor/src/identity/bot_log.rs) (new): `BotActivity`, `BotOutcome`, `BotLog::record`, the `bot_activity` table and its sweep, `bots.activity`.
- [ad4m-identity-core/src/pause.rs](../../../../ad4m-identity-core/src/pause.rs) (new): `PauseRecord`, under the signing domain `coasys:pause`.
- `BotScope` and `KeyLife::Bot` in [ad4m-identity-core/src/lib.rs](../../../../ad4m-identity-core/src/lib.rs) (new).
- [rust-executor/src/api/guards.rs](../../../../rust-executor/src/api/guards.rs): the guard layer that writes the entries.

</details>
