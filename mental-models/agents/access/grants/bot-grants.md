# Bot grants

A **bot** is a device key with the bot mark in its owner's key log; the log sets its ceiling (its actions and its expiry) ([Bots](../../identity/devices/bots.md)). A **bot grant** names where the bot may act: a statement signed by the owner's current master key that names the bot key, the one executor that runs it, up to 100 neighbourhoods (their URLs, in the field `spaces`), the actions (`read`, `write`, `join`) and an expiry, 30 days by default. On that executor, every session that acts as the **bot agent** holds at most the capabilities of its grant, for those neighbourhoods only. With no grant in force, it holds only `agent:READ` on itself.

**Why:** Every change to what a bot may do runs through the key that speaks for the person, so the grant is signed by the master key, not by the executor or the bot. The neighbourhoods stay out of the public log: only the bot's executor and the owner's apps see the grant. The check runs at each call, from a cache that verdict changes and pause records update, so a revocation, a pause or an expiry takes effect at the bot's next call with no new login. A bot never opens a person's session, and a bot grant never gives rights on the owner's agent.

**Parent:** [Grants](README.md)

<details><summary>How it works</summary>

| Action | Capabilities, with `pointers` = the bot agent's perspectives whose neighbourhood URL is in `spaces` |
|---|---|
| `read` | `perspective` `READ`, `SUBSCRIBE`; `neighbourhood` `READ`; `expression` `READ` |
| `write` | `perspective` `CREATE`, `UPDATE`, `DELETE` |
| `join` | `neighbourhood` `CREATE` on the URLs in `spaces` |
| always | `agent` `READ` on the bot agent |

1. The owner's custody service signs `BotGrant` under `coasys:bot-grant`. The owner's app calls `bot.setGrant` over its session on the bot's executor; or the owner pastes the grant into the bot's chat and the bot's harness calls it.
2. `bot.setGrant` checks: signed by the current master key at the log's head; `executor` is this executor's DID; `bot` resolves to the owner with the bot mark and one of this executor's agents holds it; `can` and `expires` within the bot scope in force; `version` above the stored one; at most 100 entries in `spaces`. `spaces: []` ends the grant.
3. `Session::may` intersects a bot agent's session capabilities with the grant at each call. A call outside it answers `403 bot-grant`. The operator of the bot's executor is never limited.
4. Each call of a bot agent that is not read-only, allowed or refused, gets one entry in the bot activity log ([Bots](../../identity/devices/bots.md)).
5. Revoking the bot key ends every grant at once, through the resolver.
6. A bot's harness on the desktop holds an app grant on the bot agent, from a capability request with a one-time pairing code ([App approval](app-approval.md)); the bot grant narrows that app grant's sessions.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/bot_grants.rs](../../../../rust-executor/src/agent/bot_grants.rs) (new): `BotGrant`, `SignedBotGrant`, `BotAction`.
- [rust-executor/src/db/bot_grants.rs](../../../../rust-executor/src/db/bot_grants.rs) (new): table `bot_grants (bot_key, agent_did, scid, grant_json, sig, version, expires_at, installed_at)`.
- [rust-executor/src/agent/session.rs](../../../../rust-executor/src/agent/session.rs): `Session::may`, the intersection with the bot grant.
- [rust-executor/src/identity/bot_log.rs](../../../../rust-executor/src/identity/bot_log.rs) (new): `BotLog::record`.
- `bot.setGrant`, `bot.grant` and `bots.activity` in the `bot` and `bots` RPC namespaces ([RPC API](../../interfaces/rpc-api.md)).

</details>
