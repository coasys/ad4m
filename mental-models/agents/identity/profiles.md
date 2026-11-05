# Agent profiles

An **agent profile** is the `Agent` record: a DID, its **public perspective** (signed links that describe the agent) and the address of its **direct-message language**. The executor stores the profile on disk and publishes it as an expression of the **agent language**, keyed by the DID. `agent.byDid` reads the main agent's profile from memory and every other profile from the agent language.

**Why:** Other agents find a profile by DID alone. The agent language behind the lookup can be a Holochain DNA, a central service or a local stub. The default agent language (`centralized-agent-language`) posts profiles over HTTP to a development profile directory ([Agent languages](../../languages/other-kinds/agent-languages.md)).

**Parent:** [Identity](README.md)

<details><summary>How it works</summary>

| Agent | Stored in | Published by |
|---|---|---|
| Main agent | `ad4m/agent.json` (`AgentStore.agent`); `ad4m/agentProfile.json` is read first if it exists | `spawn_main_agent_publish` after `agent.generate` / `agent.unlock`; `agent.updateProfile` |
| Managed agent | `ad4m/users/<email>/profile.json` (named by email, because a DID contains `:`; `user_profile_path`) | `user.create`; `agent.updateProfile` in a user session |

1. `agent.updateProfile { publicPerspective?, dmLanguage? }` needs `agent/UPDATE`. A user session updates only its own profile (`update_user_profile`). Any other session updates the main agent's profile.
2. RPC turns each link into a `DecoratedLinkExpression` with `link_expression_input_to_decorated`. That function drops the caller's `valid` field and verifies the signature on this executor.
3. `AgentService::publish_agent_to_language(ctx)` strips link decorations and calls `expression_create` on the agent language as that agent. A process-wide lock (`AGENT_PUBLISH_LOCK`) makes the last publish carry the latest profile. The lock serialises the publishes of all agents on the executor.
4. `agent.byDid(did)` answers the main agent's DID from memory. Any other DID goes to the agent language (`get_expression`), also a managed agent on the same executor. The executor verifies those links again.
5. The SDK's `AgentClient.mutatePublicPerspective` builds the new link set, then calls `agent.updateProfile`.
6. `agent-updated` events carry the new profile.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `agent.get`, `agent.byDid`, `agent.byDIDs`, `agent.updateProfile`, `update_user_profile`, `publish_agent_updated`.
- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `publish_agent_to_language`, `get_agent_for_context`, `user_profile_path`, `load_managed_agent_profile`, `store_managed_agent_profile`, `agent_to_publish_json`.
- [rust-executor/src/types/domain.rs](../../../rust-executor/src/types/domain.rs): `Agent`, `Perspective::verify_link_signatures`.
- [core/src/agent/AgentClient.ts](../../../core/src/agent/AgentClient.ts) and [core/src/agent/Agent.ts](../../../core/src/agent/Agent.ts): SDK side.
- [rust-executor/src/mcp/tools/profiles.rs](../../../rust-executor/src/mcp/tools/profiles.rs): profile tools for MCP clients.
- [bootstrap-languages/agent-language/](../../../bootstrap-languages/agent-language), [bootstrap-languages/centralized-agent-language/](../../../bootstrap-languages/centralized-agent-language): agent languages; see [Bootstrap languages](../../languages/runtime/bootstrap-languages.md).

</details>
