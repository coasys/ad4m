# Agent profiles

An **agent profile** is the `Agent` record: a DID, its **public perspective** (signed links that describe the agent) and the address of its **direct-message language**. `Agent` also carries `identity`: the person's `did:scid`, or `null` for a device-only agent. The executor stores the profile on disk and publishes it through the **agent language** as a signed **agent record** in the shared store: under the `did:scid` for an agent with an identity, under its `did:key` otherwise. `agent.byDid` reads the main agent's profile from memory and every other profile from the agent language.

**Why:** Other agents find a profile by DID alone, and check it without trusting the store: a `did:key` record is signed by that key, a `did:scid` record by a device key that the person's log lists at the record's anchor. `did` stays the device key so that apps that compare `did` with link authors keep working; the new `identity` field is where the SDK and the apps read the person. The old profile server lost: the shared store now serves the profiles of `did:key` agents too.

**Parent:** [Identity](README.md)

<details><summary>How it works</summary>

| Agent | Stored in | Published as |
|---|---|---|
| Main agent | `ad4m/agent.json` (`AgentStore.agent`); `ad4m/agentProfile.json` is read first if it exists | `identityPutAgent` with a signed request when the main agent has an identity; else `expressionCreate`, a record signed by its `did:key` |
| Managed agent | `ad4m/users/<username>/profile.json`: the `users` row's username (an email for a password user, `t-<uuid>` for a tenant), because a DID contains `:` (`user_profile_path`) | the same rule, for that agent's own key |

1. `agent.updateProfile { publicPerspective?, dmLanguage? }` needs `agent/UPDATE`. A user session updates only its own profile (`update_user_profile`). Any other session updates the main agent's profile.
2. RPC turns each link into a `DecoratedLinkExpression` with `link_expression_input_to_decorated`. That function drops the caller's `valid` field and verifies the signature on this executor.
3. `AgentService::publish_agent_to_language(ctx)` strips link decorations and publishes as that agent. An `AgentRecord` is `{ did, profile, version, signed_by, anchor, sig }`; a newer `version` replaces an older one, and the store answers `409` for an older one. A process-wide lock (`AGENT_PUBLISH_LOCK`) makes the last publish carry the latest profile. The lock serialises the publishes of all agents on the executor.
4. `agent.byDid(did)` answers the main agent's DID from memory. Any other DID, `did:key` or `did:scid`, goes to the agent language (`get_expression`), also a managed agent on the same executor. The executor verifies the record and its links again.
5. `agent.get` and `agent.status` return `did` (the device key) and `identity`. The SDK's `AgentClient.mutatePublicPerspective` builds the new link set, then calls `agent.updateProfile`.
6. `agent-updated` events carry the new profile.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `agent.get`, `agent.byDid`, `agent.byDIDs`, `agent.updateProfile`, `update_user_profile`, `publish_agent_updated`.
- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `publish_agent_to_language`, `get_agent_for_context`, `user_profile_path`, `load_managed_agent_profile`, `store_managed_agent_profile`, `agent_to_publish_json`.
- [rust-executor/src/types/domain.rs](../../../rust-executor/src/types/domain.rs): `Agent` (gains `identity`), `Perspective::verify_link_signatures`.
- `AgentRecord` in [ad4m-ldk/rust/src/identity.rs](../../../ad4m-ldk/rust/src/identity.rs) (new) and [ad4m-ldk/js/src/types.ts](../../../ad4m-ldk/js/src/types.ts).
- [core/src/agent/AgentClient.ts](../../../core/src/agent/AgentClient.ts) and [core/src/agent/Agent.ts](../../../core/src/agent/Agent.ts): SDK side.
- [rust-executor/src/mcp/tools/profiles.rs](../../../rust-executor/src/mcp/tools/profiles.rs): profile tools for MCP clients.
- The language behind the lookup: [Agent language](../../languages/other-kinds/agent-language/README.md).

</details>
