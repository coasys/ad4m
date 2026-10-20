# Language trust and templates

`language_by_ref` installs a language only in two cases:

- its author is a **trusted agent**, or
- it is a **templated language** whose **source language** has a trusted author, and the source re-templates to the same hash.

**Templating** rewrites the marked constants (`//!@ad4m-template-variable`) of a source bundle, and for Holochain languages the DNA properties. The result is a new language with a new address. The trust lists are the executor's own; on a host, tenants use the operator's lists and cannot change them.

**Why:** Each neighbourhood needs its own copy of a link language: its own address, DNA network seed or link-server room. No one has to review new code for each copy. Templating changes only marked constants, so trust in the source carries over to every copy. Any executor can check a copy by templating the source again. Hosts run no language allowlist: the trust lists plus the central language-language registry are the whole control, so one rule serves desktops and hosts.

**Parent:** [Installing languages](README.md)

<details><summary>How it works</summary>

Trust check in `language_by_ref(addr)`:

1. Get the meta expression from the language language. Refuse it if `proof.valid` is false.
2. If `author` is in `RuntimeService::get_trusted_agents()` (seed `trustedAgents` + main agent DID + DB `trusted_agents`), fetch the bundle, check hash = `meta.address`, install.
3. Otherwise read `templateAppliedParams` and `templateSourceLanguageAddress`. Refuse if either is missing, or if the source meta's author is not trusted (publishes `ExceptionType::AgentIsUntrusted`).
4. Template the source again with the params. Refuse if the result's hash differs from the fetched bundle's hash.

Trust is per executor. The one trusted-agent list applies to every agent on the executor. A managed agent's own DID is not in it, and a managed agent (a tenant included) has no list of its own.

Templating (`language_apply_template_on_source(source, params)`):

1. Fetch the source bundle from the language language. Sort the params by key.
2. If the bundle has a `var happ = "<base64>"` line, unpack the hApp in the system temp directory, set the DNA `network_seed` to `uid`, copy every param into the DNA `properties`, and pack it again. This step needs a running Holochain conductor.
3. `apply_template_data` replaces the value on the line after each `//!@ad4m-template-variable` marker whose variable name is a param key.
4. Hash the result and return `LanguageLanguageInput { bundle, meta }` with `templateAppliedParams` and `templateSourceLanguageAddress` set.

Each language names its own template variables, and keys are case-sensitive. p-diff-sync uses `uid`; the server link language uses `UID`, `SERVER_URL` and `CREATOR`. A `uid` param sets the DNA network seed (step 2); it does not fill a `UID` constant.

The server link language's template: `SERVER_URL` (from the [link-server directory](../../link-languages/server-link-language/link-server-directory.md)), `UID` (32 random bytes, base58) and `CREATOR` (the creator's `did:scid`). Only `neighbourhood.create` fills it; because the filled bundle fixes the address, a neighbourhood URL binds the server, the room and the creator. `neighbourhood.publish` refuses a copy of that template made any other way.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/mod.rs](../../../../rust-executor/src/languages/mod.rs): `language_by_ref`, `read_and_template_holochain_dna`, `language_apply_template_on_source`, `apply_template_data` (unit tests at the end of the file).
- [rust-executor/src/runtime_service/mod.rs](../../../../rust-executor/src/runtime_service/mod.rs): `get_trusted_agents`, `add_trusted_agent`, `remove_trusted_agent`.
- [rust-executor/src/api/agent_ws.rs](../../../../rust-executor/src/api/agent_ws.rs): RPC `agent.getTrustedAgents`, `addTrustedAgents`, `deleteTrustedAgents` (the last two refuse tenant and user sessions).
- [rust-executor/src/api/languages_ws.rs](../../../../rust-executor/src/api/languages_ws.rs): `language.applyTemplate`.
- [rust-executor/src/neighbourhoods/create.rs](../../../../rust-executor/src/neighbourhoods/create.rs) (new): fills `SERVER_URL`, `UID`, `CREATOR`.
- [bootstrap-languages/server-link-language/index.ts](../../../../bootstrap-languages/server-link-language/index.ts): template variables `SERVER_URL`, `UID`, `CREATOR`.

</details>
