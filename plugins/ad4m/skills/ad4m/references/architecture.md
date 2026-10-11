# AD4M Architecture — plugin notes

The architecture document itself — perspectives, links, languages,
neighbourhoods, and the SHACL subject-class (social DNA) format with every
`PropertyShape` field, the relation/setter traps, the generated-tool table
and the link mapping — is served by the executor, not kept in this skill:

```
ad4m_get_documentation(topic="architecture")
```

It needs no authentication and is compiled into the executor binary, so it
always describes the node you are connected to. `topic="overview"` is the
shorter companion: the tool surface, the workflow, authentication over MCP,
and the rules for writing data other agents and humans can use. When this
skill and the executor disagree, the executor is describing what is actually
running.

This file only records what is specific to reaching the executor through the
OpenClaw plugin.

## Tool names carry an `ad4m_` prefix

The executor docs name tools bare (`describe_perspective`, `instance_create`,
`add_model`). The plugin registers every bridged tool as `ad4m_<name>`
(`ad4m_describe_perspective`, `ad4m_instance_create`, …). Same tool, same
parameters; only the name differs. `topic` values, `class_name`s and every
parameter are passed through unchanged.

## Only manifest-listed tools are bridged

The executor exposes more tools than the plugin bridges natively. Whatever is
not in `contracts.tools` (Rule 0 of the main skill) is real but does not reach
you as an `ad4m_*` tool — for example `request_capability` and
`generate_jwt`. `ad4m-setup` calls both for you; the manual path is in
`references/setup.md` → "Calling MCP tools without the plugin".
`add_model` *is* bridged natively (SKILL.md, "Subject Classes (SHACL)"), so authoring a schema needs no
fallback.

## Per-class (dynamic) tools and the manifest

The executor's "Generated MCP Tools" section describes the opt-in
`--dynamic-class-tools` mode. Through the plugin there is a second gate: even
with the flag on server-side, a client only sees a per-class tool name that
is *also* declared in `contracts.tools`, and there is no wildcard or pattern
support for those names. So the mode does not scale past a small, fixed set
of classes known in advance — see Rules 0 and 9 of the main skill — and the
`instance_*` tools are the default for a reason.

## Where the rest lives

- Getting, running, unlocking and authenticating against an executor:
  `references/setup.md`. This is skill-only on purpose — the executor does not
  serve it, because an agent that can call `get_documentation` is already past
  setup.
- Waker / subscription configuration: `references/waker.md`.
- Everything else about the data model: `ad4m_get_documentation(topic="architecture")`.
