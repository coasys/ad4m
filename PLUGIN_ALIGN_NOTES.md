# Plugin alignment notes — plugins/ad4m vs. the consolidated tool surface

Worker report for Nico. Branch `feat/static-instance-tools`, on top of the
#972-into-#973 merge (`71157f472`). Nothing pushed. Three commits, listed at
the bottom; review order = commit order.

## What changed

### `staticToolDefs.ts` — now a verbatim capture, 26 defs

Rather than hand-copying schemas out of the `tool_attr` definitions, I ran
the release binary already in this worktree (`target/release/ad4m-executor`,
built 17:23, i.e. after `350a4f7a1`, the last functional Rust commit; the
later `2239991e9` is fmt-only) with `--enable-mcp true` on spare ports and
captured `tools/list` over the real Streamable HTTP transport. The five
requested tools were taken from that dump byte-for-byte:

- `instance_remove_from_collection` (inserted after `instance_add_to_collection`)
- `instance_transcript`, `add_child`, `get_children`, `get_documentation` (appended)

The file's header now says how to refresh it the same way.

**Drift in the existing 21 (the brief asked for a spot-check of three; I
diffed all 21 against the live list).** Five entries differed and were
refreshed to the executor's output:

| Tool | What differed | Origin |
| --- | --- | --- |
| `describe_perspective` | description lists `instance_remove_from_collection` / `instance_transcript` | consolidation |
| `instance_add_to_collection` | description ends with "Undo with instance_remove_from_collection." | consolidation |
| `add_model` | description + `class_name` doc ("Bare subject class name, matching the local name of the shape's `target_class`…") | #976, arrived via the dev merge `3bdb460a1` |
| `set_agent_profile` | every optional field carries `"nullable": true` | not a Rust change — the struct has been `Option<String>` since May; the snapshot was simply not a real capture |
| `list_link_language_templates` | schema is `{"type":"object","properties":{}}` (no-param tool); description says pass the address as `link_language_template` | not a Rust change (March); same reason as above |

The last one deserves a note: the publish tool's parameter is `link_language`,
with `#[serde(alias = "link_language_template")]`, so both spellings work on
the wire and the live description is not wrong, just the alias. I did not
touch rust-executor, per the brief; if you'd rather the description name the
canonical `link_language`, that's a one-line change in `neighbourhoods.rs`.

`instance_query` / `instance_create` matched the executor exactly — no drift.

### `index.ts` — `STATIC_MCP_TOOLS` allowlist (not in the brief)

`registerMcpTool` returns early for any name not in `STATIC_MCP_TOOLS`, so
adding a def and a manifest entry alone would have registered nothing. The
five names were added there too. The new test guards this: every
`STATIC_TOOL_DEFS` entry must both register at `register()` time (with no
executor reachable) and appear in `contracts.tools`.

### `openclaw.plugin.json`

Five names added to `contracts.tools` (`ad4m_get_documentation` next to
`ad4m_get_sample_config`; the other four beside their `instance_*`
neighbours). The manifest's tool list is names only, so "descriptions
consistent with staticToolDefs" has nothing to attach to — OpenClaw takes
the description from `registerTool`, which reads it from the def.

### Docs (`SKILL.md`, `references/architecture.md`, `references/setup.md`)

- **`get_documentation` as the cold-start entry point**: a paragraph in
  Rule 0 of `SKILL.md`, a Key Operations bullet in `architecture.md`, and a
  callout at the top of `setup.md`.
- **The "most recent first" claim was wrong, and worse than the brief
  assumed.** `instance_query` always sends a `limit` (default 100), and
  `model_query` with a limit and no `order` pushes `ORDER BY ASC(?_first_ts)`
  into SPARQL (`query.rs` ~L150, `sparql_builder.rs` L128). So
  `instance_query(parent=<channel>, limit=20)` returns the **oldest** 20,
  oldest-first — the previous Rule 6 text, the "Read a channel" recipe and
  the wake-handling Step 1 would all have shown an agent the beginning of
  the channel, not the end. All three now use `instance_transcript`, and
  the `instance_query` entries say explicitly that there is no `order` and
  that `limit` keeps the oldest matches.
- `instance_remove_from_collection`, `add_child` / `get_children` documented
  in the existing collection and "Tree structure" sections; two new recipe
  rows (top-level channels, executor docs).
- Removed-tool names: `get_children_body_parsed` no longer appears as an
  "un-bridged but real" tool; there is one deliberate "gone from the
  executor entirely" note in Rule 0 and one troubleshooting row, both there
  so an agent with an old memory of `*_subject` / `parent_address` names
  gets redirected instead of retrying via `mcporter`. `setup.md`'s
  `create_subject` mention became `{class}_create`. No `parent_address`
  anywhere outside those two "this no longer exists" sentences.
- Also fixed while there (out of the literal audit list but plainly wrong):
  `architecture.md` said twice that `add_model` is "not natively bridged";
  it has been in the manifest and the allowlist since `388498089`.

### Tests

`cd plugins/ad4m && pnpm test`, foreground, run three times (baseline,
after the surface change, after the docs change):

```
 ✓ index.test.ts (103 tests)
 Test Files  1 passed (1)
      Tests  103 passed (103)
```

No count/list assertion needed updating; the existing "registers expected
tools and services" test only checked registered ⊆ manifest, which the new
tools satisfy. I extended that test rather than adding a separate suite.
`pnpm run build` (esbuild bundle) also succeeds and the bundle loads.

## Surprises / things to decide

1. **`pnpm install` in `plugins/ad4m` installs the repo workspace, not the
   plugin.** The plugin is not a `pnpm-workspace.yaml` member, so the
   brief's `pnpm install --frozen-lockfile || pnpm install` ran a no-op
   workspace install ("Scope: all 24 workspace projects", no
   `node_modules` in the plugin). `pnpm install --ignore-workspace` is what
   works; it left an untracked `plugins/ad4m/pnpm-lock.yaml` next to the
   existing `package-lock.json`. I did not commit it — decide whether the
   plugin should carry a pnpm lockfile (and drop the npm one) or the
   README/brief should say `npm ci`.
2. **`get_documentation`'s schema uses `$ref` + `$defs`.** schemars 1.0
   renders the `DocTopic` enum as `"topic": {"$ref": "#/$defs/DocTopic"}`
   with a `oneOf` of `const`s under `$defs`. The plugin's `toParameters`
   strips only `$schema`/`$id`, so OpenClaw receives the `$ref` as-is. I
   copied it faithfully as the brief asks and pinned that in the test, but
   I could not verify OpenClaw's validator resolves local `$ref`s. If it
   turns out not to, the fix is either inlining the enum in `toParameters`
   or `#[schemars(inline)]` on the Rust side.
3. **`ad4m_remove_link` and `ad4m_agent_status`** are still in the manifest
   and in `STATIC_MCP_TOOLS` although no such executor tools exist (the
   skill already flags this). Not in the brief; left alone.
4. **`references/mcp.md` and `references/waker.md` were outside the audit
   list and are untouched.** `mcp.md` is the pre-consolidation tool
   reference: it still tables `query_subjects`, `create_subject`,
   `get_children_body_parsed`, `get_subject_children` and the
   `{class}_*` workflow as the default. Nothing links to it from
   `SKILL.md`, but it ships in the package (`files: ["skills"]`). It
   needs either a rewrite against `rust-executor/src/mcp/docs/overview.md`
   or deletion; I'd suggest the latter now that `get_documentation`
   serves the same material from the binary. `waker.md` only mentions the
   dynamic `ad4m_channel_list` once. `README.md` likewise still describes
   dynamic tool discovery and an `ad4m_refresh_ad4m_tools` tool that the
   test asserts does *not* exist.
5. Capturing `tools/list` also showed `add_model`'s class-name validation
   text and the `set_agent_profile` nullability had never been in the
   snapshot — the previous file was partly hand-written. The header now
   says "verbatim capture" and how to redo it, so the next drift is a
   diff, not a guess.

## Commits (oldest first, all local, nothing pushed)

```
fdf355565 feat(plugins/ad4m): align the static tool surface with the consolidated executor
e80938749 docs(plugins/ad4m): skill docs for the consolidated static surface
58343985d test(plugins/ad4m): every static def must register and be in the manifest
```
