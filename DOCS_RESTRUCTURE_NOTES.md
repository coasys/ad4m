# Docs restructure notes — setup topic dropped, architecture deduped

Worker report for Nico. Branch `feat/static-instance-tools`, on top of
`94f88b374`. Nothing pushed. Two code commits plus this file; review
order = commit order.

## What moved where

| Before | After |
| --- | --- |
| `get_documentation(topic=setup)` served `rust-executor/src/mcp/docs/setup.md` | Topic removed from the `DocTopic` enum, the tool description, the topic doc, the `initialize` instructions and `overview.md`. File deleted. Setup lives only in `plugins/ad4m/skills/ad4m/references/setup.md`, whose callout now says why. |
| `plugins/ad4m/skills/ad4m/references/architecture.md` = 315-line copy of the executor doc with plugin edits mixed in | 56-line pointer at `ad4m_get_documentation(topic="architecture")` + the plugin-only deltas: `ad4m_` prefix, which tools are not bridged (`mcporter` fallback; `add_model` *is* bridged), the `contracts.tools` gate on dynamic per-class tools, and where setup / waker docs live. |
| Generic content that existed only in the plugin copy | Folded into the canonical `rust-executor/src/mcp/docs/architecture.md` so it is not lost: the `instance_transcript` and `add_child`/`get_children` detail in Key Operations, and the "static tools write the same links" note under Link Mapping. |
| `overview.md` said "see setup" for the auth tools and workflow step 1 | Short **Authentication** subsection (admin credential / multi-user login / capability flow), topic table lists `overview` / `architecture` only. |
| `plugins/ad4m/skills/ad4m/references/mcp.md` | Deleted (see audit below). |
| SKILL.md pointers "full reference in `references/architecture.md`" (×3) and topic lists (×2) | Point at the executor doc and its section; topic lists say `overview` / `architecture`. `references/setup.md` is still linked from Quick Setup. |
| `staticToolDefs.ts` get_documentation entry | Re-captured from the rebuilt binary's `tools/list`; the pinned enum in `index.test.ts` updated. |

## What was deleted

- `rust-executor/src/mcp/docs/setup.md` (296 lines; the skill file is the same material plus plugin specifics).
- `plugins/ad4m/skills/ad4m/references/mcp.md` (pre-consolidation tool reference).

**mcp.md audit before deleting.** Nothing linked to it (SKILL.md, README,
setup.md, waker.md all checked). Content by section: enabling/transport/curl
warning → `references/setup.md` L70–87 and SKILL.md Rule 2; auth options
A/B → SKILL.md Rule 3 and setup.md "auth flow"; tool tables → stale (listed
the removed `*_subject` family), superseded by the executor overview;
publish/join flow → overview table row + the tools' own descriptions;
dynamic-tool naming → SKILL.md Rule 9 and the executor architecture doc.
Dropped outright: the JSON-RPC error-envelope paragraph, and the note that
`Authorization` headers are not forwarded to tool handlers — SKILL.md Rule 3c
documents the opposite as verified (mcporter ≥ 0.13 `--header`).

## How the schema was re-captured

`cd cli && cargo build --release` (1m31s, incremental), then the rebuilt
`target/release/ad4m-executor` with a fresh temp data dir on spare ports
(API 12987, MCP 3987, hc 14987/14988, `--admin-credential`, dapp server
off). A ~40-line Node Streamable-HTTP client (initialize → initialized →
tools/list; kept in /tmp, not committed) dumped 43 live tools. I regenerated
the entire 26-entry array from the dump with the same serializer the previous
worker's format implies (`json.dumps(indent=2, ensure_ascii=True)` — it
reproduces the old file byte-for-byte), so any other drift would have shown
up as a diff. Only `get_documentation` differed.

## Test results (all foreground)

| Run | Result |
| --- | --- |
| `cd rust-executor && cargo test --release mcp -- --test-threads=1` | ok — 48 passed, 0 failed, 1374 filtered (build 2m03s). Log: `.rust-mcp-docs.log` |
| `cd plugins/ad4m && pnpm test` | ok — 103 passed (1 file) |
| `cd plugins/ad4m && pnpm run build` + load `dist/index.cjs` | ok |
| `cargo fmt --all` (and `-- --check` before the build) | clean, no changes |

New Rust test `docs_only_point_at_served_topics` fails if either served doc
mentions `topic="setup"` again; `topic_names_are_lowercase_on_the_wire` now
also asserts `"setup"` no longer deserializes; the instructions test asserts
they do not mention setup.

## Decisions on things the brief left open

1. **Overview gained an Authentication subsection.** Three places in
   `overview.md` pointed at the setup topic for auth. Deleting the pointers
   would have left a cold agent (e.g. connected via mcporter without a
   credential) with no served explanation of the auth tools, so I added ~15
   lines. While writing it I checked `auth.rs`: over MCP `request_capability`
   auto-permits and returns the code in its response (the log line is
   redacted unless `AD4M_LOG_SECRETS=1`), so the overview says that rather
   than repeating setup.md's "read the code from stdout".
2. **"mcporter fallback note for add_model."** The previous worker had
   already corrected the plugin copy: `add_model` is bridged; `get_models`
   is the one that is not. The new pointer file keeps one section saying
   which tools need the Rule 3c fallback and that `add_model` does not.
3. **Historical notes files untouched.** `CONSOLIDATION_NOTES.md` and
   `PLUGIN_ALIGN_NOTES.md` still mention `references/mcp.md`; they are
   dated reports, so I left them as written.
4. **Three commits, not two.** Rust and plugin split as the brief allows;
   this notes file is a third commit, matching the previous worker's
   pattern, so the two code commits stay clean.
5. Untracked logs `.build-cli-docs.log` and `.rust-mcp-docs.log` were
   left next to the earlier workers' logs; nothing untracked was committed.

## Commits (oldest first, local only)

```
260f0749e refactor(mcp): drop the setup topic from get_documentation; one canonical architecture doc
3f6743a40 docs(plugins/ad4m): point the skill at get_documentation instead of duplicating it
```
