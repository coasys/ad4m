# How the mental models are written

This folder holds the mental models of the ADAM code base: one tree of short pages that describe the code in this repository. Each page is a starting point into the code. The pages are versioned with the code: a page describes the code of the commit it sits in, and a pull request that changes a mental model changes its page in the same pull request.

## Rules

1. **One-to-one with the code.** A page describes only what exists in the code beside it. It does not mention open pull requests, plans, paused work or future designs. A "Why" explains the reason for today's design; it does not point at a future one.
2. **No security findings.** Pages do not describe security weaknesses, attacks or missing checks. They do describe, as neutral facts, how access works: that is part of the model.
3. **The current code, with certainty.** A page states what the current code does, as plain fact. Stale docs, defects and dead or unused code stay out of the pages: a page does not record where other docs disagree with the code, behaviour that looks like a defect, or code that nothing uses.

## The tree

One root, three branches, then 2–5 children per page, until each leaf points at code. The executor process (startup, config, data, multi-user mode, executor services) sits under Agents → Executor: the executor hosts the agents, and the Agents branch covers who acts and through what.

```
mental-models/README.md (ADAM)
├── agents/README.md
├── languages/README.md
└── perspectives/README.md
```

A reader must be able to stop at any level and hold a correct, if coarse, picture. The root [README.md](README.md) holds the full tree as an index.

## Files and folders

- A page with children is a folder with a `README.md`: `agents/access/README.md`. A leaf is a file in its parent's folder: `agents/access/sessions.md`.
- File and folder names are lowercase kebab-case: `wallet-and-keys.md`, `link-languages/`.
- A name does not repeat its parent's name: `agents/executor/config.md`, not `agents/executor/executor-config.md`. The page title may still use the full name ("Executor config").
- `GLOSSARY.md` defines every term a page uses. This file, `CONVENTIONS.md`, holds the rules.

## Links

- Link pages with relative paths in the tree: `[Sessions](../access/sessions.md)`, `[Access](../access/README.md)`.
- Link code with relative paths from the page to the file or directory: `[session.rs](../../../rust-executor/src/agent/session.rs)`. The link then always matches the commit the page sits in. Link only paths that exist.
- No line anchors (`#L10-L20`): they go out of date. Name the symbol in backticks instead: ``` `Session::open` in [session.rs](../../../rust-executor/src/agent/session.rs) ```.
- No links to issues or pull requests, unless the issue describes current behaviour and the reader needs it.

## One page

Every page follows this template. What is visible must fit on one screen.

```markdown
# <Name>

<One or two sentences: the model. What this thing is and what it does, in glossary terms.>

**Why:** <One short paragraph: why it is built this way, and the option that lost, if known.>

**Parent:** [<Parent>](../README.md)

## Parts
- [<Child>](<child>.md): <one line>
- [<Child with children>](<child>/README.md): <one line>
- …

<details><summary>How it works</summary>

<A short numbered flow, a small table or an ASCII diagram. No more than the reader needs to find their way into the code.>

</details>

<details><summary>Where in the code</summary>

- <relative link to directory/file>: <what is there>
- <key types and functions in backticks, with relative links to their files>

</details>
```

- The **Parent:** line links to the parent's `README.md`. A page whose parent is the root links to the root `README.md`.
- A leaf page has no "Parts" section; its "Where in the code" is its main content.

## Writing rules

- Plain technical English (ASD-STE100 style): short sentences, active voice, one claim per sentence.
- One meaning per word. Use the glossary term every time. Say "executor", never "node", for the ADAM process.
- No analogies, no metaphors. Name the real components, types and flows.
- Never repeat a parent's explanation. Link to it.
- One owner page per concept. Other pages that mention it keep one line and a link to the owner.
- Prefer a link over a paragraph. A page invites the reader to click, not to read everything.
- Never state what you have not checked in the code. If the docs and the code disagree, the code wins, and the page states only what the code does.
- No people's names, no private notes, no local paths, no secrets.
