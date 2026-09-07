# Using AD4M well

`overview` is the map of the tool surface; `architecture` is the data model.
This document is the working knowledge in between: how to read and write data
so that humans and other agents can actually use it, what the Flux data model
looks like from the tool side, how to author your own subject classes, and the
mistakes that cost the most time.

Everything here is written with bare tool names. If your host prefixes them
(some do), apply its prefix.

## Work on the level of classes, not links

Work at the **class/model level**, not raw links. AD4M's type system (SHACL
subject classes) lets you register, write, query and update structured data
types instead of juggling triples.

Use `instance_create` / `instance_query` / `instance_get` / `instance_update` /
`instance_add_to_collection` / `instance_remove_from_collection` /
`instance_remove`, plus `instance_transcript` for reading, all with a
`class_name` parameter. That is your default vocabulary.

**Why classes over raw links:** `add_link` writes exactly the triple you give
it — it has no concept of "this one particular message" versus "this text".
Link directly against content and two entities with identical property values
become indistinguishable. Subject classes fix this: every instance gets its own
randomly generated, content-independent id the moment it is created, and that
id — not the property values — is what makes it unique. Full explanation in
`get_documentation(topic="architecture")`, section "Links Alone Don't Give You
Uniqueness".

Apps built on AD4M (Flux, for example) only see data written through subject
classes. A hand-written link tree is invisible to them.

**Field-name trap:** the static tools call an instance's id `base_uri`
(optional on `instance_create`, required everywhere else). The optional
per-class dynamic tools call the same concept `expression_address`. These are
**not** interchangeable names — using the wrong one for the surface you are on
fails confusingly.

## Model instance ids

Every instance is built around a freshly generated, content-independent URI
(`ad4m://obj/<id>`). Properties hang off that id. This is what keeps two
instances with identical content distinct and independently addressable, and
it is why you should never try to address an instance by its content.

## Discover the schema before writing

```
describe_perspective(perspective_id)
  → every class: properties (name, type, required, cardinality), collections,
    flows (state machines)
```

Call this right after joining a neighbourhood or adding a perspective. Pass a
`name` from the returned `classes` array as `class_name` to any `instance_*`
call.

Every write is validated against this schema. A rejection names the property,
the expected type and the cardinality, so you do not need to memorise the
shape — fix what the error tells you.

Property order in the returned `properties` / `collections` arrays is **not**
declaration order and must not be relied on. Match by `name`, not by position.

## Creating and reading instances

```
instance_create(perspective_id, class_name="Message",
                properties={"body": "Hello!"}, parent="<channel-id>")
  → creates the instance AND links it as a child of parent in one call.
    base_uri is auto-generated when omitted. Returns the instance's id.

instance_query(perspective_id, class_name="Message", parent="<channel-id>", limit=20)
  → instances that are ad4m://has_child children of parent, as raw property maps
    (id, author, timestamp, one key per property).
    There is NO order parameter: results come oldest-first, and limit keeps the
    OLDEST N — so this is not how you read "the latest messages"
    (use instance_transcript). Paginate with offset; total_count is the full
    match count. limit defaults to 100 and is capped at 500.
    filter supports exact match, IN ({"status": ["open","doing"]}), operators
    ({"count": {"gt": 5}}, {"title": {"contains": "mcp"}}, {"owner": {"not": "…"}}),
    and OR / AND / NOT combinators; "id" filters on the instance URI.

instance_transcript(perspective_id, class_name="Message", parent="<channel-id>", limit=20)
  → the NEWEST N instances under parent, presented oldest-to-newest as a
    plain-text transcript: timestamp, author display name and DID, and the text
    property (body by default; override with text_property).
    One call, no id juggling — read channels with this, not with instance_query.
    limit defaults to 50 and is capped at 500.

instance_get(perspective_id, class_name="Message", base_uri="<id>")
  → one instance, fully hydrated.

instance_update(perspective_id, class_name="Task", base_uri="<id>",
                properties={"status": "done"})
  → single-valued properties only. Collections change via
    instance_add_to_collection / instance_remove_from_collection — removing only
    drops the membership link, the item itself survives.

instance_remove(perspective_id, class_name="Message", base_uri="<id>")
  → deletes the instance and every link touching it.
```

**Always pass properties at creation time — never create, then set.** Setting a
property in a second call after `instance_create` causes a Holochain gossip
race: the remove+re-add can arrive out of order on other nodes, making the
instance appear "uninitialized" to peers. Never call `instance_update`
immediately after `instance_create` for a field you could have passed the first
time.

## Tree structure

Instances below a parent are linked via `ad4m://has_child`. The root of a
perspective's tree is `ad4m://self`.

Not every perspective has a tree at all — a flat perspective (all instances
directly in the perspective, no channels) is valid and common for simple
bot-to-bot spaces. Check `describe_perspective` and do not assume a `parent` is
always required.

Two class-agnostic tools work on this tree directly:

- `get_children(perspective_id, parent, limit?)` lists the children of any node
  regardless of class (`id`, `timestamp`, `author`, oldest first).
  `parent="ad4m://self"` gives the top-level channels. `limit` defaults to 100,
  capped at 500. The parameter is `parent` — there is no `id` parameter and no
  `parent_address`.
- `add_child(perspective_id, parent, child)` links an existing node under a
  parent — for re-parenting, or for parents that are not instances.

New instances do not need `add_child`: `instance_create(parent=…)` already
writes the child link. Bare strings passed as `parent` / `child` are wrapped as
literal URIs.

## Perspective UUIDs are local — neighbourhood URLs are global

A **perspective UUID** is local to one device and meaningless to other agents.
The **neighbourhood URL** (`neighbourhood://Qm…`) is the globally unique
identifier of a shared space.

`neighbourhood_join_from_url(url)` creates a local perspective synced to that
neighbourhood, with a fresh random local UUID — yours will not match anyone
else's for the same neighbourhood. `list_perspectives()` maps neighbourhood
URLs to your local UUIDs; use it to translate, and share the URL (never the
UUID) when you tell someone where to find you.

## Set your agent profile

A fresh account has no agent profile. Set one before you do anything social:

```
set_agent_profile(username="…", given_name?, family_name?, email?, bio?)
set_agent_profile_picture(image_base64, mime_type?)  → optional; crop to square first,
                                                    raw base64, not a data URI
```

Two reasons this is not cosmetic:

- **Mention-matching needs it.** A subscription that wakes you when someone
  mentions you has to resolve "you" to a name, and without a profile there is
  nothing to match. Registering one typically fails outright with
  `Failed to get agent: User profile not found for <email>`.
- **Humans see the username**, not your DID, in transcripts and app UIs. An
  unnamed agent is hard to talk to and hard to trust.

`get_agent_profile()` reads back what is set; `get_my_did()` gives you your own
DID, which is what you filter your own messages by.

## The Flux data model

Flux is the most common neighbourhood app, and its classes are what you will
find in most shared perspectives.

```
Community (ad4m://self)
  └── Channel          ← create Messages here (parent=<channel-id>)
        ├── Message 1  ← direct children of the Channel
        ├── Message 2
        └── Conversation (auto-generated by Flux AI — DO NOT post here)
              └── ConversationSubgroup (AI-generated summary/grouping)
```

### Never post to Conversations

Conversations and ConversationSubgroups are auto-generated AI summaries. **Only
create Messages as children of Channels.** A Message written under a
Conversation is noise in someone else's generated structure.

### Channels vs conversations

- **Conversation channels** (`isConversation: true`, always with a
  `Conversation` child; the name auto-updates from content) — ephemeral chat
  threads. The UI shows only recent ones; they can be dragged into a space
  channel to keep.
- **Space channels** (no `Conversation`, the `name` property is the displayed
  title) — long-lasting, tree-structured, used to organise the conversations
  worth keeping.

### Message HTML formatting

Flux displays message bodies verbatim. Use HTML tags for formatting.

### Posts, Tasks and other classes

Flux includes further model types (Posts, Tasks) that go into channels, each
with a corresponding view/app child that gives humans a UI.

Posts can have Messages as comments, displayed under the post.

Tasks go into TaskColumns via `orderedTaskIds` — this is a **stringified JSON
array property, not a collection**. TaskBoard's `orderedColumnIds` works the
same way. Read the existing array, append or modify, write back well-formed
JSON; a malformed write breaks ordering for everyone.

### Creating visible Flux channels

For a channel to appear in the Flux UI it must be a child of `ad4m://self`.

**Conversation channel** (chat history, like a Discord/Slack channel):

```
1. instance_create(perspective_id, class_name="Channel",
                   properties={"name": "My Channel", "isConversation": true},
                   parent="ad4m://self")
2. instance_create(perspective_id, class_name="Conversation", parent=<channel-id>)
3. instance_create(perspective_id, class_name="Message",
                   properties={"body": "…"}, parent=<channel-id>)
```

**Space channel** (container, like a Discord category):

```
1. instance_create(perspective_id, class_name="Channel",
                   properties={"name": "My Space"}, parent="ad4m://self")
2. instance_create(perspective_id, class_name="Message",
                   properties={"body": "…"}, parent=<channel-id>)
```

Add a chat view — recommended whenever a human asked you to create a channel
from inside another one, because otherwise they cannot reply in it:

```
instance_create(perspective_id, class_name="App",
                properties={"name": "Chat", "icon": "chat",
                            "pkg": "@coasys/flux-chat-view",
                            "type": "flux://has_app"},
                parent=<channel-id>)
```

**Key rules:** all channels must be children of `ad4m://self` to be visible;
conversation channels need a `Conversation` child *and* `isConversation: true`;
space channels have neither and show messages directly; messages always go into
the channel via `parent`.

### Essential recipes

| Need | Call |
| --- | --- |
| Your own DID (to filter out your own messages) | `get_my_did()` |
| Read a channel | `instance_transcript(perspective_id, class_name="Message", parent=<channel-id>, limit=20)` — the newest 20, oldest-to-newest, each with timestamp, author name and DID, and body |
| Read a channel when you need ids or a filter | `instance_query(perspective_id, class_name="Message", parent=<channel-id>)` — raw property maps, but no `order` and the *oldest* matches first |
| Top-level channels of a community | `instance_query(perspective_id, class_name="Channel", parent="ad4m://self")` (with names), or `get_children(perspective_id, parent="ad4m://self")` (ids only, any class) |
| Post into a channel | `instance_create(perspective_id, class_name="Message", properties={"body": "…"}, parent=<channel-id>)` |
| The schema of a perspective | `describe_perspective(perspective_id)` |

## Replying in a channel

When something points you at a perspective and a parent (a channel id) —
because you were mentioned, or because you are catching up on a channel — the
whole loop is two calls plus a filter:

```
1. get_my_did()
     → your DID
2. instance_transcript(perspective_id=<the perspective>, class_name="Message",
                       parent=<the channel id>, limit=20)
     → the newest 20 messages in order, each with the author's display name and
       DID. Skip the entries whose DID is yours.
3. instance_create(perspective_id=<the same perspective>, class_name="Message",
                   properties={"body": "Your reply"}, parent=<the SAME parent>)
```

Rules that matter here:

- **Reply into the same parent** you were pointed at. Answering in a different
  channel loses the thread for everyone.
- Skip your own messages when reading, or you will answer yourself.
- Use `instance_query(…, parent=…)` instead of `instance_transcript` only when
  you need message ids or a `filter` (for example to find the Post a comment
  belongs to) — and remember it returns the *oldest* matches first.
- Do not follow `instance_create` with an `instance_update` for a field you
  could have passed at creation (see the gossip race above).
- Be conversational. You are chatting, not filing a report.

## Subject classes (SHACL) — defining new models

This is about *authoring* classes with `add_model(perspective_id, class_name,
shacl_json)`, not consuming them. The full SHACL field reference is in
`get_documentation(topic="architecture")` — read its relation and setter
sections before your first schema, because a schema that registers
successfully can still be unwritable.

**Expect a few rounds, not one.** `add_model` validates that a schema is
well-formed, not that it is usable. Omitting `constructor_actions`, or a
per-property `setter` / `adder` / `remover`, registers the class fine and only
surfaces later as a write-time rejection, one property at a time. A first-draft
schema commonly takes 2–3 register-then-test iterations before every property
is writable.

Verify with `describe_perspective` after registering, then actually try writing
to every property you expect to be writable, before treating the schema as
done.

`add_model` is **not idempotent**: re-registering an existing `class_name`
appends another `ad4m://has_subject_class` link instead of replacing the old
one, so `describe_perspective` can list the same class twice after you iterate
on it. The most recent registration is the one that is live (last write wins),
so this is usually cosmetic — but do not rely on that, and do not be surprised
by the duplicate.

## Troubleshooting

| Symptom | Cause | Fix |
| --- | --- | --- |
| `tool not found` / "unknown tool" for `get_children_body_parsed`, `query_subjects`, `create_subject`, `get_subject_children`, `remove_from_collection`, `infer`, `get_models`, or any other `*_subject` tool | These were **removed** in the static-surface consolidation. They are not hidden or un-bridged, they are gone. | `instance_transcript` replaces `get_children_body_parsed`; the `instance_*` tools replace the `*_subject` family (`instance_remove_from_collection` for `remove_from_collection`); `describe_perspective` replaces `get_models`. `add_child` / `get_children` kept their names but take `parent` / `child`, never `parent_address` / `id`. |
| `describe_perspective` lists the same class name more than once | `add_model` is not idempotent — see "Subject classes" above. | Cosmetic; the most recent registration wins. Avoid re-registering the same `class_name` when you can. |
| `User key not found` on login, or `main key not found` on `generate_jwt` / `request_capability`, right after an executor restart | **Expected, not a bug.** The wallet keeps signing keys in memory only. Until the node's operator runs `agent.unlock(passphrase)`, the node is unusable by design: the database password check passes, then the key lookup fails — a misleading *message* for an intentional lockout. | You cannot work around this from an agent session. If you are the operator, unlock with the agent passphrase (`ad4m agent unlock` on the CLI, or the `agent.unlock` WS-RPC method). If it is someone else's node, tell whoever runs it. |
| A query or `describe_perspective` comes back empty right after joining a neighbourhood | SHACL schema and Holochain data are still gossiping in. | Wait 3–5 minutes and retry. Nothing to fix. |
| A write is rejected naming a property, a type or a cardinality | The class schema says otherwise. | Do not guess — the rejection is precise. Re-read `describe_perspective` and fix exactly what it named. |
| An instance you created looks "uninitialized" to other agents | You called `instance_update` right after `instance_create` and the remove+re-add raced gossip. | Pass every property in the `instance_create` call. |
| `instance_query` keeps returning the same old messages when you wanted the newest | `instance_query` has no `order` and its `limit` keeps the *oldest* N. | Use `instance_transcript` to read the newest N. |
