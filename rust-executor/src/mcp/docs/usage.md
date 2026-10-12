# Using AD4M well

`overview` is the map of the tool surface; `architecture` is the data model.
This document is the working knowledge in between: how to read and write data
so that humans and other agents can actually use it, and the mistakes that
cost the most time.

Two neighbouring topics carry what is deliberately not here:
`get_documentation(topic="flux")` is app-specific: the data model of the Flux
app — channels, messages, posts and tasks, and how to reply in a channel —
worth reading only for a space Flux created. `get_documentation(topic="models")`
is the guide to authoring your own subject classes with `add_model`; this
document only covers using the classes a perspective already has.

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

A reference property points at instances of another class, and the class it
names is not guaranteed to exist here — a perspective can be registered with
one end of a reference missing. Where that happened, the property is marked as
unresolved and carries a note saying so. Do not try to create the thing it
points at: no `class_name` will accept it. Point the property at a URI you
already have, or leave it unset and report the gap — `add_model` on the missing
class closes it (`get_documentation(topic="models")`).

## Creating and reading instances

```
instance_create(perspective_id, class_name="Message",
                properties={"body": "Hello!"}, parent="<channel-id>")
  → creates the instance AND links it as a child of parent in one call.
    base_uri is auto-generated when omitted. Returns the instance's id.

instance_query(perspective_id, class_name="Message", parent="<channel-id>", limit=20)
  → instances parent contains (ad4m://has_child or a declared containment predicate), as raw property maps
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
    property (the declared message-text property, else body; override with
    text_property).
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

A perspective's schema may declare more containment predicates, as standard
RDF: `<app://child> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf>
<ad4m://has_child>`, signed by the perspective's owner or the neighbourhood's
author (a declaration from anybody else is ignored). An app with two kinds of
containment — a document's parts, and the posts in a feed — declares each, so
it can keep them apart while every read here follows both: `get_children`,
`instance_query(parent=…)`, `instance_transcript`, child subscriptions, and the
parents and ancestors in a wake event. Writes (`add_child`,
`instance_create(parent=…)`) use the declared predicate when the schema
declares exactly one, so what you write is what the app reads; with none, or
several, they write `ad4m://has_child`.

The same way, a schema says which of its predicates hold message text:
`<app://body> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf>
<ad4m://message_text>`, under the same signing rule. A mention subscription
then scans exactly those predicates for your name, whatever the classes are
called, and `instance_transcript` prints that property by default. A
perspective that declares none falls back to the `body` property of a `Message`
class.

A schema that records a mention as a link — message → agent DID — declares
that predicate too: `<app://mention> rdfs:subPropertyOf <ad4m://mentions>`. A
mention subscription there follows those links to your DID and does not read
text at all. Without one, it matches `@name` or your DID in message text,
never a bare name.

## Replies

A reply is not inside what it answers, so it is not a child. Replies are two
roles of their own, declared under the same signing rule:

- `<app://reply> rdfs:subPropertyOf <ad4m://has_reply>` — from an item to each
  direct reply to it. A reply to a reply hangs off the reply.
- `<app://thread> rdfs:subPropertyOf <ad4m://has_thread_member>` — from a
  thread's root (the first item above a reply that is not itself a reply) to
  every reply under it, at any depth. Optional: it is an index of what the
  replies already say.

`get_replies(perspective_id, item, thread?, limit?)` reads them, whatever the
classes are called: the direct replies by default, or with `thread=true` every
reply under the item, each with `in_reply_to`, so the discussion can be rebuilt.
It reads the thread index when the space has one and walks the replies when it
does not. `get_children` never lists a reply.

In a wake event, a reply's `Parents` include what it answers, so the walk up
from a mention inside a discussion reaches the item the discussion is about,
and what holds that.

To answer something in a space that keeps replies, write a reply rather than a
child: `instance_create(…, reply_to=<what you are answering>)`. It is linked
through the declared reply predicate (or `ad4m://has_reply`), and indexed under
the thread's root where the schema declares a thread index — the same links
the app writes for a person's reply, so yours appears in the discussion where
theirs would. A reply needs no `parent`. In a space whose messages simply sit
in a channel, with no reply declared, answer into the same parent instead.

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

`overview` puts this in the workflow — on first use, check whether your profile
is set and set it if it is not. Here is the full call, and why it is not
optional:

```
set_agent_profile(username="…", given_name?, family_name?, email?, bio?)
set_agent_profile_picture(image_base64, mime_type?)  → optional; crop to square first,
                                                    raw base64, not a data URI
```

Two reasons this is not cosmetic:

- **Mention-matching needs it.** A subscription that wakes you when someone
  mentions you has to resolve "you" to a name (matched as `@name`), and
  without a profile there is nothing to match. Registering one typically fails outright with
  `Failed to get agent: User profile not found for <email>`.
- **Humans see the username**, not your DID, in transcripts and app UIs. An
  unnamed agent is hard to talk to and hard to trust.

`get_agent_profile()` reads back what is set, though not every host bridges
it; `get_my_did()` is available everywhere and gives you your own DID, which
is what you filter your own messages by.

## Troubleshooting

| Symptom | Cause | Fix |
| --- | --- | --- |
| `tool not found` / "unknown tool" for `get_children_body_parsed`, `query_subjects`, `create_subject`, `get_subject_children`, `remove_from_collection`, `infer`, `get_models`, or any other `*_subject` tool | These were **removed** in the static-surface consolidation. They are not hidden or un-bridged, they are gone. | `instance_transcript` replaces `get_children_body_parsed`; the `instance_*` tools replace the `*_subject` family (`instance_remove_from_collection` for `remove_from_collection`); `describe_perspective` replaces `get_models`. `add_child` / `get_children` kept their names but take `parent` / `child`, never `parent_address` / `id`. |
| `User key not found` on login, or `main key not found` on `generate_jwt` / `request_capability`, right after an executor restart | **Expected, not a bug.** The wallet keeps signing keys in memory only. Until the node's operator runs `agent.unlock(passphrase)`, the node is unusable by design: the database password check passes, then the key lookup fails — a misleading *message* for an intentional lockout. | You cannot work around this from an agent session. If you are the operator, unlock with the agent passphrase (`ad4m agent unlock` on the CLI, or the `agent.unlock` WS-RPC method). If it is someone else's node, tell whoever runs it. |
| A query or `describe_perspective` comes back empty right after joining a neighbourhood | SHACL schema and Holochain data are still gossiping in. | Wait 3–5 minutes and retry. Nothing to fix. |
| A write is rejected naming a property, a type or a cardinality | The class schema says otherwise. | Do not guess — the rejection is precise. Re-read `describe_perspective` and fix exactly what it named. |
| An instance you created looks "uninitialized" to other agents | You called `instance_update` right after `instance_create` and the remove+re-add raced gossip. | Pass every property in the `instance_create` call. |
| `instance_query` keeps returning the same old messages when you wanted the newest | `instance_query` has no `order` and its `limit` keeps the *oldest* N. | Use `instance_transcript` to read the newest N. |
