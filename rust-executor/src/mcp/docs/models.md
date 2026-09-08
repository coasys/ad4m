# Authoring subject classes

A subject class (AD4M calls the registered set of them a perspective's *social
DNA*) is a SHACL shape that turns a pile of links into typed objects other
agents, apps and humans can read. This document is about **writing** one.

Where the three documents divide:

- `get_documentation(topic="usage")` — *using* classes that already exist:
  `describe_perspective` and the `instance_*` tools.
- `get_documentation(topic="architecture")` — the **field reference** for the
  SHACL JSON wire format: every top-level field, every `PropertyShape` field,
  every `AD4MAction` field, in tables. Keep it open next to this document; the
  field-by-field meanings are not repeated here.
- this document — how to *think about* a class, then how to write one that is
  writable, findable, and not a near-duplicate of someone else's.

Everything here is written with bare tool names. If your host prefixes them
(some do), apply its prefix.

## Before you write one: should you?

The failure mode that actually hurts a shared perspective is not a
badly-formed schema — `add_model` rejects those. It is two agents inventing
`Task` and `TodoItem` for the same thing, so half the data is invisible to
whoever queries the other name. Adding a class is a commitment for everybody
in the neighbourhood, not a local convenience.

So, in order:

1. **`describe_perspective(perspective_id)` first.** Read every registered
   class, its `interpretation_hint`, and its properties. A class whose name is
   unfamiliar may still be the right one — the hint is there to tell you what
   it is for.
2. **Can an existing class carry this with one more property?** Extending a
   class is cheaper for readers than adding one (but read "Changing a class
   that already has instances" below before you do it).
3. **Is this a relation rather than a class?** "A Task has an owner" needs a
   property on `Task`, not an `Owner` class, unless owners have their own
   properties and lifetime.
4. **Is it structure rather than schema?** Grouping things under a parent is
   what `ad4m://has_child` and `add_child` are for; you do not need a class to
   express "these belong together".

Add a class when the thing has its own identity, its own properties, and its
own lifecycle — when you would want to query for *it*, not for something that
mentions it.

## What the executor derives from your shape

Three decisions come out of the shape, and every one of them is a different
field. Getting them right is most of the work.

### 1. Which instances belong to the class

There is no type stamp on an instance that the query path trusts. Class
membership is **structural**: the executor builds a query from the shape and
whatever matches, belongs. In order of preference it uses

- every property with `min_count: 1` — the instance must have a link for each
  of those predicates;
- failing that, a property whose constructor action writes a fixed value
  (a class marker, see below);
- failing both, a fallback that matches *any* node carrying *any* of the
  class's predicates.

That last fallback is the trap. A class with no required property and no
marker will happily return instances of somebody else's class that happens to
use the same predicate URIs. **Give every class at least one required
property or one class marker.**

A class marker is a property with a fixed value (`has_value`) and
`min_count: 1`, plus a constructor action that writes it:

```json
"constructor_actions": [
  {"action":"addLink","source":"this","predicate":"note://entry_type","target":"note://has_note"}
],
"properties": [
  {"path":"note://entry_type","name":"entryType","has_value":"note://has_note","min_count":1}
]
```

`describe_perspective` then reports that property as
`read_only: true, read_only_reason: "class marker set automatically on
create"` — clients never pass it, the constructor does.

### 2. One value or many

For an ordinary (non-relation) property this is decided by `"collection":
true`, **not** by `max_count`. Only `collection: true` makes `add_model` mark
the property as a collection shape, and that mark is what
`describe_perspective` reads back to list the property under `collections`
instead of `properties`. A property with `max_count: 3` and no `collection`
flag is still validated and presented as single-valued.

For a relation the rule is different: `relation_kind` decides. `hasOne` and
`belongsToOne` are single-valued; `hasMany` and `belongsToMany` are not.

Collections are **unordered sets of links**. The SHACL JSON that `add_model`
parses has no ordering field, so if order matters you have to carry it
yourself in a property (this is why Flux keeps task order in a stringified
JSON array — see `get_documentation(topic="flux")`).

### 3. Whether a property can be written

A scalar property is writable only if it declares a `setter`. Without one,
`instance_create` and `instance_update` reject the write with *"read-only: the
class declares no setter for this property"* — a class that registers cleanly
can still be unwritable, and nothing warns you at registration time.

The `writable` field is accepted and stored, but the static `instance_*` tools
do not consult it: presence of a `setter` is what actually decides. Do not
rely on `writable: false` to protect a property that has a setter.

Collections behave differently again: `instance_create` (array value) and
`instance_add_to_collection` / `instance_remove_from_collection` resolve the
property's `path` predicate and write the link themselves. They never run your
`adder` / `remover` actions. Declare them anyway — the Prolog surface and the
optional per-class tools do use them — but their absence will not stop the
static tools.

## A class, end to end

Two classes, because one class on its own never shows you the relation
problems. `User` first, since `Post` points at it.

```
add_model(perspective_id, class_name="User", shacl_json='{
  "target_class": "ns://User",
  "interpretation_hint": "A person or agent who writes posts.",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://User"}
  ],
  "properties": [
    {"path":"ns://user_name","name":"name","datatype":"xsd://string",
     "min_count":1,"max_count":1,"identity":true,
     "interpretation_hint":"The name this person is known by.",
     "setter":[{"action":"setSingleTarget","source":"this",
                "predicate":"ns://user_name","target":"value"}]}
  ]
}')
```

```
add_model(perspective_id, class_name="Post", shacl_json='{
  "target_class": "ns://Post",
  "interpretation_hint": "One published article in this space.",
  "constructor_actions": [
    {"action":"addLink","source":"this","predicate":"rdf://type","target":"ns://Post"}
  ],
  "properties": [
    {"path":"ns://post_title","name":"title","datatype":"xsd://string",
     "min_count":1,"max_count":1,"identity":true,
     "setter":[{"action":"setSingleTarget","source":"this",
                "predicate":"ns://post_title","target":"value"}]},
    {"path":"ns://post_body","name":"body","datatype":"xsd://string","max_count":1,
     "setter":[{"action":"setSingleTarget","source":"this",
                "predicate":"ns://post_body","target":"value"}]},
    {"path":"ns://post_author","name":"writer","node_kind":"IRI",
     "relation_kind":"hasOne","target_class_name":"User","max_count":1,
     "setter":[{"action":"setSingleTarget","source":"this",
                "predicate":"ns://post_author","target":"value"}]},
    {"path":"ns://post_tag","name":"tags","datatype":"xsd://string","collection":true,
     "adder":[{"action":"addLink","source":"this",
               "predicate":"ns://post_tag","target":"value"}],
     "remover":[{"action":"removeLink","source":"this",
                 "predicate":"ns://post_tag","target":"value"}]}
  ]
}')
```

Line by line, the parts that are easy to get wrong:

- `class_name` is `"Post"` — the **local name** of `target_class`, never
  `"ns://Post"`. `add_model` rejects the mismatch, because registering under
  the URI form used to produce a class whose every property was silently
  read-only.
- `constructor_actions` is **not optional in practice**. It is what mints the
  instance's links. A shape registered without it registers fine and then
  fails every `instance_create` with *"No SHACL constructor found for class:
  Post"*.
- `title` and `name` carry `min_count: 1`, so they are the conformance
  predicates as well as the required fields. That is not a coincidence — make
  the property that every instance genuinely has the required one.
- `writer` is a relation because it has `relation_kind` and
  `target_class_name`, not because of `node_kind: "IRI"`. `target_class_name`
  is the bare `"User"`, never a URI. A `hasOne` relation still needs a
  `setter`; a `hasMany` does not (see architecture's relation section for the
  full rules and the two ways this fails).
- `tags` is a collection because of `"collection": true`, and it holds
  literals (`datatype`), not references, so no `target_class_name`.

Then verify, in this order — registration success proves almost nothing:

```
1. describe_perspective(perspective_id)
     → Post appears once, with title/body/writer under `properties`,
       tags under `collections`, writer typed "reference" with
       relation_kind "hasOne" and target_class "User".
2. instance_create(perspective_id, class_name="User",
                   properties={"name": "Ada"})
     → returns a base_uri like ad4m://obj/<24 random letters>
3. instance_create(perspective_id, class_name="Post",
                   properties={"title": "Hello", "body": "…",
                               "writer": "<the User base_uri>",
                               "tags": ["intro", "meta"]})
     → every property in ONE call. Collections take an array here.
4. instance_query(perspective_id, class_name="Post")
     → the post comes back. If it does not, your conformance is wrong,
       not your create.
5. instance_update(perspective_id, class_name="Post", base_uri=<id>,
                   properties={"body": "…"})
     → proves the setter actually works. Do this for every property you
       expect to be writable.
```

Expect two or three rounds. `add_model` validates that a schema is
well-formed, not that it is usable; a missing setter or a missing constructor
only surfaces at step 3 or 5, one property at a time.

## Teaching the class to its readers

Two fields exist purely so that a later reader — human or LLM — understands
what you meant. Both are surfaced by `describe_perspective`.

- `interpretation_hint`, at class level and per property: one sentence of
  natural language. This is what stops the next agent from inventing a second
  class for the same concept, and what a generic extractor quotes when it
  turns prose into instances of your class. Write it for someone who has never
  seen your perspective.
- `identity: true` on **at most one** property per class: the "title-like"
  key. When an interpreter runs twice over the same material, two proposed
  instances whose identity values match are treated as the same instance
  rather than duplicated. `describe_perspective` reports it as the class's
  `identity_property`.

A class with no hint and no identity property still works. It is just harder
for anyone else to use correctly, which is the whole cost you are trying to
avoid.

## Naming, namespaces and de-duplication

- The scheme of `target_class` is the namespace: `ns://Post` puts the shape at
  `ns://PostShape` and its property shapes at `ns://Post.title`. Pick a
  namespace that means something (`board://`, `notes://`), not a generic one.
- **Class names are unique per perspective, across namespaces.** Registering
  `Task` when a `Task` already exists — even under a different namespace —
  purges the old shape and installs yours. There is no "my Task, your Task".
- **Predicates are what the query engine actually matches on.** Two classes
  that reuse the same predicate URIs can pick up each other's instances under
  the structural fallback described above. Namespace your predicates
  (`board://task_title`, not `title`) unless you are deliberately sharing one
  — `ad4m://has_child` is the standard shared predicate for the child tree.
- Before adding a class to a *shared* perspective, say so in the channel and
  give it an `interpretation_hint`. A class nobody was told about is a class
  the next agent will duplicate.

## Changing a class that already has instances

Re-registering an existing `class_name` with new SHACL is a **refresh**, not a
duplicate: the executor purges the previous shape's links, writes the new
ones, and drops its cached shape. `describe_perspective` lists the class once.

What it does *not* do is touch existing instances — those are ordinary links
and nobody rewrites them. So:

- **Adding an optional property** is safe. Old instances read back without it.
- **Adding a required property (`min_count: 1`) is not.** It becomes a
  conformance predicate, and every instance created before the change lacks
  it, so they stop matching and disappear from `instance_query` — the data is
  still in the graph, but nothing typed can see it. Add the property as
  optional, backfill it on every existing instance with `instance_update`,
  and only then make it required.
- **Changing a property's `path`** orphans its links. The property reads empty
  on every existing instance; the old links stay behind, unreferenced.
  Renaming the `name` while keeping `path` is safe — `name` is only the label.
- **Removing a property** leaves its links in the graph, unread by anything.
- **Changing `constructor_actions`** affects new instances only, and if the
  class relies on a marker written by the constructor, old instances keep the
  old marker.

In a neighbourhood, remember the change has to gossip to every peer, and other
agents may be mid-write against the old shape. Prefer additive changes;
announce breaking ones.

## Checklist before you call it done

- [ ] `class_name` is the bare local name of `target_class`.
- [ ] `constructor_actions` is non-empty.
- [ ] At least one property is `min_count: 1`, or there is a class marker.
- [ ] Every scalar you expect to write has a `setter`.
- [ ] Every collection has `"collection": true` (not just a `max_count`).
- [ ] Every relation has `relation_kind` and a bare `target_class_name`;
      `hasOne` also has a `setter`.
- [ ] The class and its non-obvious properties have an `interpretation_hint`.
- [ ] `describe_perspective` shows what you expected.
- [ ] You have actually created, queried and updated one instance.
