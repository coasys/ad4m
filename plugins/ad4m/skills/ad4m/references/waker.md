# AD4M Waker (Embedded)

The AD4M waker watches perspectives for data changes via WebSocket subscriptions and wakes your OpenClaw agent when relevant events occur. It runs as a background service inside the AD4M plugin — no separate process needed.

## How It Works

1. The plugin's `ad4m-waker` service connects to the AD4M executor's WebSocket event endpoint
2. When you call `subscribe_to_mentions` or `subscribe_to_children`, the plugin creates a `QuerySubscriptionProxy` with a SPARQL live query
3. When query results change, the plugin debounces and POSTs to OpenClaw's `/hooks/wake` endpoint
4. Your agent wakes up with context about what changed and processes the new data via MCP tools

## Plugin Config Fields

| Field           | Default                             | Description                                                                     |
| --------------- | ----------------------------------- | ------------------------------------------------------------------------------- |
| `wakerEnabled`  | `true`                              | Enable/disable the waker service                                                |
| `executorUrl`   | `http://localhost:12000`             | AD4M executor HTTP base URL                                                          |
| `wakeUrl`       | `http://localhost:18789/hooks/wake` | OpenClaw wake endpoint URL                                                      |
| `wakeToken`     | auto from `hooks.token`             | Override for the hooks token. Auto-read from OpenClaw global config if omitted. |
| `debounceMs`    | `2000`                              | Debounce interval to prevent rapid-fire wakes (ms)                              |

## Subscription Tools

| Tool                                                                 | Description                                                         |
| -------------------------------------------------------------------- | ------------------------------------------------------------------- |
| `ad4m_subscribe_to_mentions(perspective_id)`                         | Watch for any item that mentions you (links to your DID with `we://mention`) |
| `ad4m_subscribe_to_children(perspective_id, expression_address)`     | Watch for new children under a parent, of any class (messages in a channel, blocks in a call, …) |
| `ad4m_unsubscribe_from_mentions(perspective_id)`                     | Stop watching mentions in a neighbourhood                           |
| `ad4m_unsubscribe_from_children(perspective_id, expression_address)` | Stop watching that parent                                           |
| `ad4m_list_waker_subscriptions()`                                    | List active subscriptions, plus any still pending re-attempt        |

The subscribe tools call the MCP tools `ad4m_get_mention_waker_config` / `ad4m_generate_waker_query` internally to build the SPARQL queries — you don't need to construct queries manually.

## Wake Message Format

**Use `/hooks/wake` (recommended).** It enqueues the event into the main agent session which has your skills loaded. Do NOT use `/hooks/agent` — that spawns an isolated sub-agent without your skills.

### Mention events

For mention subscriptions, the wake message includes per-item details with resolved parents:

```json
{
  "text": "You were @mentioned in an AD4M neighbourhood.\nRead the AD4M skill for instructions on how to handle this.\n\nAgent DID: did:key:z6Mk...\nPerspective: cda8c4fc-...\nSubscription: mention-abc\nEvent type: mention\n\nMentioned items (1):\n  Item: literal:string:msg-123\n  Mention link signed by: did:key:z6Mkh... (signature verified)\n  Mention link time: 2026-10-27T00:41:23.437Z\n  Parents: literal:string:channel-1, literal:string:conv-thread-5\n  Ancestors: literal:string:community-1",
  "mode": "now"
}
```

The `Mentioned items` section lists each item that triggered the wake:
- **Item** — the address of the record that mentions you: the source of the `we://mention` link to your DID
- **Mention link signed by** — the DID that signed the `we://mention` link, and whether that signature verified. It says who put the mention on this item, and nothing more: the item's text and its other properties are other links, which someone else may have written or changed since, so a verified signer does not make the text that person's words, nor a request you must act on. Decide that from who wrote the text. `(unknown)` when the subscription's query gave no author
- **Mention link time** — when that signer signed the link (ISO 8601); a mention can reach you late, after a restart
- **Matched by**, **Spoken by**, **Spoken line time** — in place of the two mention-link lines, for a spoken transcript line that says your name (see below)
- **Parents** — every container holding this item: whatever links it with `ad4m://has_child`, and every instance of a class whose own `children` relation holds it (a We call holding a block through `we://child`)
- **Ancestors** — everything above the parents, nearest first, a few levels up (absent when there is nothing above them)

**The item is not necessarily a message.** The subscription is a SPARQL query over links, not over a class: it matches every `we://mention` link to your DID, which is how WE stores a mention (a post's root or a feed line links to each person it mentions). The name the text shows plays no part, so you are woken whatever label the writer's app gave you. Links you authored yourself never match, so your own reply mentioning you does not wake you, and neither does a link whose signature does not verify. So the address you are handed is whatever links to you as a mention: a post, a line in a feed, or a record of a class this space invented. Treat "message" as one possible answer, never the assumption.

**Spoken lines (temporary).** WE's call transcriber does not write `we://mention` links yet, so the query also matches a record whose `we://text_source` is `"spoken"` and whose `we://text` says one of your profile names (username, given or family name) as a whole word, in any case. Typed text never matches by name, only by its link. Such an item says `Matched by: your name in a spoken transcript line`, and `Spoken by` names the speaker (the signer of the line's text link). A name match can be a false alarm: "data" in "the data shows" wakes an agent called Data. Read the line before acting. This goes away once WE links spoken mentions.

An item can have multiple parents when an app derives further containers from it (auto-generated summary threads, for example).

### Reading the context around a mention

The wake event gives you two addresses and no types. Work out the rest from the space's own ontology, cheapest step first, and stop as soon as you know enough to act:

1. **`ad4m_describe_perspective(perspective_id)`** — the classes this space actually has, their properties and collections. This is the vocabulary for everything below; nothing else tells you what a "task" or a "proposal" means here.
2. **Type the item.** `ad4m_query_links(perspective_id, source=<Item>)` returns its links, including its `rdf://type` marker and its property predicates — match those against the classes from step 1. Then `ad4m_instance_get(class_name=<that class>, base_uri=<Item>)` for the resolved property values.
3. **The context the request is about, read by default.** Type the parent the same way (`query_links(source=<Parent>)`), then read its other children: `ad4m_instance_transcript(parent=<Parent>)` for the children that carry text, `ad4m_instance_query(class_name=…, parent=<Parent>)` for full property maps, `ad4m_get_children(parent=<Parent>)` when you don't yet know which classes are down there. Then go as far up the `Ancestors` as the request reaches: a request about the conversation ("what did we decide", "summarise this", "who is doing what") reads the ancestor that holds the conversation, not just the item's neighbours. Match the reading to the request rather than stopping at one step and waiting to be unclear.
4. **Further only when the request needs it.** Down: `get_children` on a sibling that looks like a container. Sideways: the collections the item's class declares, resolved by `instance_get`. Above the listed ancestors: walk down with `get_children` from `ad4m://self` to the ancestor you have. Do not look for parents with `query_links(target=…, predicate="ad4m://has_child")`: it misses every container that holds through its own `children` relation. Each hop costs a call.
5. **Then decide what is being asked of you, and in what shape the answer belongs.** The ontology says what you may write: usually a new instance of the same class as the item's siblings, under the same parent. If no class fits what you want to say, that is a signal to ask rather than to invent one.

Some containers must not be written into — for a space an app created, that app's own documentation says which (`ad4m_get_documentation(topic="flux")` for a Flux space; for other apps, the conventions are in `describe_perspective` and whatever docs the executor serves). There is no `ad4m_channel_list` on the static surface.

### Channel-messages events

```json
{
  "text": "New items in an AD4M neighbourhood.\nRead the AD4M skill for instructions on how to handle this.\n\nAgent DID: did:key:z6Mk...\nPerspective: cda8c4fc-...\nSubscription: children-xyz\nEvent type: channel-messages",
  "mode": "now"
}
```

### Common fields

- **Agent DID** — the agent's own DID (to identify own messages)
- **Perspective** — local perspective UUID to operate on (look up your memory file for context about this space)
- **Subscription** — subscription ID
- **Event type** — `"mention"` or `"channel-messages"`

The plugin manages the MCP connection — just call AD4M tools directly after waking.

### Deduplication

The waker tracks seen addresses per subscription and only wakes for **new** ones. After restart, previously seen addresses are restored from persisted state — no duplicate wakes.

## Wake Delivery

How wake events reach your agent session depends on the platform (Sovereign, OpenClaw, etc.). The current waker sends wake payloads to `wakeUrl` with `wakeToken`; platforms may auto-provision these values, but they must be present for delivery.

### OpenClaw Hooks Config

The plugin reads the hooks token from OpenClaw's global config (`hooks.token`). The `openclaw ad4m-setup` command includes `wakeToken` in the generated config snippet if hooks are enabled.

If you want to set one manually:

```json
{
  "hooks": {
    "enabled": true,
    "path": "/hooks",
    "token": "your-hooks-token"
  }
}
```
