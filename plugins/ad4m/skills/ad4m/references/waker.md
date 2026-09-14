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
| `ad4m_subscribe_to_mentions(perspective_id)`                         | Watch for any item whose text carries your name or DID              |
| `ad4m_subscribe_to_children(perspective_id, expression_address)`     | Watch for new children under a parent (messages in a channel, tasks on a board, …) |
| `ad4m_unsubscribe_from_mentions(perspective_id)`                     | Stop watching mentions in a neighbourhood                           |
| `ad4m_unsubscribe_from_children(perspective_id, expression_address)` | Stop watching that parent                                           |
| `ad4m_list_waker_subscriptions()`                                    | List all active subscriptions                                       |

The subscribe tools call the MCP tools `ad4m_get_mention_waker_config` / `ad4m_generate_waker_query` internally to build the SPARQL queries — you don't need to construct queries manually.

## Wake Message Format

**Use `/hooks/wake` (recommended).** It enqueues the event into the main agent session which has your skills loaded. Do NOT use `/hooks/agent` — that spawns an isolated sub-agent without your skills.

### Mention events

For mention subscriptions, the wake message includes per-item details with resolved parents:

```json
{
  "text": "You were @mentioned in an AD4M neighbourhood.\nRead the AD4M skill for instructions on how to handle this.\n\nAgent DID: did:key:z6Mk...\nPerspective: cda8c4fc-...\nSubscription: mention-abc\nEvent type: mention\n\nMentioned items (1):\n  Item: literal:string:msg-123\n  Parents: literal:string:channel-1, literal:string:conv-thread-5",
  "mode": "now"
}
```

The `Mentioned items` section lists each item that triggered the wake:
- **Item** — the base address of the instance whose property text carried your name or DID
- **Parents** — every node this item hangs under via `ad4m://has_child`

**The item is not necessarily a message.** The subscription is a SPARQL query over links, not over a `Message` class: it matches any link whose literal target contains one of your profile names or your DID, excluding `ad4m://ontology/*` proof metadata. If the space declares a `Message` class with a `body` property, the query is scoped to that one predicate for speed; in a space with a user-defined ontology and no such class it scans every other link. So the address you are handed is the source of whichever link carried your name — a task, a proposal, a comment, a field of a class this space invented. Treat "message" as one possible answer, never the assumption.

An item can have multiple parents when an app derives further containers from it (auto-generated summary threads, for example).

### Reading the context around a mention

The wake event gives you two addresses and no types. Work out the rest from the space's own ontology, cheapest step first, and stop as soon as you know enough to act:

1. **`ad4m_describe_perspective(perspective_id)`** — the classes this space actually has, their properties and collections. This is the vocabulary for everything below; nothing else tells you what a "task" or a "proposal" means here.
2. **Type the item.** `ad4m_query_links(perspective_id, source=<Item>)` returns its links, including its `rdf://type` marker and its property predicates — match those against the classes from step 1. Then `ad4m_instance_get(class_name=<that class>, base_uri=<Item>)` for the resolved property values.
3. **One step of outer context via the parent.** Type the parent the same way (`query_links(source=<Parent>)`), then read its other children: `ad4m_instance_transcript(class_name=…, parent=<Parent>)` when they are one text-bearing class, `ad4m_instance_query(class_name=…, parent=<Parent>)` for full property maps, `ad4m_get_children(parent=<Parent>)` when you don't yet know which classes are down there. Sibling instances are usually what makes the mention intelligible — the thread it sits in, the other items on the board.
4. **Only if that is still not enough, walk further.** Up: `ad4m_query_links(target=<Parent>, predicate="ad4m://has_child")` gives the parent's own parents. Down: `get_children` on a sibling that looks like a container. Sideways: the collections the item's class declares, resolved by `instance_get`. Each hop costs a call — take it when the ask is genuinely unclear, not by default.
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
