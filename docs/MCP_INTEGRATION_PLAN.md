# AD4M MCP Integration Plan

**Status:** Planning  
**Author:** Data  
**Date:** 2026-02-17  
**Depends on:** PR #654 (SHACL migration) - James working on final bits

## Overview

Enable AI agents to interact with AD4M at the **subject class level** (Messages, Channels, Conversations) rather than raw links, with real-time event waking.

## Architecture: Option A (MCP + External Wake Bridge)

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   AI Agent      │────▶│  AD4M Executor   │────▶│  AD4M Data      │
│  (OpenClaw)     │ MCP │  (MCP Server)    │     │  (Perspectives) │
└─────────────────┘     └──────────────────┘     └─────────────────┘
        ▲                        │
        │ wake                   │ GraphQL
        │                        ▼
┌─────────────────┐     ┌──────────────────┐
│  OpenClaw       │◀────│  Wake Bridge     │
│  Cron/Wake API  │ POST│  (Subscriptions) │
└─────────────────┘     └──────────────────┘
```

**Why this approach:**
- MCP stays stateless and simple (tools only)
- Leverages existing GraphQL subscription infrastructure
- Wake bridge is lightweight, can be shared across agents
- Easy to add more event sources later

## Phase 1: MCP Core Tools (Current - PR #665)

**Status:** Mostly done, needs login flow

### Tools Available
- `list_perspectives` - List all perspectives
- `get_perspective` - Get perspective details
- `query_links` - Query raw links
- `add_link` - Add a link
- `run_prolog` - Run Prolog queries (will become run_surreal)
- `create_perspective` - Create new perspective

### TODO
- [ ] **Login flow** - Support both:
  - Local executor (capability token)
  - Multi-user email login (JWT)
- [ ] Test with Claude Desktop / OpenClaw

## Phase 2: Subject Class Tools (After SHACL lands)

**Depends on:** PR #654 merged

### New Tools
- `list_subject_classes` - List available models (Message, Channel, Task, etc.)
- `get_subject_class_schema` - Get SHACL schema for a model
- `query_subjects` - Query instances of a subject class
- `get_subject` - Get single subject with all properties
- `create_subject` - Create new instance
- `update_subject` - Update properties
- `delete_subject` - Remove instance
- `execute_action` - Run SHACL Flow actions (state transitions)

### Implementation Notes
- Use `perspective_instance.rs` subject class API
- SHACL schemas are now queryable as links (not opaque Prolog)
- Can introspect property types, constraints, actions dynamically

## Phase 3: Wake Bridge Daemon

**Purpose:** Subscribe to AD4M events, wake AI agents

### Design
```rust
// ad4m-wake-bridge (separate binary or integrated)
struct WakeBridge {
    ad4m_client: Ad4mClient,
    openclaw_endpoint: String,
    subscriptions: Vec<SubscriptionConfig>,
}

struct SubscriptionConfig {
    perspective_id: String,
    query: SubjectClassQuery,  // e.g., "all messages in channel X"
    wake_target: String,       // OpenClaw session/agent
    context_template: String,  // What to include in wake payload
}
```

### Wake Payload
```json
{
  "event": "new_message",
  "perspective": "Qm...",
  "subject_class": "Message",
  "instance_id": "Qm...",
  "summary": "New message from @alice in #general",
  "timestamp": "2026-02-17T19:00:00Z"
}
```

### Integration Points
- OpenClaw cron wake API: `POST /wake` with system event
- Or: Direct session injection via gateway

### Configuration
```yaml
# wake-bridge.yaml
subscriptions:
  - name: "flux-messages"
    perspective: "Qm..."
    query:
      subjectClass: "Message"
      channel: "general"
    wake:
      target: "data-agent"
      template: "New message from {{author}} in {{channel}}: {{preview}}"
```

## Phase 4: Convenience Tools

### High-Level Operations
- `send_message` - Compose and send to channel
- `reply_to` - Reply in thread context
- `search_conversations` - Semantic search across messages
- `summarize_channel` - AI summarization of recent activity

### AI-Specific
- `get_context` - Fetch relevant context for responding
- `remember` - Store information in agent's perspective
- `schedule_action` - Create reminder/scheduled task

## Security Considerations

1. **Capability scoping** - MCP tools should respect perspective permissions
2. **Rate limiting** - Prevent runaway agents from spamming
3. **Audit logging** - Track AI agent actions for review
4. **Input validation** - Sanitize all tool inputs (prompt injection defense)

## Timeline

| Phase | Dependency | Estimate |
|-------|------------|----------|
| Phase 1 (login) | None | 1-2 days |
| Phase 2 (subjects) | SHACL PR #654 | 3-5 days |
| Phase 3 (wake bridge) | Phase 2 | 2-3 days |
| Phase 4 (convenience) | Phase 3 | Ongoing |

## Open Questions

1. **Wake bridge deployment** - Sidecar per agent? Shared service?
2. **Subscription persistence** - Store in AD4M perspective or external config?
3. **Multi-agent coordination** - How do agents share subscriptions?
4. **Error handling** - What if agent is unreachable when event fires?

## References

- PR #665: Current MCP server implementation
- PR #654: SHACL migration (subject class introspection)
- `rust-executor/src/perspectives/perspective_instance.rs` - Subject class API
- `rust-executor/src/mcp/` - MCP server code
- MCP Spec: https://modelcontextprotocol.io
