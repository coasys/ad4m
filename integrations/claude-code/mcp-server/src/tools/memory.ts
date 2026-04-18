import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { gql, formatError } from "../services/gql.js";
import type { LinkExpression } from "../types.js";

const CHARACTER_LIMIT = 25_000;

const WRITE_MUTATION = `
  mutation PerspectiveAddLink($uuid: String!, $link: LinkInput!) {
    perspectiveAddLink(uuid: $uuid, link: $link) {
      author timestamp data { source predicate target }
    }
  }`;

const REMOVE_MUTATION = `
  mutation PerspectiveRemoveLink($uuid: String!, $link: LinkExpressionInput!) {
    perspectiveRemoveLink(uuid: $uuid, link: $link)
  }`;

const QUERY_LINKS = `
  query PerspectiveQueryLinks($uuid: String!, $query: LinkQuery!) {
    perspectiveQueryLinks(uuid: $uuid, query: $query) {
      author timestamp data { source predicate target }
    }
  }`;

export function registerMemoryTools(server: McpServer): void {
  server.registerTool(
    "ad4m_write_memory",
    {
      title: "Write AD4M Memory",
      description: `Write a signed LinkExpression (source → predicate → target) to a Perspective.

Use for storing semantic memories, agent facts, project state, and cross-session context.

URI conventions:
  - source:    "memory://feedback/mobile-first"  or  "agent://session/2026-04-18"
  - predicate: "ad4m://has-content"  |  "ad4m://has-name"  |  "ad4m://relates"
  - target:    "literal://Your content here"

Args:
  - perspective_uuid (string): Target Perspective UUID (e.g. ClaudeMemory UUID)
  - source (string):    Source URI
  - predicate (string): Predicate URI (default: "ad4m://relates")
  - target (string):    Target URI or literal content

Returns: { author, timestamp, data: { source, predicate, target } }`,
      inputSchema: z.object({
        perspective_uuid: z.string().uuid().describe("Target Perspective UUID"),
        source:    z.string().min(1).describe('Source URI, e.g. "memory://feedback/mobile-first"'),
        predicate: z.string().min(1).default("ad4m://relates").describe('Predicate URI, e.g. "ad4m://has-content"'),
        target:    z.string().min(1).describe('Target URI or literal, e.g. "literal://content here"'),
      }),
      annotations: { readOnlyHint: false, destructiveHint: false, idempotentHint: false, openWorldHint: false },
    },
    async ({ perspective_uuid, source, predicate, target }) => {
      try {
        const data = await gql<{ perspectiveAddLink: LinkExpression }>(
          WRITE_MUTATION,
          { uuid: perspective_uuid, link: { source, predicate, target } }
        );
        return { content: [{ type: "text", text: JSON.stringify(data.perspectiveAddLink, null, 2) }] };
      } catch (e) {
        return { content: [{ type: "text", text: formatError(e) }] };
      }
    }
  );

  server.registerTool(
    "ad4m_recall",
    {
      title: "Recall AD4M Memory",
      description: `Query links from a Perspective by source, predicate, or target. Omit any field to match all.

Returns author DID + timestamp for each match, allowing you to verify who wrote a memory and when.

Args:
  - perspective_uuid (string): Perspective UUID to query
  - source (string, optional):    Filter by source URI
  - predicate (string, optional): Filter by predicate URI
  - target (string, optional):    Filter by target URI

Common recall patterns:
  - All memories:         omit source/predicate/target
  - By type:              predicate="ad4m://has-content", source="memory://feedback/*"
  - Specific memory:      source="memory://feedback/mobile-first"
  - Everything about X:  source="memory://project/zuafrique"

Returns: array of { author, timestamp, data: { source, predicate, target } }`,
      inputSchema: z.object({
        perspective_uuid: z.string().uuid().describe("Perspective UUID to query"),
        source:    z.string().optional().describe("Filter by source URI (optional)"),
        predicate: z.string().optional().describe("Filter by predicate URI (optional)"),
        target:    z.string().optional().describe("Filter by target URI (optional)"),
      }),
      annotations: { readOnlyHint: true, destructiveHint: false, idempotentHint: true, openWorldHint: false },
    },
    async ({ perspective_uuid, source, predicate, target }) => {
      try {
        const query: Record<string, string> = {};
        if (source)    query["source"]    = source;
        if (predicate) query["predicate"] = predicate;
        if (target)    query["target"]    = target;

        const data = await gql<{ perspectiveQueryLinks: LinkExpression[] }>(
          QUERY_LINKS,
          { uuid: perspective_uuid, query }
        );

        const links = data.perspectiveQueryLinks;
        let text = JSON.stringify(links, null, 2);

        if (text.length > CHARACTER_LIMIT) {
          const truncated = links.slice(0, Math.max(1, Math.floor(links.length / 2)));
          text = JSON.stringify({
            truncated: true,
            shown: truncated.length,
            total: links.length,
            truncation_message: `Response truncated to ${truncated.length}/${links.length} links. Use source/predicate/target filters to narrow results.`,
            links: truncated,
          }, null, 2);
        }

        return { content: [{ type: "text", text }] };
      } catch (e) {
        return { content: [{ type: "text", text: formatError(e) }] };
      }
    }
  );

  server.registerTool(
    "ad4m_delete_memory",
    {
      title: "Delete AD4M Memory",
      description: `Remove a LinkExpression from a Perspective. Use to delete outdated or incorrect memories.

You must provide the exact link data (source, predicate, target) to identify what to remove.
Use ad4m_recall first to find the exact values.

Args:
  - perspective_uuid (string): Perspective UUID
  - source (string):    Exact source URI of the link to delete
  - predicate (string): Exact predicate URI of the link to delete
  - target (string):    Exact target URI of the link to delete

Returns: boolean (true if removed)`,
      inputSchema: z.object({
        perspective_uuid: z.string().uuid().describe("Perspective UUID"),
        source:    z.string().min(1).describe("Exact source URI of the link to delete"),
        predicate: z.string().min(1).describe("Exact predicate URI of the link to delete"),
        target:    z.string().min(1).describe("Exact target URI of the link to delete"),
      }),
      annotations: { readOnlyHint: false, destructiveHint: true, idempotentHint: true, openWorldHint: false },
    },
    async ({ perspective_uuid, source, predicate, target }) => {
      try {
        const data = await gql<{ perspectiveRemoveLink: boolean }>(
          REMOVE_MUTATION,
          { uuid: perspective_uuid, link: { data: { source, predicate, target } } }
        );
        return { content: [{ type: "text", text: JSON.stringify({ removed: data.perspectiveRemoveLink }, null, 2) }] };
      } catch (e) {
        return { content: [{ type: "text", text: formatError(e) }] };
      }
    }
  );
}
