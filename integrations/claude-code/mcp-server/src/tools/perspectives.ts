import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { gql, formatError } from "../services/gql.js";
import type { Perspective, PerspectiveWithNeighbourhood } from "../types.js";

export function registerPerspectiveTools(server: McpServer): void {
  server.registerTool(
    "ad4m_list_perspectives",
    {
      title: "List AD4M Perspectives",
      description: `List all Perspectives on the local AD4M executor, including joined Neighbourhoods.

A Perspective is a named semantic graph. Use this to discover existing graphs (e.g. "ClaudeMemory")
and retrieve their UUIDs for subsequent link operations.

Returns: array of { uuid, name, sharedUrl, state }`,
      inputSchema: z.object({}),
      annotations: { readOnlyHint: true, destructiveHint: false, idempotentHint: true, openWorldHint: false },
    },
    async () => {
      try {
        const data = await gql<{ perspectives: Perspective[] }>(
          "{ perspectives { uuid name sharedUrl state } }"
        );
        return { content: [{ type: "text", text: JSON.stringify(data.perspectives, null, 2) }] };
      } catch (e) {
        return { content: [{ type: "text", text: formatError(e) }] };
      }
    }
  );

  server.registerTool(
    "ad4m_create_perspective",
    {
      title: "Create AD4M Perspective",
      description: `Create a new named Perspective (semantic graph). Returns its UUID for subsequent link operations.

Tip: For Claude Code memory, create one called "ClaudeMemory" and save the UUID.

Args:
  - name (string): Human-readable name, e.g. "ClaudeMemory" or "ProjectNotes"

Returns: { uuid: string, name: string }`,
      inputSchema: z.object({
        name: z.string().min(1).max(100).describe('Perspective name, e.g. "ClaudeMemory"'),
      }),
      annotations: { readOnlyHint: false, destructiveHint: false, idempotentHint: false, openWorldHint: false },
    },
    async ({ name }) => {
      try {
        const data = await gql<{ perspectiveAdd: { uuid: string; name: string } }>(
          `mutation PerspectiveAdd($name: String!) { perspectiveAdd(name: $name) { uuid name } }`,
          { name }
        );
        return { content: [{ type: "text", text: JSON.stringify(data.perspectiveAdd, null, 2) }] };
      } catch (e) {
        return { content: [{ type: "text", text: formatError(e) }] };
      }
    }
  );

  server.registerTool(
    "ad4m_get_neighbourhood",
    {
      title: "Get AD4M Neighbourhood",
      description: `Read a shared AD4M Neighbourhood by UUID. Use to inspect semantic graphs shared with other agents or communities.

Args:
  - uuid (string): Perspective UUID

Returns: { uuid, name, sharedUrl, neighbourhood: { author, timestamp } }`,
      inputSchema: z.object({
        uuid: z.string().uuid().describe("Perspective UUID"),
      }),
      annotations: { readOnlyHint: true, destructiveHint: false, idempotentHint: true, openWorldHint: true },
    },
    async ({ uuid }) => {
      try {
        const data = await gql<{ perspective: PerspectiveWithNeighbourhood }>(
          `query Perspective($uuid: String!) {
             perspective(uuid: $uuid) {
               uuid name sharedUrl neighbourhood { author timestamp }
             }
           }`,
          { uuid }
        );
        return { content: [{ type: "text", text: JSON.stringify(data.perspective, null, 2) }] };
      } catch (e) {
        return { content: [{ type: "text", text: formatError(e) }] };
      }
    }
  );
}
