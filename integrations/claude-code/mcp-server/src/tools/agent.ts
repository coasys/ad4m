import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { gql, formatError } from "../services/gql.js";
import type { AgentStatus } from "../types.js";

export function registerAgentTools(server: McpServer): void {
  server.registerTool(
    "ad4m_agent_status",
    {
      title: "AD4M Agent Status",
      description: `Get the local AD4M agent status: DID, initialization state, and keystore lock state.

Use this first to confirm the executor is reachable and the agent is unlocked before any read/write operations.

Returns:
  {
    "isInitialized": boolean,  // true if agentGenerate has been run
    "isUnlocked":    boolean,  // true if keystore is unlocked (required for writes)
    "did":           string    // agent DID, e.g. "did:key:z6Mk..."
  }

If isUnlocked is false, writes will fail. Unlock via:
  curl -X POST http://localhost:4000/graphql -d '{"query":"mutation { agentUnlock(passphrase:\\"PASS\\", holochain:true){isUnlocked}}"}'`,
      inputSchema: z.object({}),
      annotations: { readOnlyHint: true, destructiveHint: false, idempotentHint: true, openWorldHint: false },
    },
    async () => {
      try {
        const data = await gql<{ agentStatus: AgentStatus }>(
          "{ agentStatus { isInitialized isUnlocked did } }"
        );
        return { content: [{ type: "text", text: JSON.stringify(data.agentStatus, null, 2) }] };
      } catch (e) {
        return { content: [{ type: "text", text: formatError(e) }] };
      }
    }
  );
}
