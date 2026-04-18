#!/usr/bin/env node
/**
 * ad4m-mcp-server — MCP server for the AD4M decentralized social network.
 *
 * Gives Claude Code agents read/write access to the AD4M semantic memory graph:
 * Perspectives, LinkExpressions, and Neighbourhoods.
 *
 * Setup: https://github.com/coasys/ad4m/tree/main/integrations/claude-code
 *
 * Required: AD4M executor running locally (default: http://localhost:4000/graphql)
 * Override: AD4M_GQL_URL env var
 */

import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";

import { registerAgentTools }       from "./tools/agent.js";
import { registerPerspectiveTools } from "./tools/perspectives.js";
import { registerMemoryTools }      from "./tools/memory.js";

const server = new McpServer({
  name: "ad4m-mcp-server",
  version: "1.0.0",
});

registerAgentTools(server);
registerPerspectiveTools(server);
registerMemoryTools(server);

const transport = new StdioServerTransport();
await server.connect(transport);
