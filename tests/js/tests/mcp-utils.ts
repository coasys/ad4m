/**
 * Shared MCP HTTP test utilities.
 *
 * All functions take `mcpBaseUrl` as the first parameter so each test suite
 * can point at its own executor instance / port.
 */

import fetch from 'node-fetch';

// ============================================================================
// Types
// ============================================================================

export interface McpResponse {
    jsonrpc: string;
    id: number;
    result?: any;
    error?: { code: number; message: string; data?: any };
}

// ============================================================================
// SSE stream parser
// ============================================================================

/**
 * Parse an SSE stream response, extracting the first JSON-RPC message.
 * SSE streams from MCP Streamable HTTP start with a priming event (empty data),
 * followed by the actual response data event.
 */
export async function parseSSEStream(response: any): Promise<McpResponse> {
    return new Promise(function(resolve, reject) {
        var buffer = '';
        var resolved = false;
        var timeout = setTimeout(function() {
            if (!resolved) {
                resolved = true;
                reject(new Error('SSE stream timeout — no JSON data received within 30s. Buffer: ' + buffer));
            }
        }, 30000);

        var body = response.body;
        if (!body) {
            clearTimeout(timeout);
            reject(new Error('No response body'));
            return;
        }

        body.on('data', function(chunk: Buffer) {
            buffer += chunk.toString();
            var lines = buffer.split('\n');
            for (var i = 0; i < lines.length - 1; i++) {
                var line = lines[i].trim();
                if (line.indexOf('data:') === 0) {
                    var payload = line.substring(5).trim();
                    if (payload.length > 0 && !resolved) {
                        try {
                            var parsed = JSON.parse(payload);
                            if (parsed.jsonrpc) {
                                resolved = true;
                                clearTimeout(timeout);
                                resolve(parsed as McpResponse);
                                body.destroy();
                                return;
                            }
                        } catch (e) {
                            // Not valid JSON, continue
                        }
                    }
                }
            }
            buffer = lines[lines.length - 1];
        });

        body.on('end', function() {
            if (!resolved) {
                resolved = true;
                clearTimeout(timeout);
                var lines = buffer.split('\n');
                for (var i = 0; i < lines.length; i++) {
                    var line = lines[i].trim();
                    if (line.indexOf('data:') === 0) {
                        var payload = line.substring(5).trim();
                        if (payload.length > 0) {
                            try {
                                resolve(JSON.parse(payload) as McpResponse);
                                return;
                            } catch (e) { /* skip */ }
                        }
                    }
                }
                reject(new Error('SSE stream ended without JSON data'));
            }
        });

        body.on('error', function(err: Error) {
            if (!resolved) {
                resolved = true;
                clearTimeout(timeout);
                reject(err);
            }
        });
    });
}

// ============================================================================
// MCP HTTP request helpers
// ============================================================================

let requestIdCounter = 0;

/**
 * Send an MCP JSON-RPC request via HTTP.
 * Handles SSE responses from Streamable HTTP transport.
 */
export async function mcpHttpRequest(
    mcpBaseUrl: string,
    method: string,
    params: any = {},
    sessionId?: string
): Promise<McpResponse> {
    const id = ++requestIdCounter;
    const headers: Record<string, string> = {
        'Content-Type': 'application/json',
        'Accept': 'application/json, text/event-stream'
    };
    if (sessionId) {
        headers['Mcp-Session-Id'] = sessionId;
    }

    const response = await fetch(mcpBaseUrl, {
        method: 'POST',
        headers,
        body: JSON.stringify({ jsonrpc: "2.0", id, method, params })
    });

    if (!response.ok) {
        throw new Error('HTTP error: ' + response.status + ' ' + response.statusText);
    }

    const ct = response.headers.get('content-type') || '';
    if (ct.indexOf('text/event-stream') >= 0) {
        return await parseSSEStream(response);
    }

    return await response.json() as McpResponse;
}

/**
 * Call an MCP tool and return the parsed result.
 */
export async function callMcpTool(
    mcpBaseUrl: string,
    toolName: string,
    args: Record<string, any>,
    sessionId?: string
): Promise<any> {
    const response = await mcpHttpRequest(mcpBaseUrl, "tools/call", {
        name: toolName,
        arguments: args
    }, sessionId);

    if (response.error) {
        throw new Error('MCP tool error [' + toolName + ']: ' + response.error.message);
    }

    const content = response.result?.content;
    if (content?.[0]?.text) {
        try {
            return JSON.parse(content[0].text);
        } catch (e) {
            return content[0].text;
        }
    }
    return response.result;
}

/**
 * List all available MCP tools.
 */
export async function listMcpTools(
    mcpBaseUrl: string,
    sessionId?: string
): Promise<any[]> {
    const response = await mcpHttpRequest(mcpBaseUrl, "tools/list", {}, sessionId);
    return response.result?.tools ?? [];
}

/**
 * Initialize an MCP session. Returns session ID and server info.
 */
export async function initializeMcp(
    mcpBaseUrl: string,
    clientName: string = "ad4m-test-client"
): Promise<{ sessionId: string; serverInfo: any }> {
    const id = ++requestIdCounter;
    const resp = await fetch(mcpBaseUrl, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Accept': 'application/json, text/event-stream'
        },
        body: JSON.stringify({
            jsonrpc: "2.0",
            id,
            method: "initialize",
            params: {
                protocolVersion: "2024-11-05",
                capabilities: { roots: { listChanged: false } },
                clientInfo: { name: clientName, version: "1.0.0" }
            }
        })
    });

    if (!resp.ok) {
        throw new Error('MCP initialize HTTP error: ' + resp.status);
    }

    const sid = resp.headers.get('mcp-session-id') || "test-session";
    const result = await parseSSEStream(resp);

    if (result.error) {
        throw new Error('MCP initialize error: ' + result.error.message);
    }

    // Send notifications/initialized to complete the MCP handshake
    await fetch(mcpBaseUrl, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Accept': 'application/json, text/event-stream',
            'Mcp-Session-Id': sid
        },
        body: JSON.stringify({ jsonrpc: "2.0", method: "notifications/initialized" })
    });

    return { sessionId: sid, serverInfo: result.result };
}
