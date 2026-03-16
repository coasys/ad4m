export interface McpResponse {
  jsonrpc: string;
  id: number;
  result?: any;
  error?: { code: number; message: string; data?: any };
}

export interface McpTool {
  name: string;
  description?: string;
  inputSchema?: Record<string, any>;
}

// WakerSubscription is defined in wakerSubscriptionManager.ts (kept import-free for testability)
export type { WakerSubscription } from "./wakerSubscriptionManager";

export interface PluginConfig {
  mode?: "managed" | "external";
  mcpEndpoint?: string;
  /** Auth token — JWT in external mode, admin credential in managed mode (internal). */
  token?: string;
  agentPassphrase?: string;
  ad4mBinaryPath?: string;
  toolRefreshIntervalMs?: number;
  wakerEnabled?: boolean;
  executorWsUrl?: string;
  wakeUrl?: string;
  wakeToken?: string;
  debounceMs?: number;
  /** RUST_LOG value for the ad4m-executor process (e.g. "holochain=debug,kitsune=trace"). Only applies when the plugin spawns the executor (managed mode). */
  rustLog?: string;
  /** Where to send executor logs: "file" (default) = ~/.ad4m/ad4m.log only, "openclaw" = openclaw logs only, "both" = both. */
  executorLogTarget?: "file" | "openclaw" | "both";
}

