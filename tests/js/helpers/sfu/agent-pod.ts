/**
 * Docker helpers for pod-managed agent-harness scenarios (A-series).
 *
 * Start/stop a hardened, isolated AD4M executor container and wait for its MCP
 * endpoint. Host-isolated by construction: no host mounts, cap-drop + only the
 * startup caps the image needs, no-new-privileges, resource limits,
 * loopback-only ports, Holochain off by default.
 */
import { execFileSync } from "child_process";

export interface ExecutorContainerOpts {
  name: string;
  hostMcpPort: number;
  hostWsPort?: number;
  adminCredential: string;
  agentPassphrase?: string;
  image?: string;
  network?: string;
  runHolochain?: boolean;
}

export function stopContainer(name: string): void {
  try {
    execFileSync("docker", ["rm", "-f", name], { stdio: "pipe" });
  } catch {
    /* not running */
  }
}

export function startExecutorContainer(o: ExecutorContainerOpts): void {
  const args = ["run", "-d", "--name", o.name];
  if (o.network) args.push("--network", o.network);
  args.push(
    "-e", `ADMIN_CREDENTIAL=${o.adminCredential}`,
    "-e", "ENABLE_MULTI_USER=true",
    "-e", "ENABLE_MCP=true",
    "-e", "MCP_PORT=3001",
    "-e", `RUN_HOLOCHAIN=${o.runHolochain === true ? "true" : "false"}`,
  );
  if (o.agentPassphrase) args.push("-e", `AGENT_PASSPHRASE=${o.agentPassphrase}`);
  args.push("-p", `127.0.0.1:${o.hostMcpPort}:3001`);
  if (o.hostWsPort) args.push("-p", `127.0.0.1:${o.hostWsPort}:12000`);
  args.push(
    "--cap-drop", "ALL",
    "--cap-add", "CHOWN", "--cap-add", "SETUID", "--cap-add", "SETGID",
    "--cap-add", "DAC_OVERRIDE", "--cap-add", "FOWNER",
    "--security-opt", "no-new-privileges:true",
    "--memory", "2g", "--cpus", "1.5", "--pids-limit", "512",
    o.image ?? "ad4m-test:latest",
  );
  execFileSync("docker", args, { stdio: "pipe" });
}

export async function waitForMcpReady(
  baseUrl: string,
  adminCredential: string,
  timeoutMs = 120000,
): Promise<void> {
  const deadline = Date.now() + timeoutMs;
  const body = JSON.stringify({
    jsonrpc: "2.0",
    id: 1,
    method: "initialize",
    params: { protocolVersion: "2024-11-05", capabilities: {}, clientInfo: { name: "wt", version: "1" } },
  });
  while (Date.now() < deadline) {
    try {
      const res = await fetch(`${baseUrl}/mcp`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Accept: "application/json, text/event-stream",
          Authorization: `Bearer ${adminCredential}`,
        },
        body,
      });
      if (res.status === 200) {
        try {
          await res.body?.cancel();
        } catch {
          /* ignore */
        }
        return;
      }
    } catch {
      /* not ready yet */
    }
    await new Promise((r) => setTimeout(r, 2000));
  }
  throw new Error(`MCP not ready at ${baseUrl} within ${timeoutMs}ms`);
}
