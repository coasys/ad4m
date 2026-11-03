/**
 * The executor every measured call goes through: a fresh data directory, free ports, no
 * Holochain, an admin credential. Apps reach AI through it as the run does: the SDK's
 * `ai.*` RPCs and the `/v1` OpenAI-compatible API, both authorised by the credential.
 */

import { execFileSync } from "node:child_process";
import { randomUUID } from "node:crypto";
import { mkdtempSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { Ad4mClient } from "@coasys/ad4m";
import type { AiEndpoint, ModelType } from "@coasys/ad4m";
import { baseUrl, executorBinary, quitExecutor, spawnExecutor } from "../../utils/utils.js";
import { BOOTSTRAP_SEED, REPO_ROOT } from "./paths.js";
import { freePort } from "./procs.js";
import type { EndpointKindId } from "./types.js";

export interface WtExecutor {
  client: Ad4mClient;
  apiPort: number;
  token: string;
  /** `http://127.0.0.1:<port>/v1`. */
  v1: string;
  binary: string;
  /** The commit the binary was built from, or "unknown". */
  commit: string;
  /** The commit of the wind tunnel that ran. */
  harnessCommit: string;
  version?: string;
  dataPath: string;
  pid: number;
  endpoint(name: string, kind: EndpointKindId, url: string): Promise<AiEndpoint>;
  model(name: string, type: ModelType, endpointId: string, remoteModel: string): Promise<string>;
  stop(): Promise<void>;
}

function gitCommit(): string | null {
  try {
    return execFileSync("git", ["rev-parse", "HEAD"], { cwd: REPO_ROOT, encoding: "utf8" }).trim();
  } catch {
    return null;
  }
}

/**
 * The commit the executor binary was built from: AI_WT_EXECUTOR_COMMIT, else this checkout's
 * HEAD when the binary is this checkout's own build, else unknown (a binary does not say).
 */
function executorCommit(binary: string): string {
  if (process.env.AI_WT_EXECUTOR_COMMIT) return process.env.AI_WT_EXECUTOR_COMMIT;
  if (path.resolve(binary).startsWith(path.join(REPO_ROOT, "target"))) return gitCommit() ?? "unknown";
  return "unknown";
}

export async function startExecutor(log: (s: string) => void): Promise<WtExecutor> {
  const binary = executorBinary();
  const apiPort = await freePort();
  const token = `wind-tunnel-${randomUUID()}`;
  const dataPath = mkdtempSync(path.join(os.tmpdir(), "ai-wt-executor-"));
  execFileSync(binary, ["init", "--data-path", dataPath, "--network-bootstrap-seed", BOOTSTRAP_SEED], { stdio: "ignore" });
  // The executor seeds endpoints from AD4M_AI_* variables: none may leak in from the shell.
  const env: Record<string, string> = {};
  for (const k of Object.keys(process.env)) if (k.startsWith("AD4M_AI_")) env[k] = "";
  const proc = await spawnExecutor(
    ["run", "--app-data-path", dataPath, "--port", String(apiPort), "--language-language-only", "false", "--run-dapp-server", "false", "--run-holochain", "false", "--admin-credential", token],
    apiPort,
    { env },
  );
  const client = new Ad4mClient(baseUrl(apiPort), token);
  await client.agent.generate("wind-tunnel");
  log(`executor ${binary} on port ${apiPort}, data ${dataPath}`);

  const endpoints = new Map<string, AiEndpoint>();
  const models = new Map<string, string>();
  return {
    client,
    apiPort,
    token,
    v1: `http://127.0.0.1:${apiPort}/v1`,
    binary,
    commit: executorCommit(binary),
    harnessCommit: gitCommit() ?? "unknown",
    version: (() => {
      try {
        return execFileSync(binary, ["--version"], { encoding: "utf8" }).trim();
      } catch {
        return undefined;
      }
    })(),
    dataPath,
    pid: proc.pid!,
    async endpoint(name, kind, url) {
      const key = `${kind}|${url}`;
      const known = endpoints.get(key);
      if (known) return known;
      const ep = await client.ai.addEndpoint({ name, kind, baseUrl: url, requestTimeoutSecs: 900 });
      endpoints.set(key, ep);
      return ep;
    },
    async model(name, type, endpointId, remoteModel) {
      const key = `${type}|${endpointId}|${remoteModel}`;
      const known = models.get(key);
      if (known) return known;
      const id = await client.ai.addModel({ name, modelType: type, endpointId, remoteModel });
      models.set(key, id);
      return id;
    },
    async stop() {
      try {
        client.close();
      } catch {}
      await quitExecutor(proc, apiPort, token);
    },
  };
}
