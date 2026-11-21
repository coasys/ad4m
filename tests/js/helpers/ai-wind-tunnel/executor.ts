/**
 * The executor every measured call goes through: a fresh data directory, free ports, no
 * Holochain, an admin credential. Apps reach AI through it as the run does: the SDK's
 * `ai.*` RPCs and the `/v1` OpenAI-compatible API, both authorised by the credential.
 */

import { execFileSync } from "node:child_process";
import { randomUUID } from "node:crypto";
import { mkdtempSync, rmSync, statSync } from "node:fs";
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

function gitHead(): { commit: string; committedMs: number } | null {
  try {
    const [commit, secs] = execFileSync("git", ["log", "-1", "--format=%H %ct"], { cwd: REPO_ROOT, encoding: "utf8" }).trim().split(" ");
    return { commit, committedMs: Number(secs) * 1000 };
  } catch {
    return null;
  }
}

/**
 * The commit the executor binary was built from: AI_WT_EXECUTOR_COMMIT, else this checkout's
 * HEAD when the binary is this checkout's own build and was built after HEAD was committed
 * (an older binary was built from an older commit: the checkout moved on without a rebuild),
 * else unknown (a binary does not say).
 */
export function executorCommit(
  binary: string,
  explicit = process.env.AI_WT_EXECUTOR_COMMIT,
  head: () => { commit: string; committedMs: number } | null = gitHead,
  builtMs: (file: string) => number = (f) => statSync(f).mtimeMs,
): string {
  if (explicit) return explicit;
  if (!path.resolve(binary).startsWith(path.join(REPO_ROOT, "target"))) return "unknown";
  const h = head();
  if (!h) return "unknown";
  try {
    return builtMs(binary) >= h.committedMs ? h.commit : "unknown";
  } catch {
    return "unknown";
  }
}

export async function startExecutor(log: (s: string) => void): Promise<WtExecutor> {
  const binary = executorBinary();
  const apiPort = await freePort();
  const token = `wind-tunnel-${randomUUID()}`;
  const dataPath = mkdtempSync(path.join(os.tmpdir(), "ai-wt-executor-"));
  const removeData = () => rmSync(dataPath, { recursive: true, force: true });
  let proc: Awaited<ReturnType<typeof spawnExecutor>> | undefined;
  let client: Ad4mClient | undefined;
  try {
    execFileSync(binary, ["init", "--data-path", dataPath, "--network-bootstrap-seed", BOOTSTRAP_SEED], { stdio: "ignore" });
    // The executor seeds endpoints from AD4M_AI_* variables: none may leak in from the shell.
    const env: Record<string, string> = {};
    for (const k of Object.keys(process.env)) if (k.startsWith("AD4M_AI_")) env[k] = "";
    proc = await spawnExecutor(
      ["run", "--app-data-path", dataPath, "--port", String(apiPort), "--language-language-only", "false", "--run-dapp-server", "false", "--run-holochain", "false", "--admin-credential", token],
      apiPort,
      { env },
    );
    client = new Ad4mClient(baseUrl(apiPort), token);
    await client.agent.generate("wind-tunnel");
  } catch (e) {
    // Nothing of a failed start stays behind: no executor, no data directory.
    try {
      client?.close();
    } catch {}
    if (proc) await quitExecutor(proc, apiPort, token).catch(() => {});
    removeData();
    throw e;
  }
  const started = proc;
  const ad4m = client;
  log(`executor ${binary} on port ${apiPort}, data ${dataPath}`);

  const endpoints = new Map<string, AiEndpoint>();
  const models = new Map<string, string>();
  return {
    client: ad4m,
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
    pid: started.pid!,
    async endpoint(name, kind, url) {
      const key = `${kind}|${url}`;
      const known = endpoints.get(key);
      if (known) return known;
      const ep = await ad4m.ai.addEndpoint({ name, kind, baseUrl: url, requestTimeoutSecs: 900 });
      endpoints.set(key, ep);
      return ep;
    },
    async model(name, type, endpointId, remoteModel) {
      const key = `${type}|${endpointId}|${remoteModel}`;
      const known = models.get(key);
      if (known) return known;
      const id = await ad4m.ai.addModel({ name, modelType: type, endpointId, remoteModel });
      models.set(key, id);
      return id;
    },
    async stop() {
      try {
        ad4m.close();
      } catch {}
      await quitExecutor(started, apiPort, token);
      // Each run's executor state is its own and is not read again.
      removeData();
    },
  };
}
