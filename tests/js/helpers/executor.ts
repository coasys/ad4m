import { ChildProcess } from "node:child_process";
import { Ad4mClient } from "@coasys/ad4m";
import { startExecutor, apolloClient } from "../utils/utils.js";
import { getFreePorts } from "./ports.js";
import path from "path";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const TEST_DIR = path.join(__dirname, "..", "tst-tmp");
const BOOTSTRAP_SEED = path.join(__dirname, "..", "bootstrapSeed.json");

export type AgentHandle = {
  /** Connected Ad4mClient ready for use */
  client: Ad4mClient;
  /** gqlPort — useful if a second client needs to connect to the same executor */
  gqlPort: number;
  /** Kills the executor process; safe to call multiple times */
  stop(): Promise<void>;
};

/**
 * Starts a fresh executor instance for a named agent and returns an
 * AgentHandle.  Ports are allocated dynamically — no hardcoded numbers.
 *
 * Typical use:
 *
 *   let agent: AgentHandle;
 *   before(async () => { agent = await startAgent('my-test'); });
 *   after(async () => { await agent.stop(); });
 */
export async function startAgent(
  agentName: string,
  opts: {
    passphrase?: string;
    bootstrapSeedPath?: string;
    /** When set, starts the executor in admin-credential mode and connects
     *  the returned client using that credential as the bearer token. */
    adminCredential?: string;
  } = {},
): Promise<AgentHandle> {
  const [gqlPort, hcAdminPort, hcAppPort] = await getFreePorts(3);

  const appDataPath = path.join(TEST_DIR, "agents", agentName);
  const bootstrapSeedPath = opts.bootstrapSeedPath ?? BOOTSTRAP_SEED;

  const executorProcess = await startExecutor(
    appDataPath,
    bootstrapSeedPath,
    gqlPort,
    hcAdminPort,
    hcAppPort,
    false,
    opts.adminCredential,
  );

  const client = new Ad4mClient(apolloClient(gqlPort, opts.adminCredential));
  await client.agent.generate(opts.passphrase ?? "test-passphrase");

  async function stop(): Promise<void> {
    if (!executorProcess) return;
    while (!executorProcess.killed) {
      executorProcess.kill();
      await new Promise((r) => setTimeout(r, 200));
    }
  }

  return { client, gqlPort, stop };
}

/**
 * Starts a second Ad4mClient that connects to an already-running executor
 * (identified by its gqlPort).  Useful for testing multi-client scenarios
 * without spawning an extra executor process.
 */
export function connectClient(gqlPort: number, token?: string): Ad4mClient {
  return new Ad4mClient(apolloClient(gqlPort, token));
}

export { TEST_DIR, BOOTSTRAP_SEED };
export type { ChildProcess };
