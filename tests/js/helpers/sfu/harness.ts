import { expect } from "chai";
import { ChildProcess } from "node:child_process";
import { existsSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

import { InstrumentedClient } from "./client.js";
import { startExecutor, waitForHealth, stopExecutor, sleep, ExecutorConfig } from "./executor.js";
import { Scenario, ScenarioContext } from "./scenario.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const ADMIN_TOKEN = process.env.AD4M_ADMIN_TOKEN ?? "test123";

function resolveExecutorPath(): string {
  if (process.env.EXECUTOR_PATH) return process.env.EXECUTOR_PATH;
  return join(repoRoot(), "target", "release", "ad4m-executor");
}

function repoRoot(): string {
  // tests/js/helpers/sfu/ → repo root
  return join(__dirname, "..", "..", "..", "..");
}

let portCounter = 15100;
function nextPort(): number {
  return portCounter++;
}

/**
 * Define a Mocha describe block for an SFU scenario.
 *
 * Call inside a parent `describe()` — it registers before/after hooks
 * for executor lifecycle and an `it()` block that runs the scenario
 * and asserts `result.passed`.
 */
export function sfuScenario(
  scenario: Scenario,
  timeoutMs: number = 180_000,
  skip?: string,
): void {
  const block = skip ? describe.skip : describe;
  block(scenario.name, function () {
    this.timeout(timeoutMs);

    const port = nextPort();
    let proc: ChildProcess | null = null;
    let client: InstrumentedClient | null = null;
    let dataPath: string | null = null;

    if (!scenario.managesOwnEnvironment) {
      before(async function () {
        const executorPath = resolveExecutorPath();
        if (!existsSync(executorPath)) {
          throw new Error(
            `Executor binary not found at ${executorPath}. ` +
            `Build it first or set EXECUTOR_PATH env var.`
          );
        }

        dataPath = join(tmpdir(), `ad4m-sfu-test-${scenario.id}-${Date.now()}`);
        const cfg: ExecutorConfig = {
          branch: "test",
          port,
          dataPath,
          adminToken: ADMIN_TOKEN,
          adamRepoPath: repoRoot(),
          buildDir: "",
        };
        proc = await startExecutor(executorPath, cfg);
        await waitForHealth(port, 120_000, ADMIN_TOKEN);

        client = new InstrumentedClient({ port, adminToken: ADMIN_TOKEN });
        await client.connect();
        try {
          await client.call("agent.generate", { passphrase: "wind-tunnel-test" });
        } catch {
          // Already generated — fine.
        }
      });
    }

    after(async function () {
      if (client) {
        try { await client.disconnect(); } catch { /* best-effort */ }
      }
      if (proc) stopExecutor(proc);
      await sleep(1000);
      if (dataPath && existsSync(dataPath)) {
        rmSync(dataPath, { recursive: true, force: true });
      }
    });

    it(`${scenario.id}: ${scenario.description}`, async function () {
      const executorPath = resolveExecutorPath();
      const ctx: ScenarioContext = {
        client: client!,
        branch: "test",
        port,
        adminToken: ADMIN_TOKEN,
        adamRepoPath: repoRoot(),
        tmpDirBase: tmpdir(),
        executorPath,
      };
      const result = await scenario.run(ctx);
      expect(result.passed, result.summary).to.be.true;
    });
  });
}
