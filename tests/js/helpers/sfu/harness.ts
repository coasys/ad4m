import { expect } from "chai";
import { ChildProcess } from "node:child_process";
import { existsSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

import { InstrumentedClient } from "./client.js";
import { startExecutor, waitForHealth, stopExecutor, sleep, ExecutorConfig } from "./executor.js";
import { Scenario, ScenarioContext } from "./scenario.js";
import { getFreePort, registerPorts, deregisterPorts } from "../ports.js";

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

/**
 * Define a Mocha describe block for an SFU scenario.
 *
 * Call inside a parent `describe()` — it registers before/after hooks
 * for executor lifecycle and an `it()` block that runs the scenario
 * and asserts `result.passed`.
 *
 * Every scenario gets its own run directory,
 * `<tmpdir>/ad4m-sfu-test-<id>-XXXXXX` (passed as `ctx.tmpDirBase`), and
 * its executor a port the OS hands out, so two runs on one host share
 * neither.  Executor and cascade-node logs go in the run directory.  It is
 * removed when the scenario passes or is skipped; on a failure the logs
 * stay there (data dirs are removed) for CI's `/tmp/ad4m-sfu-test-*`
 * artifact collection.
 */
export function sfuScenario(
  scenario: Scenario,
  timeoutMs: number = 180_000,
): void {
  describe(scenario.name, function () {
    this.timeout(timeoutMs);

    let port = 0;
    let proc: ChildProcess | null = null;
    let client: InstrumentedClient | null = null;
    let runDir: string | null = null;
    let dataPath: string | null = null;
    let succeeded = false;

    before(async function () {
      // A scenario this host cannot run shows as pending; skipping here
      // (Mocha marks the suite's tests pending) also saves starting an
      // executor for it.
      const missing = scenario.unavailable?.();
      if (missing) {
        console.warn(`[${scenario.id}] skipped: ${missing}`);
        this.skip();
      }
      runDir = mkdtempSync(join(tmpdir(), `ad4m-sfu-test-${scenario.id}-`));
      if (scenario.managesOwnEnvironment) return;

      const executorPath = resolveExecutorPath();
      if (!existsSync(executorPath)) {
        throw new Error(
          `Executor binary not found at ${executorPath}. ` +
          `Build it first or set EXECUTOR_PATH env var.`
        );
      }

      port = await getFreePort();
      registerPorts([port]);
      dataPath = join(runDir, "executor");
      const cfg: ExecutorConfig = {
        branch: "test",
        port,
        dataPath,
        adminToken: ADMIN_TOKEN,
        adamRepoPath: repoRoot(),
        buildDir: "",
        logPath: join(runDir, "executor.log"),
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

    afterEach(function () {
      const state = this.currentTest?.state;
      succeeded = state === "passed" || state === "pending";
    });

    after(async function () {
      if (client) {
        try { await client.disconnect(); } catch { /* best-effort */ }
      }
      if (proc) {
        const running = proc;
        const exited = running.exitCode !== null || running.signalCode !== null
          ? Promise.resolve()
          : new Promise<void>((resolve) => running.once("exit", () => resolve()));
        stopExecutor(running);
        // stopExecutor escalates to SIGKILL after 5 s.
        await Promise.race([exited, sleep(7000)]);
      }
      if (dataPath && existsSync(dataPath)) {
        rmSync(dataPath, { recursive: true, force: true });
      }
      if (port) deregisterPorts([port]);
      if (runDir && existsSync(runDir)) {
        if (succeeded) {
          rmSync(runDir, { recursive: true, force: true });
        } else {
          console.warn(`[${scenario.id}] logs kept in ${runDir}`);
        }
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
        tmpDirBase: runDir!,
        executorPath,
      };
      const result = await scenario.run(ctx);
      expect(result.passed, result.summary).to.be.true;
    });
  });
}
