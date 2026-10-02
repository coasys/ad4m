/**
 * Agent Harness Integration Tests — Runner
 *
 * Pod-managed scenarios that test agent onboarding, waker subscriptions,
 * and A/V action loops across harnesses (OpenClaw, Hermes, Sovereign).
 * Moved from coasys/ad4m-wind-tunnel during consolidation.
 */

import { join } from "path";
import { Scenario, ScenarioContext, ScenarioResult } from "./scenario.js";
import { a2ProvisionConnect, a4Waker, a5AvLoop } from "./scenarios/index.js";
import { consoleReport, jsonReport } from "./reporters.js";
import { config } from "./config.js";

const ALL_SCENARIOS: Scenario[] = [
  a2ProvisionConnect,
  a4Waker,
  a5AvLoop,
];

function parseArgs() {
  const args = process.argv.slice(2);
  const result = {
    scenarios: [] as string[],
    executorPath: undefined as string | undefined,
  };
  for (let i = 0; i < args.length; i++) {
    switch (args[i]) {
      case "--scenario": result.scenarios.push(args[++i]); break;
      case "--executor-path": result.executorPath = args[++i]; break;
    }
  }
  return result;
}

async function main(): Promise<void> {
  const args = parseArgs();
  console.log("╔══════════════════════════════════════════════════════════════╗");
  console.log("║     AD4M AGENT HARNESS — Integration Testing               ║");
  console.log("╚══════════════════════════════════════════════════════════════╝");

  const unknownIds = args.scenarios.filter((id) => !ALL_SCENARIOS.some((s) => s.id === id));
  if (unknownIds.length > 0) {
    console.error(`Unknown scenario id(s): ${unknownIds.join(", ")} (known: ${ALL_SCENARIOS.map((s) => s.id).join(", ")})`);
    process.exit(1);
  }

  const scenarios = args.scenarios.length > 0
    ? ALL_SCENARIOS.filter((s) => args.scenarios.includes(s.id))
    : ALL_SCENARIOS;

  console.log(`Scenarios: ${scenarios.map((s) => s.id).join(", ")}`);

  const results: ScenarioResult[] = [];

  for (const scenario of scenarios) {
    console.log(`\n[runner] Running ${scenario.id}: ${scenario.name}...`);

    const ctx: ScenarioContext = {
      client: undefined as any,
      branch: "default",
      port: config.basePort,
      adminToken: config.adminToken,
      adamRepoPath: config.adamRepoPath,
      tmpDirBase: config.tmpDirBase,
      executorPath: args.executorPath,
    };

    try {
      const result = await scenario.run(ctx);
      results.push(result);
      console.log(`[runner] ${scenario.id} ${result.passed ? "PASS" : "FAIL"}: ${result.summary}`);
    } catch (err: any) {
      console.error(`[runner] ${scenario.id} CRASHED: ${err.message}`);
      results.push({
        scenario: scenario.id,
        branch: "default",
        passed: false,
        startTime: Date.now(),
        endTime: Date.now(),
        durationMs: 0,
        metrics: { error: err.message },
        samples: [],
        summary: `CRASHED: ${err.message}`,
      });
    }
  }

  jsonReport(results, join(config.resultsDir, "agent-harness"));
  consoleReport(results);

  const failed = results.filter((r) => !r.passed).length;
  console.log(`\n[runner] Done — ${results.length - failed} passed, ${failed} failed.`);
  if (failed > 0) process.exit(1);
}

main().catch((err) => { console.error("Fatal error:", err); process.exit(1); });
