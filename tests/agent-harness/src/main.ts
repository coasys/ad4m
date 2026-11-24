/**
 * Agent Harness Integration Tests — Runner
 *
 * Pod-managed scenarios that test agent onboarding, waker subscriptions,
 * and A/V action loops across harnesses (OpenClaw, Hermes, Sovereign).
 *
 *   tsx src/main.ts [--scenario a2|a4|a5]... [--require <scenario>/<route>[,…]]
 *
 * Exits non-zero when a scenario fails (a route failed, or none passed) or a
 * required route (`--require`, or AGENT_HARNESS_REQUIRED) did not pass: a
 * required route that SKIPs fails the run.
 */

import { join } from "path";
import { Scenario, ScenarioResult } from "./scenario.js";
import { a2ProvisionConnect, a4Waker, a5AvLoop } from "./scenarios/index.js";
import { consoleReport, jsonReport } from "./reporters.js";
import { config } from "./config.js";

const ALL_SCENARIOS: Scenario[] = [a2ProvisionConnect, a4Waker, a5AvLoop];

function parseArgs() {
  const args = process.argv.slice(2);
  const result = { scenarios: [] as string[], required: [] as string[] };
  const list = (s: string | undefined) => (s ?? "").split(",").map((x) => x.trim()).filter(Boolean);
  result.required.push(...list(process.env.AGENT_HARNESS_REQUIRED));
  for (let i = 0; i < args.length; i++) {
    switch (args[i]) {
      case "--scenario": result.scenarios.push(args[++i]); break;
      case "--require": result.required.push(...list(args[++i])); break;
      case "--results-dir": i++; break; // read by config.ts
      default:
        console.error(`Unknown argument: ${args[i]}`);
        process.exit(2);
    }
  }
  return result;
}

async function main(): Promise<void> {
  const args = parseArgs();
  console.log("AD4M AGENT HARNESS — integration scenarios");

  const unknownIds = args.scenarios.filter((id) => !ALL_SCENARIOS.some((s) => s.id === id));
  if (unknownIds.length > 0) {
    console.error(`Unknown scenario id(s): ${unknownIds.join(", ")} (known: ${ALL_SCENARIOS.map((s) => s.id).join(", ")})`);
    process.exit(2);
  }
  // A required route must name a real scenario/route, so a typo cannot pass silently.
  const required = args.required.map((r) => {
    const [id, route] = r.split("/");
    const scenario = ALL_SCENARIOS.find((s) => s.id === id);
    if (!scenario || !route || !scenario.routes.includes(route)) {
      const known = ALL_SCENARIOS.flatMap((s) => s.routes.map((x) => `${s.id}/${x}`));
      console.error(`Unknown required route: ${r} (known: ${known.join(", ")})`);
      process.exit(2);
    }
    return { id, route };
  });

  const scenarios = args.scenarios.length > 0 ? ALL_SCENARIOS.filter((s) => args.scenarios.includes(s.id)) : ALL_SCENARIOS;
  const missing = required.filter((r) => !scenarios.some((s) => s.id === r.id));
  if (missing.length > 0) {
    console.error(`Required route(s) in scenarios not selected: ${missing.map((r) => `${r.id}/${r.route}`).join(", ")}`);
    process.exit(2);
  }
  console.log(`Scenarios: ${scenarios.map((s) => s.id).join(", ")}`);
  if (required.length) console.log(`Required routes: ${required.map((r) => `${r.id}/${r.route}`).join(", ")}`);

  const outDir = join(config.resultsDir, "agent-harness");
  const results: ScenarioResult[] = [];
  for (const scenario of scenarios) {
    console.log(`\n[runner] Running ${scenario.id}: ${scenario.name}...`);
    try {
      const result = await scenario.run({ logDir: join(outDir, "logs") });
      results.push(result);
      console.log(`[runner] ${scenario.id} ${result.passed ? "PASS" : "FAIL"}: ${result.summary}`);
    } catch (err: any) {
      console.error(`[runner] ${scenario.id} CRASHED: ${err.message}`);
      const now = Date.now();
      results.push({
        id: scenario.id,
        scenario: scenario.id,
        passed: false,
        startTime: now,
        endTime: now,
        durationMs: 0,
        metrics: { error: err.message },
        samples: [],
        summary: `CRASHED: ${err.message}`,
      });
    }
  }

  jsonReport(results, outDir);
  consoleReport(results);

  const failed = results.filter((r) => !r.passed);
  const unmet = required.filter((r) => results.find((x) => x.id === r.id)?.metrics[r.route] !== "pass");
  for (const r of unmet) {
    const status = results.find((x) => x.id === r.id)?.metrics[r.route] ?? "not run";
    console.error(`[runner] required route ${r.id}/${r.route}: ${status}`);
  }
  console.log(`\n[runner] Done — ${results.length - failed.length} passed, ${failed.length} failed, ${unmet.length} required route(s) unmet.`);
  if (failed.length > 0 || unmet.length > 0) process.exit(1);
}

main().catch((err) => {
  console.error("Fatal error:", err);
  process.exit(1);
});
