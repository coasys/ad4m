/**
 * Reporters: Console and JSON
 */

import { writeFileSync, mkdirSync } from "fs";
import { join, dirname } from "path";
import { ScenarioResult } from "./scenario.js";

export function consoleReport(results: ScenarioResult[]): void {
  console.log("\n" + "=".repeat(80));
  console.log("  AD4M WIND TUNNEL — RESULTS");
  console.log("=".repeat(80));

  for (const r of results) {
    const verdict = r.passed ? "✅ PASS" : "❌ FAIL";
    console.log(`\n┌─ ${r.scenario} [${r.branch}] ${verdict}`);
    console.log(`│  Duration: ${r.durationMs}ms`);
    console.log(`│  Summary: ${r.summary}`);

    if (r.metrics.error) {
      console.log(`│  ❌ ERROR: ${r.metrics.error}`);
    }

    // Print key metrics
    const skipKeys = ["error", "scalingResults", "batchLatenciesMs", "batchErrors"];
    for (const [key, value] of Object.entries(r.metrics)) {
      if (skipKeys.includes(key)) continue;
      if (typeof value === "number") {
        console.log(`│  ${key}: ${typeof value === "number" ? value.toFixed(2) : value}`);
      }
    }
    console.log(`└${"─".repeat(79)}`);
  }

  console.log("\n" + "=".repeat(80) + "\n");
}

export function jsonReport(results: ScenarioResult[], outputDir: string): void {
  mkdirSync(outputDir, { recursive: true });

  for (const r of results) {
    const filename = `${r.scenario.replace(/[^a-z0-9._-]/gi, "-")}.json`;
    const filepath = join(outputDir, filename);
    writeFileSync(filepath, JSON.stringify(r, null, 2));
    console.log(`[reporter] Wrote ${filepath}`);
  }
}

export function comparisonReport(
  allResults: Map<string, ScenarioResult[]>,
  outputPath: string
): void {
  const lines: string[] = [];
  lines.push("# AD4M Wind Tunnel — Comparison Report");
  lines.push("");
  lines.push(`Generated: ${new Date().toISOString()}`);
  lines.push(`Machine: Apple Silicon MacBook Pro (48GB RAM, 14 CPUs)`);
  lines.push("");

  // Get scenario IDs from first branch results
  const branches = [...allResults.keys()];
  const firstResults = allResults.get(branches[0]) || [];
  const scenarioIds = firstResults.map((r) => r.scenario);

  for (const scenarioId of scenarioIds) {
    lines.push(`## ${scenarioId}`);
    lines.push("");

    // Table header
    lines.push("| Metric | " + branches.join(" | ") + " |");
    lines.push("| --- | " + branches.map(() => "---").join(" | ") + " |");

    // Collect metrics across branches for this scenario
    const metricsMap = new Map<string, Map<string, string>>();

    for (const branch of branches) {
      const branchResults = allResults.get(branch) || [];
      const result = branchResults.find((r) => r.scenario === scenarioId);
      if (!result) continue;

      for (const [key, value] of Object.entries(result.metrics)) {
        if (typeof value === "object" && !Array.isArray(value)) continue;
        if (Array.isArray(value)) continue;

        if (!metricsMap.has(key)) {
          metricsMap.set(key, new Map());
        }
        metricsMap.get(key)!.set(
          branch,
          typeof value === "number" ? value.toFixed(2) : String(value)
        );
      }
    }

    for (const [metric, values] of metricsMap) {
      const row = branches.map((b) => values.get(b) || "—");
      lines.push(`| ${metric} | ${row.join(" | ")} |`);
    }

    lines.push("");

    // Summary row
    lines.push("**Summaries:**");
    for (const branch of branches) {
      const branchResults = allResults.get(branch) || [];
      const result = branchResults.find((r) => r.scenario === scenarioId);
      if (result) {
        lines.push(`- **${branch}:** ${result.summary}`);
      }
    }
    lines.push("");
  }

  mkdirSync(dirname(outputPath), { recursive: true });
  writeFileSync(outputPath, lines.join("\n"));
  console.log(`[reporter] Wrote comparison report: ${outputPath}`);
}
