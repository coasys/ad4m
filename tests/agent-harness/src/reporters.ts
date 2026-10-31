/**
 * Reporters: console summary and one JSON file per scenario.
 */

import { writeFileSync, mkdirSync } from "fs";
import { join } from "path";
import { ScenarioResult } from "./scenario.js";

export function consoleReport(results: ScenarioResult[]): void {
  console.log("\n" + "=".repeat(80));
  console.log("  AD4M AGENT HARNESS — RESULTS");
  console.log("=".repeat(80));

  for (const r of results) {
    console.log(`\n┌─ ${r.scenario} ${r.passed ? "PASS" : "FAIL"}`);
    console.log(`│  Duration: ${(r.durationMs / 1000).toFixed(1)}s`);
    console.log(`│  Summary: ${r.summary}`);
    for (const [key, value] of Object.entries(r.metrics)) {
      console.log(`│  ${key}: ${typeof value === "number" ? Number(value.toFixed(2)) : value}`);
    }
    console.log(`└${"─".repeat(79)}`);
  }
  console.log("\n" + "=".repeat(80) + "\n");
}

export function jsonReport(results: ScenarioResult[], outputDir: string): void {
  mkdirSync(outputDir, { recursive: true });
  for (const r of results) {
    const filepath = join(outputDir, `${r.scenario.replace(/[^a-z0-9._-]/gi, "-")}.json`);
    writeFileSync(filepath, JSON.stringify(r, null, 2));
    console.log(`[reporter] Wrote ${filepath}`);
  }
}
