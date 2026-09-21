/**
 * Standalone report generator
 * Reads results from ./results/ and generates comparison.md
 */

import { readdirSync, readFileSync, existsSync } from "fs";
import { join } from "path";
import { ScenarioResult } from "./scenario.js";
import { comparisonReport, consoleReport } from "./reporters.js";

const RESULTS_DIR = join(process.cwd(), "results");

function loadResults(): Map<string, ScenarioResult[]> {
  const allResults = new Map<string, ScenarioResult[]>();

  if (!existsSync(RESULTS_DIR)) {
    console.error("No results directory found. Run scenarios first.");
    process.exit(1);
  }

  const branches = readdirSync(RESULTS_DIR, { withFileTypes: true })
    .filter((d) => d.isDirectory())
    .map((d) => d.name);

  for (const branch of branches) {
    const branchDir = join(RESULTS_DIR, branch);
    const files = readdirSync(branchDir).filter((f) => f.endsWith(".json"));
    const results: ScenarioResult[] = [];

    for (const file of files) {
      const content = readFileSync(join(branchDir, file), "utf-8");
      results.push(JSON.parse(content));
    }

    allResults.set(branch, results);
  }

  return allResults;
}

function main(): void {
  const allResults = loadResults();

  console.log(`Loaded results for ${allResults.size} branches`);

  // Console report for each branch
  for (const [branch, results] of allResults) {
    console.log(`\n--- ${branch} ---`);
    consoleReport(results);
  }

  // Comparison report
  if (allResults.size > 1) {
    const comparisonPath = join(RESULTS_DIR, "comparison.md");
    comparisonReport(allResults, comparisonPath);
  }
}

main();
