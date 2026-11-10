/**
 * Runner configuration: where results go (`--results-dir`, else
 * AD4M_WT_RESULTS_DIR, else ./results). Reports and route logs land in
 * `<resultsDir>/agent-harness/`.
 */

import { join } from "path";

function resultsDir(): string {
  const args = process.argv.slice(2);
  const i = args.indexOf("--results-dir");
  if (i >= 0 && args[i + 1]) return args[i + 1];
  return process.env.AD4M_WT_RESULTS_DIR ?? join(process.cwd(), "results");
}

export const config = { resultsDir: resultsDir() };
