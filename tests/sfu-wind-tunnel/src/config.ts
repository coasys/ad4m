/**
 * Centralized configuration for AD4M Wind Tunnel.
 *
 * All machine-specific values are configurable via environment variables
 * and/or CLI arguments. CLI args take precedence over env vars.
 */

import { tmpdir } from "os";
import { join } from "path";

export interface WindTunnelConfig {
  /** Path to the AD4M repo (for building executor from source) */
  adamRepoPath: string;
  /** Admin token for executor authentication */
  adminToken: string;
  /** Base directory for temporary files (data dirs, build dirs) */
  tmpDirBase: string;
  /** Base port for executor instances */
  basePort: number;
  /** Directory for storing results */
  resultsDir: string;
}

/**
 * Parse CLI arguments for config overrides.
 * Returns only the config-related args; other args (--scenario, --branch, etc.)
 * are handled by the main runner's parseArgs().
 */
function parseConfigArgs(): Partial<WindTunnelConfig> {
  const args = process.argv.slice(2);
  const result: Partial<WindTunnelConfig> = {};

  for (let i = 0; i < args.length; i++) {
    switch (args[i]) {
      case "--ad4m-repo":
        result.adamRepoPath = args[++i];
        break;
      case "--admin-token":
        result.adminToken = args[++i];
        break;
      case "--tmp-dir":
        result.tmpDirBase = args[++i];
        break;
      case "--base-port":
        result.basePort = parseInt(args[++i], 10);
        break;
      case "--results-dir":
        result.resultsDir = args[++i];
        break;
    }
  }

  return result;
}

/**
 * Build the resolved config by merging defaults < env vars < CLI args.
 */
function resolveConfig(): WindTunnelConfig {
  const cliArgs = parseConfigArgs();
  const systemTmp = tmpdir();

  return {
    adamRepoPath:
      cliArgs.adamRepoPath
      ?? process.env.AD4M_REPO
      ?? "",
    adminToken:
      cliArgs.adminToken
      ?? process.env.AD4M_ADMIN_TOKEN
      ?? "test123",
    tmpDirBase:
      cliArgs.tmpDirBase
      ?? process.env.AD4M_WT_TMPDIR
      ?? systemTmp,
    basePort:
      cliArgs.basePort
      ?? (process.env.AD4M_WT_BASE_PORT ? parseInt(process.env.AD4M_WT_BASE_PORT, 10) : 12100),
    resultsDir:
      cliArgs.resultsDir
      ?? process.env.AD4M_WT_RESULTS_DIR
      ?? join(process.cwd(), "results"),
  };
}

/** Singleton resolved config */
export const config = resolveConfig();

/**
 * Validate that required config values are present.
 * Call this before operations that need the AD4M repo path.
 */
export function validateAdamRepo(): void {
  if (!config.adamRepoPath) {
    console.error(
      "[config] AD4M repo path is required.\n" +
      "  Set via: --ad4m-repo <path>, or AD4M_REPO env var.\n" +
      "  Example: AD4M_REPO=/path/to/ad4m npx tsx src/main.ts"
    );
    process.exit(1);
  }
}
