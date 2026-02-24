import killProcess from "kill-process-by-name";
import { execSync } from "child_process";
import { rmSync, existsSync, readdirSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
const __dirname = dirname(fileURLToPath(import.meta.url));

async function cleanup() {
  // First try the graceful kill-by-name (works cross-platform)
  try {
    killProcess("ad4m");
  } catch (e) {
    // Ignore — process may already be gone
  }

  // Then force-kill any lingering ad4m executor processes on Linux/macOS.
  // This prevents "address already in use" errors when a previous test run
  // left an executor alive (e.g. after Ctrl-C or a test timeout).
  try {
    execSync('pkill -9 -f "ad4m" 2>/dev/null || true', { stdio: "ignore" });
  } catch (_) {
    // Ignore on Windows or if no processes match
  }

  // Also kill any lingering kitsune2-bootstrap-srv (used by integration tests).
  try {
    execSync('pkill -9 -f "kitsune2-bootstrap-srv" 2>/dev/null || true', {
      stdio: "ignore",
    });
  } catch (_) {
    // Ignore on Windows or if no processes match
  }

  // Brief pause so the OS reclaims the ports before the next executor starts
  await new Promise((r) => setTimeout(r, 500));

  // Wipe per-test agent data directories so the next executor starts from a clean state.
  // This prevents SurrealDB "disk I/O error" panics caused by half-written DB files
  // left behind when a previous executor was killed mid-run.
  // We intentionally preserve tst-tmp/agents/p (the publishing agent set up by prepare-test).
  const agentsDir = join(__dirname, "..", "tst-tmp", "agents");
  if (existsSync(agentsDir)) {
    for (const entry of readdirSync(agentsDir)) {
      if (entry === "p") continue; // preserve the publishing agent
      rmSync(join(agentsDir, entry), { recursive: true, force: true });
    }
  }
}

cleanup();
