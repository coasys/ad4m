// Cleanup script: kill processes belonging to THIS checkout only.
//
// Why not kill by process name?
//   pkill -f "ad4m" / kill-process-by-name hits executors from OTHER concurrent
//   CI jobs on the same self-hosted runner — unsafe.
//
// Why not kill by hardcoded port?
//   Our tests use getFreePorts() (dynamic allocation) — no fixed list exists.
//
// Why not kill kitsune2-bootstrap-srv by name?
//   Same reason: concurrent jobs each spawn one and they'd kill each other's.
//
// Solution:
//   ad4m-executor    → killed by absolute path to binary (unique per checkout)
//   kitsune2         → killed by PID written to tst-tmp/kitsune2-bootstrap.pid
//                      at spawn time in utils.ts (unique per process)

import { execSync } from "child_process";
import { rmSync, existsSync, readdirSync, readFileSync } from "fs";
import { join, dirname, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));

// Absolute path to this checkout's executor binary — unique across checkouts.
const executorBinary = resolve(
  __dirname,
  "..",
  "..",
  "..",
  "target",
  "release",
  "ad4m-executor",
);

async function cleanup() {
  // Kill only THIS checkout's ad4m-executor (path-based = concurrent CI safe).
  try {
    execSync(`pkill -9 -f "${executorBinary}" 2>/dev/null || true`, {
      stdio: "ignore",
    });
  } catch (_) {}

  // Kill THIS run's kitsune2-bootstrap-srv by the PID written at spawn time.
  // Killing by name would hit concurrent jobs' instances — use PID instead.
  const pidFile = join(__dirname, "..", "tst-tmp", "kitsune2-bootstrap.pid");
  if (existsSync(pidFile)) {
    try {
      const pid = parseInt(readFileSync(pidFile, "utf8").trim(), 10);
      if (!isNaN(pid)) process.kill(pid, 9);
    } catch (_) {}
    try {
      rmSync(pidFile);
    } catch (_) {}
  }

  // Brief pause so the OS reclaims ports before the next executor starts.
  await new Promise((r) => setTimeout(r, 500));

  // Wipe per-test agent data directories so the next executor starts clean.
  // Prevents SurrealDB "disk I/O error" panics from half-written DB files.
  // Preserve tst-tmp/agents/p (the publishing agent set up by prepare-test).
  const agentsDir = join(__dirname, "..", "tst-tmp", "agents");
  if (existsSync(agentsDir)) {
    for (const entry of readdirSync(agentsDir)) {
      if (entry === "p") continue;
      rmSync(join(agentsDir, entry), { recursive: true, force: true });
    }
  }
}

cleanup();
