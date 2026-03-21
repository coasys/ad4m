// Cleanup script: kill ad4m-executor processes by port, NOT by name.
// Killing by name (e.g. pkill / kill-process-by-name) would kill executors
// belonging to OTHER concurrent CI jobs on the same machine.
// Each test file uses dynamically allocated ports (via getFreePorts), so we
// rely on the port registry written by helpers/ports.ts to know which ports
// to kill between test runs.

import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// ---------------------------------------------------------------------------
// Port registry: written by getFreePorts/registerPorts in helpers/ports.ts.
// Lives at tst-tmp/active-ports.json (relative to tests/js/).
// If a test is force-killed without running after(), stale entries remain
// and cleanup.js uses them to kill the orphaned executor processes.
// ---------------------------------------------------------------------------
const REGISTRY_PATH = path.resolve(__dirname, '..', 'tst-tmp', 'active-ports.json');

function readRegistry() {
  try {
    if (fs.existsSync(REGISTRY_PATH)) {
      return JSON.parse(fs.readFileSync(REGISTRY_PATH, 'utf8'));
    }
  } catch { /* ignore */ }
  return [];
}

const registryPorts = readRegistry();

// ---------------------------------------------------------------------------
// Kill all registered ports
// ---------------------------------------------------------------------------
async function cleanup() {
  let killed = 0;
  for (const port of registryPorts) {
    try {
      execSync(`lsof -ti:${port} | xargs -r kill -9`, { stdio: 'ignore' });
      killed++;
    } catch (e) {
      // Port not in use — that's fine
    }
  }

  // Clear the registry once we've processed it (avoids killing stale entries
  // from a completely different run if the file wasn't cleaned up).
  if (registryPorts.length > 0) {
    try { fs.unlinkSync(REGISTRY_PATH); } catch { /* non-fatal */ }
  }

  if (killed > 0) {
    console.log(`cleanup: killed processes on ${killed} ports`);
  }
}

cleanup();
