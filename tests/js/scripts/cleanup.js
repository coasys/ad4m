// Cleanup script: kill ad4m-executor processes by port, NOT by name.
// Killing by name (e.g. pkill / kill-process-by-name) would kill executors
// belonging to OTHER concurrent CI jobs on the same machine.
// Each test file uses a unique port range, so port-based kills are safe.

import { execSync } from 'child_process';

// Ports used by individual test files in the test-all sequence.
// NOTE: Do NOT include setup ports (publishTestLangs.ts: 15700/15703/15706).
// Those belong to the prepare phase and are cleaned up by each job's own
// cleanup_processes() or by publishTestLangs.ts itself when it exits.
// Including them here would kill the setup executor of OTHER concurrent CI
// jobs sharing this self-hosted runner.
const TEST_PORTS = [
  15000, 15001, 15002,        // app.test.ts
  15100, 15101, 15102,        // authentication.test.ts (suite 1)
  15200, 15201, 15202, 15203, // authentication.test.ts (suite 2)
  15300, 15301, 15302,        // integration.test.ts (alice)
  15400, 15401, 15402,        // integration.test.ts (bob — multi-user section)
  15600, 15601, 15602,        // simple.test.ts
  15800, 15801, 15802,        // multi-user-connect.test.ts
  15900, 15901, 15902,        // multi-user-simple.test.ts
  15920, 15921, 15922,        // email-verification.test.ts
  16600, 16601, 16602,        // prolog-and-literals.test.ts
  16000, 16001, 16002,        // mcp-http.test.ts (GQL + HC ports)
  16010, 16011, 16012,        // mcp-auth.test.ts (GQL + HC ports)
  16020, 16021, 16022,        // mcp-mcporter.test.ts (GQL + HC ports)
  16003,                      // mcp-http.test.ts MCP port
  16013,                      // mcp-auth.test.ts MCP port
  16023,                      // mcp-mcporter.test.ts MCP port
];

async function cleanup() {
  let killed = 0;
  for (const port of TEST_PORTS) {
    try {
      execSync(`lsof -ti:${port} | xargs -r kill -9`, { stdio: 'ignore' });
      killed++;
    } catch (e) {
      // Port not in use — that's fine
    }
  }
  if (killed > 0) {
    console.log(`cleanup: killed processes on ${killed} ports`);
  }
}

cleanup();
