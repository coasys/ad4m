// Cleanup script: kill ad4m-executor processes by port, NOT by name.
// Killing by name (e.g. pkill / kill-process-by-name) would kill executors
// belonging to OTHER concurrent CI jobs on the same machine.
// Each test file uses a unique port range, so port-based kills are safe.

import { execSync } from 'child_process';

// All ports used by test files in the test-all sequence
const TEST_PORTS = [
  15000, 15001, 15002,        // app.test.ts
  15100, 15101, 15102,        // authentication.test.ts (suite 1)
  15200, 15201, 15202, 15203, // authentication.test.ts (suite 2)
  15300, 15301, 15302,        // integration.test.ts
  15600, 15601, 15602,        // simple.test.ts
  15700, 15701, 15702,        // publishTestLangs.ts (prepare-test)
  15800, 15801, 15802,        // multi-user-connect.test.ts
  15900, 15901, 15902,        // multi-user-simple.test.ts
  15920, 15921, 15922,        // email-verification.test.ts
  16600, 16601, 16602,        // prolog-and-literals.test.ts
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
