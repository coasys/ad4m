// e2e/stress.spec.ts — 8-user cascade stress test (slow)

import { test, expect } from './fixtures.js';
import { startExecutors, stopAll, type ExecutorInstance } from './helpers/executor.js';
import { sleep } from '../lib/retry.js';

test.describe('Stress: 8-User Cascade', () => {
  test.slow(); // Mark as slow — extended timeout

  let executors: ExecutorInstance[];
  let perspectiveUuid: string;

  test.beforeAll(async () => {
    executors = await startExecutors(8);

    const nhResult = await executors[0].api.query(`
      mutation { neighbourhoodCreate(name: "stress-test") { url perspectiveUuid } }
    `);
    const nh = nhResult.data?.neighbourhoodCreate as { url: string; perspectiveUuid: string };
    perspectiveUuid = nh.perspectiveUuid;

    // All others join neighbourhood
    for (let i = 1; i < executors.length; i++) {
      await executors[i].api.query(`
        mutation { neighbourhoodJoin(url: "${nh.url}") { perspectiveUuid } }
      `);
    }

    // Configure cascade mode — first 2 executors as SFU peers
    await executors[0].api.query(`
      mutation {
        webrtcConfigure(perspectiveUuid: "${perspectiveUuid}", config: {
          mode: "sfu-cascade",
          sfuPeers: ["${executors[0].did}", "${executors[1].did}"]
        }) { ok }
      }
    `);
  });

  test.afterAll(async () => {
    await stopAll(executors);
  });

  test('all 8 users connect in cascade mode', async () => {
    // All join call
    for (const exec of executors) {
      await exec.api.query(`
        mutation { webrtcJoinCall(perspectiveUuid: "${perspectiveUuid}") { ok } }
      `);
    }

    // Wait for all connections to establish
    await sleep(10_000);

    // Verify from first executor
    const status = await executors[0].api.query(`
      { webrtcStatus(perspectiveUuid: "${perspectiveUuid}") {
        mode participants { did }
      } }
    `);
    const data = status.data?.webrtcStatus as { mode: string; participants: { did: string }[] };

    expect(data.mode).toBe('sfu-cascade');
    expect(data.participants).toHaveLength(8);
  });
});
