// e2e/leave-rejoin.spec.ts — Leave and rejoin call test

import { test, expect } from './fixtures.js';
import { startExecutors, stopAll, type ExecutorInstance } from './helpers/executor.js';
import { sleep } from '../lib/retry.js';

test.describe('Leave and Rejoin', () => {
  let executors: ExecutorInstance[];
  let perspectiveUuid: string;

  test.beforeAll(async () => {
    executors = await startExecutors(2);

    const nhResult = await executors[0].api.query(`
      mutation { neighbourhoodCreate(name: "leave-rejoin-test") { url perspectiveUuid } }
    `);
    const nh = nhResult.data?.neighbourhoodCreate as { url: string; perspectiveUuid: string };
    perspectiveUuid = nh.perspectiveUuid;

    await executors[1].api.query(`
      mutation { neighbourhoodJoin(url: "${nh.url}") { perspectiveUuid } }
    `);

    // Configure SFU mode
    await executors[0].api.query(`
      mutation {
        webrtcConfigure(perspectiveUuid: "${perspectiveUuid}", config: {
          mode: "sfu-designated",
          maxMeshParticipants: 0,
          sfuPeers: ["${executors[0].did}"]
        }) { ok }
      }
    `);
  });

  test.afterAll(async () => {
    await stopAll(executors);
  });

  test('user 2 leaves and rejoins', async () => {
    const [exec1, exec2] = executors;

    // Both join
    await exec1.api.query(`mutation { webrtcJoinCall(perspectiveUuid: "${perspectiveUuid}") { ok } }`);
    await exec2.api.query(`mutation { webrtcJoinCall(perspectiveUuid: "${perspectiveUuid}") { ok } }`);

    // Verify both connected
    let status = await exec1.api.query(`
      { webrtcStatus(perspectiveUuid: "${perspectiveUuid}") { participants { did } } }
    `);
    let data = status.data?.webrtcStatus as { participants: { did: string }[] };
    expect(data.participants).toHaveLength(2);

    // User 2 leaves
    await exec2.api.query(`mutation { webrtcLeaveCall(perspectiveUuid: "${perspectiveUuid}") { ok } }`);
    await sleep(2000);

    // Verify user 1 still connected, participant count = 1
    status = await exec1.api.query(`
      { webrtcStatus(perspectiveUuid: "${perspectiveUuid}") { participants { did } } }
    `);
    data = status.data?.webrtcStatus as { participants: { did: string }[] };
    expect(data.participants).toHaveLength(1);

    // User 2 rejoins
    await exec2.api.query(`mutation { webrtcJoinCall(perspectiveUuid: "${perspectiveUuid}") { ok } }`);
    await sleep(3000);

    // Verify both connected again
    status = await exec1.api.query(`
      { webrtcStatus(perspectiveUuid: "${perspectiveUuid}") { participants { did } } }
    `);
    data = status.data?.webrtcStatus as { participants: { did: string }[] };
    expect(data.participants).toHaveLength(2);
  });
});
