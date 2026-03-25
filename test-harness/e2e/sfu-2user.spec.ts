// e2e/sfu-2user.spec.ts — 2-user SFU designated mode test

import { test, expect } from './fixtures.js';
import { startExecutors, stopAll, type ExecutorInstance } from './helpers/executor.js';

test.describe('SFU 2-User Call (Designated)', () => {
  let executors: ExecutorInstance[];

  test.beforeAll(async () => {
    executors = await startExecutors(2);
  });

  test.afterAll(async () => {
    await stopAll(executors);
  });

  test('two users connect via SFU designated mode', async () => {
    const [exec1, exec2] = executors;

    // Create neighbourhood
    const nhResult = await exec1.api.query(`
      mutation { neighbourhoodCreate(name: "sfu-test") { url perspectiveUuid } }
    `);
    const nh = nhResult.data?.neighbourhoodCreate as { url: string; perspectiveUuid: string };

    // Join from executor 2
    await exec2.api.query(`
      mutation { neighbourhoodJoin(url: "${nh.url}") { perspectiveUuid } }
    `);

    // Configure SFU designated mode (maxMeshParticipants=0 forces SFU)
    await exec1.api.query(`
      mutation {
        webrtcConfigure(perspectiveUuid: "${nh.perspectiveUuid}", config: {
          mode: "sfu-designated",
          maxMeshParticipants: 0,
          sfuPeers: ["${exec1.did}"]
        }) { ok }
      }
    `);

    // Both join call
    await exec1.api.query(`
      mutation { webrtcJoinCall(perspectiveUuid: "${nh.perspectiveUuid}") { ok } }
    `);
    await exec2.api.query(`
      mutation { webrtcJoinCall(perspectiveUuid: "${nh.perspectiveUuid}") { ok } }
    `);

    // Verify SFU mode
    const status = await exec1.api.query(`
      { webrtcStatus(perspectiveUuid: "${nh.perspectiveUuid}") {
        mode participants { did } sfuRooms { id participants }
      } }
    `);
    const statusData = status.data?.webrtcStatus as {
      mode: string;
      participants: { did: string }[];
      sfuRooms: { id: string; participants: string[] }[];
    };

    expect(statusData.mode).toBe('sfu-designated');
    expect(statusData.participants).toHaveLength(2);
    expect(statusData.sfuRooms.length).toBeGreaterThan(0);
  });
});
