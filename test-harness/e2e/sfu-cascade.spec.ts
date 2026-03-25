// e2e/sfu-cascade.spec.ts — 2-user cascade mode test

import { test, expect } from './fixtures.js';
import { startExecutors, stopAll, type ExecutorInstance } from './helpers/executor.js';

test.describe('SFU Cascade Mode', () => {
  let executors: ExecutorInstance[];

  test.beforeAll(async () => {
    executors = await startExecutors(2);
  });

  test.afterAll(async () => {
    await stopAll(executors);
  });

  test('two users connect via cascade mode with both as SFU peers', async () => {
    const [exec1, exec2] = executors;

    // Create neighbourhood
    const nhResult = await exec1.api.query(`
      mutation { neighbourhoodCreate(name: "cascade-test") { url perspectiveUuid } }
    `);
    const nh = nhResult.data?.neighbourhoodCreate as { url: string; perspectiveUuid: string };

    await exec2.api.query(`
      mutation { neighbourhoodJoin(url: "${nh.url}") { perspectiveUuid } }
    `);

    // Configure cascade mode with both executors as SFU peers
    await exec1.api.query(`
      mutation {
        webrtcConfigure(perspectiveUuid: "${nh.perspectiveUuid}", config: {
          mode: "sfu-cascade",
          sfuPeers: ["${exec1.did}", "${exec2.did}"]
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

    // Verify cascade mode
    const status1 = await exec1.api.query(`
      { webrtcStatus(perspectiveUuid: "${nh.perspectiveUuid}") {
        mode sfuRooms { id participants }
      } }
    `);
    const status2 = await exec2.api.query(`
      { webrtcStatus(perspectiveUuid: "${nh.perspectiveUuid}") {
        mode sfuRooms { id participants }
      } }
    `);

    const data1 = status1.data?.webrtcStatus as { mode: string; sfuRooms: any[] };
    const data2 = status2.data?.webrtcStatus as { mode: string; sfuRooms: any[] };

    expect(data1.mode).toBe('sfu-cascade');
    expect(data2.mode).toBe('sfu-cascade');
    // Both should have active SFU rooms
    expect(data1.sfuRooms.length).toBeGreaterThan(0);
    expect(data2.sfuRooms.length).toBeGreaterThan(0);
  });
});
