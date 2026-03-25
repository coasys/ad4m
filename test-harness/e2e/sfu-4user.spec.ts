// e2e/sfu-4user.spec.ts — 4-user SFU designated mode test

import { test, expect } from './fixtures.js';
import { startExecutors, stopAll, type ExecutorInstance } from './helpers/executor.js';

test.describe.serial('SFU 4-User Call', () => {
  let executors: ExecutorInstance[];
  let nhUrl: string;
  let perspectiveUuid: string;

  test.beforeAll(async () => {
    executors = await startExecutors(4);

    // Create neighbourhood on executor 1
    const nhResult = await executors[0].api.query(`
      mutation { neighbourhoodCreate(name: "sfu-4user-test") { url perspectiveUuid } }
    `);
    const nh = nhResult.data?.neighbourhoodCreate as { url: string; perspectiveUuid: string };
    nhUrl = nh.url;
    perspectiveUuid = nh.perspectiveUuid;

    // All others join
    for (let i = 1; i < executors.length; i++) {
      await executors[i].api.query(`
        mutation { neighbourhoodJoin(url: "${nhUrl}") { perspectiveUuid } }
      `);
    }

    // Configure SFU designated on executor 1
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

  test('all 4 users join and see each other', async () => {
    // All join call
    for (const exec of executors) {
      await exec.api.query(`
        mutation { webrtcJoinCall(perspectiveUuid: "${perspectiveUuid}") { ok } }
      `);
    }

    // Verify from each executor's perspective
    for (const exec of executors) {
      const status = await exec.api.query(`
        { webrtcStatus(perspectiveUuid: "${perspectiveUuid}") {
          mode participants { did }
        } }
      `);
      const data = status.data?.webrtcStatus as { mode: string; participants: { did: string }[] };
      expect(data.participants).toHaveLength(4);
    }
  });
});
