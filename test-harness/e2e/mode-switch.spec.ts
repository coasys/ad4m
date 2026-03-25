// e2e/mode-switch.spec.ts — Mid-call mode switching test

import { test, expect } from './fixtures.js';
import { startExecutors, stopAll, type ExecutorInstance } from './helpers/executor.js';
import { sleep } from '../lib/retry.js';

test.describe('Mode Switch Mid-Call', () => {
  let executors: ExecutorInstance[];
  let perspectiveUuid: string;

  test.beforeAll(async () => {
    executors = await startExecutors(2);

    const nhResult = await executors[0].api.query(`
      mutation { neighbourhoodCreate(name: "mode-switch-test") { url perspectiveUuid } }
    `);
    const nh = nhResult.data?.neighbourhoodCreate as { url: string; perspectiveUuid: string };
    perspectiveUuid = nh.perspectiveUuid;

    await executors[1].api.query(`
      mutation { neighbourhoodJoin(url: "${nh.url}") { perspectiveUuid } }
    `);
  });

  test.afterAll(async () => {
    await stopAll(executors);
  });

  test('switch from mesh to SFU and back mid-call', async () => {
    const [exec1, exec2] = executors;

    // Start in mesh mode
    await exec1.api.query(`
      mutation { webrtcJoinCall(perspectiveUuid: "${perspectiveUuid}") { ok } }
    `);
    await exec2.api.query(`
      mutation { webrtcJoinCall(perspectiveUuid: "${perspectiveUuid}") { ok } }
    `);

    // Verify mesh mode
    let status = await exec1.api.query(`
      { webrtcStatus(perspectiveUuid: "${perspectiveUuid}") { mode participants { did } } }
    `);
    let data = status.data?.webrtcStatus as { mode: string; participants: { did: string }[] };
    expect(data.mode).toBe('mesh');
    expect(data.participants).toHaveLength(2);

    // Switch to SFU mid-call
    await exec1.api.query(`
      mutation {
        webrtcConfigure(perspectiveUuid: "${perspectiveUuid}", config: {
          mode: "sfu-designated",
          maxMeshParticipants: 0,
          sfuPeers: ["${exec1.did}"]
        }) { ok }
      }
    `);

    // Wait for reconnection
    await sleep(5000);

    // Verify SFU mode
    status = await exec1.api.query(`
      { webrtcStatus(perspectiveUuid: "${perspectiveUuid}") { mode participants { did } } }
    `);
    data = status.data?.webrtcStatus as { mode: string; participants: { did: string }[] };
    expect(data.mode).toBe('sfu-designated');
    expect(data.participants).toHaveLength(2);

    // Switch back to mesh
    await exec1.api.query(`
      mutation {
        webrtcConfigure(perspectiveUuid: "${perspectiveUuid}", config: {
          mode: "mesh"
        }) { ok }
      }
    `);

    await sleep(5000);

    // Verify mesh again
    status = await exec1.api.query(`
      { webrtcStatus(perspectiveUuid: "${perspectiveUuid}") { mode participants { did } } }
    `);
    data = status.data?.webrtcStatus as { mode: string; participants: { did: string }[] };
    expect(data.mode).toBe('mesh');
    expect(data.participants).toHaveLength(2);
  });
});
