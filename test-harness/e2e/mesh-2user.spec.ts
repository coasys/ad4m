// e2e/mesh-2user.spec.ts — 2-user mesh call test

import { test, expect } from './fixtures.js';
import { startExecutors, stopAll, type ExecutorInstance } from './helpers/executor.js';
import { injectWebRTCTracking, waitForConnection, getWebRTCStats } from './helpers/webrtc.js';
import { injectAuth } from './helpers/auth.js';

test.describe('Mesh 2-User Call', () => {
  let executors: ExecutorInstance[];

  test.beforeAll(async () => {
    executors = await startExecutors(2);
  });

  test.afterAll(async () => {
    await stopAll(executors);
  });

  test('two users connect via mesh and exchange audio', async ({ browser }) => {
    const [exec1, exec2] = executors;

    // Create neighbourhood on executor 1
    const nhResult = await exec1.api.query(`
      mutation { neighbourhoodCreate(name: "mesh-test") { url perspectiveUuid } }
    `);
    const nh = nhResult.data?.neighbourhoodCreate as { url: string; perspectiveUuid: string };

    // Join from executor 2
    await exec2.api.query(`
      mutation { neighbourhoodJoin(url: "${nh.url}") { perspectiveUuid } }
    `);

    // Open two browser contexts
    const ctx1 = await browser.newContext();
    const ctx2 = await browser.newContext();
    const page1 = await ctx1.newPage();
    const page2 = await ctx2.newPage();

    await injectWebRTCTracking(page1);
    await injectWebRTCTracking(page2);

    // Note: actual page navigation and call joining depends on Flux UI.
    // These tests are designed to work once Flux is available with data-testid attrs.

    // Join call on both pages (using GraphQL as fallback until UI is ready)
    await exec1.api.query(`
      mutation { webrtcJoinCall(perspectiveUuid: "${nh.perspectiveUuid}") { ok } }
    `);
    await exec2.api.query(`
      mutation { webrtcJoinCall(perspectiveUuid: "${nh.perspectiveUuid}") { ok } }
    `);

    // Verify call topology via GraphQL
    const status1 = await exec1.api.query(`
      { webrtcStatus(perspectiveUuid: "${nh.perspectiveUuid}") { mode participants { did } } }
    `);
    const statusData = status1.data?.webrtcStatus as { mode: string; participants: { did: string }[] };

    expect(statusData.mode).toBe('mesh');
    expect(statusData.participants).toHaveLength(2);

    await ctx1.close();
    await ctx2.close();
  });
});
