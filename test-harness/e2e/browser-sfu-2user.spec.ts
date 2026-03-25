// e2e/browser-sfu-2user.spec.ts — Browser-level SFU config + verification
//
// Verifies SFU designated mode configuration works at both API and browser level.

import { test, expect } from '@playwright/test';
import {
  startExecutor,
  stopExecutor,
  type ExecutorInstance,
} from './helpers/executor.js';
import {
  startFluxServer,
  stopFluxServer,
  type FluxServer,
} from './helpers/flux-server.js';
import { setupAuthenticatedPage, waitForFluxReady, gql } from './helpers/browser-setup.js';
import { getWebRTCStats } from './helpers/webrtc.js';

test.describe('Browser — SFU 2-User (Designated Mode)', () => {
  test.setTimeout(120_000);

  let exec1: ExecutorInstance;
  let exec2: ExecutorInstance;
  let flux1: FluxServer;
  const nhUrl = 'nh://browser-sfu-designated-test';

  test.beforeAll(async () => {
    [exec1, exec2] = await Promise.all([
      startExecutor({ holochain: false }),
      startExecutor({ holochain: false }),
    ]);

    flux1 = await startFluxServer();

    // Configure SFU designated mode
    await gql(exec1, `
      mutation {
        sfuSetConfig(
          neighbourhoodUrl: "${nhUrl}",
          mode: "designated",
          designatedPeer: "${exec1.did}",
          maxMeshParticipants: 0,
          sfuPeers: ["${exec1.did}"]
        )
      }
    `);
  });

  test.afterAll(async () => {
    await stopFluxServer(flux1).catch(() => {});
    await Promise.all([
      stopExecutor(exec1).catch(() => {}),
      stopExecutor(exec2).catch(() => {}),
    ]);
  });

  test('SFU designated mode config persists and is queryable', async ({ browser }) => {
    // Verify config at API level
    const config = await gql(exec1, `
      { sfuConfig(neighbourhoodUrl: "${nhUrl}") {
        mode designatedPeer sfuPeers maxMeshParticipants
      } }
    `);
    const sfuConfig = config.sfuConfig as any;
    expect(sfuConfig.mode).toBe('designated');
    expect(sfuConfig.designatedPeer).toBe(exec1.did);
    expect(sfuConfig.maxMeshParticipants).toBe(0);

    // Start room in SFU mode
    await gql(exec1, `mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "sfu-lobby") { roomName } }`);

    // Verify peer lookup
    const peer = await gql(exec1, `{ sfuPeerForNeighbourhood(neighbourhoodUrl: "${nhUrl}") }`);
    expect(peer.sfuPeerForNeighbourhood).toBeTruthy();

    // Verify on second executor too
    await gql(exec2, `
      mutation {
        sfuSetConfig(
          neighbourhoodUrl: "${nhUrl}",
          mode: "designated",
          designatedPeer: "${exec1.did}",
          maxMeshParticipants: 0,
          sfuPeers: ["${exec1.did}"]
        )
      }
    `);
    const config2 = await gql(exec2, `{ sfuConfig(neighbourhoodUrl: "${nhUrl}") { mode designatedPeer } }`);
    expect((config2.sfuConfig as any).mode).toBe('designated');

    // Cleanup
    await gql(exec1, `mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "sfu-lobby") }`);
    console.log('✓ SFU designated mode configured and verified on both executors');
  });

  test('Flux loads with SFU config active', async ({ browser }) => {
    const page = await setupAuthenticatedPage(browser, flux1.url, exec1);

    try {
      await waitForFluxReady(page);
      expect(await page.locator('[data-testid="app-home"]').isVisible()).toBe(true);

      // Verify WebRTC tracking is active
      const stats = await getWebRTCStats(page);
      expect(Array.isArray(stats)).toBe(true);

      console.log('✓ Flux loaded with SFU designated mode active');
      console.log('NOTE: Full SFU call flow requires shared community');
    } finally {
      await page.context().close();
    }
  });
});
