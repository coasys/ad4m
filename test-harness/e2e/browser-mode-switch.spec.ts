// e2e/browser-mode-switch.spec.ts — Browser-level mid-call mode switching
//
// Verifies mode switching at API level and that Flux UI remains stable.

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

test.describe('Browser — Mid-Call Mode Switch', () => {
  test.setTimeout(120_000);

  let exec1: ExecutorInstance;
  let flux1: FluxServer;
  const nhUrl = 'nh://browser-mode-switch-test';

  test.beforeAll(async () => {
    exec1 = await startExecutor({ holochain: false });
    flux1 = await startFluxServer();
  });

  test.afterAll(async () => {
    await stopFluxServer(flux1).catch(() => {});
    await stopExecutor(exec1).catch(() => {});
  });

  test('switch config from mesh to SFU to cascaded and back', async ({ browser }) => {
    // Start in mesh mode (default)
    const config1 = await gql(exec1, `{ sfuConfig(neighbourhoodUrl: "${nhUrl}") { mode } }`);
    console.log(`Initial mode: ${(config1.sfuConfig as any).mode}`);

    // Start a room
    await gql(exec1, `mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") { roomName } }`);

    // Switch to designated SFU
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
    const config2 = await gql(exec1, `{ sfuConfig(neighbourhoodUrl: "${nhUrl}") { mode designatedPeer maxMeshParticipants } }`);
    expect((config2.sfuConfig as any).mode).toBe('designated');
    expect((config2.sfuConfig as any).maxMeshParticipants).toBe(0);
    console.log('✓ Switched to designated SFU mode');

    // Switch to cascaded
    await gql(exec1, `
      mutation {
        sfuSetConfig(
          neighbourhoodUrl: "${nhUrl}",
          mode: "cascaded",
          sfuPeers: ["${exec1.did}", "did:key:fake-peer"],
          maxMeshParticipants: 2,
          maxParticipantsPerNode: 8
        )
      }
    `);
    const config3 = await gql(exec1, `{ sfuConfig(neighbourhoodUrl: "${nhUrl}") { mode sfuPeers maxParticipantsPerNode } }`);
    expect((config3.sfuConfig as any).mode).toBe('cascaded');
    console.log('✓ Switched to cascaded mode');

    // Switch back to mesh
    await gql(exec1, `
      mutation {
        sfuSetConfig(
          neighbourhoodUrl: "${nhUrl}",
          mode: "mesh",
          maxMeshParticipants: 4
        )
      }
    `);
    const config4 = await gql(exec1, `{ sfuConfig(neighbourhoodUrl: "${nhUrl}") { mode maxMeshParticipants } }`);
    expect((config4.sfuConfig as any).mode).toBe('mesh');
    expect((config4.sfuConfig as any).maxMeshParticipants).toBe(4);
    console.log('✓ Switched back to mesh mode');

    // Verify room survived config changes
    const rooms = await gql(exec1, `{ sfuRooms { roomName } }`);
    expect((rooms.sfuRooms as any[]).length).toBe(1);
    console.log('✓ Room persisted through all mode switches');

    // Cleanup
    await gql(exec1, `mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") }`);
  });

  test('Flux remains stable during config changes', async ({ browser }) => {
    const page = await setupAuthenticatedPage(browser, flux1.url, exec1);

    try {
      await waitForFluxReady(page);
      expect(await page.locator('[data-testid="app-home"]').isVisible()).toBe(true);

      // Rapid config changes while Flux is loaded
      for (const mode of ['designated', 'mesh', 'cascaded', 'mesh']) {
        await gql(exec1, `
          mutation {
            sfuSetConfig(
              neighbourhoodUrl: "${nhUrl}",
              mode: "${mode}",
              maxMeshParticipants: 4
              ${mode === 'designated' ? `, designatedPeer: "${exec1.did}", sfuPeers: ["${exec1.did}"]` : ''}
              ${mode === 'cascaded' ? `, sfuPeers: ["${exec1.did}"]` : ''}
            )
          }
        `);
      }

      // Flux should still be responsive
      await page.waitForTimeout(1000);
      expect(await page.locator('[data-testid="app-home"]').isVisible()).toBe(true);
      console.log('✓ Flux remained stable during rapid config changes');
    } finally {
      await page.context().close();
    }
  });
});
