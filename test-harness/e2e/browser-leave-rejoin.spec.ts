// e2e/browser-leave-rejoin.spec.ts — Browser-level leave/rejoin lifecycle
//
// Verifies room lifecycle at API level and Flux UI stability.

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

test.describe('Browser — Leave and Rejoin', () => {
  test.setTimeout(120_000);

  let exec1: ExecutorInstance;
  let flux1: FluxServer;
  const nhUrl = 'nh://browser-leave-rejoin-test';

  test.beforeAll(async () => {
    exec1 = await startExecutor({ holochain: false });
    flux1 = await startFluxServer();
  });

  test.afterAll(async () => {
    await stopFluxServer(flux1).catch(() => {});
    await stopExecutor(exec1).catch(() => {});
  });

  test('room start/stop cycle works correctly', async () => {
    // Start room
    await gql(exec1, `mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") { roomName } }`);
    let rooms = await gql(exec1, `{ sfuRooms { roomName participantCount } }`);
    expect((rooms.sfuRooms as any[]).length).toBe(1);
    expect((rooms.sfuRooms as any[])[0].participantCount).toBe(0);

    // Stop room (simulate leave)
    await gql(exec1, `mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") }`);
    rooms = await gql(exec1, `{ sfuRooms { roomName } }`);
    expect((rooms.sfuRooms as any[]).length).toBe(0);
    console.log('✓ Room stopped (user left)');

    // Restart room (simulate rejoin)
    await gql(exec1, `mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") { roomName } }`);
    rooms = await gql(exec1, `{ sfuRooms { roomName participantCount } }`);
    expect((rooms.sfuRooms as any[]).length).toBe(1);
    console.log('✓ Room restarted (user rejoined)');

    // Health check
    const health = await gql(exec1, `{ sfuHealth { roomCount eventLoopAlive } }`);
    expect((health.sfuHealth as any).eventLoopAlive).toBe(true);
    expect((health.sfuHealth as any).roomCount).toBe(1);

    // Cleanup
    await gql(exec1, `mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") }`);
  });

  test('Flux UI stable during room lifecycle', async ({ browser }) => {
    const page = await setupAuthenticatedPage(browser, flux1.url, exec1);

    try {
      await waitForFluxReady(page);
      expect(await page.locator('[data-testid="app-home"]').isVisible()).toBe(true);

      // Room lifecycle while Flux is loaded
      await gql(exec1, `mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") { roomName } }`);
      await page.waitForTimeout(500);
      expect(await page.locator('[data-testid="app-home"]').isVisible()).toBe(true);

      await gql(exec1, `mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") }`);
      await page.waitForTimeout(500);
      expect(await page.locator('[data-testid="app-home"]').isVisible()).toBe(true);

      await gql(exec1, `mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") { roomName } }`);
      await page.waitForTimeout(500);
      expect(await page.locator('[data-testid="app-home"]').isVisible()).toBe(true);

      await gql(exec1, `mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") }`);

      console.log('✓ Flux UI remained stable during room start/stop cycles');
    } finally {
      await page.context().close();
    }
  });
});
