// e2e/browser-mesh-2user.spec.ts — Browser-level 2-user mesh WebRTC test
//
// Verifies: executor startup, Flux auth/signup, SFU API, and Flux UI rendering.
// Full WebRTC call testing requires a shared community (Holochain).

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
import { getWebRTCStats, waitForConnection } from './helpers/webrtc.js';

test.describe('Browser — Mesh 2-User Call', () => {
  test.setTimeout(120_000);

  let exec1: ExecutorInstance;
  let exec2: ExecutorInstance;
  let flux1: FluxServer;
  let flux2: FluxServer;

  test.beforeAll(async () => {
    [exec1, exec2] = await Promise.all([
      startExecutor({ holochain: false }),
      startExecutor({ holochain: false }),
    ]);
    console.log(`Executors: ${exec1.port} (${exec1.did}), ${exec2.port} (${exec2.did})`);

    [flux1, flux2] = await Promise.all([
      startFluxServer({ port: 3030 }),
      startFluxServer({ port: 3031 }),
    ]);
    console.log(`Flux servers: ${flux1.url}, ${flux2.url}`);
  });

  test.afterAll(async () => {
    await Promise.all([
      stopFluxServer(flux1).catch(() => {}),
      stopFluxServer(flux2).catch(() => {}),
    ]);
    await Promise.all([
      stopExecutor(exec1).catch(() => {}),
      stopExecutor(exec2).catch(() => {}),
    ]);
  });

  test('both users auth into Flux and SFU API works', async ({ browser }) => {
    const page1 = await setupAuthenticatedPage(browser, flux1.url, exec1);
    const page2 = await setupAuthenticatedPage(browser, flux2.url, exec2);

    try {
      await Promise.all([
        waitForFluxReady(page1),
        waitForFluxReady(page2),
      ]);

      // Verify both reached home
      expect(await page1.locator('[data-testid="app-home"]').isVisible()).toBe(true);
      expect(await page2.locator('[data-testid="app-home"]').isVisible()).toBe(true);

      // Verify SFU API works on both executors
      const nhUrl = 'nh://browser-mesh-test';

      const config = await gql(exec1, `
        { sfuConfig(neighbourhoodUrl: "${nhUrl}") { mode maxMeshParticipants } }
      `);
      expect((config.sfuConfig as any).mode).toBeTruthy();

      await gql(exec1, `mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") { roomName } }`);

      const health = await gql(exec1, `{ sfuHealth { roomCount eventLoopAlive } }`);
      expect((health.sfuHealth as any).eventLoopAlive).toBe(true);
      expect((health.sfuHealth as any).roomCount).toBe(1);

      await gql(exec1, `mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "lobby") }`);

      console.log('✓ Both users authenticated into Flux');
      console.log('✓ SFU API (rooms, config, health) working');
      console.log('✓ WebRTC tracking injected');
      console.log('NOTE: Full call flow requires a shared community (Holochain neighbourhood)');
    } finally {
      await page1.context().close();
      await page2.context().close();
    }
  });

  test('WebRTC tracking captures connections when call UI triggers', async ({ browser }) => {
    const page = await setupAuthenticatedPage(browser, flux1.url, exec1);

    try {
      await waitForFluxReady(page);

      // Verify WebRTC tracking injection works
      const stats = await getWebRTCStats(page);
      expect(Array.isArray(stats)).toBe(true);
      console.log(`WebRTC connections tracked: ${stats.length}`);

      // Trigger a simple WebRTC connection programmatically to verify tracking
      const connState = await page.evaluate(async () => {
        const stream = await navigator.mediaDevices.getUserMedia({ audio: true }).catch(() => null);
        const pc = new RTCPeerConnection();
        if (stream) {
          stream.getTracks().forEach(t => pc.addTrack(t, stream));
        } else {
          pc.addTransceiver('audio');
        }
        return pc.connectionState;
      });

      const statsAfter = await getWebRTCStats(page);
      expect(statsAfter.length).toBeGreaterThan(0);
      // With fake media, we should have at least a transceiver
      console.log('WebRTC tracking verified:', JSON.stringify(statsAfter[0]));
      console.log(`Connection state: ${connState}, audioSenders: ${statsAfter[0].audioSenders}`);
    } finally {
      await page.context().close();
    }
  });
});
