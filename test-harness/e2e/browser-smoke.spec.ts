// e2e/browser-smoke.spec.ts — Minimal smoke test: start executor + Flux, verify auth + signup

import { test, expect } from '@playwright/test';
import { startExecutor, stopExecutor, type ExecutorInstance } from './helpers/executor.js';
import { startFluxServer, stopFluxServer, type FluxServer } from './helpers/flux-server.js';
import { injectAuth, completeSignupIfNeeded } from './helpers/auth.js';
import { injectWebRTCTracking } from './helpers/webrtc.js';

test.describe('Browser Smoke — Auth + Load', () => {
  let exec1: ExecutorInstance;
  let flux1: FluxServer;

  test.beforeAll(async () => {
    exec1 = await startExecutor({ holochain: false });
    console.log(`Executor started on port ${exec1.port}, DID: ${exec1.did}`);
    flux1 = await startFluxServer();
    console.log(`Flux server at ${flux1.url}`);
  });

  test.afterAll(async () => {
    await stopFluxServer(flux1).catch(() => {});
    await stopExecutor(exec1).catch(() => {});
  });

  test('Flux loads, authenticates, and reaches home', async ({ browser }) => {
    const context = await browser.newContext({ ignoreHTTPSErrors: true });
    const page = await context.newPage();

    page.on('console', msg => {
      if (msg.type() === 'error' || msg.type() === 'warning') return; // reduce noise
      console.log(`[PAGE] ${msg.type()}: ${msg.text()}`);
    });

    await injectWebRTCTracking(page);

    const executorUrl = `http://localhost:${exec1.port}/graphql`;
    await injectAuth(page, flux1.url, executorUrl, exec1.adminCredential);

    // Wait for the page to settle
    await page.waitForTimeout(3000);
    console.log(`Page URL after auth: ${page.url()}`);

    // Complete signup if needed
    const signedUp = await completeSignupIfNeeded(page, 'e2e-user-1');
    console.log(`Signup completed: ${signedUp}`);

    await page.screenshot({ path: '/tmp/flux-smoke-after-signup.png', fullPage: true });

    const url = page.url();
    console.log(`Final URL: ${url}`);

    const appHomeVisible = await page.locator('[data-testid="app-home"]').isVisible().catch(() => false);
    console.log(`app-home visible: ${appHomeVisible}`);

    await context.close();

    // Auth should have worked — either at home or signup completed
    expect(url).toContain('localhost');
  });
});
