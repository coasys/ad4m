// e2e/helpers/browser-setup.ts — Browser page setup helpers for E2E tests

import type { Page, BrowserContext, Browser } from '@playwright/test';
import { injectWebRTCTracking } from './webrtc.js';
import { injectAuth, completeSignupIfNeeded } from './auth.js';
import type { ExecutorInstance } from './executor.js';

export interface BrowserPage {
  page: Page;
  context: BrowserContext;
}

/**
 * Create a new browser context with fake media and WebRTC tracking,
 * navigate to Flux, and inject auth.
 */
export async function setupAuthenticatedPage(
  browser: Browser,
  fluxUrl: string,
  executor: ExecutorInstance,
): Promise<Page> {
  const context = await browser.newContext({
    ignoreHTTPSErrors: true,
  });

  const page = await context.newPage();

  // Inject WebRTC tracking before any navigation
  await injectWebRTCTracking(page);

  // Navigate and inject auth
  const executorUrl = `http://localhost:${executor.port}/graphql`;
  await injectAuth(page, fluxUrl, executorUrl, executor.adminCredential);

  return page;
}

/**
 * Wait for the Flux app to fully load after auth.
 * Completes signup if needed.
 */
export async function waitForFluxReady(page: Page, timeout = 30_000): Promise<void> {
  // Wait a bit for initial load
  await page.waitForTimeout(2000);

  // Complete signup if it appears
  const signedUp = await completeSignupIfNeeded(page, `e2e-user-${Date.now()}`, timeout);
  if (signedUp) return;

  // Fall back to waiting for any main content
  try {
    await page.waitForSelector('[data-testid="app-home"]', { timeout: 10_000 });
  } catch {
    await page.waitForSelector('main, #app', { timeout: 5000 });
  }
}

/**
 * GraphQL helper — call executor API from test code.
 */
export async function gql(
  executor: ExecutorInstance,
  query: string,
): Promise<Record<string, unknown>> {
  const result = await executor.api.query(query);
  if (result.errors) {
    throw new Error(`GraphQL error: ${JSON.stringify(result.errors)}`);
  }
  return result.data ?? {};
}
