// e2e/helpers/auth.ts — Authentication helpers for E2E tests

import type { Page } from '@playwright/test';
import type { ChildProcess } from 'node:child_process';

/**
 * Approach A: Admin bypass — inject auth token directly into localStorage.
 * Fast, suitable for most tests.
 */
export async function injectAuth(
  page: Page,
  fluxUrl: string,
  executorUrl: string,
  adminCredential: string,
): Promise<void> {
  // Navigate to origin first so localStorage is accessible
  await page.goto(fluxUrl, { waitUntil: 'domcontentloaded' });

  // Inject ad4m-connect auth tokens
  await page.evaluate(
    ({ url, token }) => {
      localStorage.setItem('ad4m-token', token);
      localStorage.setItem('ad4m-url', url);
      // ad4m-connect may use versioned keys — set common variants
      localStorage.setItem('ad4m-connect-token', token);
      localStorage.setItem('ad4m-connect-url', url);
    },
    { url: executorUrl, token: adminCredential },
  );

  // Reload so the app picks up the injected credentials
  await page.reload({ waitUntil: 'domcontentloaded' });
}

/**
 * Approach B: Full ad4m-connect flow — interact with the connect dialog.
 * For auth integration tests. Reads the security code from executor stdout.
 */
export async function authViaConnect(
  page: Page,
  executorProcess: ChildProcess,
): Promise<void> {
  // Click the connect button (try data-testid first, fall back to text)
  const connectBtn = page.locator('[data-testid="connect-button"], button:has-text("Connect"), button:has-text("connect")');
  await connectBtn.first().click({ timeout: 10_000 });

  // Wait for security code from executor stdout
  const code = await waitForSecurityCode(executorProcess);

  // Enter the code
  const codeInput = page.locator('[data-testid="security-code-input"], input[type="text"], input[placeholder*="code"]');
  await codeInput.first().fill(code, { timeout: 10_000 });

  // Confirm
  const confirmBtn = page.locator('[data-testid="confirm-code-button"], button:has-text("Confirm"), button:has-text("Submit")');
  await confirmBtn.first().click({ timeout: 10_000 });

  // Wait for auth to complete
  const appHome = page.locator('[data-testid="app-home"], [data-testid="main-view"], main');
  await appHome.first().waitFor({ timeout: 15_000 });
}

/**
 * Parse the security code from executor stdout.
 */
function waitForSecurityCode(proc: ChildProcess, timeoutMs = 30_000): Promise<string> {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => reject(new Error('Timed out waiting for security code')), timeoutMs);
    let buffer = '';

    const handler = (chunk: Buffer) => {
      buffer += chunk.toString();
      // The executor prints the security code in various formats
      const match = buffer.match(/security[- ]?code[:\s]+(\d{6})/i)
        ?? buffer.match(/code[:\s]+(\d{6})/i)
        ?? buffer.match(/(\d{6})/);
      if (match) {
        clearTimeout(timer);
        proc.stdout?.off('data', handler);
        resolve(match[1]);
      }
    };

    proc.stdout?.on('data', handler);
  });
}
