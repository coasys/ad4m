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

  // ad4m-connect uses versioned localStorage keys: `${version}/key`
  // We set both versioned and unversioned to cover all cases.
  const portMatch = executorUrl.match(/:(\d+)/);
  const port = portMatch ? portMatch[1] : '12000';
  const wsUrl = executorUrl.replace(/^https?:/, 'ws:').replace('/graphql', '') + '/graphql';

  await page.evaluate(
    ({ wsUrl, port, token }) => {
      // Discover the version prefix by scanning existing keys
      // or try known versions from the ad4m-connect package
      const versions = ['0.13.0-test-1', '0.13.0', '0.12.0', '0.11.0'];

      // Set unversioned keys
      localStorage.setItem('ad4m-port', port);
      localStorage.setItem('ad4m-token', token);
      localStorage.setItem('ad4m-url', wsUrl);

      // Set versioned keys for all known versions
      for (const v of versions) {
        localStorage.setItem(`${v}/ad4m-port`, port);
        localStorage.setItem(`${v}/ad4m-token`, token);
        localStorage.setItem(`${v}/ad4m-url`, wsUrl);
      }
    },
    { wsUrl, port, token: adminCredential },
  );

  // Reload so the app picks up the injected credentials
  await page.reload({ waitUntil: 'domcontentloaded' });
}

/**
 * Complete the Flux signup form if it appears.
 * Sets a username and clicks the create button.
 */
export async function completeSignupIfNeeded(
  page: Page,
  username: string = 'e2e-test-user',
  timeout = 20_000,
): Promise<boolean> {
  // Wait for either app-home (already signed up) or signup form
  const appHome = page.locator('[data-testid="app-home"]');
  const signupInput = page.locator('j-input[label="Username"], input[placeholder*="username" i], j-input');

  const result = await Promise.race([
    appHome.waitFor({ timeout }).then(() => 'home' as const),
    signupInput.first().waitFor({ timeout }).then(() => 'signup' as const),
  ]).catch(() => 'timeout' as const);

  if (result === 'home') return true;
  if (result === 'timeout') return false;

  // Fill username — j-input is a web component, need to dispatch input event
  const input = signupInput.first();
  await input.click();

  // Try to find the inner input element inside the web component shadow DOM
  await page.evaluate((name) => {
    const jInputs = document.querySelectorAll('j-input');
    for (const ji of jInputs) {
      const inner = ji.shadowRoot?.querySelector('input');
      if (inner) {
        inner.value = name;
        inner.dispatchEvent(new Event('input', { bubbles: true }));
        inner.dispatchEvent(new Event('change', { bubbles: true }));
        // Also set value attribute on the j-input itself
        ji.setAttribute('value', name);
        // Dispatch on the j-input element too
        ji.dispatchEvent(new CustomEvent('input', { detail: { target: { value: name } }, bubbles: true }));
        break;
      }
    }
  }, username);

  await page.waitForTimeout(500);

  // Click the create/submit button
  const submitBtn = page.locator('j-button:has-text("Create"), j-button:has-text("Sign"), j-button:has(j-icon[name="check"])');
  if (await submitBtn.first().isVisible({ timeout: 3000 }).catch(() => false)) {
    await submitBtn.first().click();
  }

  // Wait for navigation to home
  try {
    await appHome.waitFor({ timeout: 15_000 });
    return true;
  } catch {
    return false;
  }
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
