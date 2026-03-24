// actions/app/connect.ts — Connect browser to app via ad4m-connect flow

import type { Action } from '../../lib/types.js';
import { BrowserDriver } from '../../lib/browser.js';
import { sleep } from '../../lib/retry.js';

const action: Action = {
  name: 'app/connect',
  description: 'Open browser, navigate to app, complete ad4m-connect authentication flow',
  params: {
    executorId: { type: 'string', description: 'Executor resource ID', required: true },
    appUrl: { type: 'string', description: 'Application URL', required: true },
    headless: { type: 'boolean', description: 'Run browser headless', default: true },
    realMedia: { type: 'boolean', description: 'Use real media devices', default: false },
    incognito: { type: 'boolean', description: 'Use incognito context', default: false },
  },

  async run(params, ctx) {
    const start = Date.now();
    const execId = params.executorId as string;
    const appUrl = params.appUrl as string;
    const exec = ctx.executor(execId);

    if (!exec) {
      return { ok: false, error: `Executor ${execId} not found`, duration_ms: Date.now() - start };
    }

    try {
      const driver = await BrowserDriver.create({
        headless: (params.headless as boolean) ?? true,
        realMedia: (params.realMedia as boolean) ?? false,
      });

      await driver.newContext({
        permissions: ['camera', 'microphone'],
      });
      await driver.newPage();

      // Inject WebRTC tracking before navigating
      await driver.injectWebRTCTracking();

      // Navigate to app
      await driver.navigate(appUrl);

      // Handle the ad4m-connect authentication flow
      // The flow typically involves:
      // 1. Click "Connect" or similar button on ad4m-connect overlay
      // 2. Enter executor URL if not auto-detected
      // 3. Accept/confirm connection

      // Wait for ad4m-connect dialog to appear
      try {
        // Look for ad4m-connect shadow DOM or known selectors
        await driver.waitForSelector('ad4m-connect', 5000);

        // Try to interact with the connect component
        // ad4m-connect uses shadow DOM, so we evaluate inside it
        await driver.evaluate(`
          (async () => {
            const el = document.querySelector('ad4m-connect');
            if (!el || !el.shadowRoot) return;
            const root = el.shadowRoot;

            // Set the port to match our executor
            const portInput = root.querySelector('input[type="number"]') || root.querySelector('input[placeholder*="port"]');
            if (portInput) {
              portInput.value = '${exec.port}';
              portInput.dispatchEvent(new Event('input', { bubbles: true }));
            }

            // Click connect button
            const buttons = root.querySelectorAll('button');
            for (const btn of buttons) {
              if (btn.textContent?.toLowerCase().includes('connect') || btn.textContent?.toLowerCase().includes('login')) {
                btn.click();
                break;
              }
            }
          })()
        `);

        // Wait for connection to establish
        await sleep(3000);

        // Accept any trust/capability prompts via the executor API
        const { ExecutorAPI } = await import('../../lib/api.js');
        const endpoint = `http://${exec.host === 'local' ? 'localhost' : exec.host}:${exec.port}/graphql`;
        const api = new ExecutorAPI(endpoint, exec.adminCredential as string | undefined);

        // Check if there are pending capability requests and auto-approve
        try {
          const res = await api.query('{ agentStatus { isUnlocked did } }');
          const status = (res.data as Record<string, Record<string, unknown>>)?.agentStatus;
          if (status?.did) {
            exec.did = status.did as string;
            ctx.resources.set(execId, exec);
          }
        } catch {
          // Non-fatal — agent may already be configured
        }

      } catch {
        // ad4m-connect might not be present (direct API app)
        // That's OK — browser is connected to the URL
      }

      const browserId = ctx.nextId('browser');
      ctx.resources.set(browserId, {
        kind: 'browser',
        id: browserId,
        url: appUrl,
        executorId: execId,
        _driver: driver, // Internal reference, not serialized
      });

      return {
        ok: true,
        data: {
          browserId,
          agentDid: exec.did,
          loggedIn: !!exec.did,
          url: appUrl,
        },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `Browser connect failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
