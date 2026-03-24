// actions/webrtc/call.ts — Start, join, or leave a WebRTC call

import type { Action } from '../../lib/types.js';
import { sleep } from '../../lib/retry.js';

const action: Action = {
  name: 'webrtc/call',
  description: 'Start, join, or leave a WebRTC call in the browser',
  params: {
    browserId: { type: 'string', description: 'Browser resource ID', required: true },
    action: { type: 'string', description: 'Action: start | join | leave', required: true },
  },

  async run(params, ctx) {
    const start = Date.now();
    const browserId = params.browserId as string;
    const callAction = params.action as string;
    const browser = ctx.browser(browserId);

    if (!browser) {
      return { ok: false, error: `Browser ${browserId} not found`, duration_ms: Date.now() - start };
    }

    const driver = (browser as Record<string, unknown>)._driver;
    if (!driver || typeof (driver as Record<string, unknown>).evaluate !== 'function') {
      return { ok: false, error: 'Browser driver not available', duration_ms: Date.now() - start };
    }

    const d = driver as { evaluate: <T>(fn: string) => Promise<T>; click: (sel: string) => Promise<void>; waitForSelector: (sel: string, t?: number) => Promise<void> };

    try {
      switch (callAction) {
        case 'start':
        case 'join': {
          // Look for call/join button in the UI
          // Flux uses various selectors for call buttons
          await d.evaluate(`
            (() => {
              const callBtn = document.querySelector('[data-testid="call-button"]')
                || document.querySelector('[data-testid="join-call"]')
                || document.querySelector('button[aria-label*="call"]')
                || document.querySelector('.call-button');
              if (callBtn) callBtn.click();
            })()
          `);

          // Wait for call to establish
          await sleep(3000);

          // Check participant count
          const participants = await d.evaluate<number>(`
            (() => {
              const pcs = window.__rtcPeerConnections || [];
              return pcs.filter(pc => pc.connectionState === 'connected').length + 1;
            })()
          `);

          return {
            ok: true,
            data: { inCall: true, participants, action: callAction, browserId },
            duration_ms: Date.now() - start,
          };
        }

        case 'leave': {
          await d.evaluate(`
            (() => {
              const leaveBtn = document.querySelector('[data-testid="leave-call"]')
                || document.querySelector('[data-testid="hangup"]')
                || document.querySelector('button[aria-label*="leave"]')
                || document.querySelector('.leave-button');
              if (leaveBtn) leaveBtn.click();
            })()
          `);

          await sleep(1000);

          return {
            ok: true,
            data: { inCall: false, participants: 0, action: 'leave', browserId },
            duration_ms: Date.now() - start,
          };
        }

        default:
          return { ok: false, error: `Unknown call action: ${callAction}`, duration_ms: Date.now() - start };
      }
    } catch (err) {
      return {
        ok: false,
        error: `Call action failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
