// actions/app/screenshot.ts — Take a screenshot of the browser

import type { Action } from '../../lib/types.js';

const action: Action = {
  name: 'app/screenshot',
  description: 'Take a screenshot of the current browser page',
  params: {
    browserId: { type: 'string', description: 'Browser resource ID', required: true },
    path: { type: 'string', description: 'Output file path' },
  },

  async run(params, ctx) {
    const start = Date.now();
    const browserId = params.browserId as string;
    const browser = ctx.browser(browserId);

    if (!browser) {
      return { ok: false, error: `Browser ${browserId} not found`, duration_ms: Date.now() - start };
    }

    const driver = (browser as Record<string, unknown>)._driver;
    if (!driver || typeof (driver as Record<string, unknown>).screenshot !== 'function') {
      return { ok: false, error: 'Browser driver not available', duration_ms: Date.now() - start };
    }

    try {
      const outPath = (params.path as string) ?? `screenshot-${browserId}-${Date.now()}.png`;
      const d = driver as { screenshot: (path: string) => Promise<string> };
      const resultPath = await d.screenshot(outPath);

      return {
        ok: true,
        data: { path: resultPath, browserId },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `Screenshot failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
