// actions/app/navigate.ts — Navigate browser to a target within the app

import type { Action } from '../../lib/types.js';

const action: Action = {
  name: 'app/navigate',
  description: 'Navigate browser to a target within the app (community, channel, settings, or custom path)',
  params: {
    browserId: { type: 'string', description: 'Browser resource ID', required: true },
    target: { type: 'string', description: 'Navigation target: community/<id>, channel/<id>, settings, or a URL path', required: true },
  },

  async run(params, ctx) {
    const start = Date.now();
    const browserId = params.browserId as string;
    const target = params.target as string;
    const browser = ctx.browser(browserId);

    if (!browser) {
      return { ok: false, error: `Browser ${browserId} not found`, duration_ms: Date.now() - start };
    }

    const driver = (browser as Record<string, unknown>)._driver;
    if (!driver || typeof (driver as Record<string, unknown>).navigate !== 'function') {
      return { ok: false, error: 'Browser driver not available (session may have been restored from disk)', duration_ms: Date.now() - start };
    }

    try {
      const d = driver as { navigate: (url: string) => Promise<void>; evaluate: <T>(fn: string) => Promise<T> };

      // Determine URL based on target type
      let url: string;
      if (target.startsWith('http://') || target.startsWith('https://') || target.startsWith('/')) {
        url = target.startsWith('/') ? `${browser.url}${target}` : target;
      } else if (target.startsWith('community/')) {
        // Flux-style navigation — click on community in sidebar
        const communityId = target.replace('community/', '');
        await d.evaluate(`
          document.querySelector('[data-community-id="${communityId}"]')?.click()
          || document.querySelector('.community-item')?.click()
        `);
        url = browser.url;
      } else if (target.startsWith('channel/')) {
        const channelId = target.replace('channel/', '');
        await d.evaluate(`
          document.querySelector('[data-channel-id="${channelId}"]')?.click()
          || document.querySelector('.channel-item')?.click()
        `);
        url = browser.url;
      } else if (target === 'settings') {
        await d.evaluate(`
          document.querySelector('[data-testid="settings"]')?.click()
          || document.querySelector('.settings-btn')?.click()
        `);
        url = browser.url;
      } else {
        url = `${browser.url}/${target}`;
        await d.navigate(url);
      }

      const title = await d.evaluate<string>('document.title');

      return {
        ok: true,
        data: { url, title, browserId },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `Navigation failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
