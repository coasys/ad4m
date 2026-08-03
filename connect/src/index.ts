import { Ad4mClient } from '@coasys/ad4m';
import Ad4mConnect from './core';
import Ad4mConnectUI from './web';
import { isEmbedded } from './utils';
import { Ad4mConnectOptions } from './types';

/**
 * Advanced API - returns core immediately and client as promise.
 * Allows attaching event listeners before authentication completes.
 * 
 * @example
 * ```typescript
 * const { core, client } = getAd4mConnect(options);
 * core.addEventListener('authstatechange', (e) => console.log(e.detail));
 * const ad4mClient = await client;
 * ```
 */
export function getAd4mConnect(options: Ad4mConnectOptions): { core: Ad4mConnect; client: Promise<Ad4mClient> } {
  // Build the core
  const core = new Ad4mConnect(options);

  if (isEmbedded()) {
    // Immediately return the core and client
    const client = core.connect();
    return { core, client };
  } else {
    // Build the UI element to allow the user to walk through authentication
    Ad4mConnectUI(core);

    // Setup a promise that resolves when authentication completes
    const client = new Promise<Ad4mClient>((resolve) => {
      core.addEventListener('authstatechange', (event: any) => {
        if (event.detail === 'authenticated' && core.ad4mClient) resolve(core.ad4mClient);
      });
    });

    return { core, client };
  }
}

/**
 * Guest fast-path API — skips the UI entirely and connects to a remote hosted
 * node using auto-generated credentials. Intended for demo/event scenarios
 * where you want new users to arrive directly in a conversation with no auth steps.
 *
 * Credentials are persisted in localStorage so refreshing the page reuses the
 * same guest identity. Pass the full HTTP URL of the AD4M executor as `hostUrl`.
 *
 * @example
 * ```typescript
 * const ad4mClient = await connectAsGuest(options, 'https://your-host.ad4m.dev');
 * ```
 */
export async function connectAsGuest(options: Ad4mConnectOptions, hostUrl: string): Promise<Ad4mClient> {
  const core = new Ad4mConnect({ ...options, url: hostUrl, hosting: false });
  return core.connectAsGuest(hostUrl);
}

/**
 * Simple API - returns just the authenticated client.
 * 
 * @example
 * ```typescript
 * const ad4mClient = await getAd4mClient(options);
 * ```
 */
export async function getAd4mClient(options: Ad4mConnectOptions): Promise<Ad4mClient> {
  const { client } = getAd4mConnect(options);
  return client;
}

// Re-export isEmbedded utility
export { isEmbedded };