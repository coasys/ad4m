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