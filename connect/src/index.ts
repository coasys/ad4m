import Ad4mConnect, { Ad4mConnectOptions } from './core';
import Ad4mConnectUI from './web';
import { Ad4mClient } from '@coasys/ad4m';

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
export function getAd4mConnect(options: Ad4mConnectOptions): {
  core: Ad4mConnect;
  client: Promise<Ad4mClient>;
} {
  const isEmbedded = typeof window !== 'undefined' && window.self !== window.top;
  
  if (isEmbedded) {
    console.log('[getAd4mConnect] Embedded mode - waiting for AD4M config from parent');
    const core = new Ad4mConnect(options);
    const client = core.connect();
    return { core, client };
  } else {
    console.log('[getAd4mConnect] Standalone mode - initializing with UI');
    const element = Ad4mConnectUI(options);
    const core = element.getCore();
    const client = core.connect();
    return { core, client };
  }
}

// Re-export core types and classes for direct usage
export { Ad4mConnect, Ad4mConnectUI };
export type { Ad4mConnectOptions };
export * from './core';

// Re-export utilities but exclude getAd4mClient (we define our own above)
export { 
  onAuthStateChanged, 
  detectOS, 
  setForVersion, 
  getForVersion, 
  removeForVersion, 
  DEFAULT_PORT 
} from './utils';
