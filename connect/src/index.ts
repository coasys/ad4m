import Ad4mConnect, { Ad4mConnectOptions } from './core';
import Ad4mConnectUI from './web';
import { Ad4mClient } from '@coasys/ad4m';
import { isEmbedded } from './utils';

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
  if (isEmbedded()) {
    console.log('[getAd4mConnect] Embedded mode - waiting for AD4M config from parent');
    const core = new Ad4mConnect(options);
    const client = core.connect();
    return { core, client };
  } else {
    console.log('[getAd4mConnect] Standalone mode - initializing with UI');
    const element = Ad4mConnectUI(options);
    const core = element.getCore();
    
    // In standalone mode, the UI will handle the connection flow
    // (user chooses local vs remote, then UI calls core.connect())
    // We return a Promise that resolves when the UI completes auth
    const client = new Promise<Ad4mClient>((resolve) => {
      core.addEventListener('authstatechange', (event: any) => {
        if (event.detail === 'authenticated' && core.ad4mClient) {
          resolve(core.ad4mClient);
        }
      });
    });
    
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
  isEmbedded,
  DEFAULT_PORT 
} from './utils';
