import Ad4mConnect, { Ad4mConnectOptions } from './core';
import Ad4mConnectUI from './web';
import { Ad4mClient } from '@coasys/ad4m';

/**
 * Smart factory function that returns an authenticated Ad4mClient.
 * Handles both embedded (iframe) and standalone (browser) modes automatically.
 * 
 * - In embedded mode: Waits for parent to send AD4M config via postMessage
 * - In standalone mode: Shows auth UI if needed, or auto-connects with stored token
 * 
 * @param options - Configuration options for ad4m-connect
 * @returns Promise that resolves with authenticated Ad4mClient
 */
export async function getAd4mClient(options: Ad4mConnectOptions): Promise<Ad4mClient> {
  // Check if we're in embedded mode
  const isEmbedded = typeof window !== 'undefined' && window.self !== window.top;
  
  if (isEmbedded) {
    // Embedded mode: wait for parent to send config and client to be ready
    console.log('[getAd4mClient] Embedded mode - waiting for AD4M config from parent');
    const core = new Ad4mConnect(options);
    
    // connect() returns a Promise that resolves when authenticated
    const client = await core.connect();
    return client;
  } else {
    // Standalone mode: create UI (if needed) and wait for authentication
    console.log('[getAd4mClient] Standalone mode - initializing with UI');
    const element = Ad4mConnectUI(options);
    const core = element.getCore();
    
    // connect() returns a Promise that resolves when authenticated
    // (either immediately if token exists, or after user authenticates via UI)
    const client = await core.connect();
    return client;
  }
}

// Legacy export - returns core instance for direct access
export function getAd4mConnect(options: Ad4mConnectOptions): Ad4mConnect {
  const isEmbedded = typeof window !== 'undefined' && window.self !== window.top;
  
  if (isEmbedded) {
    console.log('[getAd4mConnect] Embedded mode detected - core only, no UI');
    return new Ad4mConnect(options);
  } else {
    console.log('[getAd4mConnect] Standalone mode - creating UI');
    const element = Ad4mConnectUI(options);
    return element.getCore();
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
