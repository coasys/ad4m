import { Ad4mClient } from '@coasys/ad4m';
import Ad4mConnect, { Ad4mConnectOptions } from './core';

export { Ad4mConnect };
export type { Ad4mConnectOptions, AppEndpoints, AuthStates } from './core';
export type {
  ConnectAnswer,
  ConnectApp,
  ConnectDenied,
  ConnectReady,
  ConnectRenew,
  ConnectRenewed,
  ConnectRequest,
  ConnectSignOut,
  FromFrame,
  ToFrame,
} from './messages';
export { originOf } from './messages';
export { isEmbedded } from './utils';
/**
 * Opens the Connect frame and returns the core at once and the client as a promise, so listeners
 * can be attached before the person has signed in.
 *
 * @example
 * ```typescript
 * const { core, client } = getAd4mConnect({
 *   appId: 'coasys-app',
 *   endpoints: { authDomain: 'https://coasys.id' },
 *   capabilities: [{ with: { domain: '*', pointers: ['*'] }, can: ['*'] }],
 * });
 * core.addEventListener('authstatechange', (e) => console.log((e as CustomEvent).detail));
 * const ad4mClient = await client;
 * ```
 */
export function getAd4mConnect(options: Ad4mConnectOptions): { core: Ad4mConnect; client: Promise<Ad4mClient> } {
  const core = new Ad4mConnect(options);
  return { core, client: core.connect() };
}
