import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import Ad4mConnect from './core';
import Ad4mConnectUI, { Ad4mConnectElement } from './web';
import { setLocal } from './utils';
import type { RemoteHost } from './types';

const options = {
  appInfo: { name: 'Test App', description: 'A test application', url: 'http://localhost:3000', iconPath: '/icon.png' },
  capabilities: [{ with: { domain: '*', pointers: ['*'] }, can: ['*'] }],
};

const host: RemoteHost = {
  id: 'host-1',
  name: 'Hosted node',
  profilePicUrl: '',
  location: '',
  url: 'https://hosted.example',
  rates: [],
  aiModels: [],
};

/** A core whose stored session fails to connect at load, as when its node is restarting. */
const unreachableAtLoad = () => {
  setLocal('ad4m-token', 'saved-jwt');
  const core = new Ad4mConnect(options);
  vi.spyOn(core, 'connect').mockRejectedValue(new Error('ECONNREFUSED'));
  const startCreditSubscription = vi.spyOn(core, 'startCreditSubscription').mockImplementation(() => {});
  return { core, startCreditSubscription };
};

describe('<ad4m-connect> stored session', () => {
  let element: Ad4mConnectElement | undefined;

  beforeEach(() => {
    localStorage.clear();
  });

  afterEach(() => {
    element?.remove();
    element = undefined;
  });

  it('starts credit tracking when the background retry reaches a hosted node', async () => {
    const { core, startCreditSubscription } = unreachableAtLoad();
    vi.spyOn(core, 'connectWhenReachable').mockImplementation(async () => {
      core.connectedHost = host;
      return {} as any;
    });

    element = Ad4mConnectUI(core);

    await vi.waitFor(() => expect(startCreditSubscription).toHaveBeenCalled());
    expect((element as any).selectedHost).toBe(host);
  });

  it('starts no credit tracking when the background retry gives up', async () => {
    const { core, startCreditSubscription } = unreachableAtLoad();
    core.connectedHost = host;
    const retry = vi.spyOn(core, 'connectWhenReachable').mockResolvedValue(undefined);

    element = Ad4mConnectUI(core);

    await vi.waitFor(() => expect(retry).toHaveBeenCalled());
    await retry.mock.results[0].value;
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(startCreditSubscription).not.toHaveBeenCalled();
  });
});
