/**
 * Type test: `link-updated` callbacks receive `{ oldLink, newLink }`, not a
 * single `LinkExpression`. ts-jest type-checks this file, so a wrong callback
 * type fails the suite at compile time.
 */
import { PerspectiveProxy } from './PerspectiveProxy';
import type { LinkUpdate, LinkUpdatedCallback } from './PerspectiveClient';
import { LinkExpression } from '../links/Links';

function linkExpression(target: string): LinkExpression {
  return { author: 'did:test:1', timestamp: '2024-01-01T00:00:00.000Z', data: { source: 's', predicate: 'p', target }, proof: { valid: true } } as unknown as LinkExpression;
}

describe('link-updated callback type', () => {
  it('types the callback argument as { oldLink, newLink }', async () => {
    let updatedCallbacks: LinkUpdatedCallback[] = [];
    const client: any = {
      addPerspectiveLinkAddedListener: jest.fn(),
      addPerspectiveLinkRemovedListener: jest.fn(),
      addPerspectiveLinkUpdatedListener: (_uuid: string, cbs: LinkUpdatedCallback[]) => { updatedCallbacks = cbs; },
      addPerspectiveSyncStateChangeListener: jest.fn(),
    };
    const proxy = new PerspectiveProxy(
      { uuid: 'test-uuid', name: 'test', owners: [], sharedUrl: null, neighbourhood: null, state: 'Synced' } as any,
      client,
    );

    const seen: string[] = [];
    await proxy.addListener('link-updated', (update) => {
      // Compiles only if `update` is typed as LinkUpdate.
      seen.push(`${update.oldLink.data.target}->${update.newLink.data.target}`);
    });
    // @ts-expect-error a link-updated callback does not receive a LinkExpression
    await proxy.addListener('link-updated', (link: LinkExpression) => {});
    // link-added keeps its LinkExpression callback type
    await proxy.addListener('link-added', (link: LinkExpression) => {});

    const update: LinkUpdate = { oldLink: linkExpression('old'), newLink: linkExpression('new') };
    updatedCallbacks[0](update);
    expect(seen).toEqual(['old->new']);
  });
});
