import { applyUpdate, LiveQuery, QueryUpdate, Subscribed } from './LiveQuery';

describe('applyUpdate', () => {
    it('keys model results by instance id and takes the new order', () => {
        const old = { instances: [{ id: 'a', t: 1 }, { id: 'b', t: 1 }, { id: 'c', t: 1 }], totalCount: 3, nextCursor: 'x' };
        const next = applyUpdate(old, {
            subscriptionId: 's', revision: 1,
            added: [{ id: 'd', t: 1 }], removed: ['a'], changed: [{ id: 'b', t: 2 }],
            ids: ['c', 'b', 'd'], totalCount: 3,
        });
        expect(next).toEqual({ instances: [{ id: 'c', t: 1 }, { id: 'b', t: 2 }, { id: 'd', t: 1 }], totalCount: 3 });
        expect(old.instances).toHaveLength(3);
    });

    it('treats query rows as a multiset', () => {
        const next = applyUpdate([{ s: 'x' }, { s: 'x' }, { s: 'y' }], {
            subscriptionId: 's', revision: 1, added: [{ s: 'z' }], removed: [{ s: 'x' }, { s: 'y' }], changed: [],
        });
        expect(next).toEqual([{ s: 'x' }, { s: 'z' }]);
    });

    it('replaces a result that could not be keyed', () => {
        expect(applyUpdate(true, { subscriptionId: 's', revision: 1, reset: true, result: false })).toBe(false);
    });
});

/** A PerspectiveClient stand-in with the calls LiveQuery makes. */
function fakeClient() {
    let listener: ((u: QueryUpdate) => void) | undefined;
    let reconnect: (() => void) | undefined;
    const client = {
        onQueryUpdate: jest.fn((cb: (u: QueryUpdate) => void) => { listener = cb; return jest.fn(); }),
        onReconnect: jest.fn((cb: () => void) => { reconnect = cb; return jest.fn(); }),
        resyncSubscription: jest.fn(),
        disposeQuerySubscription: jest.fn().mockResolvedValue(true),
    };
    const update = (subscriptionId: string, revision: number, added: any[] = [], removed: any[] = []) =>
        listener!({ subscriptionId, revision, added, removed, changed: [] });
    return { client, update, reconnect: () => reconnect!() };
}

function deferred<T>() {
    let resolve!: (v: T) => void;
    const promise = new Promise<T>(r => { resolve = r; });
    return { promise, resolve };
}

const tick = () => new Promise(r => setTimeout(r, 0));

describe('LiveQuery', () => {
    it('returns the first result and passes later ones to onResult', async () => {
        const { client, update } = fakeClient();
        const onResult = jest.fn();
        const live = new LiveQuery(client as any, 'p', async () => ({ subscriptionId: 's1', result: [1], revision: 0 }), onResult);
        expect(await live.start()).toEqual([1]);
        expect(onResult).not.toHaveBeenCalled();

        update('other', 1, [9]);
        update('s1', 1, [2]);
        update('s1', 1, [3]);
        expect(onResult.mock.calls).toEqual([[[1, 2]]]);
        expect(live.id).toBe('s1');
    });

    it('applies updates that arrive before the subscribe reply', async () => {
        const { client, update } = fakeClient();
        const reply = deferred<Subscribed>();
        const live = new LiveQuery(client as any, 'p', () => reply.promise, jest.fn());
        const started = live.start();
        update('s1', 1, [2]);
        reply.resolve({ subscriptionId: 's1', result: [1], revision: 0 });
        await started;
        expect(live.result).toEqual([1, 2]);
    });

    it('resyncs on a revision gap and keeps the updates that arrive meanwhile', async () => {
        const { client, update } = fakeClient();
        const resync = deferred<{ revision: number, result: any }>();
        client.resyncSubscription.mockReturnValue(resync.promise);
        const onResult = jest.fn();
        const live = new LiveQuery(client as any, 'p', async () => ({ subscriptionId: 's1', result: [], revision: 0 }), onResult);
        await live.start();

        update('s1', 2, ['lost']);
        expect(client.resyncSubscription).toHaveBeenCalledWith('p', 's1');
        update('s1', 3, ['c']);
        update('s1', 4, ['d']);
        resync.resolve({ revision: 3, result: ['a', 'b', 'c'] });
        await tick();

        expect(live.result).toEqual(['a', 'b', 'c', 'd']);
        expect(onResult).toHaveBeenLastCalledWith(['a', 'b', 'c', 'd']);
    });

    it('opens a new subscription when a resync fails', async () => {
        const { client, update } = fakeClient();
        client.resyncSubscription.mockRejectedValue(new Error('RPC error 404'));
        const open = jest.fn()
            .mockResolvedValueOnce({ subscriptionId: 's1', result: [], revision: 0 })
            .mockResolvedValueOnce({ subscriptionId: 's2', result: ['fresh'], revision: 0 });
        const onResult = jest.fn();
        const live = new LiveQuery(client as any, 'p', open, onResult);
        await live.start();
        update('s1', 5);
        await tick();
        expect(live.id).toBe('s2');
        expect(onResult).toHaveBeenLastCalledWith(['fresh']);
    });

    it('re-opens after a reconnect and delivers the new result', async () => {
        const { client, update, reconnect } = fakeClient();
        const open = jest.fn()
            .mockResolvedValueOnce({ subscriptionId: 's1', result: ['old'], revision: 0 })
            .mockResolvedValueOnce({ subscriptionId: 's2', result: ['new'], revision: 0 });
        const onResult = jest.fn();
        const live = new LiveQuery(client as any, 'p', open, onResult);
        await live.start();
        reconnect();
        await tick();
        expect(onResult).toHaveBeenLastCalledWith(['new']);

        update('s1', 1, ['stale']);
        update('s2', 1, ['next']);
        expect(live.result).toEqual(['new', 'next']);
    });

    it('dispose ends the subscription and releases one that was still opening', async () => {
        const { client } = fakeClient();
        const reply = deferred<Subscribed>();
        const live = new LiveQuery(client as any, 'p', () => reply.promise, jest.fn());
        const started = live.start();
        live.dispose();
        reply.resolve({ subscriptionId: 's1', result: [], revision: 0 });
        await started;
        expect(client.disposeQuerySubscription).toHaveBeenCalledWith('p', 's1');
        expect(client.onQueryUpdate.mock.results[0].value).toHaveBeenCalled();
        expect(client.onReconnect.mock.results[0].value).toHaveBeenCalled();
    });
});
