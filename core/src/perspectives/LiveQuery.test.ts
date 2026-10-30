import { applyUpdate, LiveQuery, LiveQueryStatus, QueryLagged, QueryUpdate, reopenDelay, Subscribed } from './LiveQuery';

describe('applyUpdate', () => {
    it('keys model results by instance id and takes the new order', () => {
        const old = { instances: [{ id: 'a', t: 1 }, { id: 'b', t: 1 }, { id: 'c', t: 1 }], totalCount: 3 };
        const next = applyUpdate(old, {
            subscriptionId: 's', revision: 1,
            ids: ['c', 'b', 'd'], upsert: [{ id: 'b', t: 2 }, { id: 'd', t: 1 }], totalCount: 3,
        });
        expect(next).toEqual({ instances: [{ id: 'c', t: 1 }, { id: 'b', t: 2 }, { id: 'd', t: 1 }], totalCount: 3 });
        expect(old.instances).toHaveLength(3);
    });

    it('treats query rows as a multiset', () => {
        const next = applyUpdate([{ s: 'x' }, { s: 'x' }, { s: 'y' }], {
            subscriptionId: 's', revision: 1, added: [{ s: 'z' }], removed: [{ s: 'x' }, { s: 'y' }],
        });
        expect(next).toEqual([{ s: 'x' }, { s: 'z' }]);
    });

    it('throws on an id that neither the last result nor the update holds', () => {
        const old = { instances: [{ id: 'a' }], totalCount: 1 };
        expect(() => applyUpdate(old, { subscriptionId: 's', revision: 1, ids: ['a', 'x'], upsert: [], totalCount: 2 }))
            .toThrow('unknown instance x');
    });

    it('replaces a result that could not be diffed', () => {
        expect(applyUpdate(true, { subscriptionId: 's', revision: 1, result: false })).toBe(false);
    });
});

/** A PerspectiveClient stand-in with the calls LiveQuery makes. */
function fakeClient() {
    let listener: ((u: QueryUpdate | QueryLagged) => void) | undefined;
    let reconnect: (() => void) | undefined;
    const client = {
        onQueryUpdate: (cb: (u: QueryUpdate | QueryLagged) => void) => { listener = cb; return () => { listener = undefined; }; },
        onReconnect: (cb: () => void) => { reconnect = cb; return () => { reconnect = undefined; }; },
        resyncSubscription: jest.fn(),
        disposeQuerySubscription: jest.fn().mockResolvedValue(true),
    };
    const update = (subscriptionId: string, revision: number, added: any[] = [], removed: any[] = []) =>
        listener?.({ subscriptionId, revision, added, removed });
    return { client, update, send: (u: QueryUpdate | QueryLagged) => listener?.(u), reconnect: () => reconnect?.() };
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

    it('resyncs when the executor reports dropped updates', async () => {
        const { client, update, send } = fakeClient();
        client.resyncSubscription.mockResolvedValue({ revision: 7, result: ['a', 'b'] });
        const onResult = jest.fn();
        const live = new LiveQuery(client as any, 'p', async () => ({ subscriptionId: 's1', result: ['a'], revision: 0 }), onResult);
        await live.start();

        send({ lagged: true });
        expect(client.resyncSubscription).toHaveBeenCalledWith('p', 's1');
        await tick();
        expect(live.result).toEqual(['a', 'b']);
        expect(onResult).toHaveBeenLastCalledWith(['a', 'b']);
        update('s1', 8, ['c']);
        expect(live.result).toEqual(['a', 'b', 'c']);
    });

    it('resyncs instead of applying an update it cannot resolve', async () => {
        const { client, send } = fakeClient();
        client.resyncSubscription.mockResolvedValue({ revision: 1, result: { instances: [{ id: 'x' }], totalCount: 1 } });
        const onResult = jest.fn();
        const live = new LiveQuery(client as any, 'p', async () => ({ subscriptionId: 's1', result: { instances: [], totalCount: 0 }, revision: 0 }), onResult);
        await live.start();

        send({ subscriptionId: 's1', revision: 1, ids: ['x'], upsert: [], totalCount: 1 });
        expect(client.resyncSubscription).toHaveBeenCalledWith('p', 's1');
        await tick();
        expect(onResult.mock.calls).toEqual([[{ instances: [{ id: 'x' }], totalCount: 1 }]]);
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
        expect(client.disposeQuerySubscription).toHaveBeenCalledWith('p', 's1');
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

    it('a reconnect during the first open hands the caller the newer result', async () => {
        const { client, reconnect } = fakeClient();
        const first = deferred<Subscribed>();
        const open = jest.fn()
            .mockReturnValueOnce(first.promise)
            .mockResolvedValueOnce({ subscriptionId: 's2', result: ['new'], revision: 0 });
        const onResult = jest.fn();
        const live = new LiveQuery(client as any, 'p', open, onResult);
        const started = live.start();
        reconnect();
        first.resolve({ subscriptionId: 's1', result: ['old'], revision: 0 });
        expect(await started).toEqual(['new']);
        expect(client.disposeQuerySubscription).toHaveBeenCalledWith('p', 's1');
        expect(onResult).not.toHaveBeenCalled();
        expect(live.id).toBe('s2');
    });

    it('dispose ends the subscription and releases one that was still opening', async () => {
        const { client, update, reconnect } = fakeClient();
        const reply = deferred<Subscribed>();
        const open = jest.fn(() => reply.promise);
        const onResult = jest.fn();
        const live = new LiveQuery(client as any, 'p', open, onResult);
        const started = live.start();
        live.dispose();
        reply.resolve({ subscriptionId: 's1', result: [1], revision: 0 });
        expect(await started).toEqual([1]);
        expect(client.disposeQuerySubscription).toHaveBeenCalledWith('p', 's1');

        update('s1', 1, [2]);
        reconnect();
        await tick();
        expect(onResult).not.toHaveBeenCalled();
        expect(open).toHaveBeenCalledTimes(1);
    });

    describe('re-open after a reconnect (#1595)', () => {
        beforeEach(() => {
            jest.useFakeTimers();
            // The top of each backoff window: 1 s, 2 s, 4 s, ...
            jest.spyOn(Math, 'random').mockReturnValue(1);
            jest.spyOn(console, 'error').mockImplementation(() => {});
        });
        afterEach(() => {
            jest.useRealTimers();
            jest.restoreAllMocks();
        });

        it('retries a failed re-open with backoff until the perspective is back, and reports it', async () => {
            const { client, update, reconnect } = fakeClient();
            const notFound = new Error('Perspective 9995ba16 not found');
            const open = jest.fn()
                .mockResolvedValueOnce({ subscriptionId: 's1', result: ['a'], revision: 0 })
                .mockRejectedValueOnce(notFound)
                .mockRejectedValueOnce(notFound)
                .mockResolvedValueOnce({ subscriptionId: 's2', result: ['a', 'b'], revision: 0 });
            const onResult = jest.fn();
            const live = new LiveQuery(client as any, 'p', open, onResult);
            const statuses: LiveQueryStatus[] = [];
            live.onStatus(s => statuses.push(s));
            await live.start();

            reconnect();
            await jest.advanceTimersByTimeAsync(0);
            expect(open).toHaveBeenCalledTimes(2);
            expect(live.reopening).toBe(true);
            expect(statuses).toEqual([{ state: 'reopening', error: notFound, retryInMs: 1000 }]);

            await jest.advanceTimersByTimeAsync(999);
            expect(open).toHaveBeenCalledTimes(2);
            await jest.advanceTimersByTimeAsync(1);
            expect(open).toHaveBeenCalledTimes(3);
            expect(statuses[1]).toEqual({ state: 'reopening', error: notFound, retryInMs: 2000 });

            await jest.advanceTimersByTimeAsync(2000);
            expect(open).toHaveBeenCalledTimes(4);
            expect(live.reopening).toBe(false);
            expect(statuses[2]).toEqual({ state: 'open' });
            expect(live.id).toBe('s2');
            expect(onResult).toHaveBeenLastCalledWith(['a', 'b']);

            // Live again: updates to the new subscription apply.
            update('s2', 1, ['c']);
            expect(onResult).toHaveBeenLastCalledWith(['a', 'b', 'c']);

            // No retry is left over.
            await jest.advanceTimersByTimeAsync(120_000);
            expect(open).toHaveBeenCalledTimes(4);
        });

        it('stops retrying once disposed', async () => {
            const { client, reconnect } = fakeClient();
            const open = jest.fn()
                .mockResolvedValueOnce({ subscriptionId: 's1', result: [], revision: 0 })
                .mockRejectedValue(new Error('Perspective p not found'));
            const live = new LiveQuery(client as any, 'p', open, jest.fn());
            await live.start();
            reconnect();
            await jest.advanceTimersByTimeAsync(0);
            expect(open).toHaveBeenCalledTimes(2);
            live.dispose();
            await jest.advanceTimersByTimeAsync(120_000);
            expect(open).toHaveBeenCalledTimes(2);
        });

        it('a new reconnect starts the backoff over', async () => {
            const { client, reconnect } = fakeClient();
            const open = jest.fn()
                .mockResolvedValueOnce({ subscriptionId: 's1', result: [], revision: 0 })
                .mockRejectedValue(new Error('Perspective p not found'));
            const live = new LiveQuery(client as any, 'p', open, jest.fn());
            const statuses: LiveQueryStatus[] = [];
            live.onStatus(s => statuses.push(s));
            await live.start();
            reconnect();
            await jest.advanceTimersByTimeAsync(1000 + 2000);
            expect(open).toHaveBeenCalledTimes(4);
            reconnect();
            await jest.advanceTimersByTimeAsync(0);
            expect(open).toHaveBeenCalledTimes(5);
            expect(statuses.map(s => s.state === 'reopening' && s.retryInMs)).toEqual([1000, 2000, 4000, 1000]);
            // Only the newest chain of retries is left.
            await jest.advanceTimersByTimeAsync(1000);
            expect(open).toHaveBeenCalledTimes(6);
        });

        it('retries a re-open after a failed resync too', async () => {
            const { client, update } = fakeClient();
            client.resyncSubscription.mockRejectedValue(new Error('gone'));
            const open = jest.fn()
                .mockResolvedValueOnce({ subscriptionId: 's1', result: [], revision: 0 })
                .mockRejectedValueOnce(new Error('Perspective p not found'))
                .mockResolvedValueOnce({ subscriptionId: 's2', result: ['x'], revision: 0 });
            const onResult = jest.fn();
            const live = new LiveQuery(client as any, 'p', open, onResult);
            await live.start();
            update('s1', 2, ['x']);
            await jest.advanceTimersByTimeAsync(0);
            expect(open).toHaveBeenCalledTimes(2);
            await jest.advanceTimersByTimeAsync(1000);
            expect(open).toHaveBeenCalledTimes(3);
            expect(live.id).toBe('s2');
            expect(onResult).toHaveBeenLastCalledWith(['x']);
        });
    });

    it('dispose with cancelOpening aborts the subscribe call still in flight', async () => {
        const { client } = fakeClient();
        let signal: AbortSignal | undefined;
        const open = jest.fn((s: AbortSignal) => {
            signal = s;
            return new Promise<Subscribed>((_resolve, reject) =>
                s.addEventListener('abort', () => reject(new Error('Aborted'))));
        });
        const live = new LiveQuery(client as any, 'p', open, jest.fn());
        const started = live.start();
        expect(signal?.aborted).toBe(false);
        live.dispose({ cancelOpening: true });
        expect(signal?.aborted).toBe(true);
        await expect(started).rejects.toThrow('Aborted');
        expect(client.disposeQuerySubscription).not.toHaveBeenCalled();
    });

    it('a plain dispose leaves the subscribe call to finish', async () => {
        const { client } = fakeClient();
        const reply = deferred<Subscribed>();
        let signal: AbortSignal | undefined;
        const live = new LiveQuery(client as any, 'p', (s: AbortSignal) => { signal = s; return reply.promise; }, jest.fn());
        const started = live.start();
        live.dispose();
        expect(signal?.aborted).toBe(false);
        reply.resolve({ subscriptionId: 's1', result: [1], revision: 0 });
        expect(await started).toEqual([1]);
    });
});

describe('reopenDelay', () => {
    it('doubles from 1 s to a 60 s cap, between half and all of each step', () => {
        expect([1, 2, 3, 6, 7, 20].map(n => reopenDelay(n, () => 1))).toEqual([1000, 2000, 4000, 32000, 60000, 60000]);
        expect(reopenDelay(1, () => 0)).toBe(500);
        expect(reopenDelay(20, () => 0)).toBe(30000);
    });
});
