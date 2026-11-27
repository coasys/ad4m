import { applyUpdate, LiveSubscription, LiveQueryStatus, QueryLagged, QueryUpdate, reopenDelay, Subscribed } from './LiveQuery';

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

/** A live query over the fake client; the result is the raw rows. */
function open(client: any, openCall: (signal: AbortSignal) => Promise<Subscribed>) {
    return new LiveSubscription(client, 'p', openCall);
}

describe('LiveSubscription', () => {
    it('opens on the first result, marked initial, and marks later changes not initial', async () => {
        const { client, update } = fakeClient();
        const live = open(client, async () => ({ subscriptionId: 's1', result: [1], revision: 0 }));
        expect(live.getSnapshot()).toMatchObject({ status: 'loading', initial: false });

        expect(await live.first).toEqual([1]);
        expect(live.getSnapshot()).toMatchObject({ status: 'ready', data: [1], initial: true, revision: 0 });

        update('other', 1, [9]);
        update('s1', 1, [2]);
        update('s1', 1, [3]);
        expect(live.getSnapshot()).toMatchObject({ data: [1, 2], initial: false, revision: 1 });
    });

    it('getSnapshot returns the same object until something changes', async () => {
        const { client, update } = fakeClient();
        const live = open(client, async () => ({ subscriptionId: 's1', result: [1], revision: 0 }));
        const loading = live.getSnapshot();
        expect(live.getSnapshot()).toBe(loading);
        await live.first;
        const ready = live.getSnapshot();
        expect(ready).not.toBe(loading);
        expect(live.getSnapshot()).toBe(ready);
        update('s1', 1, [2]);
        expect(live.getSnapshot()).not.toBe(ready);
    });

    it('subscribe calls the listener at once when a snapshot exists, then once per change', async () => {
        const { client, update } = fakeClient();
        const live = open(client, async () => ({ subscriptionId: 's1', result: [1], revision: 0 }));
        await live.first;
        const seen = jest.fn();
        const off = live.subscribe(seen);
        expect(seen).toHaveBeenCalledTimes(1);
        expect(seen.mock.calls[0][0]).toMatchObject({ data: [1], initial: true });

        update('s1', 1, [2]);
        expect(seen).toHaveBeenCalledTimes(2);
        expect(seen.mock.calls[1][0]).toMatchObject({ data: [1, 2], initial: false });

        off();
        update('s1', 2, [3]);
        expect(seen).toHaveBeenCalledTimes(2);
    });

    it('subscribe before the first value waits for it', async () => {
        const { client } = fakeClient();
        const reply = deferred<Subscribed>();
        const live = open(client, () => reply.promise);
        const seen = jest.fn();
        live.subscribe(seen);
        expect(seen).not.toHaveBeenCalled();

        reply.resolve({ subscriptionId: 's1', result: [1], revision: 0 });
        await live.first;
        expect(seen).toHaveBeenCalledTimes(1);
        expect(seen.mock.calls[0][0]).toMatchObject({ data: [1], initial: true });
    });

    it('a listener that throws does not stop the others', async () => {
        jest.spyOn(console, 'error').mockImplementation(() => {});
        try {
            const { client, update } = fakeClient();
            const live = open(client, async () => ({ subscriptionId: 's1', result: [], revision: 0 }));
            await live.first;
            const after = jest.fn();
            live.subscribe(() => { throw new Error('consumer failed'); });
            live.subscribe(after);
            after.mockClear();
            update('s1', 1, [2]);
            expect(after).toHaveBeenCalledTimes(1);
        } finally {
            jest.restoreAllMocks();
        }
    });

    it('applies updates that arrive before the subscribe reply', async () => {
        const { client, update } = fakeClient();
        const reply = deferred<Subscribed>();
        const live = open(client, () => reply.promise);
        update('s1', 1, [2]);
        reply.resolve({ subscriptionId: 's1', result: [1], revision: 0 });
        await live.first;
        expect(live.getSnapshot().data).toEqual([1, 2]);
    });

    it('an update that leaves the result as it was emits nothing', async () => {
        const { client, update } = fakeClient();
        const live = open(client, async () => ({ subscriptionId: 's1', result: [{ a: 1 }], revision: 0 }));
        await live.first;
        const seen = jest.fn();
        live.subscribe(seen);
        seen.mockClear();
        const before = live.getSnapshot();

        update('s1', 1, [], []);
        expect(seen).not.toHaveBeenCalled();
        expect(live.getSnapshot()).toBe(before);

        // A row removed and added back in one delta leaves the result as it was.
        update('s1', 2, [{ a: 1 }], [{ a: 1 }]);
        expect(seen).not.toHaveBeenCalled();
        expect(live.getSnapshot()).toBe(before);

        update('s1', 3, [{ a: 2 }], []);
        expect(seen).toHaveBeenCalledTimes(1);
    });

    it('a resync that returns the same result emits nothing and keeps initial', async () => {
        const { client, update } = fakeClient();
        client.resyncSubscription.mockResolvedValue({ revision: 5, result: [{ b: 1, a: 1 }] });
        const live = open(client, async () => ({ subscriptionId: 's1', result: [{ a: 1, b: 1 }], revision: 0 }));
        await live.first;
        const seen = jest.fn();
        live.subscribe(seen);
        seen.mockClear();
        const before = live.getSnapshot();

        update('s1', 3);
        await tick();

        expect(client.resyncSubscription).toHaveBeenCalledWith('p', 's1');
        expect(seen).not.toHaveBeenCalled();
        expect(live.getSnapshot()).toBe(before);
        expect(live.getSnapshot().initial).toBe(true);
    });

    it('resyncs on a revision gap and keeps the updates that arrive meanwhile', async () => {
        const { client, update } = fakeClient();
        const resync = deferred<{ revision: number, result: any }>();
        client.resyncSubscription.mockReturnValue(resync.promise);
        const live = open(client, async () => ({ subscriptionId: 's1', result: [], revision: 0 }));
        await live.first;

        update('s1', 2, ['lost']);
        expect(client.resyncSubscription).toHaveBeenCalledWith('p', 's1');
        update('s1', 3, ['c']);
        update('s1', 4, ['d']);
        resync.resolve({ revision: 3, result: ['a', 'b', 'c'] });
        await tick();

        expect(live.getSnapshot()).toMatchObject({ data: ['a', 'b', 'c', 'd'], initial: false });
    });

    it('resyncs when the executor reports dropped updates', async () => {
        const { client, update, send } = fakeClient();
        client.resyncSubscription.mockResolvedValue({ revision: 7, result: ['a', 'b'] });
        const live = open(client, async () => ({ subscriptionId: 's1', result: ['a'], revision: 0 }));
        await live.first;

        send({ lagged: true });
        expect(client.resyncSubscription).toHaveBeenCalledWith('p', 's1');
        await tick();
        expect(live.getSnapshot().data).toEqual(['a', 'b']);
        update('s1', 8, ['c']);
        expect(live.getSnapshot().data).toEqual(['a', 'b', 'c']);
    });

    it('resyncs instead of applying an update it cannot resolve', async () => {
        const { client, send } = fakeClient();
        client.resyncSubscription.mockResolvedValue({ revision: 1, result: { instances: [{ id: 'x' }], totalCount: 1 } });
        const live = new LiveSubscription(client as any, 'p', async () => ({ subscriptionId: 's1', result: { instances: [], totalCount: 0 }, revision: 0 }));
        await live.first;

        send({ subscriptionId: 's1', revision: 1, ids: ['x'], upsert: [], totalCount: 1 });
        expect(client.resyncSubscription).toHaveBeenCalledWith('p', 's1');
        await tick();
        expect(live.getSnapshot().data).toEqual({ instances: [{ id: 'x' }], totalCount: 1 });
    });

    it('opens a new subscription when a resync fails', async () => {
        const { client, update } = fakeClient();
        client.resyncSubscription.mockRejectedValue(new Error('RPC error 404'));
        const openCall = jest.fn()
            .mockResolvedValueOnce({ subscriptionId: 's1', result: [], revision: 0 })
            .mockResolvedValueOnce({ subscriptionId: 's2', result: ['fresh'], revision: 0 });
        const live = open(client, openCall);
        await live.first;
        update('s1', 5);
        await tick();
        expect(live.getSnapshot()).toMatchObject({ data: ['fresh'], initial: false });
        expect(client.disposeQuerySubscription).toHaveBeenCalledWith('p', 's1');
    });

    it('re-opens after a reconnect and delivers the new result as a change', async () => {
        const { client, update, reconnect } = fakeClient();
        const openCall = jest.fn()
            .mockResolvedValueOnce({ subscriptionId: 's1', result: ['old'], revision: 0 })
            .mockResolvedValueOnce({ subscriptionId: 's2', result: ['new'], revision: 0 });
        const live = open(client, openCall);
        await live.first;
        reconnect();
        await tick();
        expect(live.getSnapshot()).toMatchObject({ data: ['new'], initial: false });

        update('s1', 1, ['stale']);
        update('s2', 1, ['next']);
        expect(live.getSnapshot().data).toEqual(['new', 'next']);
    });

    it('a re-open that yields the same result emits nothing', async () => {
        const { client, reconnect } = fakeClient();
        const openCall = jest.fn()
            .mockResolvedValueOnce({ subscriptionId: 's1', result: ['same'], revision: 0 })
            .mockResolvedValueOnce({ subscriptionId: 's2', result: ['same'], revision: 0 });
        const live = open(client, openCall);
        await live.first;
        const seen = jest.fn();
        live.subscribe(seen);
        seen.mockClear();

        reconnect();
        await tick();

        expect(openCall).toHaveBeenCalledTimes(2);
        expect(seen).not.toHaveBeenCalled();
        expect(live.getSnapshot().initial).toBe(true);
    });

    it('a reconnect during the first open hands the caller the newer result', async () => {
        const { client, reconnect } = fakeClient();
        const first = deferred<Subscribed>();
        const openCall = jest.fn()
            .mockReturnValueOnce(first.promise)
            .mockResolvedValueOnce({ subscriptionId: 's2', result: ['new'], revision: 0 });
        const live = open(client, openCall);
        const seen = jest.fn();
        live.subscribe(seen);
        reconnect();
        first.resolve({ subscriptionId: 's1', result: ['old'], revision: 0 });
        expect(await live.first).toEqual(['new']);
        expect(client.disposeQuerySubscription).toHaveBeenCalledWith('p', 's1');
        expect(seen).toHaveBeenCalledTimes(1);
        expect(seen.mock.calls[0][0]).toMatchObject({ data: ['new'], initial: true });
    });

    it('a failed first open sets the error status, rejects first, tells listeners and disposes', async () => {
        const { client, reconnect } = fakeClient();
        const openCall = jest.fn().mockRejectedValue(new Error('refused'));
        const live = open(client, openCall);
        const seen = jest.fn();
        live.subscribe(seen);

        await expect(live.first).rejects.toThrow('refused');

        expect(live.getSnapshot()).toMatchObject({ status: 'error' });
        expect(live.getSnapshot().error?.message).toBe('refused');
        expect(seen).toHaveBeenCalledTimes(1);
        reconnect();
        await tick();
        expect(openCall).toHaveBeenCalledTimes(1);
    });

    it('dispose before the first value aborts the open, rejects first and never calls a listener', async () => {
        const { client } = fakeClient();
        let signal: AbortSignal | undefined;
        const live = open(client, (s: AbortSignal) => {
            signal = s;
            return new Promise<Subscribed>((_resolve, reject) =>
                s.addEventListener('abort', () => reject(new Error('Aborted'))));
        });
        const seen = jest.fn();
        live.subscribe(seen);
        expect(signal?.aborted).toBe(false);

        live.dispose();

        expect(signal?.aborted).toBe(true);
        await expect(live.first).rejects.toThrow('disposed');
        expect(seen).not.toHaveBeenCalled();
        expect(client.disposeQuerySubscription).not.toHaveBeenCalled();
    });

    it('dispose releases a subscription whose reply still arrives, and ignores later events', async () => {
        const { client, update, reconnect } = fakeClient();
        const reply = deferred<Subscribed>();
        const openCall = jest.fn(() => reply.promise);
        const live = open(client, openCall);
        const seen = jest.fn();
        live.subscribe(seen);
        live.dispose();
        reply.resolve({ subscriptionId: 's1', result: [1], revision: 0 });
        await tick();
        expect(client.disposeQuerySubscription).toHaveBeenCalledWith('p', 's1');

        update('s1', 1, [2]);
        reconnect();
        await tick();
        expect(seen).not.toHaveBeenCalled();
        expect(openCall).toHaveBeenCalledTimes(1);
    });

    it('dispose ends the subscription once, however often it is called', async () => {
        const { client } = fakeClient();
        const live = open(client, async () => ({ subscriptionId: 's1', result: [], revision: 0 }));
        await live.first;
        live.dispose();
        live.dispose();
        expect(client.disposeQuerySubscription).toHaveBeenCalledTimes(1);
        expect(client.disposeQuerySubscription).toHaveBeenCalledWith('p', 's1');
    });

    it('a listener removed or a handle disposed by an earlier listener is not called for that change', async () => {
        const { client, update } = fakeClient();
        const live = open(client, async () => ({ subscriptionId: 's1', result: [1], revision: 0 }));
        await live.first;
        const later = jest.fn();
        let removeLater = () => {};
        live.subscribe(snapshot => { if (!snapshot.initial) removeLater() });
        removeLater = live.subscribe(later);
        later.mockClear();
        update('s1', 1, [2]);
        expect(later).not.toHaveBeenCalled();

        // The snapshot is already not initial, so a flag keeps the disposer quiet
        // for its immediate call; it disposes on the next update.
        const last = jest.fn();
        let armed = false;
        live.subscribe(() => { if (armed) live.dispose() });
        live.subscribe(last);
        last.mockClear();
        armed = true;
        update('s1', 2, [3]);
        expect(last).not.toHaveBeenCalled();
    });

    it('subscribe on a disposed handle delivers nothing and keeps nothing', async () => {
        const { client } = fakeClient();
        const live = open(client, async () => ({ subscriptionId: 's1', result: [1], revision: 0 }));
        await live.first;
        live.dispose();
        const seen = jest.fn();
        const remove = live.subscribe(seen);
        expect(seen).not.toHaveBeenCalled();
        expect(remove).not.toThrow();
    });

    it('a dispose during the opening notification does not resync for the updates buffered meanwhile', async () => {
        const { client, send } = fakeClient();
        const reply = deferred<Subscribed>();
        const live = open(client, () => reply.promise);
        live.subscribe(() => live.dispose());
        // A revision gap, so applying this update would call `resyncSubscription`.
        send({ subscriptionId: 's1', revision: 5, added: [9], removed: [] });
        reply.resolve({ subscriptionId: 's1', result: [1], revision: 0 });
        await tick();
        expect(client.resyncSubscription).not.toHaveBeenCalled();
    });

    it('getSnapshot and subscribe work detached from the handle', async () => {
        const { client } = fakeClient();
        const live = open(client, async () => ({ subscriptionId: 's1', result: [1], revision: 0 }));
        const { getSnapshot, subscribe } = live;
        await live.first;
        expect(getSnapshot().data).toEqual([1]);
        const seen = jest.fn();
        subscribe(seen)();
        expect(seen).toHaveBeenCalledTimes(1);
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
            const openCall = jest.fn()
                .mockResolvedValueOnce({ subscriptionId: 's1', result: ['a'], revision: 0 })
                .mockRejectedValueOnce(notFound)
                .mockRejectedValueOnce(notFound)
                .mockResolvedValueOnce({ subscriptionId: 's2', result: ['a', 'b'], revision: 0 });
            const live = open(client, openCall);
            const statuses: LiveQueryStatus[] = [];
            live.onStatus(s => statuses.push(s));
            await live.first;

            reconnect();
            await jest.advanceTimersByTimeAsync(0);
            expect(openCall).toHaveBeenCalledTimes(2);
            expect(live.getSnapshot()).toMatchObject({ reopening: true, data: ['a'] });
            expect(statuses).toEqual([{ state: 'reopening', error: notFound, retryInMs: 1000 }]);

            await jest.advanceTimersByTimeAsync(999);
            expect(openCall).toHaveBeenCalledTimes(2);
            await jest.advanceTimersByTimeAsync(1);
            expect(openCall).toHaveBeenCalledTimes(3);
            expect(statuses[1]).toEqual({ state: 'reopening', error: notFound, retryInMs: 2000 });

            await jest.advanceTimersByTimeAsync(2000);
            expect(openCall).toHaveBeenCalledTimes(4);
            expect(live.getSnapshot()).toMatchObject({ reopening: false, data: ['a', 'b'], initial: false });
            expect(statuses[2]).toEqual({ state: 'open' });

            // Live again: updates to the new subscription apply.
            update('s2', 1, ['c']);
            expect(live.getSnapshot().data).toEqual(['a', 'b', 'c']);

            // No retry is left over.
            await jest.advanceTimersByTimeAsync(120_000);
            expect(openCall).toHaveBeenCalledTimes(4);
        });

        it('a re-open that succeeds with the same result after a failure clears reopening without a value change', async () => {
            const { client, reconnect } = fakeClient();
            const openCall = jest.fn()
                .mockResolvedValueOnce({ subscriptionId: 's1', result: ['a'], revision: 0 })
                .mockRejectedValueOnce(new Error('Perspective p not found'))
                .mockResolvedValueOnce({ subscriptionId: 's2', result: ['a'], revision: 0 });
            const live = open(client, openCall);
            await live.first;
            reconnect();
            await jest.advanceTimersByTimeAsync(0);
            expect(live.getSnapshot().reopening).toBe(true);
            await jest.advanceTimersByTimeAsync(1000);
            expect(live.getSnapshot()).toMatchObject({ reopening: false, data: ['a'], initial: true });
        });

        it('a second failed re-open tells the listeners nothing new', async () => {
            const { client, reconnect } = fakeClient();
            const openCall = jest.fn()
                .mockResolvedValueOnce({ subscriptionId: 's1', result: ['a'], revision: 0 })
                .mockRejectedValue(new Error('Perspective p not found'));
            const live = open(client, openCall);
            await live.first;
            const seen = jest.fn();
            live.subscribe(seen);
            const statuses: LiveQueryStatus[] = [];
            live.onStatus(s => statuses.push(s));
            reconnect();
            await jest.advanceTimersByTimeAsync(0);
            const reopening = live.getSnapshot();
            seen.mockClear();
            await jest.advanceTimersByTimeAsync(1000);
            expect(openCall).toHaveBeenCalledTimes(3);
            expect(statuses).toHaveLength(2);
            expect(live.getSnapshot()).toBe(reopening);
            expect(seen).not.toHaveBeenCalled();
        });

        it('stops retrying once disposed', async () => {
            const { client, reconnect } = fakeClient();
            const openCall = jest.fn()
                .mockResolvedValueOnce({ subscriptionId: 's1', result: [], revision: 0 })
                .mockRejectedValue(new Error('Perspective p not found'));
            const live = open(client, openCall);
            await live.first;
            reconnect();
            await jest.advanceTimersByTimeAsync(0);
            expect(openCall).toHaveBeenCalledTimes(2);
            live.dispose();
            await jest.advanceTimersByTimeAsync(120_000);
            expect(openCall).toHaveBeenCalledTimes(2);
        });

        it('a new reconnect starts the backoff over', async () => {
            const { client, reconnect } = fakeClient();
            const openCall = jest.fn()
                .mockResolvedValueOnce({ subscriptionId: 's1', result: [], revision: 0 })
                .mockRejectedValue(new Error('Perspective p not found'));
            const live = open(client, openCall);
            const statuses: LiveQueryStatus[] = [];
            live.onStatus(s => statuses.push(s));
            await live.first;
            reconnect();
            await jest.advanceTimersByTimeAsync(1000 + 2000);
            expect(openCall).toHaveBeenCalledTimes(4);
            reconnect();
            await jest.advanceTimersByTimeAsync(0);
            expect(openCall).toHaveBeenCalledTimes(5);
            expect(statuses.map(s => s.state === 'reopening' && s.retryInMs)).toEqual([1000, 2000, 4000, 1000]);
            // Only the newest chain of retries is left.
            await jest.advanceTimersByTimeAsync(1000);
            expect(openCall).toHaveBeenCalledTimes(6);
        });

        it('a re-open that fails after a newer one took over leaves the newer one alone', async () => {
            const { client, reconnect } = fakeClient();
            let failLate!: (e: Error) => void;
            const openCall = jest.fn()
                .mockResolvedValueOnce({ subscriptionId: 's1', result: [], revision: 0 })
                // The first re-open hangs (the old socket), and fails only after the next reconnect.
                .mockReturnValueOnce(new Promise((_, reject) => { failLate = reject; }))
                .mockResolvedValueOnce({ subscriptionId: 's2', result: ['x'], revision: 0 });
            const live = open(client, openCall);
            const statuses: LiveQueryStatus[] = [];
            live.onStatus(s => statuses.push(s));
            await live.first;
            reconnect();
            reconnect();
            await jest.advanceTimersByTimeAsync(0);
            expect(live.getSnapshot().data).toEqual(['x']);

            failLate(new Error('socket closed'));
            await jest.advanceTimersByTimeAsync(120_000);
            expect(statuses).toEqual([]);
            expect(live.getSnapshot().reopening).toBe(false);
            expect(openCall).toHaveBeenCalledTimes(3);
        });

        it('retries a re-open after a failed resync too', async () => {
            const { client, update } = fakeClient();
            client.resyncSubscription.mockRejectedValue(new Error('gone'));
            const openCall = jest.fn()
                .mockResolvedValueOnce({ subscriptionId: 's1', result: [], revision: 0 })
                .mockRejectedValueOnce(new Error('Perspective p not found'))
                .mockResolvedValueOnce({ subscriptionId: 's2', result: ['x'], revision: 0 });
            const live = open(client, openCall);
            await live.first;
            update('s1', 2, ['x']);
            await jest.advanceTimersByTimeAsync(0);
            expect(openCall).toHaveBeenCalledTimes(2);
            await jest.advanceTimersByTimeAsync(1000);
            expect(openCall).toHaveBeenCalledTimes(3);
            expect(live.getSnapshot().data).toEqual(['x']);
        });
    });
});

describe('reopenDelay', () => {
    it('doubles from 1 s to a 60 s cap, between half and all of each step', () => {
        expect([1, 2, 3, 6, 7, 20].map(n => reopenDelay(n, () => 1))).toEqual([1000, 2000, 4000, 32000, 60000, 60000]);
        expect(reopenDelay(1, () => 0)).toBe(500);
        expect(reopenDelay(20, () => 0)).toBe(30000);
    });
});
