import { Ad4mClient } from '../Ad4mClient'

type Msg = Record<string, any>

/** A socket that answers each request from `Sock.handle`; a returned promise holds the reply. */
class Sock {
    static instances: Sock[] = []
    static handle: (msg: Msg, sock: Sock) => unknown = () => ({})
    readyState = 0
    sent: Msg[] = []
    onopen: (() => void) | null = null
    onmessage: ((ev: { data: string }) => void) | null = null
    onerror: ((ev: unknown) => void) | null = null
    onclose: ((ev: { code: number; reason: string }) => void) | null = null

    constructor(public url: string) {
        Sock.instances.push(this)
        Promise.resolve().then(() => this.open())
    }
    open() { this.readyState = 1; this.onopen?.() }
    close() { this.drop() }
    drop() { this.readyState = 3; this.onclose?.({ code: 1006, reason: '' }) }
    send(data: string) {
        const msg = JSON.parse(data)
        this.sent.push(msg)
        if (msg.type === 'ping') return this.reply({ type: 'pong' })
        if (msg.type === 'events.watch') return this.reply({ id: msg.id, result: {} })
        Promise.resolve()
            .then(() => Sock.handle(msg, this))
            .then(
                (result) => { if (this.readyState === 1) this.reply({ id: msg.id, result }) },
                (e: Error) => { if (this.readyState === 1) this.reply({ id: msg.id, error: { code: 500, message: e.message } }) },
            )
    }
    reply(msg: Msg) { this.onmessage?.({ data: JSON.stringify(msg) }) }
}

const asks = (method: string) => Sock.instances.flatMap((s) => s.sent).filter((m) => m.type === method).length
const settle = () => jest.advanceTimersByTimeAsync(0)
const emit = (event: Msg) => Sock.instances[0].reply(event)
const handle = (uuid: string, name = uuid) => ({ uuid, name, state: 'PRIVATE', sharedUrl: null, neighbourhood: null, owners: ['did:test:me'] })
const names = async (client: Ad4mClient) => (await client.perspective.all()).map((p) => `${p.uuid}:${p.name}`)

let ad4m: Ad4mClient
let stored: Msg[]
beforeEach(() => {
    jest.useFakeTimers()
    Sock.instances = []
    stored = [handle('a'), handle('b')]
    Sock.handle = (msg) => {
        if (msg.type === 'perspective.all') return stored
        if (msg.type === 'perspective.create') return handle('c', msg.params.name)
        if (msg.type === 'perspective.update') return handle(msg.params.uuid, msg.params.name)
        if (msg.type === 'perspective.remove') return true
        if (msg.type === 'neighbourhood.join') return { ...handle('n'), sharedUrl: 'neighbourhood://n', state: 'SYNCED' }
        if (msg.type === 'hosting.info') return { userInfo: { email: 'me@test.com', credits: 5, hotWalletAddress: null, freeAccess: false } }
        if (msg.type === 'hosting.setHotWallet') return true
        return {}
    }
    ad4m = new Ad4mClient('http://localhost:1234', 'token', { webSocketImpl: Sock as unknown as new (url: string) => WebSocket })
})
afterEach(() => { jest.useRealTimers() })

describe('perspective.all, a pushed entry', () => {
    it('reads once; later reads and live.all() are local', async () => {
        expect(await names(ad4m)).toEqual(['a:a', 'b:b'])
        expect(await names(ad4m)).toEqual(['a:a', 'b:b'])
        expect((await ad4m.perspective.live.all().read()).map((h) => h.uuid)).toEqual(['a', 'b'])
        expect(asks('perspective.all')).toBe(1)
    })

    it('perspective-added, -updated and -removed change the list with no request', async () => {
        await names(ad4m)
        const seen: string[][] = []
        ad4m.perspective.live.all().subscribe((s) => seen.push((s.value ?? []).map((h) => `${h.uuid}:${h.name}`)))

        emit({ type: 'perspective-added', perspectiveUuid: 'c', perspective: handle('c') })
        emit({ type: 'perspective-updated', perspectiveUuid: 'a', perspective: handle('a', 'renamed') })
        emit({ type: 'perspective-removed', perspectiveUuid: 'b', uuid: 'b' })

        expect(await names(ad4m)).toEqual(['a:renamed', 'c:c'])
        expect(seen.at(-1)).toEqual(['a:renamed', 'c:c'])
        expect(asks('perspective.all')).toBe(1)
    })

    it('the handle an own write returns is in the list with no request: add, update, remove, join', async () => {
        await names(ad4m)
        await ad4m.perspective.add('mine')
        expect(await names(ad4m)).toEqual(['a:a', 'b:b', 'c:mine'])
        await ad4m.perspective.update('c', 'renamed')
        expect(await names(ad4m)).toEqual(['a:a', 'b:b', 'c:renamed'])
        await ad4m.perspective.remove('a')
        expect(await names(ad4m)).toEqual(['b:b', 'c:renamed'])
        await ad4m.neighbourhood.joinFromUrl('neighbourhood://n')
        expect(await names(ad4m)).toEqual(['b:b', 'c:renamed', 'n:n'])
        // The event that follows the write says the same: nothing changes.
        emit({ type: 'perspective-added', perspectiveUuid: 'n', perspective: { ...handle('n'), sharedUrl: 'neighbourhood://n', state: 'SYNCED' } })
        expect(await names(ad4m)).toEqual(['b:b', 'c:renamed', 'n:n'])
        expect(asks('perspective.all')).toBe(1)
    })

    it('a write before the first read does not make the list wrong', async () => {
        await ad4m.perspective.add('mine')
        stored = [handle('a'), handle('b'), handle('c', 'mine')]
        expect(await names(ad4m)).toEqual(['a:a', 'b:b', 'c:mine'])
        expect(asks('perspective.all')).toBe(1)
    })

    it('a reconnect makes the list stale; an event on a stale list does not make it fresh', async () => {
        await names(ad4m)
        Sock.instances[0].drop()
        await jest.advanceTimersByTimeAsync(500)
        Sock.instances[1].reply({ type: 'perspective-removed', perspectiveUuid: 'b', uuid: 'b' })
        stored = [handle('a'), handle('z')]
        expect(await names(ad4m)).toEqual(['a:a', 'z:z'])
        expect(asks('perspective.all')).toBe(2)
    })

    it('two clients share nothing', async () => {
        const other = new Ad4mClient('http://localhost:1234', 'other', { webSocketImpl: Sock as unknown as new (url: string) => WebSocket })
        await names(ad4m)
        await names(other)
        expect(asks('perspective.all')).toBe(2)
    })
})

describe('hosting.info, a pushed entry', () => {
    it('reads once and follows hosting-user-info-changed with no request', async () => {
        expect((await ad4m.agent.hostingUserInfo()).remainingCredits).toBe('5')
        const seen: string[] = []
        ad4m.agent.live.hostingUserInfo().subscribe((s) => seen.push(s.value?.remainingCredits ?? ''))
        emit({ type: 'hosting-user-info-changed', email: 'me@test.com', remainingCredits: '3', hotWalletAddress: '0x1', freeAccess: false })
        const info = await ad4m.agent.hostingUserInfo()
        expect(info).toMatchObject({ email: 'me@test.com', remainingCredits: '3', hotWalletAddress: '0x1' })
        expect(seen.at(-1)).toBe('3')
        expect(asks('hosting.info')).toBe(1)
    })

    it('an event for another user (an operator hears every user) makes it stale, not replaced', async () => {
        await ad4m.agent.hostingUserInfo()
        emit({ type: 'hosting-user-info-changed', email: 'other@test.com', remainingCredits: '99', hotWalletAddress: null, freeAccess: false })
        expect((await ad4m.agent.hostingUserInfo()).email).toBe('me@test.com')
        expect(asks('hosting.info')).toBe(2)
    })

    it('setHotWalletAddress makes it stale', async () => {
        await ad4m.agent.hostingUserInfo()
        await ad4m.agent.setHotWalletAddress('0x2')
        await ad4m.agent.hostingUserInfo()
        expect(asks('hosting.info')).toBe(2)
    })

    it('sends no request after the first in 10 idle minutes with a subscriber', async () => {
        ad4m.agent.live.hostingUserInfo().subscribe(() => {})
        await settle()
        await jest.advanceTimersByTimeAsync(10 * 60_000)
        expect(asks('hosting.info')).toBe(1)
    })
})
