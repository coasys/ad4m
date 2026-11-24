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
const status = (did: string | null, isUnlocked = true) => ({ did, didDocument: null, error: null, isInitialized: true, isUnlocked, identity: null })
const SFU = { reachability: 'public', isPublic: true, bindAddress: '127.0.0.1:1', detail: 'ok' }

function newClient(token = 'token') {
    return new Ad4mClient('http://localhost:1234', token, { webSocketImpl: Sock as unknown as new (url: string) => WebSocket })
}

/** What a reconnect looks like: the socket drops, the client redials after its backoff, the new one opens. */
async function reconnect() {
    const before = Sock.instances.length
    Sock.instances[before - 1].drop()
    await jest.advanceTimersByTimeAsync(500)
    expect(Sock.instances.length).toBe(before + 1)
}

let ad4m: Ad4mClient
beforeEach(() => {
    jest.useFakeTimers()
    Sock.instances = []
    Sock.handle = (msg) => {
        if (msg.type === 'agent.status') return status('did:test:me')
        if (msg.type === 'sfu.status') return SFU
        return {}
    }
    ad4m = newClient()
})
afterEach(() => { jest.useRealTimers() })

const join = (client = ad4m, uuid = 'p1') =>
    client.perspective.getNeighbourhoodProxy(uuid).createSession('room', { neighbourhoodUrl: 'neighbourhood://n' })

describe('the caller DID and the SFU status, per client', () => {
    it('two joins send agent.status and sfu.status once', async () => {
        await join()
        await join()
        await join(ad4m, 'p2')
        expect(asks('agent.status')).toBe(1)
        expect(asks('sfu.status')).toBe(1)
    })

    it('getNeighbourhoodProxy returns one proxy per uuid', () => {
        expect(ad4m.perspective.getNeighbourhoodProxy('p1')).toBe(ad4m.perspective.getNeighbourhoodProxy('p1'))
        expect(ad4m.perspective.getNeighbourhoodProxy('p1')).not.toBe(ad4m.perspective.getNeighbourhoodProxy('p2'))
    })

    it('a perspective-removed event drops the uuid\'s proxy, and only that one', async () => {
        const p1 = ad4m.perspective.getNeighbourhoodProxy('p1')
        const p2 = ad4m.perspective.getNeighbourhoodProxy('p2')
        await settle()
        const sock = Sock.instances[Sock.instances.length - 1]
        sock.reply({ type: 'perspective-removed', perspectiveUuid: 'p1', uuid: 'p1' })
        expect(ad4m.perspective.getNeighbourhoodProxy('p2')).toBe(p2)
        expect(ad4m.perspective.getNeighbourhoodProxy('p1')).not.toBe(p1)
    })

    it('both go out in one wave', async () => {
        Sock.handle = () => new Promise(() => {}) // no answer yet
        const joining = join()
        joining.catch(() => {})
        await settle()
        expect(asks('agent.status')).toBe(1)
        expect(asks('sfu.status')).toBe(1)
    })

    it('an SFU that is not available is asked again', async () => {
        let up = false
        Sock.handle = (msg) => {
            if (msg.type === 'sfu.status') { if (!up) throw new Error('sfu not running'); return SFU }
            return status('did:test:me')
        }
        await join() // a session works without the SFU status
        expect(asks('sfu.status')).toBe(1)
        up = true
        await join()
        expect(asks('sfu.status')).toBe(2)
        await join()
        expect(asks('sfu.status')).toBe(2)
        expect(await ad4m.runtime.sfuStatus()).toEqual(SFU)
    })

    it('a reconnect refetches both', async () => {
        await join()
        await reconnect()
        await join()
        expect(asks('agent.status')).toBe(2)
        expect(asks('sfu.status')).toBe(2)
        await join()
        expect(asks('agent.status')).toBe(2)
        expect(asks('sfu.status')).toBe(2)
    })

    it('an answer sent before a reconnect is used once and not kept', async () => {
        let held: ((v: unknown) => void) | null = null
        Sock.handle = (msg) => {
            if (msg.type === 'sfu.status') return new Promise((resolve) => { held = resolve })
            return status('did:test:me')
        }
        const first = ad4m.runtime.sfuStatus()
        await settle()
        await reconnect() // the read is sent again on the new socket
        await settle()
        expect(asks('sfu.status')).toBe(2)
        held!(SFU)
        expect(await first).toEqual(SFU)
        Sock.handle = () => ({ ...SFU, detail: 'newer' })
        expect((await ad4m.runtime.sfuStatus()).detail).toBe('newer')
        expect(asks('sfu.status')).toBe(3)
    })

    it('a caller with no DID is asked again', async () => {
        let did: string | null = null
        Sock.handle = (msg) => (msg.type === 'agent.status' ? status(did) : SFU)
        await expect(join()).rejects.toThrow('no agent DID')
        did = 'did:test:me'
        await expect(join()).resolves.toBeDefined()
        expect(asks('agent.status')).toBe(2)
    })

    it('agent-status-changed after unlock updates did()', async () => {
        Sock.handle = (msg) => {
            if (msg.type === 'agent.status') return status(null, false)
            if (msg.type === 'agent.unlock') return status(null, false)
            return {}
        }
        expect(await ad4m.agent.did()).toBeNull()
        await ad4m.agent.unlock('pw')
        Sock.instances[0].reply({ type: 'agent-status-changed', agent: status('did:test:unlocked') })
        expect(await ad4m.agent.did()).toBe('did:test:unlocked')
        expect((await ad4m.agent.status()).isUnlocked).toBe(true)
        expect(asks('agent.status')).toBe(1)
    })

    it('the result of unlock, lock, generate and import is the status, with no new request', async () => {
        const answers: Record<string, unknown> = {
            'agent.unlock': status('did:test:a'),
            'agent.lock': status('did:test:a', false),
            'agent.generate': status('did:test:b'),
            'agent.import': status('did:test:c'),
        }
        Sock.handle = (msg) => answers[msg.type] ?? status('did:test:me')
        await ad4m.agent.unlock('pw')
        expect(await ad4m.agent.did()).toBe('did:test:a')
        await ad4m.agent.lock('pw')
        expect((await ad4m.agent.status()).isUnlocked).toBe(false)
        await ad4m.agent.generate('pw')
        expect(await ad4m.agent.did()).toBe('did:test:b')
        await ad4m.agent.import({ did: 'did:test:c', didDocument: '', keystore: '', passphrase: '' })
        expect(await ad4m.agent.did()).toBe('did:test:c')
        expect(asks('agent.status')).toBe(0)
    })

    it('two clients with two tokens keep separate values', async () => {
        Sock.handle = (msg, sock) => (msg.type === 'agent.status' ? status(`did:test:${Sock.instances.indexOf(sock)}`) : SFU)
        const other = newClient('other-token')
        const [mine, theirs] = [await ad4m.agent.did(), await other.agent.did()]
        expect(mine).not.toBe(theirs)
        expect(await ad4m.agent.did()).toBe(mine)
        expect(await other.agent.did()).toBe(theirs)
        expect(asks('agent.status')).toBe(2)
    })

    it('a lagged marker makes every entry stale', async () => {
        await join()
        Sock.instances[0].reply({ type: 'query-subscription-update', lagged: true })
        await join()
        expect(asks('agent.status')).toBe(2)
        expect(asks('sfu.status')).toBe(2)
    })

    it('a lagged marker refetches an entry that has a subscriber', async () => {
        const live = ad4m.agent.live.status()
        live.subscribe(() => {})
        await settle()
        expect(asks('agent.status')).toBe(1)
        Sock.instances[0].reply({ type: 'query-subscription-update', lagged: true })
        await settle()
        expect(asks('agent.status')).toBe(2)
    })
})

const profile = (did: string, directMessageLanguage: string | null) => ({ did, directMessageLanguage, perspective: null })
const lastSock = () => Sock.instances[Sock.instances.length - 1]

describe('values and events across a lost socket, a close and a cold start', () => {
    it('an agent-updated that arrives before the first agent.get answer is not lost', async () => {
        const answers: ((v: unknown) => void)[] = []
        Sock.handle = (msg) => {
            if (msg.type === 'agent.get') return new Promise((resolve) => { answers.push(resolve) })
            return {}
        }
        const first = ad4m.agent.me()
        await settle()
        expect(answers.length).toBe(1)
        // the own DID is not known yet: the event cannot be matched to the own agent
        lastSock().reply({ type: 'agent-updated', agent: profile('did:test:me', 'lang://new') })
        answers[0](profile('did:test:me', 'lang://old'))
        expect((await first).directMessageLanguage).toBe('lang://old') // used once
        Sock.handle = (msg) => (msg.type === 'agent.get' ? profile('did:test:me', 'lang://new') : {})
        expect((await ad4m.agent.me()).directMessageLanguage).toBe('lang://new')
    })

    it('a scalar read after the socket dropped goes to the executor, not to the stored value', async () => {
        await ad4m.agent.did()
        expect(asks('agent.status')).toBe(1)
        lastSock().drop()
        const read = ad4m.agent.did()
        await jest.advanceTimersByTimeAsync(500) // the client redials
        expect(await read).toBe('did:test:me')
        expect(asks('agent.status')).toBe(2)
    })

    it('a one-shot query after the socket dropped goes to the executor, not to the live result', async () => {
        Sock.handle = (msg) => {
            if (msg.type === 'perspective.subscribe') return { subscriptionId: msg.params.subscriptionId, revision: 0, result: [{ s: 'live' }] }
            if (msg.type === 'perspective.querySparql') return JSON.stringify([{ s: 'one-shot' }])
            return {}
        }
        const live = ad4m.perspective.liveQuery('p1', { sparql: 'SELECT ?s WHERE { ?s ?p ?o }' }, (rows) => rows)
        expect(await live.first).toEqual([{ s: 'live' }])
        expect(await ad4m.perspective.querySparql('p1', 'SELECT ?s WHERE { ?s ?p ?o }')).toEqual([{ s: 'live' }])
        expect(asks('perspective.querySparql')).toBe(0)

        lastSock().drop()
        const rows = ad4m.perspective.querySparql('p1', 'SELECT ?s WHERE { ?s ?p ?o }')
        await jest.advanceTimersByTimeAsync(500)
        expect(await rows).toEqual([{ s: 'one-shot' }])
        expect(asks('perspective.querySparql')).toBe(1)
        // the live snapshot stayed for display
        expect(live.getSnapshot()).toMatchObject({ status: 'ready', data: [{ s: 'live' }] })
    })

    it('a client reused after close() reads from the executor again and hears events', async () => {
        Sock.handle = (msg) => {
            if (msg.type === 'perspective.subscribe') return { subscriptionId: msg.params.subscriptionId, revision: 0, result: [] }
            return status('did:test:me')
        }
        expect(await ad4m.agent.did()).toBe('did:test:me')
        const live = ad4m.perspective.liveQuery('p1', { sparql: 'q' }, (rows) => rows)
        await live.first
        ad4m.close()
        await settle()

        Sock.handle = (msg) => {
            if (msg.type === 'perspective.subscribe') return { subscriptionId: msg.params.subscriptionId, revision: 0, result: [] }
            return status('did:test:again')
        }
        expect(await ad4m.agent.did()).toBe('did:test:again')
        expect(asks('agent.status')).toBe(2)
        // the entry hears events again
        lastSock().reply({ type: 'agent-status-changed', agent: status('did:test:pushed') })
        expect(await ad4m.agent.did()).toBe('did:test:pushed')
        expect(asks('agent.status')).toBe(2)
        // a pooled live query of the closed socket is not served
        await ad4m.perspective.liveQuery('p1', { sparql: 'q' }, (rows) => rows).first
        expect(asks('perspective.subscribe')).toBe(2)
    })
})

describe('watched values: no request without a socket, one read when it opens', () => {
    const agentWire = (did: string, lang: string) => ({ did, directMessageLanguage: lang, perspective: null })
    const requests = () => Sock.instances.flatMap((s) => s.sent).filter((m) => m.type !== 'ping' && m.type !== 'events.watch').length
    beforeEach(() => {
        Sock.handle = (msg) => {
            if (msg.type === 'agent.status') return status('did:test:me')
            if (msg.type === 'agent.byDIDs') return msg.params.dids.map((did: string) => agentWire(did, 'lang://one'))
            return {}
        }
    })

    /** A profile and the status entry watched, both loaded. */
    async function watch() {
        const profile = ad4m.agent.live.byDID('did:test:bob')
        const own = ad4m.agent.live.status()
        const offs = [profile.subscribe(() => {}), own.subscribe(() => {})]
        await settle()
        expect(profile.getSnapshot().state).toBe('fresh')
        expect(own.getSnapshot().state).toBe('fresh')
        return { profile, own, offs }
    }

    it('closeAll() with watched values opens no socket and sends no request; a reuse loads each once', async () => {
        const { profile, own } = await watch()
        const sockets = Sock.instances.length
        const sent = requests()
        ad4m.close()
        await settle()
        await jest.advanceTimersByTimeAsync(60_000)
        expect(Sock.instances.length).toBe(sockets)
        expect(requests()).toBe(sent)
        expect(profile.getSnapshot().state).toBe('stale')
        expect(own.getSnapshot().state).toBe('stale')

        // explicit reuse: the app's own call opens the socket, the watched values load once
        Sock.handle = (msg) => {
            if (msg.type === 'agent.byDIDs') return msg.params.dids.map((did: string) => agentWire(did, 'lang://two'))
            return status('did:test:again')
        }
        await ad4m.agent.did()
        await settle()
        expect(Sock.instances.length).toBe(sockets + 1)
        expect(asks('agent.status')).toBe(2)
        expect(asks('agent.byDIDs')).toBe(2)
        expect(profile.getSnapshot()).toMatchObject({ state: 'fresh' })
        expect(profile.getSnapshot().value?.directMessageLanguage).toBe('lang://two')
        // and an executor event reaches them
        lastSock().reply({ type: 'agent-status-changed', agent: status('did:test:pushed') })
        expect(own.getSnapshot().value?.did).toBe('did:test:pushed')
        expect(asks('agent.status')).toBe(2)
    })

    it('a reconnect sends one read per watched value, none before the socket reopens', async () => {
        await watch()
        const sent = requests()
        const before = Sock.instances.length
        Sock.instances[before - 1].drop()
        await jest.advanceTimersByTimeAsync(0)
        expect(requests()).toBe(sent)
        expect(Sock.instances.length).toBe(before)
        await jest.advanceTimersByTimeAsync(500)
        expect(Sock.instances.length).toBe(before + 1)
        expect(asks('agent.status')).toBe(2)
        expect(asks('agent.byDIDs')).toBe(2)
    })

    it('a lagged marker refetches a watched profile at once', async () => {
        const { profile } = await watch()
        lastSock().reply({ type: 'query-subscription-update', lagged: true })
        await settle()
        expect(asks('agent.byDIDs')).toBe(2)
        expect(asks('agent.status')).toBe(2)
        expect(profile.getSnapshot().state).toBe('fresh')
    })
})
