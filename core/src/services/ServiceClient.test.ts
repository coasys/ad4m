import { ApiClient, LONG_TIMEOUT_MS, RpcError } from "../apiClient"
import { ServiceClient, type ServiceDefinition } from "./ServiceClient"
import { ServicesClient } from "./ServicesClient"

type AnyMsg = Record<string, any>

/** A socket the tests drive by hand. Opens on the next microtask. */
class TestSocket {
    static instances: TestSocket[] = []
    readyState = 0
    sent: AnyMsg[] = []
    onopen: (() => void) | null = null
    onmessage: ((ev: { data: string }) => void) | null = null
    onerror: ((ev: unknown) => void) | null = null
    onclose: (() => void) | null = null
    constructor(public url: string) {
        TestSocket.instances.push(this)
        queueMicrotask(() => { this.readyState = 1; this.onopen?.() })
    }
    send(data: string) { this.sent.push(JSON.parse(data)) }
    close() { this.readyState = 3; this.onclose?.() }
    reply(msg: AnyMsg) { this.onmessage?.({ data: JSON.stringify(msg) }) }
}

const flush = () => new Promise((r) => setTimeout(r, 0))
const socket = () => TestSocket.instances[0]
/** Calls sent, without the watches and pings. */
const calls = () => socket().sent.filter((m) => m.type !== 'events.watch' && m.type !== 'ping')
const lastWatch = () => [...socket().sent].reverse().find((m) => m.type === 'events.watch')

interface EchoMethods {
    say: { params: { room: string; text: string }; result: { text: string } }
    count: { params: { to: number; streamId: string }; result: { total: number }; chunk: { streamId: string; n: number } }
    [k: string]: { params: unknown; result: unknown; chunk?: unknown }
}
interface EchoEvents {
    said: { room: string; text: string }
    "count-tick": { streamId: string; n: number }
    [k: string]: unknown
}

const ECHO: ServiceDefinition<EchoMethods, EchoEvents> = {
    hash: "QmEchoInterface",
    moduleId: "did:key:z6Mkx/QmEchoInterface",
    name: "echo",
    version: "1.0.0",
    read: new Set(["say"]),
    long: new Set(["count"]),
    streams: { count: "count-tick" },
    scopes: { said: "room", "count-tick": "streamId" },
}

let api: ApiClient
let echo: ServiceClient<EchoMethods, EchoEvents>
beforeEach(() => {
    TestSocket.instances = []
    api = new ApiClient("http://localhost:1234", undefined, TestSocket as unknown as new (url: string) => WebSocket)
    echo = new ServiceClient(api, ECHO)
})
afterEach(() => api.closeAll())

describe("ServiceClient", () => {
    it("calls `<hash>.<method>` and resolves with the result", async () => {
        const p = echo.call("say", { room: "r", text: "hi" })
        await flush()
        const req = calls()[0]
        expect(req).toMatchObject({ type: "QmEchoInterface.say", params: { room: "r", text: "hi" } })
        socket().reply({ id: req.id, result: { text: "hi" } })
        await expect(p).resolves.toEqual({ text: "hi" })
    })

    it("calls a pinned implementation when asked", async () => {
        const pinned = new ServiceClient(api, ECHO, { implementation: "QmImpl" })
        pinned.call("say", { room: "r", text: "hi" }).catch(() => {})
        await flush()
        expect(calls()[0].type).toBe("QmImpl.say")
    })

    it("rejects with the error code, message and typed data", async () => {
        const p = echo.call("say", { room: "r", text: "mute" })
        await flush()
        socket().reply({ id: calls()[0].id, error: { code: 409, message: "muted", data: { name: "Muted", room: "r" } } })
        const e = await p.catch((e) => e)
        expect(e).toBeInstanceOf(RpcError)
        expect([e.status, e.body, e.data]).toEqual([409, "muted", { name: "Muted", room: "r" }])
    })

    it("uses the long timeout for long methods", async () => {
        jest.useFakeTimers()
        try {
            const p = echo.call("count", { to: 2, streamId: "s" })
            p.catch(() => {})
            await jest.advanceTimersByTimeAsync(60_000)
            const settled = await Promise.race([p.then(() => "settled", () => "settled"), Promise.resolve("pending")])
            expect(settled).toBe("pending")
            await jest.advanceTimersByTimeAsync(LONG_TIMEOUT_MS)
            await expect(p).rejects.toMatchObject({ status: 408 })
        } finally {
            jest.useRealTimers()
        }
    })

    it("watches `<hash>.<event>` and narrows on the interface's scope field", async () => {
        const r1: string[] = []
        const all: string[] = []
        echo.on("said", (e) => { r1.push(e.text) }, { scope: "r1" })
        echo.on("said", (e) => { all.push(e.text) })
        await flush()
        expect(lastWatch()!.params).toEqual({ "QmEchoInterface.said": null })
        socket().reply({ type: "QmEchoInterface.said", room: "r1", text: "a" })
        socket().reply({ type: "QmEchoInterface.said", room: "r2", text: "b" })
        expect(r1).toEqual(["a"])
        expect(all).toEqual(["a", "b"])
    })

    it("watches only the scopes asked for", async () => {
        const off = echo.on("said", () => {}, { scope: "r1" })
        await flush()
        expect(lastWatch()!.params).toEqual({ "QmEchoInterface.said": ["r1"] })
        off()
        await flush()
        // The last handler left: the client closes the socket instead of watching nothing.
        expect(socket().readyState).toBe(3)
    })

    it("streams: watches the chunk event first, then calls, then waits for the end marker", async () => {
        const chunks: number[] = []
        const p = echo.stream("count", { to: 2 }, (c) => { chunks.push(c.n) })
        await flush()
        const req = calls()[0]
        const id = req.params.streamId as string
        expect(id).toBeTruthy()
        // The watch for this stream went out before the call.
        const watchIndex = socket().sent.findIndex((m) => m.type === "events.watch" && m.params["QmEchoInterface.count-tick"])
        expect(watchIndex).toBeLessThan(socket().sent.indexOf(req))
        expect(socket().sent[watchIndex].params).toMatchObject({ "QmEchoInterface.count-tick": [id], "service-stream-end": [id] })

        socket().reply({ type: "QmEchoInterface.count-tick", streamId: id, n: 1 })
        socket().reply({ type: "QmEchoInterface.count-tick", streamId: "other", n: 99 })
        // The reply overtakes the last chunk; the stream is not over yet.
        socket().reply({ id: req.id, result: { total: 2 } })
        await flush()
        socket().reply({ type: "QmEchoInterface.count-tick", streamId: id, n: 2 })
        socket().reply({ type: "service-stream-end", streamId: id, method: "QmEchoInterface.count", ok: true })
        await expect(p).resolves.toEqual({ total: 2 })
        expect(chunks).toEqual([1, 2])
        await flush()
        // Both stream listeners are gone, so the client closed its socket.
        expect(socket().readyState).toBe(3)
    })

    it("refuses to stream a method that does not stream", async () => {
        await expect(echo.stream("say", { room: "r", text: "x" } as never, () => {})).rejects.toThrow("does not stream")
    })
})

describe("ServicesClient", () => {
    it("calls the registry methods", async () => {
        const services = new ServicesClient(api)
        services.describe("QmEchoInterface").catch(() => {})
        services.interface("QmEchoInterface").catch(() => {})
        services.setPreference("QmEchoInterface", "did:key:a/QmImpl").catch(() => {})
        services.setPreference("QmEchoInterface", "did:key:a/QmImpl", true).catch(() => {})
        await flush()
        expect(calls().map((c) => [c.type, c.params])).toEqual([
            ["services.describe", { target: "QmEchoInterface" }],
            ["services.interface", { hash: "QmEchoInterface" }],
            ["services.setPreference", { interface: "QmEchoInterface", module: "did:key:a/QmImpl" }],
            ["services.setPreference", { interface: "QmEchoInterface", module: "did:key:a/QmImpl", forAllUsers: true }],
        ])
        expect(services.use(ECHO)).toBeInstanceOf(ServiceClient)
    })
})
