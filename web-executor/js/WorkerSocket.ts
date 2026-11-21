// The host's side of a web executor Worker: `WorkerHost`, and the `WorkerSocket` class it
// makes, which has the WebSocket surface that `ApiClient` uses (core/src/apiClient.ts
// `_ensureWs`: `onopen`, `onmessage`, `onclose`, `onerror`, `readyState`, `send`, `close`).
// So the unchanged client runs on it:
//
//   new Ad4mClient(url, '', { webSocketImpl: host.WorkerSocket })
//
// The URL and token are ignored: every socket is one operator connection in the core
// (decision 6). As a real socket, it opens on a later task (the client sets its handlers
// first), and closes on a later task too.

import { CLOSE_DEAD, type FromWorker, type ToWorker } from './protocol.ts'

type Listener = (event: any) => void

const CLOSE_GOING_AWAY = 1001

interface SocketEnd {
  deliver(text: string): void
  ended(code: number, reason: string, wasClean: boolean): void
}

export class WorkerHost {
  /** Resolves with the reason once the Worker is dead; every socket is then closed. */
  readonly dead: Promise<string>
  /** Resolves when the core booted; rejects if the Worker dies first. */
  readonly ready: Promise<void>
  /** A WebSocket class whose sockets are connections to this Worker's core. */
  readonly WorkerSocket: new (url: string, protocols?: string | string[]) => WebSocket

  #post: (message: ToWorker) => void
  #sockets = new Map<number, SocketEnd>()
  #nextCid = 0
  #deadReason: string | null = null
  #deadCode = CLOSE_DEAD
  #resolveDead!: (reason: string) => void
  #readyHooks!: { resolve: () => void; reject: (e: Error) => void }

  constructor(post: (message: ToWorker) => void) {
    this.#post = post
    this.dead = new Promise((resolve) => { this.#resolveDead = resolve })
    this.ready = new Promise((resolve, reject) => { this.#readyHooks = { resolve, reject } })
    this.ready.catch(() => {})
    this.WorkerSocket = makeWorkerSocket(this)
  }

  /** The reason the Worker died, or null while it lives. */
  get deadReason(): string | null { return this.#deadReason }

  /** The close code sockets opened after the end get. */
  get deadCode(): number { return this.#deadCode }

  /** Starts the core: the first message the Worker gets. */
  boot(config: object = {}, logLevel?: string): void {
    this.#post({ op: 'boot', config: JSON.stringify(config), logLevel })
  }

  /** One message from the Worker. */
  receive(message: FromWorker): void {
    switch (message.op) {
      case 'ready': this.#readyHooks.resolve(); break
      case 'frame': this.#sockets.get(message.cid)?.deliver(message.text); break
      case 'closed': this.#sockets.get(message.cid)?.ended(message.code, message.reason, true); break
      case 'dead': this.died(message.reason); break
    }
  }

  /** The Worker is gone (it said so, or it failed or exited): close every socket with `reason`. */
  died(reason: string): void {
    this.#end(CLOSE_DEAD, reason, false)
  }

  /** The host stops the Worker on purpose: every socket closes cleanly, "going away" (1001). */
  shutDown(reason = 'the web executor was stopped'): void {
    this.#end(CLOSE_GOING_AWAY, reason, true)
  }

  #end(code: number, reason: string, wasClean: boolean): void {
    if (this.#deadReason !== null) return
    this.#deadReason = reason
    this.#deadCode = code
    this.#readyHooks.reject(new Error(reason))
    for (const socket of [...this.#sockets.values()]) socket.ended(code, reason, wasClean)
    this.#resolveDead(reason)
  }

  /** @internal */ _open(socket: SocketEnd): number | null {
    if (this.#deadReason !== null) return null
    const cid = ++this.#nextCid
    this.#sockets.set(cid, socket)
    this.#post({ op: 'connect', cid })
    return cid
  }

  /** @internal */ _send(cid: number, text: string): void {
    this.#post({ op: 'send', cid, text })
  }

  /** @internal */ _close(cid: number, tellWorker: boolean): void {
    if (!this.#sockets.delete(cid)) return
    if (tellWorker && this.#deadReason === null) this.#post({ op: 'close', cid })
  }

  /** @internal */ _post(message: ToWorker): void {
    this.#post(message)
  }
}

function makeWorkerSocket(host: WorkerHost) {
  return class WorkerSocket {
    static readonly CONNECTING = 0
    static readonly OPEN = 1
    static readonly CLOSING = 2
    static readonly CLOSED = 3
    readonly CONNECTING = 0
    readonly OPEN = 1
    readonly CLOSING = 2
    readonly CLOSED = 3

    readonly url: string
    readonly protocol = ''
    readonly extensions = ''
    binaryType = 'blob'
    bufferedAmount = 0
    readyState = 0
    onopen: Listener | null = null
    onmessage: Listener | null = null
    onclose: Listener | null = null
    onerror: Listener | null = null

    #cid: number | null = null
    #listeners = new Map<string, Set<Listener>>()

    constructor(url: string) {
      this.url = url
      setTimeout(() => {
        if (this.readyState !== 0) return
        const cid = host._open({
          deliver: (text) => {
            if (this.readyState === 1) this.#emit('message', { type: 'message', data: text })
          },
          ended: (code, reason, wasClean) => this.#ended(code, reason, wasClean),
        })
        if (cid === null) {
          this.#ended(host.deadCode, host.deadReason ?? 'the web executor is dead', false)
          return
        }
        this.#cid = cid
        this.readyState = 1
        this.#emit('open', { type: 'open' })
      }, 0)
    }

    send(text: string): void {
      if (this.readyState === 0) throw new Error("InvalidStateError: WorkerSocket is still CONNECTING")
      if (this.readyState !== 1) return // as WebSocket: data sent after close is dropped
      host._send(this.#cid!, String(text))
    }

    close(code = 1000, reason = ''): void {
      if (this.readyState >= 2) return
      this.readyState = 2
      if (this.#cid !== null) host._close(this.#cid, true)
      setTimeout(() => this.#ended(code, reason, true), 0)
    }

    addEventListener(type: string, listener: Listener): void {
      let set = this.#listeners.get(type)
      if (!set) this.#listeners.set(type, set = new Set())
      set.add(listener)
    }

    removeEventListener(type: string, listener: Listener): void {
      this.#listeners.get(type)?.delete(listener)
    }

    dispatchEvent(event: { type: string }): boolean {
      this.#emit(event.type, event)
      return true
    }

    #ended(code: number, reason: string, wasClean: boolean): void {
      if (this.readyState === 3) return
      this.readyState = 3
      if (this.#cid !== null) host._close(this.#cid, false)
      if (!wasClean) this.#emit('error', { type: 'error', message: reason })
      this.#emit('close', { type: 'close', code, reason, wasClean })
    }

    #emit(type: string, event: object): void {
      const handler = (this as any)['on' + type]
      if (typeof handler === 'function') handler.call(this, event)
      for (const listener of [...this.#listeners.get(type) ?? []]) listener.call(this, event)
    }
  } as unknown as new (url: string, protocols?: string | string[]) => WebSocket
}
