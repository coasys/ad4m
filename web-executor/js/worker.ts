// The Worker's side, for either entry (node-worker.ts on worker_threads, browser-worker.ts in a
// dedicated Worker): boot the core on the first message, open one core connection per
// `connect`, and pass frames both ways (protocol.ts).
//
// A panic in the core aborts the Wasm module: the JS that called in (often a task on the
// microtask queue) gets a `RuntimeError: unreachable`, and the instance cannot run again.
// The entry hands every uncaught error to `fatal`; once the core panicked (or a call into it
// trapped), the Worker posts `dead` with the panic's message, so the host closes every socket
// with that reason, and it answers any later `connect` the same way.

import { closeReason, CLOSE_DEAD, type FromWorker, type ToWorker } from './protocol.ts'

/** The wasm-bindgen exports of the `web-executor` crate this file uses. */
export interface WebExecutorCore {
  boot(config: string, logLevel?: string): void
  connect(onFrame: (text: string) => void, onClosed: () => void): CoreConnection
  on_panic(callback: (message: string) => void): void
  panic_for_test(message: string): void
}

export interface CoreConnection {
  send(frame: string): boolean
  close(): void
  free?(): void
}

export interface Port {
  post(message: FromWorker): void
  listen(handler: (message: ToWorker) => void): void
}

export interface ServedCore {
  /** An uncaught error in the Worker: ends it when the core trapped. Returns whether it did. */
  fatal(error: unknown): boolean
}

export function serveCore(core: WebExecutorCore, port: Port): ServedCore {
  const connections = new Map<number, CoreConnection>()
  let booted = false
  let deadReason: string | null = null
  let panicMessage: string | null = null
  const waiting: ToWorker[] = []

  core.on_panic((message) => { panicMessage = message })

  const die = (reason: string) => {
    if (deadReason !== null) return
    deadReason = closeReason(reason)
    connections.clear()
    port.post({ op: 'dead', reason: deadReason })
  }

  const fatal = (error: unknown): boolean => {
    const trapped = panicMessage !== null
      || (typeof WebAssembly !== 'undefined' && error instanceof WebAssembly.RuntimeError)
    if (!trapped) return false
    die(`web executor panicked: ${panicMessage ?? String((error as Error)?.message ?? error)}`)
    return true
  }

  /** Calls into the core; a trap there ends the Worker. */
  const guarded = (call: () => void) => {
    try {
      call()
    } catch (error) {
      if (!fatal(error)) throw error
    }
  }

  const handle = (message: ToWorker) => {
    if (deadReason !== null) {
      if (message.op === 'connect') {
        port.post({ op: 'closed', cid: message.cid, code: CLOSE_DEAD, reason: deadReason })
      }
      return
    }
    switch (message.op) {
      case 'connect': guarded(() => {
        const cid = message.cid
        connections.set(cid, core.connect(
          (text) => port.post({ op: 'frame', cid, text }),
          () => {
            if (connections.delete(cid)) {
              port.post({ op: 'closed', cid, code: 1000, reason: 'the core ended the connection' })
            }
          },
        ))
      }); break
      case 'send': guarded(() => { connections.get(message.cid)?.send(message.text) }); break
      case 'close': guarded(() => {
        const connection = connections.get(message.cid)
        connections.delete(message.cid)
        connection?.close()
      }); break
      case 'panic-for-test': guarded(() => core.panic_for_test(message.message)); break
    }
  }

  port.listen((message) => {
    if (message.op === 'boot') {
      if (booted) return
      try {
        core.boot(message.config, message.logLevel)
      } catch (error) {
        if (!fatal(error)) die(`web executor did not boot: ${String((error as Error)?.message ?? error)}`)
        return
      }
      booted = true
      port.post({ op: 'ready' })
      for (const queued of waiting.splice(0)) handle(queued)
      return
    }
    if (!booted) waiting.push(message)
    else handle(message)
  })

  return { fatal }
}
