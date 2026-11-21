// The web executor's dedicated Worker in a browser (started by browser.ts as a module Worker).
// It loads the browser glue from web-executor/pkg/web (`bash scripts/web-executor/build.sh web`).

import init, * as glue from '../pkg/web/web_executor.js'
import { serveCore, type WebExecutorCore } from './worker.ts'

const scope = self as unknown as {
  postMessage(message: unknown): void
  addEventListener(type: string, listener: (event: any) => void): void
}

const queued: MessageEvent[] = []
scope.addEventListener('message', (event) => queued.push(event))

try {
  await init()
} catch (error) {
  scope.postMessage({ op: 'dead', reason: `web executor did not load: ${String((error as Error)?.message ?? error)}`.slice(0, 120) })
  throw error
}
const served = serveCore(glue as unknown as WebExecutorCore, {
  post: (message) => scope.postMessage(message),
  listen: (handler) => {
    scope.addEventListener('message', (event) => handler(event.data))
    for (const event of queued.splice(0)) handler(event.data)
  },
})

// A trap in a task of the core surfaces as an uncaught error or rejection of the Worker.
scope.addEventListener('error', (event) => {
  if (served.fatal(event.error ?? event.message)) event.preventDefault()
})
scope.addEventListener('unhandledrejection', (event) => {
  if (served.fatal(event.reason)) event.preventDefault()
})
