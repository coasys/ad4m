// The web executor's Worker on Node `worker_threads` (started by node.ts). `workerData.glue` is
// the wasm-bindgen Node glue (default: web-executor/pkg/node, from
// `bash scripts/web-executor/build.sh`). `workerData.quiet` drops the core's console output.

import { createRequire } from 'node:module'
import { parentPort, workerData } from 'node:worker_threads'
import { serveCore, type WebExecutorCore } from './worker.ts'

const options = (workerData ?? {}) as { glue?: string; quiet?: boolean }
if (options.quiet) {
  console.error = () => {}
  console.warn = () => {}
}
const glue = options.glue ?? new URL('../pkg/node/web_executor.js', import.meta.url).pathname
const core = createRequire(import.meta.url)(glue) as WebExecutorCore
const port = parentPort!
const served = serveCore(core, {
  post: (message) => port.postMessage(message),
  listen: (handler) => port.on('message', handler),
})

// A trap in a task of the core surfaces here, not at a call: hand it over, and keep the
// Worker alive (dead, answering every socket with the reason) when it was the core's.
process.on('uncaughtException', (error) => {
  if (!served.fatal(error)) throw error
})
process.on('unhandledRejection', (error) => {
  if (!served.fatal(error)) throw error
})
