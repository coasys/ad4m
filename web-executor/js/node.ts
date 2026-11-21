// The web executor in a Node `worker_threads` Worker, for Node hosts and tests:
//
//   const host = await startWebExecutor()
//   const ad4m = new Ad4mClient('http://web-executor', '', { webSocketImpl: host.WorkerSocket })
//
// Every WorkerSocket is one operator connection in the core (no token, decision 6).

import { Worker } from 'node:worker_threads'
import { WorkerHost } from './WorkerSocket.ts'

export interface WebExecutorOptions {
  /** The core's boot config: `{}` or `{ languageLanguageOnly }`. */
  config?: object
  /** How much of the core's log reaches the console: error, warn (default), info, debug, trace, off. */
  logLevel?: string
  /** The wasm-bindgen Node glue (default web-executor/pkg/node/web_executor.js). */
  glue?: string
  /** Drop the core's console output (its panic message included). */
  quiet?: boolean
}

export interface WebExecutor {
  WorkerSocket: WorkerHost['WorkerSocket']
  /** Resolves with the reason once the Worker is dead. */
  dead: Promise<string>
  /** Panics in the core (worker-death test only). */
  panicForTest(message: string): void
  terminate(): Promise<void>
  worker: Worker
}

/** Starts a Worker, boots the core in it, and resolves once it answers. */
export async function startWebExecutor(options: WebExecutorOptions = {}): Promise<WebExecutor> {
  const worker = new Worker(new URL('./node-worker.ts', import.meta.url), {
    workerData: { glue: options.glue, quiet: options.quiet },
  })
  const host = new WorkerHost((message) => worker.postMessage(message))
  worker.on('message', (message) => host.receive(message))
  worker.on('error', (error) => host.died(`web executor Worker failed: ${error.message}`))
  worker.on('exit', (code) => host.died(`web executor Worker exited with code ${code}`))
  host.boot(options.config ?? {}, options.logLevel)
  try {
    await host.ready
  } catch (error) {
    // The caller gets no handle to a Worker that did not boot: stop it here.
    await worker.terminate()
    throw error
  }
  return {
    WorkerSocket: host.WorkerSocket,
    dead: host.dead,
    panicForTest: (message) => host._post({ op: 'panic-for-test', message }),
    terminate: async () => {
      host.shutDown()
      await worker.terminate()
    },
    worker,
  }
}
