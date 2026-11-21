// The web executor in a dedicated Worker of a page:
//
//   const host = await startWebExecutor()
//   const ad4m = new Ad4mClient(location.origin, '', { webSocketImpl: host.WorkerSocket })
//
// Every WorkerSocket is one operator connection in the core (no token, decision 6): any
// script on the page is the operator. Bundle this file with its Worker (`new URL(...,
// import.meta.url)` is the pattern Vite, webpack and esbuild follow).

import { WorkerHost } from './WorkerSocket.ts'

export interface WebExecutorOptions {
  config?: object
  logLevel?: string
  /** Where the Worker script is, if not next to this file. */
  workerUrl?: string | URL
}

export interface WebExecutor {
  WorkerSocket: WorkerHost['WorkerSocket']
  dead: Promise<string>
  terminate(): void
  worker: Worker
}

export async function startWebExecutor(options: WebExecutorOptions = {}): Promise<WebExecutor> {
  const url = options.workerUrl ?? new URL('./browser-worker.ts', import.meta.url)
  const worker = new Worker(url, { type: 'module' })
  const host = new WorkerHost((message) => worker.postMessage(message))
  worker.addEventListener('message', (event) => host.receive(event.data))
  worker.addEventListener('error', (event) => host.died(`web executor Worker failed: ${event.message}`))
  host.boot(options.config ?? {}, options.logLevel)
  try {
    await host.ready
  } catch (error) {
    // The caller gets no handle to a Worker that did not boot: stop it here.
    worker.terminate()
    throw error
  }
  return {
    WorkerSocket: host.WorkerSocket,
    dead: host.dead,
    terminate: () => {
      host.shutDown()
      worker.terminate()
    },
    worker,
  }
}
