/**
 * Executor lifecycle for vitest ORM benchmarks.
 *
 * Provides `setupExecutor()` / `teardownExecutor()` for use in vitest
 * `beforeAll` / `afterAll` hooks. Creates both a lightweight GraphQL client
 * (for seeding) and a full Ad4mClient + PerspectiveProxy (for ORM tests).
 */
import { ChildProcess, spawn } from 'child_process'
import { mkdtemp, rm, access, copyFile } from 'fs/promises'
import * as path from 'path'
import * as os from 'os'
import { GraphQLClient, type ExecutorEndpoint } from './client'
import { Ad4mClient } from '@coasys/ad4m'
import type { PerspectiveProxy } from '@coasys/ad4m'
import { ApolloClient, InMemoryCache, HttpLink } from '@apollo/client/core/index.js'
import fetch from 'cross-fetch'

const __filename = new URL(import.meta.url).pathname
const __dirname = path.dirname(__filename)

const EXECUTOR_PATH =
  process.env.BENCH_EXECUTOR_PATH ??
  path.resolve(__dirname, '../../target/release/ad4m-executor')

const BOOTSTRAP_SEED =
  process.env.BENCH_BOOTSTRAP_SEED ??
  path.resolve(__dirname, '../../cli/mainnet_seed.json')

const PASSPHRASE = 'benchmark-test-passphrase'
const BASE_PORT = 14_500 + Math.floor(Math.random() * 500)
let portCounter = 0

export interface OrmExecutorContext {
  /** Lightweight GraphQL client for seeding and raw SPARQL */
  client: GraphQLClient
  /** Full Ad4mClient for ORM operations */
  ad4mClient: Ad4mClient
  /** PerspectiveProxy for Ad4mModel.register() and instance methods */
  perspective: PerspectiveProxy
  perspectiveUuid: string
  executorUrl: string
  adminCredential: string
}

interface ExecutorHandle {
  ctx: OrmExecutorContext
  proc: ChildProcess
  dataDir: string
}

async function waitForExecutor(url: string, timeoutMs = 90_000): Promise<void> {
  const deadline = Date.now() + timeoutMs
  while (Date.now() < deadline) {
    try {
      const resp = await fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ query: '{ agentStatus { isInitialized } }' }),
        signal: AbortSignal.timeout(3000),
      })
      if (resp.ok) return
    } catch {
      // not ready yet
    }
    await new Promise((r) => setTimeout(r, 1000))
  }
  throw new Error(`Executor at ${url} did not become ready within ${timeoutMs}ms`)
}

let _handle: ExecutorHandle | null = null

export async function setupExecutor(): Promise<OrmExecutorContext> {
  if (_handle) return _handle.ctx

  const port = BASE_PORT + portCounter++
  const gqlUrl = `http://127.0.0.1:${port}/graphql`
  const dataDir = await mkdtemp(path.join(os.tmpdir(), 'bench-ad4m-'))
  const adminCredential = 'bench-admin-secret'

  const executorArgs = [
    'run',
    '--app-data-path', dataDir,
    '--gql-port', String(port),
    '--admin-credential', adminCredential,
    '--run-dapp-server', 'false',
    '--language-language-only', 'true',
    '--connect-holochain', 'false',
  ]

  try {
    await access(BOOTSTRAP_SEED)
    executorArgs.push('--network-bootstrap-seed', BOOTSTRAP_SEED)
    await copyFile(BOOTSTRAP_SEED, path.join(dataDir, 'mainnet_seed.seed'))
  } catch { /* no seed file */ }

  console.log(`[executor] Spawning on port ${port}, data: ${dataDir}`)
  const proc = spawn(EXECUTOR_PATH, executorArgs, {
    stdio: ['ignore', 'pipe', 'pipe'],
    env: { ...process.env, RUST_LOG: process.env.BENCH_RUST_LOG ?? 'warn' },
  })

  let output = ''
  const collect = (chunk: Buffer) => {
    output += chunk.toString()
    if (output.length > 20480) output = output.slice(-20480)
  }
  proc.stdout?.on('data', collect)
  proc.stderr?.on('data', collect)
  proc.on('error', (err) => {
    throw new Error(`Failed to spawn executor: ${err.message}\nPath: ${EXECUTOR_PATH}`)
  })

  try {
    console.log('[executor] Waiting for startup...')
    await waitForExecutor(gqlUrl, 120_000)
    console.log('[executor] Ready')

    const endpoint: ExecutorEndpoint = { label: 'orm-bench', url: gqlUrl, adminCredential }
    const client = new GraphQLClient(endpoint)

    const status = await client.agentStatus()
    if (!status.isInitialized) {
      await client.agentGenerate(PASSPHRASE)
    } else if (!status.isUnlocked) {
      await client.agentUnlock(PASSPHRASE)
    }
    console.log('[executor] Agent ready')

    const perspectiveUuid = await client.addPerspective(`bench-${port}`)
    console.log(`[executor] Perspective: ${perspectiveUuid}`)

    const apolloClient = new ApolloClient({
      link: new HttpLink({
        uri: gqlUrl,
        fetch,
        headers: { authorization: adminCredential },
      }),
      cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
      defaultOptions: {
        query: { fetchPolicy: 'no-cache' },
        mutate: { fetchPolicy: 'no-cache' },
      },
    })
    const ad4mClient = new Ad4mClient(apolloClient as any, false)
    const perspective = await ad4mClient.perspective.byUUID(perspectiveUuid)
    if (!perspective) {
      throw new Error(`Perspective ${perspectiveUuid} not found after creation`)
    }
    console.log('[executor] Ad4mClient + PerspectiveProxy ready')

    const ctx: OrmExecutorContext = {
      client,
      ad4mClient,
      perspective,
      perspectiveUuid,
      executorUrl: `http://127.0.0.1:${port}`,
      adminCredential,
    }

    _handle = { ctx, proc, dataDir }
    return ctx
  } catch (err) {
    console.error('[executor] Startup output:\n', output)
    proc.kill('SIGTERM')
    await rm(dataDir, { recursive: true, force: true }).catch(() => {})
    throw err
  }
}

export async function teardownExecutor(): Promise<void> {
  if (!_handle) return
  const { ctx, proc, dataDir } = _handle
  _handle = null

  try { await ctx.client.removePerspective(ctx.perspectiveUuid) } catch {}
  proc.kill('SIGTERM')
  await new Promise<void>((resolve) => {
    const timeout = setTimeout(() => { proc.kill('SIGKILL'); resolve() }, 5000)
    proc.on('exit', () => { clearTimeout(timeout); resolve() })
  })
  await rm(dataDir, { recursive: true, force: true }).catch(() => {})
  console.log('[executor] Stopped, data cleaned')
}
