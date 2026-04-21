// Executor lifecycle management

import { spawn, type ChildProcess } from 'node:child_process'
import { mkdir, rm } from 'node:fs/promises'
import { GraphQLClient, type ExecutorEndpoint } from './client'

export interface ExecutorConfig {
  binary: string
  port: number
  dataPath: string
  adminCredential: string
  label: string
}

const processes = new Map<string, ChildProcess>()

// Ensure child processes are cleaned up on unexpected exit
function cleanupAll(): void {
  for (const [label, child] of processes) {
    child.kill('SIGKILL')
    processes.delete(label)
  }
}
process.on('exit', cleanupAll)
process.on('SIGINT', () => { cleanupAll(); process.exit(130) })
process.on('SIGTERM', () => { cleanupAll(); process.exit(143) })

function makeEndpoint(config: ExecutorConfig): ExecutorEndpoint {
  return {
    label: config.label,
    url: `http://127.0.0.1:${config.port}/graphql`,
    adminCredential: config.adminCredential,
  }
}

export function getClient(config: ExecutorConfig): GraphQLClient {
  return new GraphQLClient(makeEndpoint(config))
}

export async function startExecutor(config: ExecutorConfig): Promise<void> {
  console.log(`  Starting ${config.label} executor on port ${config.port}...`)

  // Clean data dir
  await rm(config.dataPath, { recursive: true, force: true })
  await mkdir(config.dataPath, { recursive: true })

  const child = spawn(config.binary, [
    '--app-data-path', config.dataPath,
    '--port', String(config.port),
    '--admin-credential', config.adminCredential,
    '--run-dapp-server', 'false',
  ], {
    stdio: ['ignore', 'pipe', 'pipe'],
    detached: false,
    env: { ...process.env },
  })

  processes.set(config.label, child)

  child.stdout?.on('data', () => {}) // drain
  child.stderr?.on('data', () => {}) // drain

  child.on('exit', (code) => {
    if (processes.has(config.label)) {
      console.warn(`  ⚠ ${config.label} executor exited with code ${code}`)
    }
  })

  await waitForReady(config, 120_000)
  await initializeAgent(config, 'benchmark-passphrase')
  console.log(`  ✓ ${config.label} executor ready`)
}

export async function stopExecutor(config: ExecutorConfig): Promise<void> {
  const child = processes.get(config.label)
  if (child) {
    processes.delete(config.label)
    child.kill('SIGTERM')
    await new Promise<void>((resolve) => {
      const timer = setTimeout(() => { child.kill('SIGKILL'); resolve() }, 5000)
      child.on('exit', () => { clearTimeout(timer); resolve() })
    })
    console.log(`  ✓ ${config.label} executor stopped`)
  }
}

export async function waitForReady(config: ExecutorConfig, timeoutMs: number): Promise<void> {
  const client = getClient(config)
  const deadline = Date.now() + timeoutMs
  while (Date.now() < deadline) {
    if (await client.healthCheck()) return
    await new Promise(r => setTimeout(r, 500))
  }
  throw new Error(`${config.label} executor did not become ready within ${timeoutMs}ms`)
}

export async function initializeAgent(config: ExecutorConfig, passphrase: string): Promise<string> {
  const client = getClient(config)
  const status = await client.agentStatus()
  if (status.isInitialized) {
    if (!status.isUnlocked) {
      return client.agentUnlock(passphrase)
    }
    return status.did
  }
  return client.agentGenerate(passphrase)
}
