#!/usr/bin/env tsx
// AD4M Benchmark CLI — compare Oxigraph/SPARQL vs baseline executors

import { parseArgs } from 'node:util'
import { resolve } from 'node:path'
import { HttpBenchClient } from './client'
import { runBenchmarks, SUITE_NAMES, type SuiteName } from './runner'
import { printTerminal, writeResults } from './reporter'
import {
  startExecutor, stopExecutor, getClient,
  type ExecutorConfig,
} from './executor'

const { values } = parseArgs({
  options: {
    'sparql-port': { type: 'string', default: '12000' },
    'baseline-port': { type: 'string', default: '12001' },
    'admin-credential': { type: 'string', default: 'test-admin' },
    'suite': { type: 'string' },
    'iterations': { type: 'string', default: '100' },
    'scale': { type: 'string', default: '10000' },
    'output': { type: 'string', default: './results' },
    'warmup': { type: 'string', default: '5' },
    'no-manage': { type: 'boolean', default: false },
    'json': { type: 'boolean', default: false },
    'sparql-binary': { type: 'string' },
    'baseline-binary': { type: 'string' },
    'help': { type: 'boolean', short: 'h', default: false },
  },
  strict: true,
})

if (values.help) {
  console.log(`
AD4M Benchmark Tool — Oxigraph/SPARQL vs baseline

Usage: npx tsx src/index.ts [options]

Options:
  --sparql-port <port>       Port for SPARQL executor (default: 12000)
  --baseline-port <port>      Port for baseline executor (default: 12001)
  --admin-credential <str>   Admin credential (default: test-admin)
  --suite <name>             Run specific suite: write|query|sparql|subject|scale
  --iterations <n>           Iterations per test (default: 100)
  --scale <n>                Max scale for scale tests (default: 10000)
  --output <dir>             Output directory (default: ./results)
  --warmup <n>               Warmup iterations (default: 5)
  --no-manage                Don't start/stop executors (assume running)
  --sparql-binary <path>     Path to SPARQL executor binary
  --baseline-binary <path>    Path to baseline executor binary
  --json                     Output JSON only
  -h, --help                 Show this help
`)
  process.exit(0)
}

const sparqlPort = parseInt(values['sparql-port']!, 10)
const baselinePort = parseInt(values['baseline-port']!, 10)
const adminCredential = values['admin-credential']!
const iterations = parseInt(values.iterations!, 10)
const maxScale = parseInt(values.scale!, 10)
const warmup = parseInt(values.warmup!, 10)

for (const [name, val] of Object.entries({ sparqlPort, baselinePort, iterations, maxScale, warmup })) {
  if (Number.isNaN(val)) {
    console.error(`Invalid numeric value for --${name.replace(/([A-Z])/g, '-$1').toLowerCase()}: expected a number`)
    process.exit(1)
  }
}
const outputDir = resolve(values.output!)
const noManage = values['no-manage']!
const jsonOnly = values.json!

const suites: SuiteName[] = values.suite
  ? [values.suite as SuiteName]
  : [...SUITE_NAMES]

if (values.suite && !SUITE_NAMES.includes(values.suite as SuiteName)) {
  console.error(`Unknown suite: ${values.suite}. Valid: ${SUITE_NAMES.join(', ')}`)
  process.exit(1)
}

const sparqlConfig: ExecutorConfig = {
  binary: values['sparql-binary'] || resolve(__dirname, '../bin/ad4m-executor'),
  port: sparqlPort,
  dataPath: resolve('/tmp/ad4m-bench-sparql'),
  adminCredential,
  label: 'sparql',
}

if (!values['baseline-binary']) {
  console.error('--baseline-binary is required: path to the baseline executor binary')
  process.exit(1)
}

const baselineConfig: ExecutorConfig = {
  binary: values['baseline-binary'],
  port: baselinePort,
  dataPath: resolve('/tmp/ad4m-bench-baseline'),
  adminCredential,
  label: 'baseline',
}

async function main(): Promise<void> {
  console.log('\n  AD4M Benchmark: Oxigraph/SPARQL vs baseline')
  console.log('  ' + '═'.repeat(50))
  console.log(`  Iterations: ${iterations} | Warmup: ${warmup} | Suites: ${suites.join(', ')}`)

  if (!noManage) {
    try {
      await startExecutor(sparqlConfig)
      await startExecutor(baselineConfig)
    } catch (err) {
      console.error(`\n  ✗ Failed to start executors: ${err}`)
      await stopExecutor(sparqlConfig).catch(() => {})
      await stopExecutor(baselineConfig).catch(() => {})
      process.exit(1)
    }
  }

  const sparqlClient = noManage
    ? new HttpBenchClient({ label: 'sparql', url: `http://127.0.0.1:${sparqlPort}`, adminCredential })
    : getClient(sparqlConfig)

  const baselineClient = noManage
    ? new HttpBenchClient({ label: 'baseline', url: `http://127.0.0.1:${baselinePort}`, adminCredential })
    : getClient(baselineConfig)

  // Verify connectivity
  for (const [label, client] of [['SPARQL', sparqlClient], ['baseline', baselineClient]] as const) {
    const ok = await client.healthCheck()
    if (!ok) {
      console.error(`\n  ✗ Cannot reach ${label} executor. Is it running?`)
      if (!noManage) {
        await stopExecutor(sparqlConfig).catch(() => {})
        await stopExecutor(baselineConfig).catch(() => {})
      }
      process.exit(1)
    }
    console.log(`  ✓ ${label} executor reachable`)
  }

  try {
    const report = await runBenchmarks({
      sparqlClient,
      baselineClient,
      suites,
      iterations,
      warmup,
      maxScale,
    })

    if (jsonOnly) {
      console.log(JSON.stringify(report, null, 2))
    } else {
      console.log('')
      printTerminal(report)
      const { jsonPath, mdPath } = await writeResults(report, outputDir)
      console.log(`\n  Results written to:`)
      console.log(`    JSON: ${jsonPath}`)
      console.log(`    Markdown: ${mdPath}`)
    }
  } finally {
    if (!noManage) {
      await stopExecutor(sparqlConfig).catch(() => {})
      await stopExecutor(baselineConfig).catch(() => {})
    }
  }
}

main().catch((err) => {
  console.error('Fatal:', err)
  process.exit(1)
})
