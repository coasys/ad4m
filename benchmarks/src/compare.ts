#!/usr/bin/env tsx
/**
 * Compare two benchmark result JSON files and output a Markdown diff table.
 *
 * Usage:
 *   pnpm bench:compare results/bench-A.json results/bench-B.json
 *   pnpm bench:compare results/bench-A.json results/bench-B.json --threshold 10
 *   pnpm bench:compare  # auto-detect last two reports
 */
import { readFile, readdir } from 'node:fs/promises'
import * as path from 'node:path'
import { formatDuration } from './utils'

interface Stats {
  mean: number
  median: number
  p95: number
  p99: number
  min: number
  max: number
  count: number
}

interface BenchmarkResult {
  name: string
  spec: string
  transport: string
  stats: Stats
}

interface BenchmarkReport {
  timestamp: string
  gitBranch?: string
  gitCommit?: string
  executor: string
  linkCount: number
  results: BenchmarkResult[]
}

function pctChange(baseline: number, current: number): string {
  if (baseline === 0) return 'N/A'
  const pct = ((current - baseline) / baseline) * 100
  const sign = pct > 0 ? '+' : ''
  const emoji = pct > 10 ? '🔴' : pct < -10 ? '🟢' : '⚪'
  return `${emoji} ${sign}${pct.toFixed(1)}%`
}

async function loadReport(filepath: string): Promise<BenchmarkReport> {
  const content = await readFile(filepath, 'utf-8')
  return JSON.parse(content)
}

async function findLatestReports(dir: string, count = 2): Promise<string[]> {
  const files = await readdir(dir)
  const jsonFiles = files
    .filter((f) => f.endsWith('.json') && f.startsWith('bench-'))
    .sort()
    .reverse()
  return jsonFiles.slice(0, count).reverse().map((f) => path.join(dir, f))
}

async function main() {
  const args = process.argv.slice(2)
  let fileA: string
  let fileB: string
  let threshold = 10

  const nonFlags = args.filter((a) => !a.startsWith('--'))
  const thresholdIdx = args.indexOf('--threshold')
  if (thresholdIdx >= 0 && args[thresholdIdx + 1]) {
    threshold = parseFloat(args[thresholdIdx + 1])
  }

  const __filename = new URL(import.meta.url).pathname
  const __dirname = path.dirname(__filename)
  const resultsDir = path.resolve(__dirname, '../results')

  if (nonFlags.length >= 2) {
    fileA = nonFlags[0]
    fileB = nonFlags[1]
  } else if (nonFlags.length === 0) {
    const latest = await findLatestReports(resultsDir)
    if (latest.length < 2) {
      console.error('Need at least 2 benchmark reports to compare. Run benchmarks twice first.')
      process.exit(1)
    }
    ;[fileA, fileB] = latest
    console.log(`Auto-detected:\n  Baseline: ${path.basename(fileA)}\n  Current:  ${path.basename(fileB)}\n`)
  } else {
    console.error('Usage: pnpm bench:compare [baseline.json] [current.json] [--threshold N]')
    process.exit(1)
  }

  const baseline = await loadReport(fileA)
  const current = await loadReport(fileB)

  console.log('# Benchmark Comparison\n')
  console.log(`| | Baseline | Current |`)
  console.log(`|---|---|---|`)
  console.log(`| Timestamp | ${baseline.timestamp} | ${current.timestamp} |`)
  console.log(`| Branch | ${baseline.gitBranch ?? '?'} | ${current.gitBranch ?? '?'} |`)
  console.log(`| Commit | ${baseline.gitCommit ?? '?'} | ${current.gitCommit ?? '?'} |`)
  console.log(`| Links | ${baseline.linkCount} | ${current.linkCount} |`)
  console.log()

  const baselineMap = new Map<string, BenchmarkResult>()
  for (const r of baseline.results) {
    baselineMap.set(`${r.spec}::${r.name}`, r)
  }

  console.log('| Benchmark | Baseline (mean) | Current (mean) | Change | P95 Δ |')
  console.log('|-----------|-----------------|----------------|--------|-------|')

  for (const r of current.results) {
    const key = `${r.spec}::${r.name}`
    const b = baselineMap.get(key)

    if (b) {
      const meanChange = pctChange(b.stats.mean, r.stats.mean)
      const p95Change = pctChange(b.stats.p95, r.stats.p95)
      console.log(
        `| ${r.name} | ${formatDuration(b.stats.mean)} | ${formatDuration(r.stats.mean)} | ${meanChange} | ${p95Change} |`,
      )
    } else {
      console.log(`| ${r.name} | — | ${formatDuration(r.stats.mean)} | NEW | — |`)
    }
  }

  for (const [key, b] of baselineMap) {
    const exists = current.results.some((r) => `${r.spec}::${r.name}` === key)
    if (!exists) {
      console.log(`| ~~${b.name}~~ | ${formatDuration(b.stats.mean)} | — | REMOVED | — |`)
    }
  }

  console.log()

  const regressions = current.results.filter((r) => {
    const b = baselineMap.get(`${r.spec}::${r.name}`)
    return b && b.stats.mean > 0 && ((r.stats.mean - b.stats.mean) / b.stats.mean) * 100 > threshold
  })

  const improvements = current.results.filter((r) => {
    const b = baselineMap.get(`${r.spec}::${r.name}`)
    return b && b.stats.mean > 0 && ((r.stats.mean - b.stats.mean) / b.stats.mean) * 100 < -threshold
  })

  if (regressions.length > 0) {
    console.log(`⚠️  ${regressions.length} regression(s) above ${threshold}% threshold:`)
    for (const r of regressions) {
      const b = baselineMap.get(`${r.spec}::${r.name}`)!
      const pct = (((r.stats.mean - b.stats.mean) / b.stats.mean) * 100).toFixed(1)
      console.log(`   - ${r.name}: ${formatDuration(b.stats.mean)} → ${formatDuration(r.stats.mean)} (+${pct}%)`)
    }
  }

  if (improvements.length > 0) {
    console.log(`✅ ${improvements.length} improvement(s) above ${threshold}% threshold:`)
    for (const r of improvements) {
      const b = baselineMap.get(`${r.spec}::${r.name}`)!
      const pct = (((r.stats.mean - b.stats.mean) / b.stats.mean) * 100).toFixed(1)
      console.log(`   - ${r.name}: ${formatDuration(b.stats.mean)} → ${formatDuration(r.stats.mean)} (${pct}%)`)
    }
  }

  if (regressions.length === 0 && improvements.length === 0) {
    console.log(`✅ All benchmarks within ±${threshold}% of baseline`)
  }
}

main().catch((err) => {
  console.error(err)
  process.exit(1)
})
