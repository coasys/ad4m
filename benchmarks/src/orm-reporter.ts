/**
 * JSON reporter for ORM benchmarks — collects results and writes to results/ dir.
 */
import { writeFile, mkdir } from 'node:fs/promises'
import { join, dirname } from 'node:path'
import { computeStats, formatDuration, type Stats } from './utils'

const __filename = new URL(import.meta.url).pathname
const __dirname = dirname(__filename)

export interface OrmBenchmarkResult {
  name: string
  spec: string
  transport: string
  samples: number[]
  stats: Stats
  metadata?: Record<string, unknown>
}

export interface OrmBenchmarkReport {
  timestamp: string
  gitBranch?: string
  gitCommit?: string
  executor: string
  linkCount: number
  results: OrmBenchmarkResult[]
}

const resultsDir = join(__dirname, '../results')

function getGitInfo(): { branch?: string; commit?: string } {
  try {
    const { execSync } = require('child_process')
    const branch = process.env.BENCH_LABEL ??
      execSync('git rev-parse --abbrev-ref HEAD', { encoding: 'utf-8' }).trim()
    const commit = execSync('git rev-parse --short HEAD', { encoding: 'utf-8' }).trim()
    return { branch, commit }
  } catch {
    return {}
  }
}

export async function writeOrmReport(
  results: OrmBenchmarkResult[],
  linkCount: number,
  executorInfo = 'ad4m-executor',
): Promise<string> {
  await mkdir(resultsDir, { recursive: true })

  const git = getGitInfo()
  const report: OrmBenchmarkReport = {
    timestamp: new Date().toISOString(),
    gitBranch: git.branch,
    gitCommit: git.commit,
    executor: executorInfo,
    linkCount,
    results,
  }

  const filename = `bench-${report.timestamp.replace(/[:.]/g, '-')}.json`
  const filepath = join(resultsDir, filename)
  await writeFile(filepath, JSON.stringify(report, null, 2))
  return filepath
}

export function formatOrmMarkdownTable(results: OrmBenchmarkResult[]): string {
  const lines: string[] = []
  lines.push('| Benchmark | Mean | Median | P95 | P99 | Min | Max | Samples |')
  lines.push('|-----------|------|--------|-----|-----|-----|-----|---------|')

  for (const r of results) {
    const s = r.stats
    lines.push(
      `| ${r.name} | ${formatDuration(s.mean)} | ${formatDuration(s.median)} | ${formatDuration(s.p95)} | ${formatDuration(s.p99)} | ${formatDuration(s.min)} | ${formatDuration(s.max)} | ${s.count} |`,
    )
  }

  return lines.join('\n')
}
