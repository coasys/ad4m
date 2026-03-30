// Reporter — terminal, JSON, and markdown output

import { writeFile, mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { formatDuration, formatOps, type Stats } from './utils'

export interface SuiteResult {
  name: string
  tests: Array<{
    name: string
    sparql: Stats
    baseline: Stats
  }>
}

export interface BenchmarkReport {
  timestamp: string
  suites: SuiteResult[]
  config: Record<string, unknown>
}

function speedup(sparql: Stats, baseline: Stats): string {
  if (sparql.median === 0 || baseline.median === 0) return 'N/A'
  const ratio = baseline.median / sparql.median
  if (ratio > 1) return `SPARQL ${ratio.toFixed(2)}x faster`
  if (ratio < 1) return `baseline ${(1 / ratio).toFixed(2)}x faster`
  return 'Equal'
}

function padRight(s: string, len: number): string {
  return s + ' '.repeat(Math.max(0, len - s.length))
}

export function printTerminal(report: BenchmarkReport): void {
  const W = 66
  console.log('╔' + '═'.repeat(W) + '╗')
  console.log('║' + padRight('  AD4M Benchmark: Oxigraph/SPARQL vs baseline', W) + '║')
  console.log('║' + padRight(`  ${report.timestamp}`, W) + '║')
  console.log('╠' + '═'.repeat(W) + '╣')
  console.log('')

  for (const suite of report.suites) {
    console.log(`  ${suite.name}`)
    console.log('  ' + '─'.repeat(W - 2))

    for (const test of suite.tests) {
      console.log(`    ${test.name}`)
      console.log(`      SPARQL:    ${padRight(formatDuration(test.sparql.median) + ' median', 16)} (p95: ${formatDuration(test.sparql.p95)}, p99: ${formatDuration(test.sparql.p99)})  ${formatOps(test.sparql.opsPerSec)}`)
      console.log(`      baseline: ${padRight(formatDuration(test.baseline.median) + ' median', 16)} (p95: ${formatDuration(test.baseline.p95)}, p99: ${formatDuration(test.baseline.p99)})  ${formatOps(test.baseline.opsPerSec)}`)
      console.log(`      Δ: ${speedup(test.sparql, test.baseline)}`)
      console.log('')
    }
  }

  console.log('╚' + '═'.repeat(W) + '╝')
}

export function generateMarkdown(report: BenchmarkReport): string {
  const lines: string[] = [
    `# AD4M Benchmark: Oxigraph/SPARQL vs baseline`,
    ``,
    `**Date:** ${report.timestamp}`,
    ``,
  ]

  for (const suite of report.suites) {
    lines.push(`## ${suite.name}`, '')
    lines.push('| Test | Engine | Median | P95 | P99 | Ops/s | Speedup |')
    lines.push('|------|--------|--------|-----|-----|-------|---------|')

    for (const test of suite.tests) {
      const delta = speedup(test.sparql, test.baseline)
      lines.push(`| ${test.name} | SPARQL | ${formatDuration(test.sparql.median)} | ${formatDuration(test.sparql.p95)} | ${formatDuration(test.sparql.p99)} | ${formatOps(test.sparql.opsPerSec)} | ${delta} |`)
      lines.push(`| | baseline | ${formatDuration(test.baseline.median)} | ${formatDuration(test.baseline.p95)} | ${formatDuration(test.baseline.p99)} | ${formatOps(test.baseline.opsPerSec)} | |`)
    }
    lines.push('')
  }

  return lines.join('\n')
}

export async function writeResults(report: BenchmarkReport, outputDir: string): Promise<{ jsonPath: string; mdPath: string }> {
  await mkdir(outputDir, { recursive: true })
  const ts = report.timestamp.replace(/[: ]/g, '-').replace(/[T]/g, 'T')
  const baseName = `benchmark-${ts}`

  const jsonPath = join(outputDir, `${baseName}.json`)
  const mdPath = join(outputDir, `${baseName}.md`)

  await writeFile(jsonPath, JSON.stringify(report, null, 2))
  await writeFile(mdPath, generateMarkdown(report))

  return { jsonPath, mdPath }
}
