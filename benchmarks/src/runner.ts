// Benchmark runner — orchestrates suites

import type { GraphQLClient } from './client'
import type { SuiteResult, BenchmarkReport } from './reporter'
import { run as runWrite } from './suites/write-throughput'
import { run as runPointQueries } from './suites/point-queries'
import { run as runSparqlQueries } from './suites/sparql-queries'
import { run as runSubjectOps } from './suites/subject-operations'
import { run as runScale } from './suites/scale'

export const SUITE_NAMES = ['write', 'query', 'sparql', 'subject', 'scale'] as const
export type SuiteName = typeof SUITE_NAMES[number]

export interface RunConfig {
  sparqlClient: GraphQLClient
  baselineClient: GraphQLClient
  suites: SuiteName[]
  iterations: number
  warmup: number
  maxScale: number
}

export async function runBenchmarks(config: RunConfig): Promise<BenchmarkReport> {
  const results: SuiteResult[] = []
  const { sparqlClient, baselineClient, iterations, warmup, suites, maxScale } = config

  for (const suite of suites) {
    console.log(`\n  Running: ${suite}`)
    console.log('  ' + '─'.repeat(50))

    switch (suite) {
      case 'write': {
        const r = await runWrite(sparqlClient, baselineClient, iterations, warmup)
        results.push(r)
        break
      }
      case 'query': {
        const r = await runPointQueries(sparqlClient, baselineClient, iterations, warmup)
        results.push(r)
        break
      }
      case 'sparql': {
        const r = await runSparqlQueries(sparqlClient, baselineClient, iterations, warmup)
        results.push(r)
        break
      }
      case 'subject': {
        const r = await runSubjectOps(sparqlClient, baselineClient, iterations, warmup)
        results.push(r)
        break
      }
      case 'scale': {
        const r = await runScale(sparqlClient, baselineClient, iterations, warmup, maxScale)
        results.push(r)
        break
      }
    }
  }

  return {
    timestamp: new Date().toISOString(),
    suites: results,
    config: {
      iterations,
      warmup,
      maxScale,
      suites,
    },
  }
}
