// Statistics and timing utilities

export interface Stats {
  mean: number
  median: number
  p95: number
  p99: number
  stddev: number
  min: number
  max: number
  count: number
  opsPerSec: number
}

export function computeStats(samples: number[]): Stats {
  if (samples.length === 0) {
    return { mean: 0, median: 0, p95: 0, p99: 0, stddev: 0, min: 0, max: 0, count: 0, opsPerSec: 0 }
  }

  const sorted = [...samples].sort((a, b) => a - b)
  const count = sorted.length
  const sum = sorted.reduce((a, b) => a + b, 0)
  const mean = sum / count
  const median = count % 2 === 0
    ? (sorted[count / 2 - 1] + sorted[count / 2]) / 2
    : sorted[Math.floor(count / 2)]
  const p95 = sorted[Math.min(Math.ceil(count * 0.95) - 1, count - 1)]
  const p99 = sorted[Math.min(Math.ceil(count * 0.99) - 1, count - 1)]
  const variance = sorted.reduce((acc, v) => acc + (v - mean) ** 2, 0) / count
  const stddev = Math.sqrt(variance)
  const min = sorted[0]
  const max = sorted[count - 1]
  const opsPerSec = mean > 0 ? 1000 / mean : 0

  return { mean, median, p95, p99, stddev, min, max, count, opsPerSec }
}

export function formatDuration(ms: number): string {
  if (ms < 1) return `${(ms * 1000).toFixed(0)}µs`
  if (ms < 1000) return `${ms.toFixed(1)}ms`
  return `${(ms / 1000).toFixed(2)}s`
}

export function formatOps(opsPerSec: number): string {
  if (opsPerSec >= 1000) return `${(opsPerSec / 1000).toFixed(1)}K ops/s`
  return `${opsPerSec.toFixed(0)} ops/s`
}

export function timeIt(): () => number {
  const start = performance.now()
  return () => performance.now() - start
}

// Simple seeded PRNG (mulberry32)
export function createRng(seed: number): () => number {
  let s = seed | 0
  return () => {
    s = (s + 0x6d2b79f5) | 0
    let t = Math.imul(s ^ (s >>> 15), 1 | s)
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

export function generateId(rng: () => number, prefix: string): string {
  return `${prefix}://${Math.floor(rng() * 0xffffffff).toString(16).padStart(8, '0')}`
}
