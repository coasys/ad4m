# AD4M Benchmark: Oxigraph/SPARQL vs Baseline

Compare the performance of the new Oxigraph/SPARQL executor against the reference Baseline executor.

## Quick Start

```bash
# Install dependencies
pnpm install

# Run with both executors already running
npx tsx src/index.ts --no-manage --sparql-port 12000 --baseline-port 12001

# Run specific suite
npx tsx src/index.ts --no-manage --suite write --iterations 50

# Let the tool manage executor lifecycle
npx tsx src/index.ts --sparql-binary /path/to/new/ad4m-executor --baseline-binary ./bin/ad4m-cli-executor-macos-0.12.0-rc2-aarch64
```

## Benchmark Suites

| Suite | Flag | What it tests |
|-------|------|---------------|
| **write** | `--suite write` | Single & batch link write throughput |
| **query** | `--suite query` | Point queries by source, predicate, combined |
| **sparql** | `--suite sparql` | Complex queries (SPARQL vs SPARQL) |
| **subject** | `--suite subject` | SHACL subject class operations |
| **scale** | `--suite scale` | Performance at 100, 1K, 10K links |

## CLI Options

| Option | Default | Description |
|--------|---------|-------------|
| `--sparql-port` | 12000 | Port for SPARQL/Oxigraph executor |
| `--baseline-port` | 12001 | Port for Baseline executor |
| `--admin-credential` | test-admin | Admin credential for both executors |
| `--suite` | all | Run specific suite only |
| `--iterations` | 100 | Iterations per test |
| `--warmup` | 5 | Warmup iterations (discarded) |
| `--scale` | 10000 | Max links for scale tests |
| `--output` | ./results | Output directory |
| `--no-manage` | false | Don't start/stop executors |
| `--sparql-binary` | ./bin/ad4m-executor | Path to SPARQL executor |
| `--baseline-binary` | ./bin/ad4m-cli-executor-* | Path to reference executor |
| `--json` | false | JSON output only |

## Output

Results are written to `results/` as both JSON (raw data) and Markdown (formatted report).

## Interpreting Results

- **Median** is the primary comparison metric (robust to outliers)
- **P95/P99** show tail latency behaviour
- **Ops/sec** computed from median latency
- **Δ** shows relative speedup between engines
- Warmup runs are discarded to avoid cold-start bias
- Each test uses isolated perspectives (created fresh, torn down after)

## Reference Binary

The Baseline reference binary (`ad4m-cli-executor-macos-0.12.0-rc2-aarch64`) is downloaded from [GitHub releases](https://github.com/coasys/ad4m/releases/tag/v0.12.0-rc2) into `bin/`. This directory is gitignored.
