# sparopt 0.3.9: pointer-width-dependent cost saturation changes estimates on wasm32

Draft for an upstream Oxigraph issue. Do not file it from this document. Nobody files this upstream without Nico.

The diff below is Data's original `sparopt-u64.patch`. This AD4M tree does not vendor the patched crate and does not apply the patch. The original file's sha256 is `fbbe9ef558eabb8c8e67425270dfa876b0a9b6ce0149a76f5b0754041ca28919`. It replaces 12 lines. `patch --dry-run -p0` from an unpacked sparopt 0.3.9 crate succeeded against `src/optimizer.rs`. The unpacked file is byte-identical to `src/optimizer.rs` in the crates.io tarball whose sha256 is the sparopt checksum in the Versions table. Applying the original patch in a temporary copy also succeeded and left no `usize` in that `optimizer.rs`. That apply was not a build, and this run did not compile sparopt.

The review states that every hunk matches stock sparopt 0.3.9 and that applying the hunks in memory leaves no `usize` in `optimizer.rs` (`lal-web-exec-s0b-review.md` line 42). It did not execute the benchmarks: line 12 says the raw run files were not supplied to that review. What the review says was tested, and the numbers it gives, are its lines 34–35: Q2 from 160.9 ms stock Wasm to 2.94 ms patched, against 1.90 ms native, about 54.7× within Wasm, and about 84.7× against native. Those five figures are the review's. They are not recomputed here. The raw logs that sit next to them are quoted under Historical measurements. The review's 160.9 and 2.94 are the one-decimal and two-decimal roundings of `wasm-stock-run2.txt` line 5 (`160.886`) and `wasm-u64-run2.txt` line 5 (`2.937`). The review's 1.90 is not a printed log line. The native Q2 medians are printed below.

## Versions

Checked against AD4M commit `2963e3c81a4ea638ffb5796e8412abecebf7a3bc` (`origin/spike/web-executor-s10`). `Cargo.lock` is format version 3, which does not record feature resolution. Package identity below is the lockfile `source` and `checksum`. The feature chain is from the crates.io manifests whose checksums match that lock.

| Package | Version | Lock source | Checksum |
| --- | --- | --- | --- |
| oxigraph | 0.5.13 | `registry+https://github.com/rust-lang/crates.io-index` | `9b993c12c391a7733c93171f9d07fbd80e4b99060ce3e4c76e0875cb3e63d0a8` |
| spareval | 0.2.9 | same registry | `072b1b44c9003e54a7b0817b65ce156d94ab46250dcefe0f24a9b3cc4020c1c6` |
| sparopt | 0.3.9 | same registry | `8c1d1327a49bef5f6f294a2ddcf01635c5449a0654e9a35d3f50d7a45e4ecc78` |
| spargebra | 0.4.8 | same registry | `43d5fd4ba16eea5586aa54227ca2c0a394794a0f82e39c6c98a57cc6877f0230` |
| oxrdf | 0.3.5 | same registry | `08037a3c3cf580d065caec6a864cf97f51be219862d95c691a48c7543406d675` |
| sparesults | 0.3.5 | same registry | `dd0fd25bd7e7eb2af56a45bc65ca768a246a50163db9215fc26da002135f6ece` |

`sparopt` 0.3.9 declares `oxrdf = "=0.3.5"` and `spargebra = "=0.4.8"`. Its `.cargo_vcs_info.json` records git `2837810c8993da671d496eabf7677d7573c5c522` at `lib/sparopt`. `lib/sparopt/src/optimizer.rs` at that commit is byte-identical to `src/optimizer.rs` in the crates.io tarball. Line numbers below are that file:

https://github.com/oxigraph/oxigraph/blob/2837810c8993da671d496eabf7677d7573c5c522/lib/sparopt/src/optimizer.rs

The S10 lock uses registry sparopt 0.3.9. Root `Cargo.toml` has no sparopt patch. Its `[patch.crates-io]` (line 30) names `lair_keystore`, `lair_keystore_api`, `hc_seed_bundle`, and the `sqlx` family. `rust-executor/Cargo.toml` `[patch.crates-io]` (line 330) names `rusqlite` only. No `Cargo.toml` in this commit names `sparopt` or `spareval`.

Native executor dependency (`rust-executor/Cargo.toml` line 261): `oxigraph` 0.5.13 with features `rocksdb` and `rdf-12`. Wasm32 executor dependency (lines 265–267): `oxigraph` 0.5.13, `default-features = false`, features `rdf-12` and `js`, target `wasm32-unknown-unknown`.

Published `oxigraph` 0.5.13 enables `spareval` `=0.2.9` with features `sep-0002`, `sep-0006`, and `calendar-ext` (`Cargo.toml` lines 126–132 of the checksummed crate). Those features sit on the dependency, so `default-features = false` on `oxigraph` does not remove them. `spareval` 0.2.9 maps `sep-0006` to `sparopt/sep-0006` (its `Cargo.toml` line 43) and depends on `sparopt` `=0.3.9`. `oxigraph`'s `rdf-12` feature also enables `spareval/sparql-12` (lines 65–68), which enables `sparopt/sparql-12`.

The native, wasm-stock, wasm-u64, and `wasm-196.txt` logs do not print a runtime name or a compiler version. `browser-run1.txt` line 9 prints `HeadlessChrome/146.0.0.0` and `Linux x86_64` in the user agent. `browser-run1.txt` line 25 prints `Node.js v24.20.0` on the harness stack after that Chrome run. The review's line 96 states the primary comparison was one Linux x86_64 machine, Node 24.20.0, and release Oxigraph in memory on both sides. That host string is the review's. The wasm and native timing files do not print it.

## Reproducer

`usize` couples heuristic costs to pointer width. On `wasm32-unknown-unknown`, `usize` is 32 bits and saturates at `4,294,967,295`. On a 64-bit target, `usize` saturates at `18,446,744,073,709,551,615`.

This expression is the red case. `1,000,000 × 1,000,000 / 1,000` must produce `1,000,000,000`. Stock 32-bit saturating arithmetic produces `4,294,967`, because `u32::MAX / 1000 = 4,294,967`.

```rust
let left = 1_000_000usize;
let right = 1_000_000usize;
let estimated = left.saturating_mul(right).saturating_div(1_000);
println!("{estimated}");
```

On a 32-bit `usize` the product saturates to `u32::MAX` before the division, and the quotient is `4,294,967`. On a 64-bit `usize` the same expression prints `1,000,000,000`. Checked with `rustc` for `u32` and `u64` (`4294967` and `1000000000`). The same multiply-then-divide shape is what `estimate_join_cost` does (`src/optimizer.rs` lines 1046–1048).

A smaller recorded probe has the query shape from the AD4M hydration workload: RDF-star reification, one mandatory annotation, and one OPTIONAL. The fence below is that structure example. It is not a line copied from a timing log, and it is not a standalone timing reproducer. The dataset generator is not in the archive.

```sparql
SELECT * WHERE {
  VALUES ?source { <todo://t9999> <todo://t9998> }
  ?source ?predicate ?target .
  ?reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies>
    <<( ?source ?predicate ?target )>> .
  ?reifier <ad4m://ontology/author> ?author .
  OPTIONAL { ?reifier <ad4m://ontology/status> ?status . }
}
```

Stock and patched runs of this query should return the same solution set. Compare the solutions, not only the row count. This draft does not claim a timing for this query.

`q2b.txt` line 1 is the same author triple and one OPTIONAL, with `VALUES ?source { __VALUES__ }`. `q2b.txt` line 7 is `VALUES ?source { <todo://t9999> <todo://t9998> }` and that OPTIONAL, with no author triple. `q2-variants.txt` line 1 is a SELECT with five OPTIONAL predicates (`proofKey`, `proofSignature`, `proofValid`, `status`, `wireTarget`). The timing logs label Q2 `hydrate 50 ids (decorated links)` and do not print any of those SELECT strings, so a log line does not tie the Q2 median to one of these texts. The review's line 98 describes Q2 as 50-ID hydration with five OPTIONALs, which is the review's description, not a plan dump. `probe-run1.txt` and `probe-run1-native.txt` are empty, so the archive has no probe timing.

## Expected behavior

The same query and the same data should receive comparable heuristic costs on wasm32 and on 64-bit targets. A cardinality estimate should keep the same saturation ceiling on both pointer widths.

## Observed behavior

The optimizer stores cardinalities and costs as `usize`. Several estimates call `saturating_mul` and then `saturating_div`. On wasm32 the intermediate saturates at `4,294,967,295`. On 64-bit targets it can reach `18,446,744,073,709,551,615`.

The quotient is an underestimate caused by saturation of the numerator before the division. The `1000^keys` divisor can saturate as well. The OPTIONAL lateral rewrite compares `lateral_cost` with `join_cost.saturating_mul(100)`, so that comparison can see architecture-dependent inputs. The exact plan-choice chain is inferred until a plan or estimate trace proves it. The historical logs below are timings and row counts, not that trace.

Historical measurements from the raw logs are the next section. This run did not re-time them. They record the runs in that archive, not every query.

## Historical measurements

Copied from the raw logs. Not re-timed in this run. Each cell is one printed line. `median_ms` and `min_ms` stay on that line. No figure in this section was computed from the others.

The archive names `wasm-stock-runN.txt` and `wasm-u64-runN.txt` are the stock and u64 labels these logs have. A log line does not print the words "patch applied". The review's statement that the patch was applied is the in-memory check on its line 42, quoted above. The numbers those runs gave, as these logs print them, are the `wasm-u64-runN.txt` lines.

Q2 `median_ms` on the three wasm-stock logs is 153.593, 160.886, and 166.823 (`wasm-stock-run1.txt` line 5, `wasm-stock-run2.txt` line 5, `wasm-stock-run3.txt` line 5). Q2 `median_ms` on the three wasm-u64 logs is 2.805, 2.937, and 3.615 (`wasm-u64-run1.txt` line 5, `wasm-u64-run2.txt` line 5, `wasm-u64-run3.txt` line 5). Each of those six lines prints `rows=200`. The four native Q2 lines also print `rows=200`, with `median_ms` 1.896, 1.889, 1.921, and 1.893.

`r1-native.log`, `r1-wasm.log`, `r2-native.log`, `r2-wasm.log`, `r3-native.log`, `r3-wasm.log`, `r4-native.log`, `r4-wasm.log`, `r5-native.log`, `r5-wasm.log`, `r6-native.log`, `r6-wasm.log`, `r6-summary.txt`, `r7-wasm.log`, `r8-wasm.log`, `deno-wasm.log`, `deno-wasm-kg.log`, and `runtime-hazards.txt` print no `median_ms`. This section takes no timing from them.

### wasm-stock

Environment: wasm result log whose file name says stock. The files do not print Node, Deno, or a browser name.

| File | Line | Printed line |
| --- | --- | --- |
| `wasm-stock-run1.txt` | 1 | `wasm raw=2721463 gzip9=894521 brotli11=599588` |
| `wasm-stock-run1.txt` | 2 | `instantiate_ms=7.6` |
| `wasm-stock-run1.txt` | 3 | `links=50000 quads=400000 per_link=false load_ms=1071.6 mem_before=1572864 mem_after=146079744 delta=144506880 bytes_per_link=2890` |
| `wasm-stock-run1.txt` | 4 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=120.810	min_ms=120.049` |
| `wasm-stock-run1.txt` | 5 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=153.593	min_ms=152.422` |
| `wasm-stock-run1.txt` | 6 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=42.013	min_ms=41.276` |
| `wasm-stock-run1.txt` | 7 | `Q4 count where state=done	rows=1	median_ms=15.229	min_ms=15.098` |
| `wasm-stock-run1.txt` | 8 | `Q5 single-plan all instances+links	rows=40000	median_ms=295.193	min_ms=292.220` |
| `wasm-stock-run1.txt` | 9 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=60.080	min_ms=59.760` |
| `wasm-stock-run1.txt` | 10 | `mem_end=252641280` |
| `wasm-stock-run2.txt` | 1 | `wasm raw=2721463 gzip9=894521 brotli11=599588` |
| `wasm-stock-run2.txt` | 2 | `instantiate_ms=7.4` |
| `wasm-stock-run2.txt` | 3 | `links=50000 quads=400000 per_link=false load_ms=1068.4 mem_before=1572864 mem_after=146079744 delta=144506880 bytes_per_link=2890` |
| `wasm-stock-run2.txt` | 4 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=128.133	min_ms=124.432` |
| `wasm-stock-run2.txt` | 5 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=160.886	min_ms=158.973` |
| `wasm-stock-run2.txt` | 6 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=42.286	min_ms=41.824` |
| `wasm-stock-run2.txt` | 7 | `Q4 count where state=done	rows=1	median_ms=15.084	min_ms=14.948` |
| `wasm-stock-run2.txt` | 8 | `Q5 single-plan all instances+links	rows=40000	median_ms=300.233	min_ms=292.031` |
| `wasm-stock-run2.txt` | 9 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=64.534	min_ms=63.078` |
| `wasm-stock-run2.txt` | 10 | `mem_end=252641280` |
| `wasm-stock-run3.txt` | 1 | `wasm raw=2721463 gzip9=894521 brotli11=599588` |
| `wasm-stock-run3.txt` | 2 | `instantiate_ms=7.3` |
| `wasm-stock-run3.txt` | 3 | `links=50000 quads=400000 per_link=false load_ms=1085.0 mem_before=1572864 mem_after=146079744 delta=144506880 bytes_per_link=2890` |
| `wasm-stock-run3.txt` | 4 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=124.382	min_ms=124.125` |
| `wasm-stock-run3.txt` | 5 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=166.823	min_ms=161.154` |
| `wasm-stock-run3.txt` | 6 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=43.698	min_ms=42.028` |
| `wasm-stock-run3.txt` | 7 | `Q4 count where state=done	rows=1	median_ms=14.794	min_ms=14.541` |
| `wasm-stock-run3.txt` | 8 | `Q5 single-plan all instances+links	rows=40000	median_ms=304.573	min_ms=294.339` |
| `wasm-stock-run3.txt` | 9 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=60.770	min_ms=60.636` |
| `wasm-stock-run3.txt` | 10 | `mem_end=252641280` |

### wasm-u64

Environment: wasm result log whose file name says u64. The files do not print Node, Deno, or a browser name.

| File | Line | Printed line |
| --- | --- | --- |
| `wasm-u64-run1.txt` | 1 | `wasm raw=2719904 gzip9=893456 brotli11=599240` |
| `wasm-u64-run1.txt` | 2 | `instantiate_ms=10.4` |
| `wasm-u64-run1.txt` | 3 | `links=50000 quads=400000 per_link=false load_ms=1064.3 mem_before=1572864 mem_after=146079744 delta=144506880 bytes_per_link=2890` |
| `wasm-u64-run1.txt` | 4 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=121.508	min_ms=120.788` |
| `wasm-u64-run1.txt` | 5 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=2.805	min_ms=2.723` |
| `wasm-u64-run1.txt` | 6 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=42.331	min_ms=41.866` |
| `wasm-u64-run1.txt` | 7 | `Q4 count where state=done	rows=1	median_ms=15.491	min_ms=15.431` |
| `wasm-u64-run1.txt` | 8 | `Q5 single-plan all instances+links	rows=40000	median_ms=293.042	min_ms=292.419` |
| `wasm-u64-run1.txt` | 9 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=60.934	min_ms=60.208` |
| `wasm-u64-run1.txt` | 10 | `mem_end=155189248` |
| `wasm-u64-run2.txt` | 1 | `wasm raw=2719904 gzip9=893456 brotli11=599240` |
| `wasm-u64-run2.txt` | 2 | `instantiate_ms=10.1` |
| `wasm-u64-run2.txt` | 3 | `links=50000 quads=400000 per_link=false load_ms=1044.3 mem_before=1572864 mem_after=146079744 delta=144506880 bytes_per_link=2890` |
| `wasm-u64-run2.txt` | 4 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=119.982	min_ms=119.660` |
| `wasm-u64-run2.txt` | 5 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=2.937	min_ms=2.835` |
| `wasm-u64-run2.txt` | 6 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=41.317	min_ms=40.651` |
| `wasm-u64-run2.txt` | 7 | `Q4 count where state=done	rows=1	median_ms=15.017	min_ms=14.862` |
| `wasm-u64-run2.txt` | 8 | `Q5 single-plan all instances+links	rows=40000	median_ms=290.587	min_ms=288.626` |
| `wasm-u64-run2.txt` | 9 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=59.017	min_ms=58.680` |
| `wasm-u64-run2.txt` | 10 | `mem_end=155189248` |
| `wasm-u64-run3.txt` | 1 | `wasm raw=2719904 gzip9=893456 brotli11=599240` |
| `wasm-u64-run3.txt` | 2 | `instantiate_ms=10.1` |
| `wasm-u64-run3.txt` | 3 | `links=50000 quads=400000 per_link=false load_ms=1070.9 mem_before=1572864 mem_after=146079744 delta=144506880 bytes_per_link=2890` |
| `wasm-u64-run3.txt` | 4 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=124.393	min_ms=122.413` |
| `wasm-u64-run3.txt` | 5 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=3.615	min_ms=2.869` |
| `wasm-u64-run3.txt` | 6 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=42.527	min_ms=42.137` |
| `wasm-u64-run3.txt` | 7 | `Q4 count where state=done	rows=1	median_ms=15.237	min_ms=15.115` |
| `wasm-u64-run3.txt` | 8 | `Q5 single-plan all instances+links	rows=40000	median_ms=299.010	min_ms=296.352` |
| `wasm-u64-run3.txt` | 9 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=61.575	min_ms=60.248` |
| `wasm-u64-run3.txt` | 10 | `mem_end=155189248` |

### wasm-196

Environment: wasm result log whose file name says 196. The file does not say stock or u64, and it does not print Node, Deno, or a browser name. Its Q2 median is on line 5.

| File | Line | Printed line |
| --- | --- | --- |
| `wasm-196.txt` | 1 | `wasm raw=2441115 gzip9=835893 brotli11=561044` |
| `wasm-196.txt` | 2 | `instantiate_ms=9.6` |
| `wasm-196.txt` | 3 | `links=50000 quads=400000 per_link=false load_ms=1026.7 mem_before=1572864 mem_after=146079744 delta=144506880 bytes_per_link=2890` |
| `wasm-196.txt` | 4 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=115.588	min_ms=115.006` |
| `wasm-196.txt` | 5 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=2.816	min_ms=2.541` |
| `wasm-196.txt` | 6 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=38.794	min_ms=38.382` |
| `wasm-196.txt` | 7 | `Q4 count where state=done	rows=1	median_ms=13.922	min_ms=13.864` |
| `wasm-196.txt` | 8 | `Q5 single-plan all instances+links	rows=40000	median_ms=279.649	min_ms=278.334` |
| `wasm-196.txt` | 9 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=57.819	min_ms=57.293` |
| `wasm-196.txt` | 10 | `mem_end=155189248` |

### native

Environment: native logs. Each file prints `rss_before`, `rss_after`, and `rss_end`, and no `wasm` size line. They do not print a runtime name.

| File | Line | Printed line |
| --- | --- | --- |
| `native-run1.txt` | 1 | `links=50000 quads=400000 per_link=false load_ms=688.1 rss_before=2600960 rss_after=169512960 delta=166912000 bytes_per_link=3338` |
| `native-run1.txt` | 2 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=97.660	min_ms=97.132` |
| `native-run1.txt` | 3 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=1.896	min_ms=1.876` |
| `native-run1.txt` | 4 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=32.435	min_ms=32.309` |
| `native-run1.txt` | 5 | `Q4 count where state=done	rows=1	median_ms=13.877	min_ms=13.858` |
| `native-run1.txt` | 6 | `Q5 single-plan all instances+links	rows=40000	median_ms=243.872	min_ms=242.945` |
| `native-run1.txt` | 7 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=50.709	min_ms=50.256` |
| `native-run1.txt` | 8 | `rss_end=174211072` |
| `native-run2.txt` | 1 | `links=50000 quads=400000 per_link=false load_ms=676.7 rss_before=2531328 rss_after=169582592 delta=167051264 bytes_per_link=3341` |
| `native-run2.txt` | 2 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=100.433	min_ms=97.425` |
| `native-run2.txt` | 3 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=1.889	min_ms=1.873` |
| `native-run2.txt` | 4 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=32.636	min_ms=32.517` |
| `native-run2.txt` | 5 | `Q4 count where state=done	rows=1	median_ms=14.019	min_ms=13.975` |
| `native-run2.txt` | 6 | `Q5 single-plan all instances+links	rows=40000	median_ms=240.598	min_ms=239.664` |
| `native-run2.txt` | 7 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=51.469	min_ms=50.431` |
| `native-run2.txt` | 8 | `rss_end=174182400` |
| `native-run3.txt` | 1 | `links=50000 quads=400000 per_link=false load_ms=745.5 rss_before=2584576 rss_after=169578496 delta=166993920 bytes_per_link=3340` |
| `native-run3.txt` | 2 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=102.519	min_ms=99.489` |
| `native-run3.txt` | 3 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=1.921	min_ms=1.909` |
| `native-run3.txt` | 4 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=33.450	min_ms=32.637` |
| `native-run3.txt` | 5 | `Q4 count where state=done	rows=1	median_ms=14.142	min_ms=13.892` |
| `native-run3.txt` | 6 | `Q5 single-plan all instances+links	rows=40000	median_ms=251.306	min_ms=247.833` |
| `native-run3.txt` | 7 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=52.289	min_ms=51.197` |
| `native-run3.txt` | 8 | `rss_end=174215168` |
| `native-196.txt` | 1 | `links=50000 quads=400000 per_link=false load_ms=683.1 rss_before=2760704 rss_after=169771008 delta=167010304 bytes_per_link=3340` |
| `native-196.txt` | 2 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=100.201	min_ms=99.417` |
| `native-196.txt` | 3 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=1.893	min_ms=1.871` |
| `native-196.txt` | 4 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=32.815	min_ms=32.650` |
| `native-196.txt` | 5 | `Q4 count where state=done	rows=1	median_ms=14.058	min_ms=13.961` |
| `native-196.txt` | 6 | `Q5 single-plan all instances+links	rows=40000	median_ms=247.888	min_ms=245.760` |
| `native-196.txt` | 7 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=53.449	min_ms=52.833` |
| `native-196.txt` | 8 | `rss_end=174477312` |

### browser

Environment: one headless Chrome run. Line 9 prints the user agent. Line 10 is `END`. Lines 11–24 are the harness cleanup stack and are not query measurements, so they are not copied here. Line 25 prints the Node version on that stack. The file does not say stock or u64. Its Q2 line is line 4. Line 9 prints `mem_end=155189248`, which is also the `mem_end` on each wasm-u64 line 10 and on `wasm-196.txt` line 10. Each wasm-stock line 10 prints `mem_end=252641280`.

| File | Line | Printed line |
| --- | --- | --- |
| `browser-run1.txt` | 1 | `init_ms=27.1 (fetch+compile+instantiate, localhost)` |
| `browser-run1.txt` | 2 | `links=50000 quads=400000 load_ms=886.7 delta=144506880 bytes_per_link=2890` |
| `browser-run1.txt` | 3 | `Q1 id-phase ts-sort LIMIT 50	rows=50	median_ms=119.600	min_ms=118.400` |
| `browser-run1.txt` | 4 | `Q2 hydrate 50 ids (decorated links)	rows=200	median_ms=3.600	min_ms=2.800` |
| `browser-run1.txt` | 5 | `Q3 id-phase where+property-sort LIMIT 50	rows=50	median_ms=41.900	min_ms=40.900` |
| `browser-run1.txt` | 6 | `Q4 count where state=done	rows=1	median_ms=14.800	min_ms=14.500` |
| `browser-run1.txt` | 7 | `Q5 single-plan all instances+links	rows=40000	median_ms=286.700	min_ms=285.700` |
| `browser-run1.txt` | 8 | `Q6 channel last-activity GROUP BY (sparql_store test shape)	rows=20	median_ms=58.800	min_ms=57.900` |
| `browser-run1.txt` | 9 | `mem_end=155189248 ua=Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) HeadlessChrome/146.0.0.0 Safari/537.36` |
| `browser-run1.txt` | 25 | `Node.js v24.20.0` |

### scaling

`scaling.txt` does not name native, wasm, stock, or u64. It prints two lines under each link count. This draft does not relabel those lines. Line 6, the second line under `== 50000`, prints `bytes_per_link=2890`, which is the `bytes_per_link` value on every wasm-stock, wasm-u64, wasm-196, and browser load line above. That line prints `Q2 median_ms=3.119`. The wasm-stock Q2 medians printed above are 153.593, 160.886, and 166.823.

| File | Line | Printed line |
| --- | --- | --- |
| `scaling.txt` | 1 | `== 10000` |
| `scaling.txt` | 2 | `load_ms=131.1 bytes_per_link=3515 Q1 median_ms=16.488 Q2 median_ms=1.883 Q3 median_ms=4.694 Q4 median_ms=1.646 Q5 median_ms=45.907 Q6 median_ms=9.935 ` |
| `scaling.txt` | 3 | `load_ms=175.8 bytes_per_link=2995 Q1 median_ms=21.101 Q2 median_ms=2.755 Q3 median_ms=6.586 Q4 median_ms=1.920 Q5 median_ms=56.109 Q6 median_ms=11.521 ` |
| `scaling.txt` | 4 | `== 50000` |
| `scaling.txt` | 5 | `load_ms=700.2 bytes_per_link=3339 Q1 median_ms=99.956 Q2 median_ms=1.899 Q3 median_ms=32.866 Q4 median_ms=14.072 Q5 median_ms=244.909 Q6 median_ms=51.844 ` |
| `scaling.txt` | 6 | `load_ms=1064.8 bytes_per_link=2890 Q1 median_ms=121.476 Q2 median_ms=3.119 Q3 median_ms=41.653 Q4 median_ms=15.076 Q5 median_ms=290.682 Q6 median_ms=59.890 ` |
| `scaling.txt` | 7 | `== 100000` |
| `scaling.txt` | 8 | `load_ms=1419.5 bytes_per_link=3302 Q1 median_ms=217.971 Q2 median_ms=1.932 Q3 median_ms=72.704 Q4 median_ms=28.494 Q5 median_ms=515.837 Q6 median_ms=106.216 ` |
| `scaling.txt` | 9 | `load_ms=2323.3 bytes_per_link=2882 Q1 median_ms=265.062 Q2 median_ms=2.765 Q3 median_ms=90.176 Q4 median_ms=30.290 Q5 median_ms=606.939 Q6 median_ms=124.803 ` |

## Numerical cause

All sites are `sparopt` 0.3.9 `src/optimizer.rs` unless noted. The OPTIONAL rewrite and the lateral cost helper are compiled only with `sep-0006`, which the locked Oxigraph graph enables (see Versions).

Numerator saturation before division:

- `estimate_join_cost` multiplies the two side estimates, then divides (lines 1046–1048).
- The same shape estimates a hash left join (lines 991–999).
- The left-join reorder computes `join_cost` the same way (lines 671–675) after `lateral_cost` is a product with no division (lines 667–668).

For one shared key the divisor is `1_000`. Two predicate-bound triple estimates are `1_000_000` each (`estimate_triple_pattern_size`, line 1075, pattern `(false, true, false)`). The mathematical quotient is `1_000_000,000`. The 32-bit saturating quotient is `4,294,967`.

Saturation of the divisor:

- `1_000_usize.saturating_pow(keys.len().try_into().unwrap())` at lines 674, 999, and 1048.
- `1_000_u32.saturating_pow(4)` is `u32::MAX` (`4,294,967,295`). `1_000_u64.saturating_pow(4)` is `1_000_000_000_000`. Checked with `rustc`.
- `1_000_u64.saturating_pow(6)` still fits (`1_000_000_000_000_000_000`). `1_000_u64.saturating_pow(7)` saturates at `u64::MAX`. Checked with `rustc`.

Saturation of the comparison multiplier:

- Line 677: `lateral_cost <= join_cost.saturating_mul(100)`, inside the `sep-0006` left-join branch that may rewrite OPTIONAL to a lateral (lines 662–698).
- On `u32`, `saturating_mul(100)` saturates once `join_cost` is greater than `u32::MAX / 100` (`4,294,967,295 / 100 = 42,949,672`). Checked with `rustc`: `42_949_672u32.saturating_mul(100)` stays `4,294,967,200`, and `42_949_673u32.saturating_mul(100)` is `u32::MAX`.

Greedy join reordering also picks a lateral cost, when `sep-0006` and `is_fit_for_for_loop_join` both hold, inside `min_by_key` (lines 552–564). That choice uses the same `usize` helpers.

Other `usize` estimate sites the width change has to cover, because their receivers follow the helper return type:

- `estimate_graph_pattern_size` returns `usize` (line 941). `Values` uses `bindings.len()` (line 943). `Union` folds with `usize::saturating_add` (line 1013).
- `Slice` offsets stay `usize` on the algebra (`src/algebra.rs` lines 704–705) and are subtracted and capped in the estimator (lines 1028–1030).
- `estimate_lateral_cost` (lines 1053–1060), `estimate_triple_pattern_size` (lines 1063–1077), and `estimate_path_size` (line 1080). Property-path sequence multiplies with `saturating_mul` (lines 1090 and 1092). `ZeroOrMore` and `OneOrMore` use `saturating_mul(1000)` (lines 1101 and 1110). Unbound `ZeroOrMore` and `ZeroOrOne` return `1_000_000_000` (lines 1103 and 1119), which still fits in `u32`; the following `saturating_mul(1000)` does not.

## Suggested fix

Use `u64` for these heuristic costs and cardinalities so wasm32 and 64-bit targets share one saturation ceiling. That is pointer-width parity for the estimator that already exists. A general estimator redesign is a separate change.

Convert collection lengths and slice offsets to `u64` at the estimator boundary. Keep `GraphPattern::Slice`'s `start` and `length` as `usize`. Widen the three `1_000_usize` divisor bases to `1_000_u64`. The comparison multiplier at line 677 and the property-path `saturating_mul(1000)` calls then widen with the receiver; their literals can stay unsuffixed.

Add an upstream regression that evaluates a product above `u32::MAX` on wasm32 and on a 64-bit target. The red value is the reproducer above: the estimate must be `1_000_000_000` on both.

Data's patch, inlined from `sparopt-u64.patch`. Apply with `patch -p0` from the unpacked `sparopt-0.3.9` crate (`+++` is `src/optimizer.rs`). `patch --dry-run -p0` succeeded against that crate. The original patch has three blank context lines stored as a single space. In this fence those three lines are empty. `patch --dry-run -p0` succeeded for that form as well. This run did not compile sparopt.

The `) -> u64` lines are the returns of `estimate_join_cost`, `estimate_lateral_cost`, and `estimate_triple_pattern_size`. The two full signature lines are `estimate_graph_pattern_size` and `estimate_path_size`. The other replacements are the three `1_000_u64` divisors, `bindings.len() as u64`, `u64::saturating_add`, `*start as u64`, and `*length as u64`. The review's line 46 says this patch replaces 12 lines, not an earlier count of 11. The casts are `as u64`.

```diff
--- /home/data/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sparopt-0.3.9/src/optimizer.rs	2006-07-24 03:21:28.000000000 +0200
+++ src/optimizer.rs	2026-11-19 06:34:21.512339192 +0100
@@ -671,7 +671,7 @@
                         let join_cost = estimate_graph_pattern_size(&left, input_types)
                             .saturating_mul(estimate_graph_pattern_size(&right, input_types))
                             .saturating_div(
-                                1_000_usize.saturating_pow(keys.len().try_into().unwrap()),
+                                1_000_u64.saturating_pow(keys.len().try_into().unwrap()),
                             );

                         if lateral_cost <= join_cost.saturating_mul(100) {
@@ -938,9 +938,9 @@
         .collect()
 }

-fn estimate_graph_pattern_size(pattern: &GraphPattern, input_types: &VariableTypes) -> usize {
+fn estimate_graph_pattern_size(pattern: &GraphPattern, input_types: &VariableTypes) -> u64 {
     match pattern {
-        GraphPattern::Values { bindings, .. } => bindings.len(),
+        GraphPattern::Values { bindings, .. } => bindings.len() as u64,
         GraphPattern::QuadPattern {
             subject,
             predicate,
@@ -996,7 +996,7 @@
                             right,
                             &infer_graph_pattern_types(right, input_types.clone()),
                         ))
-                        .saturating_div(1_000_usize.saturating_pow(keys.len().try_into().unwrap())),
+                        .saturating_div(1_000_u64.saturating_pow(keys.len().try_into().unwrap())),
                 )
             }
         },
@@ -1010,7 +1010,7 @@
         GraphPattern::Union { inner } => inner
             .iter()
             .map(|inner| estimate_graph_pattern_size(inner, input_types))
-            .fold(0, usize::saturating_add),
+            .fold(0, u64::saturating_add),
         GraphPattern::Minus { left, .. } => estimate_graph_pattern_size(left, input_types),
         GraphPattern::Filter { inner, .. }
         | GraphPattern::Extend { inner, .. }
@@ -1025,9 +1025,9 @@
             start,
             length,
         } => {
-            let inner = estimate_graph_pattern_size(inner, input_types).saturating_sub(*start);
+            let inner = estimate_graph_pattern_size(inner, input_types).saturating_sub(*start as u64);
             if let Some(length) = length {
-                min(inner, *length)
+                min(inner, *length as u64)
             } else {
                 inner
             }
@@ -1040,12 +1040,12 @@
     right: &GraphPattern,
     algorithm: &JoinAlgorithm,
     input_types: &VariableTypes,
-) -> usize {
+) -> u64 {
     match algorithm {
         JoinAlgorithm::HashBuildLeftProbeRight { keys } => {
             estimate_graph_pattern_size(left, input_types)
                 .saturating_mul(estimate_graph_pattern_size(right, input_types))
-                .saturating_div(1_000_usize.saturating_pow(keys.len().try_into().unwrap()))
+                .saturating_div(1_000_u64.saturating_pow(keys.len().try_into().unwrap()))
         }
     }
 }
@@ -1055,7 +1055,7 @@
     left_types: &VariableTypes,
     right: &GraphPattern,
     input_types: &VariableTypes,
-) -> usize {
+) -> u64 {
     estimate_graph_pattern_size(left, input_types)
         .saturating_mul(estimate_graph_pattern_size(right, left_types))
 }
@@ -1064,7 +1064,7 @@
     subject_bound: bool,
     predicate_bound: bool,
     object_bound: bool,
-) -> usize {
+) -> u64 {
     match (subject_bound, predicate_bound, object_bound) {
         (true, true, true) => 1,
         (true, true, false) => 10,
@@ -1077,7 +1077,7 @@
     }
 }

-fn estimate_path_size(start_bound: bool, path: &PropertyPathExpression, end_bound: bool) -> usize {
+fn estimate_path_size(start_bound: bool, path: &PropertyPathExpression, end_bound: bool) -> u64 {
     match path {
         PropertyPathExpression::NamedNode(_) => {
             estimate_triple_pattern_size(start_bound, true, end_bound)
```

Local workaround, not done in this commit: pin a narrow patch of registry sparopt 0.3.9, with this estimator change, for Wasm performance until an upstream release has the same `u64` costs. Drop the pin when that release is selected. Do not treat a blind upgrade of `oxigraph`, `spareval`, or `sparopt` as the fix. Root `Cargo.toml` stays without a sparopt patch in this change.

## Limitations

`u64` still saturates. A product above `u64::MAX`, or `1_000_u64.saturating_pow(7)`, saturates before the division. This patch does not reorder the multiply and the divide, and it does not replace the heuristic with statistics.

The exact plan-choice chain is inferred until a plan or estimate trace proves it. The historical logs are medians, row counts, sizes, and memory figures. None of them is a plan dump. The review's lines 37–38 say the same: no Q2 algebra or estimate trace was in the report, and the precise chain remains inferred.

The timings above are the log lines. This run did not re-time them and did not compile sparopt. The check this run ran on the patch text is `patch --dry-run`, plus the temporary apply described at the top.

The archive does not include the dataset generator or the stock and patched command lines. `probe-run1.txt` and `probe-run1-native.txt` are empty. The printed row counts are not a solution-set comparison. The review's line 60 says performance numbers alone do not establish semantic equivalence of the patched query results.

On 64-bit native the review's lines 47–48 say these `u64` types have the same unsigned range as `usize`, and that no native plan-semantic change is expected from the casts. That is the review's source reading. The native logs do not print a patched-or-stock flag.

Do not file this upstream from this document. Nobody files it without Nico.
