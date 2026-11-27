# sparopt 0.3.9: pointer-width-dependent cost saturation changes estimates on wasm32

Draft for an upstream Oxigraph issue. Do not file it from this document. The diff below is illustrative, written from the 0.3.9 source, not Data's original patch. This tree does not apply the patch.

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

The work order names Node 24.20.0 on Linux x86_64 as the comparison host. The raw benchmark log that would confirm that host was not available for this draft, so the host runtime is not a checked version.

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

A smaller recorded probe has the query shape from the AD4M hydration workload: RDF-star reification, one mandatory annotation, and one OPTIONAL. It is a structure example. It is not a standalone timing reproducer, and this draft has no dataset generator for it.

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

## Expected behavior

The same query and the same data should receive comparable heuristic costs on wasm32 and on 64-bit targets. A cardinality estimate should keep the same saturation ceiling on both pointer widths.

## Observed behavior

The optimizer stores cardinalities and costs as `usize`. Several estimates call `saturating_mul` and then `saturating_div`. On wasm32 the intermediate saturates at `4,294,967,295`. On 64-bit targets it can reach `18,446,744,073,709,551,615`.

The quotient is an underestimate caused by saturation of the numerator before the division. The `1000^keys` divisor can saturate as well. The OPTIONAL lateral rewrite compares `lateral_cost` with `join_cost.saturating_mul(100)`, so that comparison can see architecture-dependent inputs. The exact plan-choice chain is inferred until a plan or estimate trace proves it.

HISTORICAL: Data reproduced Q2 at 153.7 ms stock versus 2.76 ms patched. The work order records 200 rows for both sides of that run. These are historical single-run figures from that work order. This draft did not re-time them, and the raw log was not available to re-check the row count. They are corroboration for one recorded workload, not a claim about every query.

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

Illustrative patch, written from the 0.3.9 source, not Data's original patch. Apply with `patch -p1` from the unpacked `sparopt-0.3.9` crate. `patch --dry-run -p1` succeeded against that crate. This run did not compile sparopt.

```diff
--- a/src/optimizer.rs
+++ b/src/optimizer.rs
@@ -674 +674 @@
-                                1_000_usize.saturating_pow(keys.len().try_into().unwrap()),
+                                1_000_u64.saturating_pow(keys.len().try_into().unwrap()),
@@ -941 +941 @@
-fn estimate_graph_pattern_size(pattern: &GraphPattern, input_types: &VariableTypes) -> usize {
+fn estimate_graph_pattern_size(pattern: &GraphPattern, input_types: &VariableTypes) -> u64 {
@@ -943 +943 @@
-        GraphPattern::Values { bindings, .. } => bindings.len(),
+        GraphPattern::Values { bindings, .. } => u64::try_from(bindings.len()).unwrap_or(u64::MAX),
@@ -999 +999 @@
-                        .saturating_div(1_000_usize.saturating_pow(keys.len().try_into().unwrap())),
+                        .saturating_div(1_000_u64.saturating_pow(keys.len().try_into().unwrap())),
@@ -1013 +1013 @@
-            .fold(0, usize::saturating_add),
+            .fold(0, u64::saturating_add),
@@ -1028 +1028,2 @@
-            let inner = estimate_graph_pattern_size(inner, input_types).saturating_sub(*start);
+            let start = u64::try_from(*start).unwrap_or(u64::MAX);
+            let inner = estimate_graph_pattern_size(inner, input_types).saturating_sub(start);
@@ -1030 +1031 @@
-                min(inner, *length)
+                min(inner, u64::try_from(*length).unwrap_or(u64::MAX))
@@ -1043 +1044 @@
-) -> usize {
+) -> u64 {
@@ -1048 +1049 @@
-                .saturating_div(1_000_usize.saturating_pow(keys.len().try_into().unwrap()))
+                .saturating_div(1_000_u64.saturating_pow(keys.len().try_into().unwrap()))
@@ -1058 +1059 @@
-) -> usize {
+) -> u64 {
@@ -1067 +1068 @@
-) -> usize {
+) -> u64 {
@@ -1080 +1081 @@
-fn estimate_path_size(start_bound: bool, path: &PropertyPathExpression, end_bound: bool) -> usize {
+fn estimate_path_size(start_bound: bool, path: &PropertyPathExpression, end_bound: bool) -> u64 {
```

The three `) -> usize` hunks close helpers declared just above them: `estimate_join_cost` (line 1038, return at 1043), `estimate_lateral_cost` (line 1053, return at 1058), and `estimate_triple_pattern_size` (line 1063, return at 1067).

Local workaround, not done in this commit: pin a narrow patch of registry sparopt 0.3.9, with this estimator change, for Wasm performance until an upstream release has the same `u64` costs. Drop the pin when that release is selected. Do not treat a blind upgrade of `oxigraph`, `spareval`, or `sparopt` as the fix. Root `Cargo.toml` stays without a sparopt patch in this change.

## Limitations

`u64` still saturates. A product above `u64::MAX`, or `1_000_u64.saturating_pow(7)`, saturates before the division. This patch does not reorder the multiply and the divide, and it does not replace the heuristic with statistics.

The exact plan-choice chain is inferred until a plan or estimate trace proves it. The HISTORICAL Q2 times are one recorded run. They were not reproduced here.

The illustrative diff was applied only with `patch --dry-run`. It was not compiled. It is not Data's measured patch.

These evidence items were not available and are not attached:

- `.scratch/web-exec-s0b/bench/sparopt-u64.patch`
- the raw benchmark logs
- `agent-mail/lal-web-exec-s0b-review.md`

The dataset generator and the exact stock and patched commands were not in the copied evidence either. Attach those, and Data's original patch, before posting. Compare solution sets as well as row counts.
