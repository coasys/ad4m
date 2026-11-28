# sparopt 0.3.9: usize cost estimates saturate differently on wasm32

In sparopt 0.3.9 (used by Oxigraph 0.5.13), heuristic cardinalities and costs use `usize`. On wasm32, saturating operations on that `usize` (`saturating_mul`, `saturating_div`, and `saturating_pow`) hit a 32-bit ceiling before selectivity division. On a 64-bit target the same saturating operations use a 64-bit ceiling.

[`estimate_join_cost`](https://github.com/oxigraph/oxigraph/blob/2837810c8993da671d496eabf7677d7573c5c522/lib/sparopt/src/optimizer.rs#L1038-L1049) multiplies the two estimates with `saturating_mul`, then divides by `1000.saturating_pow(number_of_keys)`. Two predicate-bound triple patterns each have an estimate of 1,000,000. With one shared key, these saturating operations produce 4,294,967 on a 32-bit target and 1,000,000,000 on a 64-bit target.

This standalone arithmetic example demonstrates the difference on any Rust host. No graph file or generator is required:

```rust
fn main() {
    let narrow = 1_000_000u32.saturating_mul(1_000_000).saturating_div(1_000);
    let wide = 1_000_000u64.saturating_mul(1_000_000).saturating_div(1_000);
    assert_eq!(narrow, 4_294_967);
    assert_eq!(wide, 1_000_000_000);
    println!("u32: {narrow}; u64: {wide}");
}
```

Save as `main.rs` and run `rustc main.rs -o cost-width && ./cost-width`. This models the estimator arithmetic in `src/optimizer.rs`. It is a cost-width portability example. A query timing reproduction is absent: there is no graph file and no generator from which to rebuild one.

The same arithmetic occurs in left-join estimates. The [`sep-0006` OPTIONAL rewrite](https://github.com/oxigraph/oxigraph/blob/2837810c8993da671d496eabf7677d7573c5c522/lib/sparopt/src/optimizer.rs#L662-L698) compares `lateral_cost <= join_cost.saturating_mul(100)`, subject to additional eligibility checks. Oxigraph 0.5.13 enables this feature transitively through spareval. `estimate_lateral_cost` has no `cfg` attribute, so the helper is part of every build. The OPTIONAL rewrite is the block behind `#[cfg(feature = "sep-0006")]`. Architecture-dependent estimates can therefore affect planning. We have not traced a particular query's plan-choice chain.

Unbound `ZeroOrMore` and `ZeroOrOne` return `1_000_000_000` on those arms. Saturating multiplication sits on other arms: property-path sequence, one-bound `ZeroOrMore`, and `OneOrMore`. Composed path estimates can exceed the 32-bit ceiling.

Would using `u64` for these heuristic estimates be appropriate? A candidate change in `src/optimizer.rs` covers all five estimator return types, three divisor bases, the union accumulator, and conversions of VALUES length and slice offsets. Algebra slice offsets can remain `usize`. This would align the saturation ceiling across wasm32 and 64-bit targets without redesigning the heuristic.

`u64` can still saturate for larger products and powers. This is a pointer-width portability fix, not a general solution to saturation-before-division. A regression should exercise the actual estimator with a product above `u32::MAX` and check the same estimate across widths. Result checks should compare solution multisets, including duplicate multiplicities. We do not yet have a self-contained query and graph performance reproducer.
