use deno_core::op2;

#[op2(fast)]
fn flat_wasm_nop() {}

deno_core::extension!(
    flat_wasm_imports_service,
    ops = [flat_wasm_nop],
    esm = [
        dir "src/js_core",
        "flat_wasm_imports.js"
    ]
);
