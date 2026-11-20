#[cfg(not(target_arch = "wasm32"))]
include!("lib_native.rs");
#[cfg(target_arch = "wasm32")]
include!("lib_wasm.rs");
