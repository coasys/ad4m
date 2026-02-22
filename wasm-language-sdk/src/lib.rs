//! AD4M WASM Language SDK
//!
//! This crate provides types, traits, and macros for building AD4M language modules
//! that compile to WebAssembly. Language authors use this SDK to implement the
//! AD4M Language interface, and the SDK handles all WASM export generation,
//! memory management, and host function bindings.
//!
//! # Quick Start
//!
//! ```rust,ignore
//! use ad4m_wasm_language_sdk::prelude::*;
//!
//! struct MyLanguage {
//!     // your state
//! }
//!
//! impl ExpressionLanguage for MyLanguage {
//!     fn get(&mut self, address: &str) -> Option<Expression> {
//!         // ...
//!         None
//!     }
//!     fn put(&mut self, content: &serde_json::Value) -> String {
//!         // ...
//!         "some-address".to_string()
//!     }
//! }
//!
//! // Then use the ad4m_language! macro to generate exports
//! ad4m_language!(MyLanguage, "my-language");
//! ```

pub mod host;
pub mod memory;
pub mod types;

/// Re-export commonly used items.
pub mod prelude {
    pub use crate::host::*;
    pub use crate::memory::*;
    pub use crate::types::*;
    pub use crate::ad4m_links_adapter;
}

/// Current ABI version. Must match the host's expected version.
pub const AD4M_LANGUAGE_ABI_VERSION: u32 = 1;

/// Macro to generate all required WASM exports for an AD4M language.
///
/// This macro takes a language implementation type and its name, then generates:
/// - Memory management exports (`ad4m_alloc`, `ad4m_dealloc`)
/// - ABI version export (`ad4m_abi_version`)
/// - Language name export (`ad4m_language_name`)
/// - Expression adapter exports (if the type implements `ExpressionLanguage`)
/// - Interaction exports
/// - Teardown export
///
/// # Usage
///
/// ```rust,ignore
/// use ad4m_wasm_language_sdk::prelude::*;
///
/// struct MyLanguage;
///
/// impl ExpressionLanguage for MyLanguage {
///     fn get(&mut self, address: &str) -> Option<Expression> { None }
///     fn put(&mut self, content: &serde_json::Value) -> String { String::new() }
/// }
///
/// impl LanguageInteractions for MyLanguage {
///     fn interactions(&self, _address: &str) -> Vec<Interaction> { vec![] }
/// }
///
/// ad4m_language!(MyLanguage, "my-language");
/// ```
#[macro_export]
macro_rules! ad4m_language {
    ($lang_type:ty, $name:expr) => {
        // Static mutable language instance (safe in single-threaded WASM)
        static mut LANGUAGE_INSTANCE: Option<$lang_type> = None;

        fn get_language() -> &'static mut $lang_type {
            unsafe {
                if LANGUAGE_INSTANCE.is_none() {
                    LANGUAGE_INSTANCE = Some(<$lang_type>::default());
                }
                LANGUAGE_INSTANCE.as_mut().unwrap()
            }
        }

        // ---- Memory management ----

        #[no_mangle]
        pub extern "C" fn ad4m_alloc(size: u32) -> u32 {
            $crate::memory::wasm_alloc(size)
        }

        #[no_mangle]
        pub extern "C" fn ad4m_dealloc(ptr: u32, size: u32) {
            $crate::memory::wasm_dealloc(ptr, size);
        }

        // ---- ABI version ----

        #[no_mangle]
        pub extern "C" fn ad4m_abi_version() -> u32 {
            $crate::AD4M_LANGUAGE_ABI_VERSION
        }

        // ---- Language name ----

        #[no_mangle]
        pub extern "C" fn ad4m_language_name() -> u64 {
            let name_bytes = $name.as_bytes();
            let ptr = $crate::memory::wasm_alloc(name_bytes.len() as u32);
            if ptr == 0 {
                return 0;
            }
            unsafe {
                core::ptr::copy_nonoverlapping(
                    name_bytes.as_ptr(),
                    ptr as *mut u8,
                    name_bytes.len(),
                );
            }
            $crate::memory::encode_fat_ptr(ptr, name_bytes.len() as u32)
        }

        // ---- Expression adapter ----

        #[no_mangle]
        pub extern "C" fn ad4m_expression_get(ptr: u32, len: u32) -> u64 {
            let input = $crate::memory::read_input(ptr, len);
            let address: String = match serde_json::from_slice(&input) {
                Ok(a) => a,
                Err(_) => return 0,
            };
            let lang = get_language();
            match lang.get(&address) {
                Some(expr) => {
                    let json = match serde_json::to_vec(&expr) {
                        Ok(j) => j,
                        Err(_) => return 0,
                    };
                    $crate::memory::write_output(&json)
                }
                None => {
                    // Return JSON null
                    let null_bytes = b"null";
                    $crate::memory::write_output(null_bytes)
                }
            }
        }

        #[no_mangle]
        pub extern "C" fn ad4m_expression_put(ptr: u32, len: u32) -> u64 {
            let input = $crate::memory::read_input(ptr, len);
            let content: serde_json::Value = match serde_json::from_slice(&input) {
                Ok(c) => c,
                Err(_) => return 0,
            };
            let lang = get_language();
            let address = lang.put(&content);
            let json = match serde_json::to_vec(&address) {
                Ok(j) => j,
                Err(_) => return 0,
            };
            $crate::memory::write_output(&json)
        }

        // ---- Interactions ----

        #[no_mangle]
        pub extern "C" fn ad4m_interactions(ptr: u32, len: u32) -> u64 {
            let input = $crate::memory::read_input(ptr, len);
            let address: String = match serde_json::from_slice(&input) {
                Ok(a) => a,
                Err(_) => return 0,
            };
            let lang = get_language();
            let interactions = lang.interactions(&address);
            let json = match serde_json::to_vec(&interactions) {
                Ok(j) => j,
                Err(_) => return 0,
            };
            $crate::memory::write_output(&json)
        }

        // ---- Teardown ----

        #[no_mangle]
        pub extern "C" fn ad4m_teardown() {
            let lang = get_language();
            lang.teardown();
        }
    };
}

/// Macro to generate WASM exports for LinksAdapter methods.
///
/// Use this in addition to `ad4m_language!` when your language implements `LinksAdapter`.
/// These exports are optional — if not present, the host will detect that the language
/// does not have a links adapter via capability flags.
///
/// # Usage
/// ```rust,ignore
/// ad4m_language!(MyLanguage, "my-language");
/// ad4m_links_adapter!(MyLanguage);
/// ```
#[macro_export]
macro_rules! ad4m_links_adapter {
    ($lang_type:ty) => {
        #[no_mangle]
        pub extern "C" fn ad4m_sync() -> u64 {
            let lang = get_language();
            match lang.sync() {
                Ok(()) => {
                    let json = b"null";
                    $crate::memory::write_output(json)
                }
                Err(e) => {
                    let err_json = match serde_json::to_vec(&serde_json::json!({"error": e})) {
                        Ok(j) => j,
                        Err(_) => return 0,
                    };
                    $crate::memory::write_output(&err_json)
                }
            }
        }

        #[no_mangle]
        pub extern "C" fn ad4m_commit(ptr: u32, len: u32) -> u64 {
            let input = $crate::memory::read_input(ptr, len);
            let diff: $crate::types::PerspectiveDiff = match serde_json::from_slice(&input) {
                Ok(d) => d,
                Err(_) => return 0,
            };
            let lang = get_language();
            match lang.commit(&diff) {
                Ok(revision) => {
                    let json = match serde_json::to_vec(&revision) {
                        Ok(j) => j,
                        Err(_) => return 0,
                    };
                    $crate::memory::write_output(&json)
                }
                Err(e) => {
                    let err_json = match serde_json::to_vec(&serde_json::json!({"error": e})) {
                        Ok(j) => j,
                        Err(_) => return 0,
                    };
                    $crate::memory::write_output(&err_json)
                }
            }
        }

        #[no_mangle]
        pub extern "C" fn ad4m_render() -> u64 {
            let lang = get_language();
            match lang.render() {
                Ok(links) => {
                    let json = match serde_json::to_vec(&links) {
                        Ok(j) => j,
                        Err(_) => return 0,
                    };
                    $crate::memory::write_output(&json)
                }
                Err(e) => {
                    let err_json = match serde_json::to_vec(&serde_json::json!({"error": e})) {
                        Ok(j) => j,
                        Err(_) => return 0,
                    };
                    $crate::memory::write_output(&err_json)
                }
            }
        }

        #[no_mangle]
        pub extern "C" fn ad4m_current_revision() -> u64 {
            let lang = get_language();
            match lang.current_revision() {
                Ok(revision) => {
                    let json = match serde_json::to_vec(&revision) {
                        Ok(j) => j,
                        Err(_) => return 0,
                    };
                    $crate::memory::write_output(&json)
                }
                Err(e) => {
                    let err_json = match serde_json::to_vec(&serde_json::json!({"error": e})) {
                        Ok(j) => j,
                        Err(_) => return 0,
                    };
                    $crate::memory::write_output(&err_json)
                }
            }
        }

        #[no_mangle]
        pub extern "C" fn ad4m_others() -> u64 {
            let lang = get_language();
            match lang.others() {
                Ok(dids) => {
                    let json = match serde_json::to_vec(&dids) {
                        Ok(j) => j,
                        Err(_) => return 0,
                    };
                    $crate::memory::write_output(&json)
                }
                Err(e) => {
                    let err_json = match serde_json::to_vec(&serde_json::json!({"error": e})) {
                        Ok(j) => j,
                        Err(_) => return 0,
                    };
                    $crate::memory::write_output(&err_json)
                }
            }
        }
    };
}
