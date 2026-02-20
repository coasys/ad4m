//! Integration tests for the WASM language runtime.
//!
//! These tests load the example note-store WASM language and verify
//! it can be instantiated and its exports are correct.

#[cfg(all(test, feature = "wasm-languages"))]
mod wasm_integration_tests {
    use crate::wasm_core::abi::*;
    use crate::wasm_core::error::WasmLanguageError;
    use crate::wasm_core::*;
    use std::path::PathBuf;

    fn note_store_wasm_path() -> PathBuf {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        PathBuf::from(manifest_dir)
            .join("tests")
            .join("fixtures")
            .join("wasm")
            .join("note_store_wasm.wasm")
    }

    #[test]
    fn test_load_wasm_language() {
        let wasm_path = note_store_wasm_path();
        if !wasm_path.exists() {
            eprintln!(
                "Skipping test: WASM fixture not found at {}. Build the example language first.",
                wasm_path.display()
            );
            return;
        }
        let result = load_wasm_language(&wasm_path, "test-note-store");
        assert!(result.is_ok(), "Failed to load WASM language: {:?}", result.err());
        let instance = result.unwrap();
        assert_eq!(instance.name(), "note-store");
        assert_eq!(instance.address(), "test-note-store");
    }

    #[test]
    fn test_capabilities_detection() {
        let wasm_path = note_store_wasm_path();
        if !wasm_path.exists() {
            return;
        }
        let instance = load_wasm_language(&wasm_path, "test-caps").unwrap();
        let caps = instance.capabilities();
        assert!(caps.has_expression_adapter);
        assert!(caps.has_put_adapter);
        assert!(caps.has_interactions);
        assert!(caps.has_teardown);
        // note-store doesn't implement link adapter
        assert!(!caps.has_link_adapter);
    }

    #[test]
    fn test_abi_version() {
        let wasm_path = note_store_wasm_path();
        if !wasm_path.exists() {
            return;
        }
        // The WASM module should have been loaded successfully,
        // which means ABI version was validated
        let result = load_wasm_language(&wasm_path, "test-abi");
        assert!(result.is_ok());
    }

    #[test]
    fn test_expression_get_not_found() {
        let wasm_path = note_store_wasm_path();
        if !wasm_path.exists() {
            return;
        }
        let mut instance = load_wasm_language(&wasm_path, "test-get-miss").unwrap();
        let result = instance.expression_get("nonexistent-address");
        assert!(result.is_ok());
        assert!(result.unwrap().is_none());
    }

    #[test]
    fn test_interactions_empty() {
        let wasm_path = note_store_wasm_path();
        if !wasm_path.exists() {
            return;
        }
        let mut instance = load_wasm_language(&wasm_path, "test-interactions").unwrap();
        let result = instance.interactions("some-address");
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn test_teardown() {
        let wasm_path = note_store_wasm_path();
        if !wasm_path.exists() {
            return;
        }
        let mut instance = load_wasm_language(&wasm_path, "test-teardown").unwrap();
        let result = instance.teardown();
        assert!(result.is_ok());
    }

    #[test]
    fn test_link_adapter_not_available() {
        let wasm_path = note_store_wasm_path();
        if !wasm_path.exists() {
            return;
        }
        let mut instance = load_wasm_language(&wasm_path, "test-no-links").unwrap();
        let link = AbiLink {
            source: "did:key:abc".to_string(),
            target: "expression://xyz".to_string(),
            predicate: None,
        };
        let result = instance.link_add(&link);
        assert!(matches!(
            result,
            Err(WasmLanguageError::FunctionNotAvailable(_))
        ));
    }

    #[test]
    fn test_registry() {
        let wasm_path = note_store_wasm_path();
        if !wasm_path.exists() {
            return;
        }
        let addr = "test-registry-lang";
        assert!(!is_wasm_language(addr));

        register_wasm_language(&wasm_path, addr).unwrap();
        assert!(is_wasm_language(addr));

        let lang = get_wasm_language(addr);
        assert!(lang.is_ok());

        unregister_wasm_language(addr).unwrap();
        assert!(!is_wasm_language(addr));
    }

    #[test]
    fn test_invalid_wasm() {
        let result = load_wasm_language_from_bytes(b"not a wasm module", "invalid");
        assert!(matches!(result, Err(WasmLanguageError::CompilationError(_))));
    }
}
