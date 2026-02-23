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


// ============================================================================
// Link Store (LinksAdapter) tests
// ============================================================================

#[cfg(all(test, feature = "wasm-languages"))]
mod wasm_links_adapter_tests {
    use crate::wasm_core::abi::*;
    use crate::wasm_core::*;
    use std::path::PathBuf;

    fn link_store_wasm_path() -> PathBuf {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        PathBuf::from(manifest_dir)
            .join("tests")
            .join("fixtures")
            .join("wasm")
            .join("link_store_wasm.wasm")
    }

    #[test]
    fn test_link_store_capabilities() {
        let wasm_path = link_store_wasm_path();
        if !wasm_path.exists() { return; }
        let instance = load_wasm_language(&wasm_path, "test-link-caps").unwrap();
        let caps = instance.capabilities();
        assert!(caps.has_expression_adapter);
        assert!(caps.has_put_adapter);
        assert!(caps.has_links_adapter, "link-store should have links adapter");
    }

    #[test]
    fn test_link_store_sync() {
        let wasm_path = link_store_wasm_path();
        if !wasm_path.exists() { return; }
        let mut instance = load_wasm_language(&wasm_path, "test-link-sync").unwrap();
        let result = instance.sync();
        assert!(result.is_ok(), "sync failed: {:?}", result.err());
    }

    #[test]
    fn test_link_store_current_revision_initially_none() {
        let wasm_path = link_store_wasm_path();
        if !wasm_path.exists() { return; }
        let mut instance = load_wasm_language(&wasm_path, "test-link-rev0").unwrap();
        let result = instance.current_revision().unwrap();
        assert!(result.is_none(), "initial revision should be None");
    }

    #[test]
    fn test_link_store_commit_and_render() {
        let wasm_path = link_store_wasm_path();
        if !wasm_path.exists() { return; }
        let mut instance = load_wasm_language(&wasm_path, "test-link-commit").unwrap();

        let diff = AbiPerspectiveDiff {
            additions: vec![AbiLinkExpression {
                author: "did:key:test".to_string(),
                timestamp: "2026-02-23T00:00:00Z".to_string(),
                data: AbiLink {
                    source: "src://a".to_string(),
                    target: "tgt://b".to_string(),
                    predicate: Some("pred://c".to_string()),
                },
                proof: AbiExpressionProof {
                    key: "key".to_string(),
                    signature: "sig".to_string(),
                },
                status: Some("shared".to_string()),
            }],
            removals: vec![],
        };

        let rev = instance.commit(&diff).unwrap();
        assert!(rev.is_some(), "commit should return a revision");
        assert_eq!(rev.unwrap(), "1");

        // current_revision should now be "1"
        let cur = instance.current_revision().unwrap();
        assert_eq!(cur, Some("1".to_string()));

        // render should return the committed link
        let rendered = instance.render().unwrap();
        assert!(rendered.is_some(), "render should return links");
        let links = rendered.unwrap();
        assert_eq!(links.len(), 1);
        assert_eq!(links[0].data.source, "src://a");
        assert_eq!(links[0].data.target, "tgt://b");
    }

    #[test]
    fn test_link_store_commit_removal() {
        let wasm_path = link_store_wasm_path();
        if !wasm_path.exists() { return; }
        let mut instance = load_wasm_language(&wasm_path, "test-link-remove").unwrap();

        // Add a link
        let add_diff = AbiPerspectiveDiff {
            additions: vec![AbiLinkExpression {
                author: "did:key:test".to_string(),
                timestamp: "2026-02-23T00:00:00Z".to_string(),
                data: AbiLink {
                    source: "src://x".to_string(),
                    target: "tgt://y".to_string(),
                    predicate: Some("pred://z".to_string()),
                },
                proof: AbiExpressionProof {
                    key: "k".to_string(),
                    signature: "s".to_string(),
                },
                status: None,
            }],
            removals: vec![],
        };
        instance.commit(&add_diff).unwrap();

        // Remove it
        let rm_diff = AbiPerspectiveDiff {
            additions: vec![],
            removals: vec![AbiLinkExpression {
                author: "did:key:test".to_string(),
                timestamp: "2026-02-23T00:00:00Z".to_string(),
                data: AbiLink {
                    source: "src://x".to_string(),
                    target: "tgt://y".to_string(),
                    predicate: Some("pred://z".to_string()),
                },
                proof: AbiExpressionProof {
                    key: "k".to_string(),
                    signature: "s".to_string(),
                },
                status: None,
            }],
        };
        instance.commit(&rm_diff).unwrap();

        // render should be empty
        let rendered = instance.render().unwrap();
        assert!(rendered.is_none(), "render should be None after removal");
    }

    #[test]
    fn test_link_store_others_empty() {
        let wasm_path = link_store_wasm_path();
        if !wasm_path.exists() { return; }
        let mut instance = load_wasm_language(&wasm_path, "test-link-others").unwrap();
        let others = instance.others().unwrap();
        assert!(others.is_empty());
    }


    // ============================================================================
    // p-diff-sync-wasm tests (Holochain-backed link language)
    // ============================================================================

    fn p_diff_sync_wasm_path() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("examples/wasm-languages/p-diff-sync-wasm/target/wasm32-unknown-unknown/release")
            .join("p_diff_sync_wasm.wasm")
    }

    #[test]
    fn test_p_diff_sync_load_and_capabilities() {
        let wasm_path = p_diff_sync_wasm_path();
        if !wasm_path.exists() {
            eprintln!("p-diff-sync WASM not found at {:?}, skipping", wasm_path);
            return;
        }
        // Loading will fail because ad4m_init tries to install a DNA via Holochain
        // which requires a running conductor. Verify the error is the expected one.
        let result = load_wasm_language(&wasm_path, "test-p-diff-sync");
        match result {
            Ok(instance) => {
                // If a tokio runtime + conductor are available, verify caps
                assert_eq!(instance.name(), "p-diff-sync-wasm");
                let caps = instance.capabilities();
                assert!(caps.has_links_adapter, "p-diff-sync should have links adapter");
            }
            Err(e) => {
                let err_str = format!("{}", e);
                assert!(
                    err_str.contains("ad4m_init failed") || err_str.contains("hc_install_app"),
                    "Expected DNA install error, got: {}", err_str
                );
                eprintln!("p-diff-sync load correctly failed without conductor: {}", err_str);
            }
        }
    }

    #[test]
    fn test_p_diff_sync_size_reasonable() {
        let wasm_path = p_diff_sync_wasm_path();
        if !wasm_path.exists() { return; }
        let metadata = std::fs::metadata(&wasm_path).unwrap();
        let size_mb = metadata.len() as f64 / (1024.0 * 1024.0);
        // Should be ~1.4MB (1.1MB happ + code)
        assert!(size_mb > 1.0, "WASM should be > 1MB (has embedded .happ)");
        assert!(size_mb < 3.0, "WASM should be < 3MB");
        eprintln!("p-diff-sync-wasm size: {:.2} MB", size_mb);
    }
}
