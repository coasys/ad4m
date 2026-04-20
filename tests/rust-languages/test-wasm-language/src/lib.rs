//! # Test WASM Language — authored against `ad4m-ldk`
//!
//! Demonstrates the new spec v1.0 authoring style: implement the
//! `Language` trait plus the capability traits you need, then invoke
//! `ad4m_language!` to emit the flat-export shims.
//!
//! This language implements:
//!   - `expression` capability (create + get, backed by storage KV)
//!   - `perspective-query` capability (single kind: "test.echo")
//!
//! It deliberately does NOT implement perspective-commit / sync / peers
//! / telepresence — the macro only emits exports for the listed
//! capabilities, so export-presence-as-capability-detection should
//! correctly classify this Language as expression + query only.

use ad4m_ldk::imports as rt;
use ad4m_ldk::prelude::*;

pub struct TestLang {
    storage_dir: String,
    address: String,
}

impl Language for TestLang {
    fn name() -> &'static str { "test-wasm-language" }
    fn version() -> &'static str { "0.1.0" }
    fn is_public() -> bool { true }

    async fn init() -> LanguageResult<Self> {
        let storage_dir = rt::language_storage_directory();
        let address = rt::language_address();

        // Emit a diagnostic so test harnesses can observe init ordering.
        rt::emit_signal(::wasm_bindgen::JsValue::from_str(&format!(
            "[test-wasm-language] init: storage={}, address={}",
            storage_dir, address
        )));

        Ok(Self { storage_dir, address })
    }

    fn teardown(&mut self) -> LanguageResult<()> {
        self.storage_dir.clear();
        self.address.clear();
        Ok(())
    }

    /// Spec §5.7 — describe a single interaction so the smoke test can
    /// exercise the descriptor + execute round-trip.
    fn interactions(&self, _address: Address) -> Vec<Interaction> {
        vec![Interaction {
            label: "Echo".to_string(),
            name: "echo".to_string(),
            parameters: vec![InteractionParameter {
                name: "message".to_string(),
                param_type: "string".to_string(),
            }],
        }]
    }

    /// Spec §5.7 — runtime fall-back path when interactions() entries
    /// don't carry a JS callable. Rust ALDK languages always go through
    /// here.
    fn expression_interact(
        &mut self,
        _address: Address,
        name: String,
        parameters: serde_json::Value,
    ) -> LanguageResult<Option<serde_json::Value>> {
        if name != "echo" {
            return Err(LanguageError::invalid_input(format!(
                "unknown interaction: {}",
                name
            )));
        }
        Ok(Some(parameters))
    }
}

impl ExpressionCapability for TestLang {
    async fn expression_create(&mut self, content: serde_json::Value) -> LanguageResult<Address> {
        // Sign via the host agent — this exercises the agent import.
        // Use the typed wrapper so `content`'s maps serialize as JS
        // objects; the raw import with serde_wasm_bindgen::to_value
        // would serialize maps as JS Map instances, which the runtime's
        // JSON.stringify path then loses silently.
        let signed = rt::agent_create_signed_expression_typed(&content);

        // Derive a content address by hex-signing the serialized content.
        let serialized = serde_json::to_string(&content)?;
        let addr = rt::agent_sign_string_hex(&serialized);
        let addr = format!("test:{}", &addr[..addr.len().min(32)]);

        // Persist via the storage KV import.
        let stored: serde_json::Value = serde_wasm_bindgen::from_value(signed)
            .unwrap_or(serde_json::Value::Null);
        rt::storage_put(&addr, &serde_json::to_string(&stored)?);

        rt::emit_signal(::wasm_bindgen::JsValue::from_str(&format!(
            "[test-wasm-language] created: {}",
            addr
        )));

        Ok(addr)
    }

    async fn expression_get(&mut self, address: Address) -> LanguageResult<Option<Expression>> {
        let raw = rt::storage_get(&address);
        if raw.is_null() || raw.is_undefined() {
            return Ok(None);
        }
        // The KV stub returns a JS string; parse it as an Expression envelope.
        let s: String = raw.as_string().unwrap_or_default();
        if s.is_empty() { return Ok(None); }
        let exp: Expression = serde_json::from_str(&s)
            .unwrap_or_else(|_| Expression {
                author: rt::agent_did(),
                timestamp: String::new(),
                data: serde_json::Value::String(s),
                proof: ExpressionProof::default(),
            });
        Ok(Some(exp))
    }
}

impl PerspectiveQueryCapability for TestLang {
    fn perspective_query_supported_kinds(&self) -> Vec<String> {
        vec!["test.echo".to_string()]
    }

    fn perspective_query_run(&mut self, request: QueryRequest) -> LanguageResult<QueryResponse> {
        if request.kind != "test.echo" {
            return Err(LanguageError::invalid_input(format!(
                "unsupported query kind: {}",
                request.kind
            )));
        }
        Ok(QueryResponse { results: request.params })
    }
}

ad4m_language! {
    language: TestLang,
    capabilities: [expression, perspective_query],
}
