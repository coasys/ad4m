//! S0b step 2 compile probe. Real modules are copied verbatim from
//! rust-executor@425311398; everything under `stubs` marks a seam.
#![allow(dead_code, unused_imports)]
pub mod types;
pub mod secret;
pub mod perspectives {
    pub mod model_query;
    pub mod ordering;
    pub mod shacl_parser;
    pub mod sparql_store;
    pub mod subject_classes_of;
    pub mod write_log;
    pub mod flow_evaluator {
        // stub: role_gate_keys reads this constant from the flow evaluator.
        pub(crate) const LINKED_TO_KEYS: &[&str] = &["via", "to"];
    }
}
pub mod languages {
    pub mod error;
    pub mod literal;
    // stub: model_query's resolveLanguage transform calls the language controller.
    pub struct LanguageController;
    impl LanguageController {
        pub fn global_instance() -> Self { LanguageController }
        pub fn parse_expr_url(_u: &str) -> Result<(String, String), error::LanguageError> { Ok((String::new(), String::new())) }
        pub async fn is_language_loaded(&self, _a: &str) -> bool { false }
        pub async fn language_by_ref(&self, _a: &str) -> Result<(), error::LanguageError> { Ok(()) }
        pub async fn get_expression(&self, _l: &str, _a: &str) -> Result<Option<serde_json::Value>, error::LanguageError> { Ok(None) }
    }
}
// ---- stubs (not executor code) ----
pub mod agent {
    pub mod capabilities {
        pub mod types;
        pub use types::*;
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum ListenerReach { Loopback, Network }
    }
    pub mod signatures {
        use crate::types::Expression;
        use serde::Serialize;
        pub fn verify_or_false<T: Serialize>(_expr: &Expression<T>, _context: &str) -> bool { false }
    }
    pub mod session {
        #[derive(Debug, Clone)]
        pub struct Session;
        #[derive(Debug, Clone)]
        pub enum Principal { Operator, Agent(super::AgentContext) }
        #[derive(Debug)]
        pub struct AuthError;
        impl Session {
            pub fn require(&self, _c: &super::capabilities::Capability) -> Result<(), String> { Ok(()) }
            pub fn acts_as(&self) -> super::AgentContext { super::AgentContext }
            pub fn managed_did(&self) -> Option<&str> { None }
            pub fn is_operator(&self) -> bool { true }
            pub fn open(_t: &str, _a: &Option<crate::secret::Secret>, _r: super::capabilities::ListenerReach) -> Result<Self, AuthError> { Ok(Session) }
            pub fn for_test(_p: Principal) -> Self { Session }
        }
    }
    #[derive(Debug, Clone, Default, PartialEq)]
    pub struct AgentContext;
    impl AgentContext {
        pub fn main_agent() -> Self { AgentContext }
        pub fn managed(_d: String) -> Self { AgentContext }
    }
}
pub mod ai_service {
    pub mod providers {
        pub fn api_type_supports_native_tools(_t: &crate::types::ModelApiType) -> bool { false }
    }
}
pub mod utils {
    pub fn constant_time_eq(a: &str, b: &str) -> bool { a == b }
}
pub mod config_file { pub const REDACTED: &str = "<redacted>"; }
pub mod config {
    #[derive(Default)]
    pub struct Config { pub query_deadline_secs: Option<u64> }
    pub fn get_global_config() -> Config { Config::default() }
}
