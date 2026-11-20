//! `crate::prolog_service` in the web executor: the items ring 2 names, without scryer-prolog.
//!
//! Prolog is off on native too (`PROLOG_MODE` is `Disabled`, `src/prolog_service/mod.rs`), and
//! the mode is the same here, so ring 2 takes its "Prolog disabled" paths and does not call the
//! engine. If it does, every query answers with an error; nothing panics.
//! Signatures match the native `PrologService` so ring 2 compiles unchanged.
//! `scripts/web-executor/check-prolog-stub.py` checks that every `prolog_service::` path ring 2
//! uses exists here.

use crate::types::DecoratedLinkExpression;
use anyhow::{anyhow, Error};

pub mod engine_pool;
pub mod types;

use self::types::{QueryResolution, QueryResult};

pub const DEFAULT_POOL_SIZE: usize = 5;
pub const DEFAULT_POOL_SIZE_WITH_FILTERING: usize = 2;

/// Same variants as native; only `Disabled` is ever the mode.
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrologMode {
    Simple,
    Pooled,
    SdnaOnly,
    Disabled,
}

pub static PROLOG_MODE: PrologMode = PrologMode::Disabled;

const NOT_AVAILABLE: &str = "Prolog is not available in the web executor";

fn not_available() -> Error {
    anyhow!(NOT_AVAILABLE)
}

/// Holds no engines. Every query is an error.
#[derive(Clone, Default)]
pub struct PrologService;

impl PrologService {
    pub fn new() -> Self {
        PrologService
    }

    /// No engine to mark.
    pub async fn mark_dirty(&self, _perspective_id: &str) {}

    pub async fn run_query_simple(
        &self,
        _perspective_id: &str,
        _query: String,
        _links: &[DecoratedLinkExpression],
        _neighbourhood_author: Option<String>,
        _owner_did: Option<String>,
    ) -> Result<QueryResolution, Error> {
        Err(not_available())
    }

    pub async fn ensure_perspective_pool(
        &self,
        _perspective_id: String,
        _pool_size: Option<usize>,
    ) -> Result<(), Error> {
        Err(not_available())
    }

    pub async fn run_query_sdna(
        &self,
        _perspective_id: String,
        _query: String,
    ) -> Result<QueryResult, Error> {
        Err(not_available())
    }

    pub async fn run_query_smart(
        &self,
        _perspective_id: String,
        _query: String,
    ) -> Result<QueryResult, Error> {
        Err(not_available())
    }

    pub async fn run_query_all(
        &self,
        _perspective_id: String,
        _query: String,
    ) -> Result<(), Error> {
        Err(not_available())
    }

    pub async fn update_perspective_links(
        &self,
        _perspective_id: String,
        _module_name: String,
        _all_links: Vec<DecoratedLinkExpression>,
        _neighbourhood_author: Option<String>,
        _owner_did: Option<String>,
    ) -> Result<(), Error> {
        Err(not_available())
    }

    /// There are no pools.
    pub async fn has_perspective_pool(&self, _perspective_id: String) -> bool {
        false
    }
}

/// Nothing to set up. Kept so the call sites ring 2 shares with native compile.
pub async fn init_prolog_service() {}

/// Unlike native, never panics: there is no service to have forgotten to set up.
pub async fn get_prolog_service() -> PrologService {
    PrologService
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    async fn prolog_is_disabled_and_every_query_is_an_error() {
        assert_eq!(PROLOG_MODE, PrologMode::Disabled);
        init_prolog_service().await;
        let service = get_prolog_service().await;
        let id = "p".to_string();
        let err = service
            .run_query_smart(id.clone(), "true.".to_string())
            .await
            .unwrap_err();
        assert_eq!(err.to_string(), NOT_AVAILABLE);
        assert!(service
            .run_query_sdna(id.clone(), "true.".to_string())
            .await
            .is_err());
        assert!(service
            .run_query_all(id.clone(), "true.".to_string())
            .await
            .is_err());
        assert!(service
            .run_query_simple("p", "true.".to_string(), &[], None, None)
            .await
            .is_err());
        assert!(service
            .ensure_perspective_pool(id.clone(), None)
            .await
            .is_err());
        assert!(service
            .update_perspective_links(id.clone(), "m".to_string(), vec![], None, None)
            .await
            .is_err());
        assert!(!service.has_perspective_pool(id).await);
        service.mark_dirty("p").await;
    }
}
