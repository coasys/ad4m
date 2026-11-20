//! Prolog reads in the web executor answer as on native with Prolog off: empty matches,
//! not an error, and without reaching the Prolog service.
//!
//! `PerspectiveInstance`'s `PrologMode::Disabled` branches return `Matches([])` before any
//! service call. Native's `PROLOG_MODE` is `Disabled` too, so `perspective.queryProlog` and
//! `get_subject_data` read the same on both. The web stub's service answers every query with
//! an error, so an `Ok` here also shows the service was not called.
#![cfg(target_arch = "wasm32")]

use rust_executor::agent::AgentContext;
use rust_executor::perspectives::perspective_instance::PerspectiveInstance;
use rust_executor::types::PerspectiveHandle;
use wasm_bindgen_test::wasm_bindgen_test;

#[wasm_bindgen_test]
async fn prolog_reads_answer_empty_matches_with_prolog_disabled() {
    let perspective = PerspectiveInstance::new(PerspectiveHandle::new_from_name("p".into()), None);
    let query = "triple(S, P, O).".to_string();

    let answers = [
        perspective
            .prolog_query_with_context(query.clone(), &AgentContext::Main)
            .await,
        perspective.prolog_query_sdna(query.clone()).await,
        perspective
            .prolog_query_sdna_with_context(query, &AgentContext::Main)
            .await,
    ];

    for answer in answers {
        let resolution = answer.expect("a Prolog read with Prolog disabled is not an error");
        assert_eq!(format!("{resolution:?}"), "Matches([])");
    }
}
