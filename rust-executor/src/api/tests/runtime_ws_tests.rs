//! Host-rate and membrane-proof runtime handlers. No case here reaches the
//! global DB, which other tests share.

use serde_json::json;
use std::sync::Arc;

use crate::api::runtime_ws::{register_ws_handlers, validate_host_rates};
use crate::api::types::HostRate;
use crate::api::ws_handler::HandlerMap;
use crate::types::RequestContext;

async fn error_code(method: &str, params: serde_json::Value, is_admin_credential: bool) -> u16 {
    let mut map = HandlerMap::new();
    register_ws_handlers(&mut map);
    let ctx = Arc::new(RequestContext {
        capabilities: Ok(vec![]),
        auto_permit_cap_requests: false,
        auth_token: String::new(),
        is_admin_credential,
        user_email: None,
        user_did: None,
        cancel_token: None,
    });
    map.dispatch(method, params, ctx).await.unwrap_err().code
}

fn rate(description: &str, price_in_hot: f64) -> HostRate {
    HostRate {
        description: description.to_string(),
        price_in_hot,
    }
}

#[test]
fn validate_host_rates_keeps_valid_rates_and_refuses_bad_ones() {
    assert_eq!(
        serde_json::to_value(rate("a", 0.5)).unwrap(),
        json!({ "description": "a", "priceInHOT": 0.5 })
    );
    assert_eq!(
        validate_host_rates(vec![rate("a", 0.0), rate("b", 1.5)]).unwrap(),
        vec![("a".to_string(), 0.0), ("b".to_string(), 1.5)]
    );
    for bad in [
        rate("", 1.0),
        rate("a", -0.1),
        rate("a", f64::NAN),
        rate("a", f64::INFINITY),
    ] {
        let err = validate_host_rates(vec![rate("ok", 1.0), bad.clone()]).unwrap_err();
        assert_eq!(err.code, 400, "{:?}", bad);
        assert!(err.message.starts_with("Rate 1 "), "{}", err.message);
    }
}

#[tokio::test]
async fn host_rate_and_membrane_proof_handlers_check_access() {
    assert_eq!(error_code("runtime.hostRates", json!({}), false).await, 403);
    assert_eq!(
        error_code("runtime.setHostRates", json!({ "rates": [] }), false).await,
        403
    );
    let proof = json!({ "proof": "cHJvb2Y=" });
    assert_eq!(
        error_code("runtime.setUnytMembraneProof", proof, false).await,
        403
    );
}

#[tokio::test]
async fn host_rate_and_membrane_proof_setters_refuse_invalid_params() {
    for (method, params) in [
        ("runtime.setHostRates", json!({})),
        ("runtime.setHostRates", json!({ "rates": "[]" })),
        (
            "runtime.setHostRates",
            json!({ "rates": [{ "description": "a" }] }),
        ),
        (
            "runtime.setHostRates",
            json!({ "rates": [{ "description": "a", "priceInHOT": -1 }] }),
        ),
        ("runtime.setUnytMembraneProof", json!({})),
        ("runtime.setUnytMembraneProof", json!({ "proof": "" })),
    ] {
        assert_eq!(
            error_code(method, params.clone(), true).await,
            400,
            "{method} {params}"
        );
    }
}
