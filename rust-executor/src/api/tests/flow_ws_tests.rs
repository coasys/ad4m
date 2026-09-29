//! Param parsing of the flow WS-RPC endpoints.

use serde_json::json;

use crate::api::perspectives_ws::flow_valid_outputs_state;

/// `perspective.flowValidOutputs` reads an absent or `null` `state` as "any
/// terminal state" and takes a string as that state's name. Anything else is
/// a bad request: dropping it to `None` would answer a wider question than
/// the caller asked, which the `producedByFlow` model-query filter already
/// refuses for the same input.
#[test]
fn flow_valid_outputs_state_rejects_non_string_state() {
    assert_eq!(flow_valid_outputs_state(&json!({})).unwrap(), None);
    assert_eq!(
        flow_valid_outputs_state(&json!({ "state": null })).unwrap(),
        None
    );
    assert_eq!(
        flow_valid_outputs_state(&json!({ "state": "done" })).unwrap(),
        Some("done".to_string())
    );

    for bad in [
        json!(42),
        json!(["done"]),
        json!({ "name": "done" }),
        json!(true),
    ] {
        let err = flow_valid_outputs_state(&json!({ "state": bad.clone() })).expect_err(&format!(
            "state {bad} must be refused, not read as no filter"
        ));
        assert_eq!(err.code, 400, "state {bad}: {}", err.message);
        assert!(
            err.message.contains("`state`"),
            "state {bad}: error must name the param, got {}",
            err.message
        );
    }
}
