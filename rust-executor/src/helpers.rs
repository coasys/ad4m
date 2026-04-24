//! Shared helper functions used across REST handlers, MCP tools, and perspective instances.

use crate::agent::AgentService;
use crate::types::domain::LanguageRef;
use crate::types::{DecoratedExpressionProof, ExpressionRendered, Icon, PerspectiveHandle};

/// Check if a user (identified by email) or the main agent can access a perspective.
pub fn can_access_perspective(
    user_email: &Option<String>,
    perspective: &PerspectiveHandle,
) -> bool {
    match user_email {
        Some(email) => {
            // User context: check if user is in owners list
            if let Ok(user_did) = AgentService::get_user_did_by_email(email) {
                log::debug!(
                    "📋 can_access_perspective(): user {} perspective {} user_did {}",
                    email,
                    perspective.uuid,
                    user_did
                );
                log::debug!(
                    "📋 can_access_perspective(): perspective.owners {:?}",
                    perspective.owners
                );
                perspective.is_owned_by(&user_did)
            } else {
                log::debug!("📋 can_access_perspective(): No DID for user {}", email);
                false
            }
        }
        None => {
            // Main agent / admin context: can see ALL perspectives
            true
        }
    }
}

/// Build an `ExpressionRendered` from a JSON expression value and language address.
pub fn build_expression_rendered(
    expr_json: &serde_json::Value,
    lang_address: &str,
) -> ExpressionRendered {
    let author = expr_json
        .get("author")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    let timestamp = expr_json
        .get("timestamp")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    let data = match expr_json.get("data") {
        Some(d) => serde_json::to_string(d).unwrap_or_default(),
        None => String::new(),
    };

    let proof = if let Some(p) = expr_json.get("proof") {
        DecoratedExpressionProof {
            key: p
                .get("key")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string(),
            signature: p
                .get("signature")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string(),
            valid: p.get("valid").and_then(|v| v.as_bool()),
            invalid: p.get("invalid").and_then(|v| v.as_bool()),
        }
    } else {
        DecoratedExpressionProof::default()
    };

    ExpressionRendered {
        author,
        timestamp,
        data,
        proof,
        language: LanguageRef {
            address: lang_address.to_string(),
            name: String::new(),
        },
        icon: Icon { code: None },
    }
}
