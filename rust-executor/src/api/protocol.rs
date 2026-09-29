//! Protocol feature negotiation: `runtime.protocol`.
//!
//! Every opt-in protocol feature (a new RPC method, or an optional parameter
//! that changes a reply) is listed in [`PROTOCOL_FEATURES`]. Clients read the
//! list once and fall back to v1 behaviour for anything absent. Executors that
//! predate this module answer `runtime.protocol` with "Unknown type", which
//! clients treat as the v1 feature set (an empty list).

use serde_json::{json, Value};
use std::sync::Arc;

use crate::types::RequestContext;

use super::ws_handler::WsRpcError;

/// Protocol version. v1 = every executor without `runtime.protocol`.
pub const PROTOCOL_VERSION: u32 = 2;

/// Opt-in features this executor supports. Append new features here; never
/// remove one (clients rely on its presence to choose a code path).
pub const PROTOCOL_FEATURES: &[&str] = &[
    "runtime.protocol",
    "perspective.discardBatch",
    "perspective.getAllShacl.names",
    "agent.byDIDs",
    // Registered before v2; listed so v2 clients need not probe for it.
    "expression.getMany",
    "perspective.keepAliveLease",
    "events.watch",
    "modelQuery.cursor",
];

/// `runtime.protocol` → `{ version, features }`.
///
/// No capability check: like `runtime.info`, the reply is static build
/// information that a client needs before it can choose which calls to make.
pub async fn get_protocol(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    Ok(json!({
        "version": PROTOCOL_VERSION,
        "features": PROTOCOL_FEATURES,
    }))
}
