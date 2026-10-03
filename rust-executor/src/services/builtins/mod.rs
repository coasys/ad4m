//! The executor's built-in services: AI, billing, Unyt and Holochain behind
//! their service interfaces. Each module owns its interface (from Rust types,
//! see [`InterfaceBuilder`](super::schema_export::InterfaceBuilder)) and a
//! thin [`ServiceImplementation`] over the existing service code.
//!
//! The checked-in documents in `interfaces/` are the reviewed contracts; a
//! test fails when the Rust types drift from them.

pub mod ai;
pub mod billing;
pub mod holochain;
pub mod unyt;

#[cfg(test)]
mod tests;

use std::sync::{Arc, OnceLock};

use serde::de::DeserializeOwned;
use serde_json::{json, Value};

use super::builtin::{CallContext, ServiceError, ServiceImplementation};
use super::capability::service_capability;
use super::host::ServiceHost;
use super::interface::InterfaceDocument;
use super::registry::{BuiltinManifest, Instancing, Requirement};
use crate::agent::capabilities::Capability;

/// Author of every built-in interface and implementation. Built-ins are
/// compiled in and trusted, so nothing is signed at run time; the DID is part
/// of each genesis document, so it is fixed by the module IDs.
pub const AUTHOR: &str = "did:key:z6MknSghZ8tdR9EtAQDqi6qTrCBKYh8x4kzha27aeBLAz2ix";

/// The built-in interfaces, by the name their module goes under.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Builtin {
    AiInference,
    AiModels,
    BillingLedger,
    BillingSettlement,
    UnytWallet,
    HolochainConductor,
}

impl Builtin {
    pub const ALL: [Builtin; 6] = [
        Builtin::AiInference,
        Builtin::AiModels,
        Builtin::BillingLedger,
        Builtin::BillingSettlement,
        Builtin::UnytWallet,
        Builtin::HolochainConductor,
    ];

    /// File stem of the checked-in document and the generated SDK module.
    pub fn file_stem(self) -> &'static str {
        match self {
            Builtin::AiInference => "ai.inference",
            Builtin::AiModels => "ai.models",
            Builtin::BillingLedger => "billing.ledger",
            Builtin::BillingSettlement => "billing.settlement",
            Builtin::UnytWallet => "unyt.wallet",
            Builtin::HolochainConductor => "holochain.conductor",
        }
    }

    /// The interface document, built from the Rust types.
    pub fn interface(self) -> Value {
        match self {
            Builtin::AiInference => ai::inference_interface(),
            Builtin::AiModels => ai::models_interface(),
            Builtin::BillingLedger => billing::ledger_interface(),
            Builtin::BillingSettlement => billing::settlement_interface(),
            Builtin::UnytWallet => unyt::wallet_interface(),
            Builtin::HolochainConductor => holochain::conductor_interface(),
        }
    }
}

/// The parsed built-in interfaces, with their hashes.
pub fn documents() -> &'static [(Builtin, InterfaceDocument)] {
    static DOCS: OnceLock<Vec<(Builtin, InterfaceDocument)>> = OnceLock::new();
    DOCS.get_or_init(|| {
        Builtin::ALL
            .iter()
            .map(|b| {
                let doc = InterfaceDocument::parse(b.interface()).unwrap_or_else(|e| {
                    panic!("built-in interface {} is invalid: {}", b.file_stem(), e)
                });
                (*b, doc)
            })
            .collect()
    })
}

pub fn document(builtin: Builtin) -> &'static InterfaceDocument {
    &documents()
        .iter()
        .find(|(b, _)| *b == builtin)
        .expect("every built-in has a document")
        .1
}

/// The grant one action of a built-in interface needs. For entry points
/// outside the service host (the OpenAI-compatible API, the transcription
/// audio feed) that check grants themselves.
pub fn capability(builtin: Builtin, action: &str) -> Capability {
    let doc = document(builtin);
    debug_assert!(
        doc.doc.actions.contains_key(action),
        "{} has no action {}",
        doc.doc.name,
        action
    );
    service_capability(&doc.module_id(), &doc.compat(), action)
}

/// Call a built-in service method in-process, through the host (grants,
/// validation and metering apply), and decode the result.
pub async fn call<R: DeserializeOwned>(
    builtin: Builtin,
    method: &str,
    params: Value,
    ctx: &CallContext,
) -> Result<R, crate::api::ws_handler::WsRpcError> {
    let value = super::host::host()
        .dispatch(
            &format!("{}.{}", document(builtin).hash, method),
            params,
            ctx.clone(),
        )
        .await?;
    serde_json::from_value(value).map_err(|e| {
        crate::api::ws_handler::WsRpcError::internal(format!(
            "{} {} answered outside its type: {}",
            builtin.file_stem(),
            method,
            e
        ))
    })
}

/// The wire type of a built-in event (`<hash>.<event>`).
pub fn event_type(builtin: Builtin, event: &str) -> String {
    format!("{}.{}", document(builtin).hash, event)
}

fn manifest(name: &str, implements: &[Builtin], requires: Vec<Requirement>) -> BuiltinManifest {
    BuiltinManifest {
        name: name.into(),
        author: AUTHOR.into(),
        module: None,
        previous: None,
        version: env!("CARGO_PKG_VERSION").into(),
        description: String::new(),
        implements: implements
            .iter()
            .map(|b| document(*b).hash.clone())
            .collect(),
        requires,
        instancing: Instancing::Shared,
        runtime: json!({ "kind": "builtin", "id": name }),
    }
}

/// Register and start every built-in service on `host`, and meter against
/// the billing ledger. Safe to call more than once.
pub async fn start_all(host: &Arc<ServiceHost>) -> Result<(), String> {
    for (_, doc) in documents() {
        host.register_interface(doc.raw.clone(), None, true)?;
    }
    let services: Vec<(&str, Vec<Builtin>, Arc<dyn ServiceImplementation>)> = vec![
        (
            "ai",
            vec![Builtin::AiInference, Builtin::AiModels],
            Arc::new(ai::Ai::default()),
        ),
        (
            "billing",
            vec![Builtin::BillingLedger, Builtin::BillingSettlement],
            Arc::new(billing::Billing::default()),
        ),
        ("unyt", vec![Builtin::UnytWallet], Arc::new(unyt::Unyt)),
        (
            "holochain",
            vec![Builtin::HolochainConductor],
            Arc::new(holochain::Holochain),
        ),
    ];
    for (name, implements, service) in services {
        let m = manifest(name, &implements, vec![]);
        let existing = host
            .registry()
            .implementations()
            .find(|i| i.manifest == m)
            .map(|i| (i.hash.clone(), i.is_running()));
        let hash = match existing {
            Some((hash, true)) => {
                let _ = hash;
                continue;
            }
            Some((hash, false)) => hash,
            None => host.register_builtin(m, service)?,
        };
        host.start(&hash, json!({})).await?;
    }
    host.set_meter(Some(document(Builtin::BillingLedger).hash.clone()));
    Ok(())
}

// ── Helpers for the implementations ─────────────────────────────────────────

/// Parse `params` into the method's Rust type. The host already validated
/// them against the schema, so a failure here is a contract bug.
pub(crate) fn params<T: DeserializeOwned>(params: Value) -> Result<T, ServiceError> {
    serde_json::from_value(params).map_err(|e| ServiceError::Internal(format!("params: {}", e)))
}

pub(crate) fn to_value<T: serde::Serialize>(v: T) -> Result<Value, ServiceError> {
    serde_json::to_value(v).map_err(|e| ServiceError::Internal(e.to_string()))
}

pub(crate) fn internal(e: impl std::fmt::Display) -> ServiceError {
    ServiceError::Internal(e.to_string())
}

/// Only the admin credential, not just a broad grant, may do this.
pub(crate) fn require_admin(ctx: &CallContext) -> Result<(), ServiceError> {
    if ctx.is_admin {
        Ok(())
    } else {
        Err(ServiceError::Forbidden("Admin credential required".into()))
    }
}

/// The caller's session token, for service code keyed by it.
pub(crate) fn token(ctx: &CallContext) -> String {
    ctx.auth_token.clone().unwrap_or_default()
}

/// The empty object a method with no params takes.
#[derive(serde::Deserialize, schemars::JsonSchema)]
#[serde(deny_unknown_fields)]
pub struct NoParams {}
