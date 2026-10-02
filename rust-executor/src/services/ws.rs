//! `services.*` core RPC methods (SPEC §9.1). Service methods themselves
//! (`<hash>.<method>`) route through `HandlerMap::dispatch` to the host.

use std::sync::Arc;

use serde::{Deserialize, Serialize};
use serde_json::Value;
use ts_rs::TS;

use super::builtin::ServiceHealth;
use super::capability::{allowed, service_capability};
use super::host::{host, ServiceHost};
use crate::agent::capabilities::{check_capability, AGENT_READ_CAPABILITY};
use crate::api::ws_handler::{HandlerMap, WsRpcError};
use crate::types::RequestContext;

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub struct ServicesDescribeParams {
    /// An interface or implementation hash; omit for everything.
    #[ts(optional)]
    pub target: Option<String>,
}

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ServiceInterfaceSummary {
    pub hash: String,
    /// `<authorDID>/<moduleHash>`
    pub module_id: String,
    pub name: String,
    pub version: String,
    /// The compatibility line: the major, or `0.<minor>`.
    pub compat: String,
    pub methods: Vec<String>,
    pub events: Vec<String>,
    /// The actions of this interface the caller holds.
    pub granted_actions: Vec<String>,
}

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ServiceImplementationSummary {
    pub hash: String,
    pub module_id: String,
    pub name: String,
    pub version: String,
    /// Interface version hashes.
    pub implements: Vec<String>,
    pub health: ServiceHealth,
}

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct ServicesDescription {
    pub interfaces: Vec<ServiceInterfaceSummary>,
    pub implementations: Vec<ServiceImplementationSummary>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub struct ServicesInterfaceParams {
    pub hash: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub struct ServicesSetPreferenceParams {
    /// An interface version hash: the preference covers its whole compatible line.
    pub interface: String,
    /// The Service Language module ID to prefer.
    pub module: String,
    /// Set the executor default instead of the caller's own preference (admin only).
    #[ts(optional)]
    pub for_all_users: Option<bool>,
}

fn read_check(ctx: &RequestContext) -> Result<(), WsRpcError> {
    check_capability(&ctx.capabilities, &AGENT_READ_CAPABILITY).map_err(WsRpcError::forbidden)
}

pub(crate) fn describe(host: &ServiceHost, target: Option<&str>, grants: &[Vec<crate::agent::capabilities::Capability>]) -> ServicesDescription {
    let reg = host.registry();
    let mut interfaces: Vec<ServiceInterfaceSummary> = reg
        .interfaces()
        .filter(|d| {
            target.is_none_or(|t| {
                d.hash == t || reg.implementation(t).is_some_and(|i| i.manifest.implements.contains(&d.hash))
            })
        })
        .map(|d| ServiceInterfaceSummary {
            hash: d.hash.clone(),
            module_id: d.module_id(),
            name: d.doc.name.clone(),
            version: d.doc.version.clone(),
            compat: d.compat(),
            methods: d.doc.methods.keys().cloned().collect(),
            events: d.doc.events.keys().cloned().collect(),
            granted_actions: d
                .doc
                .actions
                .keys()
                .filter(|a| allowed(grants, &service_capability(&d.module_id(), &d.compat(), a)))
                .cloned()
                .collect(),
        })
        .collect();
    interfaces.sort_by(|a, b| (&a.module_id, &a.version).cmp(&(&b.module_id, &b.version)));
    let mut implementations: Vec<ServiceImplementationSummary> = reg
        .implementations()
        .filter(|i| target.is_none_or(|t| i.hash == t || i.manifest.implements.iter().any(|h| h == t)))
        .map(|i| ServiceImplementationSummary {
            hash: i.hash.clone(),
            module_id: i.module_id.clone(),
            name: i.manifest.name.clone(),
            version: i.manifest.version.clone(),
            implements: i.manifest.implements.clone(),
            health: i.health.clone(),
        })
        .collect();
    implementations.sort_by(|a, b| a.hash.cmp(&b.hash));
    ServicesDescription { interfaces, implementations }
}

async fn services_describe(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    read_check(&ctx)?;
    let p: ServicesDescribeParams = serde_json::from_value(params)?;
    let host = host();
    host.refresh_health().await;
    let grants = ServiceHost::context_for_request(&ctx).grants;
    Ok(serde_json::to_value(describe(&host, p.target.as_deref(), &grants))?)
}

async fn services_interface(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    read_check(&ctx)?;
    let p: ServicesInterfaceParams = serde_json::from_value(params)?;
    host()
        .registry()
        .interface(&p.hash)
        .map(|d| d.raw.clone())
        .ok_or_else(|| WsRpcError::not_found(format!("unknown interface {}", p.hash)))
}

async fn services_set_preference(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    read_check(&ctx)?;
    let p: ServicesSetPreferenceParams = serde_json::from_value(params)?;
    let user = if p.for_all_users.unwrap_or(false) {
        if !ctx.is_admin_credential {
            return Err(WsRpcError::forbidden("only the admin sets the executor default"));
        }
        None
    } else {
        // Single-user executors have no user: the preference is the default.
        ctx.user_email.clone()
    };
    host()
        .set_preference(user, &p.interface, &p.module)
        .map_err(WsRpcError::bad_request)?;
    Ok(Value::Bool(true))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<ServicesDescribeParams, ServicesDescription>("services.describe", services_describe)
        .read();
    map.method::<ServicesInterfaceParams, Value>("services.interface", services_interface)
        .read();
    map.method::<ServicesSetPreferenceParams, bool>("services.setPreference", services_set_preference);
}
