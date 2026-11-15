//! `runtime.config` and `runtime.setConfig`: the executor's config file as RPC. The rules are in
//! `config_rpc`.

use std::sync::Arc;

use serde_json::Value;

use super::guards::Guard;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};
use crate::config_rpc::{
    RuntimeConfig, SetConfigError, SetConfigParams, SetConfigResult, VALIDATOR_LIMIT,
};
use crate::types::RequestContext;

async fn runtime_config(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let config = crate::config::get_global_config();
    let answer = crate::config_rpc::runtime_config(
        config.config_file.as_deref(),
        &config.pinned_config_keys,
    )
    .map_err(WsRpcError::internal)?;
    Ok(serde_json::to_value(answer)?)
}

async fn set_config(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let params: SetConfigParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    let config = crate::config::get_global_config();
    let result = crate::config_rpc::set_config(
        &config,
        ctx.session.is_operator_on_this_machine(),
        params,
        VALIDATOR_LIMIT,
    )
    .await
    .map_err(|e| match e {
        SetConfigError::NoConfigFile => WsRpcError::conflict(
            "no-config-file: this executor started without --config, so it has no file to write",
        ),
        SetConfigError::File(message) => WsRpcError::internal(message),
    })?;
    Ok(serde_json::to_value(result)?)
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<NoParams, RuntimeConfig>("runtime.config", runtime_config)
        .read()
        .guard(Guard::Operator);
    // Machine keys are local only, per key, in the handler: the call itself is not.
    map.method::<SetConfigParams, SetConfigResult>("runtime.setConfig", set_config)
        .guard(Guard::Operator);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::session::Session;
    use crate::config_rpc::RefusalReason;
    use serde_json::json;

    /// Over RPC, a machine key from the admin credential on a network listener is refused
    /// `local-only`, and from the operator on this machine it is written; a restart key is
    /// written from either. Without `--config` the call is 409.
    #[tokio::test]
    async fn set_config_refuses_machine_keys_to_a_caller_on_the_network() {
        let dir = tempfile::tempdir().unwrap();
        let file = dir.path().join("config.json");
        std::fs::write(&file, r#"{"port": 12000}"#).unwrap();
        let saved = crate::config::GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .clone();
        crate::config::set_global_config(crate::Ad4mConfig {
            config_file: Some(file.clone()),
            insecure_no_admin_credential: Some(true),
            ..Default::default()
        });

        let mut map = HandlerMap::new();
        register_ws_handlers(&mut map);
        let call = |ctx: RequestContext, params: Value| {
            let map = &map;
            async move {
                map.dispatch("runtime.setConfig", params, Arc::new(ctx))
                    .await
            }
        };
        let network = RequestContext::for_session(
            Session::for_test(crate::agent::session::Principal::Operator).on_network(),
        );
        let remote = call(
            network,
            json!({ "set": { "port": 12100, "mcp_port": 3009 } }),
        )
        .await
        .unwrap();
        let local = call(
            RequestContext::operator_for_test(),
            json!({ "set": { "port": 12200 } }),
        )
        .await
        .unwrap();
        let written = crate::config_rpc::read_values(&file).unwrap();

        crate::config::set_global_config(crate::Ad4mConfig::default());
        let no_file = call(
            RequestContext::operator_for_test(),
            json!({ "set": { "mcp_port": 3009 } }),
        )
        .await
        .unwrap_err();
        *crate::config::GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner()) = saved;

        let remote: SetConfigResult = serde_json::from_value(remote).unwrap();
        assert_eq!(remote.refused.len(), 1, "{remote:?}");
        assert_eq!(remote.refused[0].key, "port");
        assert_eq!(remote.refused[0].reason, RefusalReason::LocalOnly);
        let local: SetConfigResult = serde_json::from_value(local).unwrap();
        assert!(local.refused.is_empty(), "{local:?}");
        assert_eq!(written.get("port"), Some(&json!(12200)));
        assert_eq!(written.get("mcp_port"), Some(&json!(3009)));
        assert_eq!(no_file.code, 409);
        assert!(no_file.message.starts_with("no-config-file"));
    }

    /// `runtime.config` lists the keys with their classes and the file's values, and only the
    /// operator calls it.
    #[tokio::test]
    async fn runtime_config_lists_keys_with_classes_for_the_operator_only() {
        let mut map = HandlerMap::new();
        register_ws_handlers(&mut map);
        let saved = crate::config::GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .clone();
        crate::config::set_global_config(crate::Ad4mConfig {
            pinned_config_keys: vec!["mcp_port".to_string()],
            ..Default::default()
        });
        let answer = map
            .dispatch(
                "runtime.config",
                json!({}),
                Arc::new(RequestContext::operator_for_test()),
            )
            .await;
        let app = map
            .dispatch(
                "runtime.config",
                json!({}),
                Arc::new(RequestContext::app_for_test(vec![
                    crate::agent::capabilities::ALL_CAPABILITY.clone(),
                ])),
            )
            .await;
        *crate::config::GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner()) = saved;

        let answer: RuntimeConfig = serde_json::from_value(answer.unwrap()).unwrap();
        assert_eq!(answer.file, None);
        assert!(answer.values.is_empty());
        let mcp_port = answer.keys.iter().find(|k| k.key == "mcp_port").unwrap();
        assert_eq!(mcp_port.pinned, Some(crate::config_rpc::Pinned::Config));
        assert_eq!(app.unwrap_err().code, 403);
    }
}
