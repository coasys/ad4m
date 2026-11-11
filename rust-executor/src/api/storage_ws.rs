//! The storage methods: `storage.unlock`, `storage.keys` and `runtime.storageStatus`. The first
//! and the last answer while the executor is sealed (`at_rest::ANSWERED_WHILE_GATED`); the
//! rules are in `at_rest`.

use std::sync::Arc;

use serde::{Deserialize, Serialize};
use serde_json::Value;
use ts_rs::TS;
use zeroize::Zeroizing;

use super::guards::Guard;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};
use crate::at_rest::{
    KeyringError, KeyringSummary, SecretSourceKind, StorageGate, StorageSecret, StorageStatus,
    UnlockError,
};
use crate::types::RequestContext;

/// `storage.unlock`: the operator secret, 64 hex characters.
#[derive(Deserialize, Serialize, TS)]
#[ts(export)]
pub struct StorageUnlockParams {
    pub secret: String,
}

/// `storage.unlock`'s answer: the status after the unlock and the secret's fingerprint (the
/// first 8 hex characters of its SHA-256), so the caller can tell which secret it gave.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct StorageUnlockResult {
    pub status: StorageStatus,
    pub fingerprint: String,
    /// This unlock made the keyring: the data folder had none.
    pub created: bool,
}

/// The gate the handlers answer from: the one they were registered with (tests), else the
/// process's.
fn resolve(gate: &Option<Arc<StorageGate>>) -> Result<Arc<StorageGate>, WsRpcError> {
    gate.clone()
        .or_else(|| crate::at_rest::gate().cloned())
        .ok_or_else(|| WsRpcError::unavailable("this executor has not started its storage"))
}

async fn unlock(gate: Option<Arc<StorageGate>>, params: Value) -> Result<Value, WsRpcError> {
    let gate = resolve(&gate)?;
    let params: StorageUnlockParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    let text = Zeroizing::new(params.secret);
    let secret = StorageSecret::from_hex(&text, SecretSourceKind::UnlockCall, None)
        .map_err(|e| WsRpcError::bad_request(e.to_string()))?;
    let outcome = gate.unlock(&secret).map_err(|e| match e {
        UnlockError::WrongSecret => WsRpcError::forbidden(e.to_string()),
        UnlockError::SecretOnDataDisk(_) => WsRpcError::bad_request(e.to_string()),
        UnlockError::CorruptKeyring(_) | UnlockError::Io(_) => WsRpcError::internal(e.to_string()),
    })?;
    Ok(serde_json::to_value(StorageUnlockResult {
        status: gate.status(true),
        fingerprint: crate::at_rest::secret_source::fingerprint(secret.bytes()),
        created: outcome.created,
    })?)
}

async fn keys(gate: Option<Arc<StorageGate>>) -> Result<Value, WsRpcError> {
    let gate = resolve(&gate)?;
    let summary = gate.keys().summary().map_err(|e| match e {
        KeyringError::Sealed => WsRpcError::unavailable(e.to_string()),
        e => WsRpcError::internal(e.to_string()),
    })?;
    Ok(serde_json::to_value(summary)?)
}

async fn status(
    gate: Option<Arc<StorageGate>>,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let gate = resolve(&gate)?;
    Ok(serde_json::to_value(
        gate.status(ctx.session.is_operator()),
    )?)
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    register(map, None);
}

/// Registers the storage methods answering from `gate`, or from the process's gate when
/// `None`.
pub(crate) fn register(map: &mut HandlerMap, gate: Option<Arc<StorageGate>>) {
    let g = gate.clone();
    // The secret is typed on this machine: a caller over the network would send it across.
    map.method::<StorageUnlockParams, StorageUnlockResult>(
        "storage.unlock",
        move |params: Value, _ctx: Arc<RequestContext>| unlock(g.clone(), params),
    )
    .local_only()
    .guard(Guard::Operator);
    let g = gate.clone();
    map.method::<NoParams, KeyringSummary>(
        "storage.keys",
        move |_params: Value, _ctx: Arc<RequestContext>| keys(g.clone()),
    )
    .read()
    .guard(Guard::Operator);
    map.method::<NoParams, StorageStatus>(
        "runtime.storageStatus",
        move |_params: Value, ctx: Arc<RequestContext>| status(gate.clone(), ctx),
    )
    .read()
    .guard(Guard::Open);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::session::{Principal, Session};
    use crate::api::ws_handler::build_handler_map;
    use crate::at_rest::{StorageState, ANSWERED_WHILE_GATED};
    use serde_json::json;

    const SECRET: &str = "0101010101010101010101010101010101010101010101010101010101010101";

    fn map_with(gate: &Arc<StorageGate>) -> HandlerMap {
        let mut map = HandlerMap::new().with_gate(gate.clone());
        register(&mut map, Some(gate.clone()));
        map
    }

    fn operator() -> Arc<RequestContext> {
        Arc::new(RequestContext::operator_for_test())
    }

    /// While sealed, the full method table answers only the status and the unlock; every
    /// other method is refused 503 `storage-sealed` before its guard or its params are looked
    /// at, and nothing is written to the data folder.
    #[tokio::test]
    async fn a_sealed_executor_answers_only_the_status_and_the_unlock() {
        let folder = tempfile::tempdir().unwrap();
        let gate = Arc::new(StorageGate::new(folder.path(), false));
        let map = build_handler_map().with_gate(gate.clone());
        let names: Vec<String> = map
            .guards()
            .into_iter()
            .map(|(name, _)| name.to_string())
            .collect();
        assert!(names.len() > 100, "the full table: {}", names.len());
        for name in &names {
            let answer = map
                .admit(name, &json!({}), &RequestContext::operator_for_test())
                .await;
            if ANSWERED_WHILE_GATED.contains(&name.as_str()) {
                assert!(
                    answer.as_ref().map_or_else(|e| e.code != 503, |_| true),
                    "{name} must answer while sealed: {answer:?}"
                );
            } else {
                let refused = answer.expect_err(name);
                assert_eq!(refused.code, 503, "{name}");
                assert!(
                    refused.message.starts_with("storage-sealed"),
                    "{name}: {}",
                    refused.message
                );
            }
        }
        // A dispatched call is refused the same way, and runs nothing.
        let refused = map
            .dispatch("agent.status", json!({}), operator())
            .await
            .unwrap_err();
        assert_eq!(refused.code, 503);
        // The full table's storage methods answer from the process's gate; these from `gate`.
        let status = map_with(&gate)
            .dispatch("runtime.storageStatus", json!({}), operator())
            .await
            .unwrap();
        assert_eq!(status["state"], "sealed");
        assert!(
            std::fs::read_dir(folder.path()).unwrap().next().is_none(),
            "no store and no keyring is written while sealed"
        );
    }

    /// The unlock opens the keyring and answers the status and the fingerprint; the id stays
    /// the same; a wrong secret is 403 and leaves the keyring file as it was.
    #[tokio::test]
    async fn unlock_opens_the_keyring_and_a_wrong_secret_changes_nothing() {
        let folder = tempfile::tempdir().unwrap();
        let gate = Arc::new(StorageGate::new(folder.path(), false));
        let map = map_with(&gate);
        let sealed: StorageStatus = serde_json::from_value(
            map.dispatch("runtime.storageStatus", json!({}), operator())
                .await
                .unwrap(),
        )
        .unwrap();
        assert_eq!(
            map.dispatch("storage.keys", json!({}), operator())
                .await
                .unwrap_err()
                .code,
            503
        );

        let answer: StorageUnlockResult = serde_json::from_value(
            map.dispatch("storage.unlock", json!({ "secret": SECRET }), operator())
                .await
                .unwrap(),
        )
        .unwrap();
        assert!(answer.created);
        assert_eq!(answer.status.state, StorageState::Unlocking);
        assert_eq!(answer.status.keyring_id, sealed.keyring_id);
        assert_eq!(
            answer.status.secret_source,
            Some(SecretSourceKind::UnlockCall)
        );
        assert_eq!(
            answer.fingerprint,
            crate::at_rest::secret_source::fingerprint(&[1u8; 32])
        );
        gate.mark_open();
        let summary: KeyringSummary = serde_json::from_value(
            map.dispatch("storage.keys", json!({}), operator())
                .await
                .unwrap(),
        )
        .unwrap();
        assert_eq!(summary.stores.len(), 1, "the executor's store: {summary:?}");

        let file = crate::at_rest::keyring_file::path_in(folder.path());
        let before = std::fs::read(&file).unwrap();
        let restarted = Arc::new(StorageGate::new(folder.path(), false));
        let map = map_with(&restarted);
        let wrong = map
            .dispatch(
                "storage.unlock",
                json!({ "secret": "02".repeat(32) }),
                operator(),
            )
            .await
            .unwrap_err();
        assert_eq!(wrong.code, 403);
        assert!(
            wrong.message.starts_with("wrong-secret"),
            "{}",
            wrong.message
        );
        assert!(!wrong.message.contains(&"02".repeat(32)));
        assert_eq!(std::fs::read(&file).unwrap(), before);
        assert_eq!(restarted.state(), StorageState::Sealed);
        let not_hex = map
            .dispatch("storage.unlock", json!({ "secret": "xyz" }), operator())
            .await
            .unwrap_err();
        assert_eq!(not_hex.code, 400);
    }

    /// The unlock is the operator's, on this machine; the status answers anyone, and only the
    /// operator sees the operator's fields.
    #[tokio::test]
    async fn only_the_operator_on_this_machine_unlocks() {
        let folder = tempfile::tempdir().unwrap();
        let gate = Arc::new(StorageGate::new(folder.path(), false));
        let map = map_with(&gate);
        let network = Arc::new(RequestContext::for_session(
            Session::for_test(Principal::Operator).on_network(),
        ));
        let remote = map
            .dispatch("storage.unlock", json!({ "secret": SECRET }), network)
            .await
            .unwrap_err();
        assert_eq!(remote.code, 403);
        let app = Arc::new(RequestContext::app_for_test(vec![
            crate::agent::capabilities::ALL_CAPABILITY.clone(),
        ]));
        let by_app = map
            .dispatch("storage.unlock", json!({ "secret": SECRET }), app.clone())
            .await
            .unwrap_err();
        assert_eq!(by_app.code, 403);
        assert_eq!(gate.state(), StorageState::Sealed);

        gate.unlock(&StorageSecret::in_process([1; 32])).unwrap();
        let seen: StorageStatus = serde_json::from_value(
            map.dispatch("runtime.storageStatus", json!({}), app.clone())
                .await
                .unwrap(),
        )
        .unwrap();
        assert_eq!(seen.secret_source, None);
        assert_eq!(seen.root_generation, None);
        gate.mark_open();
        assert_eq!(
            map.dispatch("storage.keys", json!({}), app)
                .await
                .unwrap_err()
                .code,
            403
        );
    }
}
