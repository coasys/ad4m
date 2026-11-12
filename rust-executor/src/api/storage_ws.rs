//! The storage methods: `storage.unlock`, `storage.keys`, `runtime.storageStatus` and the
//! rotations (`storage.rotateSecret`, `storage.rotatePersonKey`, `storage.reencrypt`). The
//! unlock and the status answer while the executor is sealed
//! (`at_rest::ANSWERED_WHILE_GATED`); the rules are in `at_rest`.

use std::sync::Arc;

use serde::{Deserialize, Serialize};
use serde_json::Value;
use ts_rs::TS;
use zeroize::Zeroizing;

use super::guards::Guard;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};
use crate::at_rest::keyring_file::{KeyOwner, StoreId};
use crate::at_rest::rotation::{self, Claim, ExecutorRows, ReencryptError};
use crate::at_rest::secret_source::{fingerprint, parse_hex};
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

/// `storage.rotateSecret`: the secret that opens the keyring now and the one that will open it
/// from now on, 64 hex characters each.
#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct StorageRotateSecretParams {
    pub current_secret: String,
    pub new_secret: String,
}

/// `storage.rotateSecret`'s answer: the keyring's new root generation and the fingerprints of
/// both secrets (the first 8 hex characters of their SHA-256).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct StorageRotateSecretResult {
    pub generation: u32,
    pub old_fingerprint: String,
    pub new_fingerprint: String,
}

/// `storage.rotatePersonKey`: whose data key gets a new version: `main` (the default), or a
/// managed user as `user:<name>` or by its name.
#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct StorageRotatePersonKeyParams {
    #[serde(default)]
    #[ts(optional)]
    pub owner: Option<String>,
}

/// `storage.rotatePersonKey`'s answer: the owner and its data key's new version.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct StorageRotatePersonKeyResult {
    pub owner: String,
    pub version: u32,
}

/// `storage.reencrypt`: the store whose data moves to a new key version, as `storage.keys`
/// names it (`perspective:<uuid>`, `person:main`, `person:user:<name>`).
#[derive(Deserialize, Serialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct StorageReencryptParams {
    pub store: String,
}

/// `storage.reencrypt`'s answer, given when the job starts: the store, the key version its
/// data moves to, and whether the job resumes one that stopped. The job runs on; when it ends
/// `storage-state-changed` is published, and `storage.keys` shows the store with no retiring
/// version once its old versions are retired.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct StorageReencryptResult {
    pub store: String,
    pub version: u32,
    pub resumed: bool,
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

/// A keyring error as the caller sees it.
fn keyring_refusal(e: KeyringError) -> WsRpcError {
    match e {
        KeyringError::WrongSecret => WsRpcError::forbidden(e.to_string()),
        KeyringError::SameSecret => WsRpcError::bad_request(e.to_string()),
        KeyringError::Sealed => WsRpcError::unavailable(e.to_string()),
        KeyringError::UnknownOwner | KeyringError::UnknownStore => {
            WsRpcError::not_found(e.to_string())
        }
        e => WsRpcError::internal(e.to_string()),
    }
}

async fn rotate_secret(gate: Option<Arc<StorageGate>>, params: Value) -> Result<Value, WsRpcError> {
    let gate = resolve(&gate)?;
    let params: StorageRotateSecretParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    let current_text = Zeroizing::new(params.current_secret);
    let new_text = Zeroizing::new(params.new_secret);
    let current = parse_hex(&current_text)
        .map_err(|e| WsRpcError::bad_request(format!("currentSecret: {e}")))?;
    let new =
        parse_hex(&new_text).map_err(|e| WsRpcError::bad_request(format!("newSecret: {e}")))?;
    let old_fingerprint = fingerprint(&current);
    let new_fingerprint = fingerprint(&new);
    // The keyring file is written and synced: off the async workers.
    let generation = tokio::task::spawn_blocking(move || gate.rotate_secret(current, new))
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?
        .map_err(keyring_refusal)?;
    Ok(serde_json::to_value(StorageRotateSecretResult {
        generation,
        old_fingerprint,
        new_fingerprint,
    })?)
}

/// The owner `storage.rotatePersonKey` names: absent or `main` is the main agent; `user:<name>`
/// or a bare name is a managed user.
fn owner_of(text: Option<&str>) -> Result<KeyOwner, WsRpcError> {
    match text.map(str::trim) {
        None | Some("main") => Ok(KeyOwner::Main),
        Some("") => Err(WsRpcError::bad_request("owner: empty")),
        Some(text) if text.starts_with("user:") => text
            .parse()
            .map_err(|e: String| WsRpcError::bad_request(format!("owner: {e}"))),
        Some(name) => Ok(KeyOwner::User(name.to_string())),
    }
}

async fn rotate_person_key(
    gate: Option<Arc<StorageGate>>,
    params: Value,
) -> Result<Value, WsRpcError> {
    let gate = resolve(&gate)?;
    let params: StorageRotatePersonKeyParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    let owner = owner_of(params.owner.as_deref())?;
    let keys = gate.keys().clone();
    let rotated = owner.clone();
    let version = tokio::task::spawn_blocking(move || keys.rotate_person(&rotated))
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?
        .map_err(keyring_refusal)?;
    log::info!(
        "🔑 The data key of {owner} is now version {}: its stores' keys are wrapped under it",
        version.0
    );
    gate.publish();
    Ok(serde_json::to_value(StorageRotatePersonKeyResult {
        owner: owner.to_string(),
        version: version.0,
    })?)
}

fn reencrypt_refusal(e: ReencryptError) -> WsRpcError {
    match e {
        ReencryptError::ExecutorStore => WsRpcError::bad_request(e.to_string()),
        ReencryptError::UnknownStore => WsRpcError::not_found(e.to_string()),
        ReencryptError::Running(_) => WsRpcError::conflict(e.to_string()),
        ReencryptError::Keyring(e) => keyring_refusal(e),
        e @ (ReencryptError::Rows(_) | ReencryptError::Files(_)) => {
            WsRpcError::internal(e.to_string())
        }
    }
}

async fn reencrypt(gate: Option<Arc<StorageGate>>, params: Value) -> Result<Value, WsRpcError> {
    let gate = resolve(&gate)?;
    let params: StorageReencryptParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    let store: StoreId = params
        .store
        .trim()
        .parse()
        .map_err(|e: String| WsRpcError::bad_request(format!("store: {e}")))?;
    let claim = Claim::take(&store).map_err(reencrypt_refusal)?;
    let keys = gate.keys().clone();
    let (version, resumed) = {
        let (keys, store) = (keys.clone(), store.clone());
        tokio::task::spawn_blocking(move || rotation::begin(&keys, &store))
            .await
            .map_err(|e| WsRpcError::internal(e.to_string()))?
            .map_err(reencrypt_refusal)?
    };
    log::info!(
        "🔑 Re-encrypting {store} under its key version {}{}",
        version.0,
        if resumed { " (resumed)" } else { "" }
    );
    gate.publish();
    let job_store = store.clone();
    let job_gate = gate.clone();
    tokio::task::spawn_blocking(move || {
        let _claim = claim;
        let languages = crate::utils::languages_directory();
        match rotation::finish(
            &keys,
            &ExecutorRows,
            &languages,
            &job_store,
            version,
            resumed,
        ) {
            Ok(report) => log::info!(
                "🔑 Re-encrypted {job_store}: {} values and {} files sealed again, {} left \
                 under an older version ({} do not open), versions retired: {:?}",
                report.resealed_values,
                report.resealed_files,
                report.left,
                report.unopenable,
                report.retired.iter().map(|v| v.0).collect::<Vec<_>>()
            ),
            Err(e) => log::error!("🔑 The re-encryption of {job_store} stopped: {e}"),
        }
        job_gate.publish();
    });
    Ok(serde_json::to_value(StorageReencryptResult {
        store: store.to_string(),
        version: version.0,
        resumed,
    })?)
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
    // Both secrets are typed on this machine, as for the unlock.
    let g = gate.clone();
    map.method::<StorageRotateSecretParams, StorageRotateSecretResult>(
        "storage.rotateSecret",
        move |params: Value, _ctx: Arc<RequestContext>| rotate_secret(g.clone(), params),
    )
    .local_only()
    .guard(Guard::Operator);
    let g = gate.clone();
    map.method::<StorageRotatePersonKeyParams, StorageRotatePersonKeyResult>(
        "storage.rotatePersonKey",
        move |params: Value, _ctx: Arc<RequestContext>| rotate_person_key(g.clone(), params),
    )
    .guard(Guard::Operator);
    // A long job over the whole store: started from this machine only.
    let g = gate.clone();
    map.method::<StorageReencryptParams, StorageReencryptResult>(
        "storage.reencrypt",
        move |params: Value, _ctx: Arc<RequestContext>| reencrypt(g.clone(), params),
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

    /// An open gate on `folder`, unlocked with [`SECRET`].
    fn open_gate(folder: &std::path::Path) -> Arc<StorageGate> {
        let gate = Arc::new(StorageGate::new(folder, false));
        gate.unlock(&StorageSecret::in_process([1; 32])).unwrap();
        gate.mark_open();
        gate
    }

    fn network_operator() -> Arc<RequestContext> {
        Arc::new(RequestContext::for_session(
            Session::for_test(Principal::Operator).on_network(),
        ))
    }

    fn app() -> Arc<RequestContext> {
        Arc::new(RequestContext::app_for_test(vec![
            crate::agent::capabilities::ALL_CAPABILITY.clone(),
        ]))
    }

    /// `storage.rotateSecret` moves the keyring to the new secret: after it a restarted
    /// executor opens with the new secret and refuses the old one. A wrong current secret, the
    /// same secret twice or text that is not a secret change nothing; neither secret appears
    /// in an answer; only the operator on this machine calls it.
    #[tokio::test]
    async fn rotate_secret_moves_the_keyring_to_the_new_secret_only() {
        let folder = tempfile::tempdir().unwrap();
        let gate = open_gate(folder.path());
        let map = map_with(&gate);
        let new = "02".repeat(32);
        let file = crate::at_rest::keyring_file::path_in(folder.path());
        let before = std::fs::read(&file).unwrap();

        for caller in [network_operator(), app()] {
            let refused = map
                .dispatch(
                    "storage.rotateSecret",
                    json!({ "currentSecret": SECRET, "newSecret": new }),
                    caller,
                )
                .await
                .unwrap_err();
            assert_eq!(refused.code, 403);
        }
        for (current, next, code) in [
            ("03".repeat(32), new.clone(), 403),
            (SECRET.to_string(), SECRET.to_string(), 400),
            (SECRET.to_string(), "xyz".to_string(), 400),
            ("xyz".to_string(), new.clone(), 400),
        ] {
            let refused = map
                .dispatch(
                    "storage.rotateSecret",
                    json!({ "currentSecret": current, "newSecret": next }),
                    operator(),
                )
                .await
                .unwrap_err();
            assert_eq!(refused.code, code, "{}", refused.message);
            assert!(!refused.message.contains(SECRET) && !refused.message.contains(&new));
        }
        assert_eq!(std::fs::read(&file).unwrap(), before, "nothing written");

        let answer = map
            .dispatch(
                "storage.rotateSecret",
                json!({ "currentSecret": SECRET, "newSecret": new }),
                operator(),
            )
            .await
            .unwrap();
        let text = answer.to_string();
        assert!(!text.contains(SECRET) && !text.contains(&new));
        let answer: StorageRotateSecretResult = serde_json::from_value(answer).unwrap();
        assert_eq!(
            answer,
            StorageRotateSecretResult {
                generation: 2,
                old_fingerprint: fingerprint(&[1; 32]),
                new_fingerprint: fingerprint(&[2; 32]),
            }
        );
        assert_eq!(gate.status(true).root_generation, Some(2));
        // The running executor keeps its keys: the keyring answers.
        assert!(gate.keys().summary().is_ok());
        // Rotating again needs the new secret as the current one.
        let stale = map
            .dispatch(
                "storage.rotateSecret",
                json!({ "currentSecret": SECRET, "newSecret": "04".repeat(32) }),
                operator(),
            )
            .await
            .unwrap_err();
        assert_eq!(stale.code, 403);

        let restarted = Arc::new(StorageGate::new(folder.path(), false));
        let map = map_with(&restarted);
        let old = map
            .dispatch("storage.unlock", json!({ "secret": SECRET }), operator())
            .await
            .unwrap_err();
        assert_eq!(old.code, 403, "the old secret no longer opens");
        let opened: StorageUnlockResult = serde_json::from_value(
            map.dispatch("storage.unlock", json!({ "secret": new }), operator())
                .await
                .unwrap(),
        )
        .unwrap();
        assert!(!opened.created);
        assert_eq!(opened.status.root_generation, Some(2));
    }

    /// `storage.rotatePersonKey` gives the named owner's data key a new version; the main
    /// agent by default, a managed user by `user:<name>` or its name. An owner the keyring does
    /// not know is 404.
    #[tokio::test]
    async fn rotate_person_key_gives_the_owner_a_new_version() {
        let folder = tempfile::tempdir().unwrap();
        let gate = open_gate(folder.path());
        gate.keys()
            .create_person(KeyOwner::User("alice".into()))
            .unwrap();
        let map = map_with(&gate);

        let main: StorageRotatePersonKeyResult = serde_json::from_value(
            map.dispatch("storage.rotatePersonKey", json!({}), operator())
                .await
                .unwrap(),
        )
        .unwrap();
        assert_eq!(main.owner, "main");
        assert_eq!(main.version, 2);
        for (name, version) in [("user:alice", 2), ("alice", 3)] {
            let alice: StorageRotatePersonKeyResult = serde_json::from_value(
                map.dispatch(
                    "storage.rotatePersonKey",
                    json!({ "owner": name }),
                    operator(),
                )
                .await
                .unwrap(),
            )
            .unwrap();
            assert_eq!(alice.owner, "user:alice");
            assert_eq!(alice.version, version);
        }
        let unknown = map
            .dispatch(
                "storage.rotatePersonKey",
                json!({ "owner": "bob" }),
                operator(),
            )
            .await
            .unwrap_err();
        assert_eq!(unknown.code, 404);
        let refused = map
            .dispatch("storage.rotatePersonKey", json!({}), app())
            .await
            .unwrap_err();
        assert_eq!(refused.code, 403);
    }

    /// `storage.reencrypt` refuses the executor's own store, a name that is no store, a store
    /// the keyring does not have and a store whose job runs; only the operator on this machine
    /// starts one. A refused call makes no key version.
    #[tokio::test]
    async fn reencrypt_starts_a_job_on_a_known_store_only() {
        let folder = tempfile::tempdir().unwrap();
        let gate = open_gate(folder.path());
        let map = map_with(&gate);
        let known = StoreId::Perspective(uuid::Uuid::new_v4());
        gate.keys()
            .create_store(known.clone(), &[KeyOwner::Main])
            .unwrap();
        let unknown = StoreId::Perspective(uuid::Uuid::new_v4());
        for (store, code) in [
            ("executor".to_string(), 400),
            ("nothing".to_string(), 400),
            (unknown.to_string(), 404),
        ] {
            let refused = map
                .dispatch("storage.reencrypt", json!({ "store": store }), operator())
                .await
                .unwrap_err();
            assert_eq!(refused.code, code, "{store}: {}", refused.message);
        }
        for caller in [network_operator(), app()] {
            let refused = map
                .dispatch(
                    "storage.reencrypt",
                    json!({ "store": known.to_string() }),
                    caller,
                )
                .await
                .unwrap_err();
            assert_eq!(refused.code, 403);
        }
        {
            let _running = Claim::take(&known).unwrap();
            let refused = map
                .dispatch(
                    "storage.reencrypt",
                    json!({ "store": known.to_string() }),
                    operator(),
                )
                .await
                .unwrap_err();
            assert_eq!(refused.code, 409);
        }
        assert!(
            gate.keys().retiring_versions(&known).unwrap().is_empty(),
            "a refused call makes no version"
        );
        assert_eq!(gate.keys().summary().unwrap().stores.len(), 2);
    }
}
