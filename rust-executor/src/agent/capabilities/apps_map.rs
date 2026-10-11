use super::token::read_stored_token_id;
use super::types::AuthInfoExtended;
use crate::agent::session::TokenId;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::Path;
use std::sync::Mutex;

/// An approved app. The registry keeps its token's id and expiry, never the token: the token
/// goes once to the app (`agent.generateJwt`), and revocation needs only the id.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct App {
    auth_info_extended: AuthInfoExtended,
    revoked: bool,
    token_id: TokenId,
    token_expires_at: u64,
}

/// A record of the registry file. Executors before Plan 20 commit 20-4d stored the raw token;
/// [`load_apps`] migrates such records.
#[derive(Deserialize)]
#[serde(untagged)]
enum StoredApp {
    Current(App),
    WithRawToken {
        auth_info_extended: AuthInfoExtended,
        revoked: bool,
        token: String,
    },
}

use std::env;

lazy_static! {
    static ref DATA_FILE_PATH: Mutex<String> =
        Mutex::new(env::var("APPS_DATA_FILE").unwrap_or_else(|_| "apps_data.json".to_string()));
}

pub(crate) fn get_data_file_path() -> String {
    DATA_FILE_PATH.lock().unwrap().clone()
}

pub fn set_data_file_path(file_path: String) {
    let mut data_file_path = DATA_FILE_PATH.lock().unwrap();
    *data_file_path = file_path;
}

fn write_apps(path: &Path, apps: &HashMap<String, App>) -> io::Result<()> {
    fs::write(path, serde_json::to_string(apps)?)
}

fn persist_apps_to_file(apps: &HashMap<String, App>) -> io::Result<()> {
    write_apps(Path::new(&get_data_file_path()), apps)
}

/// Reads the registry file at `path`. A file that still holds raw tokens is migrated once:
/// each record keeps its token's id and expiry, and the file is rewritten without the tokens.
/// Tokens issued before keep working, because they carry the same id, and revoked apps stay
/// revoked: their records keep `revoked` and the id ([`revoked_token_ids`]).
fn load_apps(path: &Path) -> io::Result<HashMap<String, App>> {
    let stored: HashMap<String, StoredApp> = serde_json::from_str(&fs::read_to_string(path)?)?;
    let mut migrated = 0;
    let mut apps = HashMap::with_capacity(stored.len());
    for (request_id, record) in stored {
        let app = match record {
            StoredApp::Current(app) => app,
            StoredApp::WithRawToken {
                auth_info_extended,
                revoked,
                token,
            } => {
                migrated += 1;
                let Some((token_id, token_expires_at)) = read_stored_token_id(&token) else {
                    log::warn!(
                        "App registry: dropped app {request_id}, whose stored token does not \
                         parse and so opens no session"
                    );
                    continue;
                };
                App {
                    auth_info_extended,
                    revoked,
                    token_id,
                    token_expires_at,
                }
            }
        };
        apps.insert(request_id, app);
    }
    if migrated > 0 {
        // The migrated records hold in memory even if the rewrite fails; the next change
        // writes the file again.
        match write_apps(path, &apps) {
            Ok(()) => log::info!("App registry: {migrated} stored tokens replaced by their ids"),
            Err(e) => log::error!("App registry: migrated in memory, but not saved: {e}"),
        }
    }
    Ok(apps)
}

lazy_static! {
    static ref APPS: Mutex<HashMap<String, App>> = {
        let path = get_data_file_path();
        let apps = if Path::new(&path).exists() {
            load_apps(Path::new(&path)).unwrap_or_else(|e| {
                log::error!("App registry: {path} does not load, starting empty: {e}");
                HashMap::new()
            })
        } else {
            HashMap::new()
        };
        Mutex::new(apps)
    };
}

/// Registers an approved app under the id and expiry of the token it received.
pub fn insert_app(
    request_key: String,
    auth_info_extended: AuthInfoExtended,
    token_id: TokenId,
    token_expires_at: u64,
) -> Result<(), String> {
    let mut apps = APPS.lock().map_err(|e| e.to_string())?;
    let app = App {
        auth_info_extended,
        revoked: false,
        token_id,
        token_expires_at,
    };
    apps.insert(request_key, app);
    persist_apps_to_file(&apps).map_err(|e| e.to_string())?;
    Ok(())
}

/// Why `revoke_app` failed.
#[derive(Debug)]
pub enum RevokeError {
    /// No app has this request id or token.
    NotFound,
    /// The app stays revoked in memory, but the registry file did not take the change, so a
    /// restart would bring the app back.
    Store(String),
}

impl std::fmt::Display for RevokeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RevokeError::NotFound => write!(f, "No app matches this token or request id"),
            RevokeError::Store(e) => write!(f, "Revoked until restart, but not saved: {e}"),
        }
    }
}

/// Revokes the app that `token_or_request_id` names: its request id, which the SDK sends, or
/// its JWT, which the RPC's field name (`token`) promises. A JWT names its app by its id
/// (`nonce`), read without the wallet. A value that names no app is an error: a revoke that
/// reports success and revokes nothing ends the search for a leak (#1060).
///
/// The token's id joins the revoked set, so the token stays revoked after `remove_app`
/// deletes the record (#396). The record keeps its `revoked` flag for the app list.
pub fn revoke_app(token_or_request_id: &str) -> Result<(), RevokeError> {
    let mut apps = APPS.lock().map_err(|e| RevokeError::Store(e.to_string()))?;
    let key = if apps.contains_key(token_or_request_id) {
        Some(token_or_request_id.to_string())
    } else {
        read_stored_token_id(token_or_request_id).and_then(|(id, _)| {
            apps.iter()
                .find(|(_, app)| app.token_id == id)
                .map(|(key, _)| key.clone())
        })
    };
    let Some(app) = key.and_then(|key| apps.get_mut(&key)) else {
        return Err(RevokeError::NotFound);
    };
    app.revoked = true;
    let saved = crate::agent::revoked::revoke(app.token_id.clone(), app.token_expires_at);
    let persisted = persist_apps_to_file(&apps).map_err(|e| e.to_string());
    saved.and(persisted).map_err(RevokeError::Store)
}

/// The ids and expiries of the tokens of revoked apps. Apps revoked before the revoked set
/// existed are in the registry file only; start-up adds them to the set.
pub fn revoked_token_ids() -> Vec<(TokenId, u64)> {
    let apps = APPS.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
    apps.values()
        .filter(|app| app.revoked)
        .map(|app| (app.token_id.clone(), app.token_expires_at))
        .collect()
}

pub fn remove_app(request_key: &str) -> Result<(), String> {
    let mut apps = APPS.lock().map_err(|e| e.to_string())?;
    if apps.remove(request_key).is_some() {
        persist_apps_to_file(&apps).map_err(|e| e.to_string())?;
        Ok(())
    } else {
        Err(format!("App with request_key '{}' not found.", request_key))
    }
}

pub fn get_app(request_key: &str) -> Result<Option<App>, String> {
    let apps = APPS.lock().map_err(|e| e.to_string())?;
    Ok(apps.get(request_key).cloned())
}

/// The app list. The registry holds no token, so `token` is always empty; the field stays on
/// the wire for clients that require it.
pub fn get_apps() -> Vec<crate::types::Apps> {
    let apps = APPS.lock().unwrap();
    apps.iter()
        .map(|(request_id, app)| crate::types::Apps {
            auth: app.auth_info_extended.auth.clone(),
            request_id: request_id.clone(),
            revoked: Some(app.revoked),
            token: String::new(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::capabilities::{capabilities_from_token, generate_jwt, AuthInfo};

    fn info(request_id: &str) -> AuthInfoExtended {
        AuthInfoExtended {
            request_id: request_id.to_string(),
            auth: AuthInfo::default(),
        }
    }

    /// A record as executors before 20-4d wrote it.
    fn record_with_raw_token(request_id: &str, revoked: bool, token: &str) -> serde_json::Value {
        serde_json::json!({
            "auth_info_extended": info(request_id),
            "revoked": revoked,
            "token": token,
        })
    }

    // An executor upgraded from before 20-4d finds raw tokens in its registry file. The
    // migration keeps each token's id, drops the token, keeps revocations, and the tokens
    // the apps already hold keep working.
    #[test]
    fn a_registry_file_with_raw_tokens_migrates_to_token_ids() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::use_test_apps_file();
        let mint = || generate_jwt("migrated app".to_string(), 3600, AuthInfo::default()).unwrap();
        let (live_token, revoked_token) = (mint(), mint());
        let live = format!("migrate-live-{}", uuid::Uuid::new_v4());
        let revoked = format!("migrate-revoked-{}", uuid::Uuid::new_v4());
        let broken = format!("migrate-broken-{}", uuid::Uuid::new_v4());
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("apps_data.json");
        let old = serde_json::json!({
            &live: record_with_raw_token(&live, false, &live_token),
            &revoked: record_with_raw_token(&revoked, true, &revoked_token),
            &broken: record_with_raw_token(&broken, false, "not-a-jwt"),
        });
        fs::write(&path, old.to_string()).unwrap();

        let apps = load_apps(&path).unwrap();
        let (live_id, live_exp) = read_stored_token_id(&live_token).unwrap();
        let (revoked_id, _) = read_stored_token_id(&revoked_token).unwrap();
        assert_eq!(apps[&live].token_id, live_id);
        assert_eq!(apps[&live].token_expires_at, live_exp);
        assert!(!apps[&live].revoked);
        assert_eq!(apps[&revoked].token_id, revoked_id);
        assert!(apps[&revoked].revoked);
        assert!(!apps.contains_key(&broken), "a token that does not parse");

        let file = fs::read_to_string(&path).unwrap();
        for token in [&live_token, &revoked_token] {
            assert!(!file.contains(token.as_str()), "the file kept a raw token");
        }
        assert!(file.contains(&live_id.0) && file.contains(&revoked_id.0));
        assert!(!file.contains("\"token\""));
        // Migrated once: a second load reads the new format and leaves the file as it is.
        assert_eq!(load_apps(&path).unwrap().len(), 2);
        assert_eq!(fs::read_to_string(&path).unwrap(), file);

        // The executor starts with the migrated registry, before the revoked-tokens table
        // knew the revoked app.
        APPS.lock().unwrap().extend(apps);
        crate::agent::revoked::load_at_start().unwrap();
        assert!(capabilities_from_token(live_token.clone(), &None).is_ok());
        assert!(capabilities_from_token(revoked_token, &None).is_err());

        // Revoking by JWT finds the migrated app by the token's id.
        revoke_app(&live_token).unwrap();
        assert!(get_app(&live).unwrap().unwrap().revoked);
        assert!(capabilities_from_token(live_token, &None).is_err());
        remove_app(&live).unwrap();
        remove_app(&revoked).unwrap();
    }

    #[test]
    fn a_jwt_revokes_only_the_app_it_was_issued_to() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        crate::test_utils::use_test_apps_file();
        let mint = || generate_jwt("jwt revoke".to_string(), 3600, AuthInfo::default()).unwrap();
        let (token, other_token) = (mint(), mint());
        let request_id = format!("jwt-revoke-{}", uuid::Uuid::new_v4());
        crate::test_utils::insert_app_with_token(request_id.clone(), info(&request_id), &token);

        assert!(matches!(
            revoke_app(&other_token),
            Err(RevokeError::NotFound)
        ));
        assert!(!get_app(&request_id).unwrap().unwrap().revoked);
        revoke_app(&token).unwrap();
        assert!(get_app(&request_id).unwrap().unwrap().revoked);
        assert!(capabilities_from_token(token, &None).is_err());
        assert!(capabilities_from_token(other_token, &None).is_ok());
        remove_app(&request_id).unwrap();
    }
}
