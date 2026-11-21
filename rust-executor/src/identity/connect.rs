//! Connect on the web: what a browser login (`client: web`) needs beyond a native one.
//!
//! A web page never gets its token from `user.loginWithKey`. The frame (the auth domain's page)
//! signs the statement with the person's browser key, the executor checks it as any key login
//! and mints the grant, but keeps the token: the answer is a one-time code. The app's page
//! holds a PKCE `codeVerifier` the frame never saw, and `user.redeemCode` gives the token for
//! the code only with the verifier whose SHA-256 is the statement's `codeChallenge`.
//!
//! - [`Codes`]: the open codes, 60 s, single use. A code that is used (right or wrong),
//!   expires or is dropped ends its grant: the token it held was never released.
//! - [`AppRegistry`]: `GET <platform>/v1/connect/apps/{appId}` with a 5 minute cache. The
//!   executor reads it itself (the frame's word is not enough): the statement's origin must be
//!   one of the app's registered origins, and only a `firstParty` app may hold the operator
//!   role.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use base64::Engine;
use rand::RngCore;
use serde::Deserialize;
use sha2::{Digest, Sha256};

use super::key_login::{normalise_origin, EndUnlessAnswered, KeyLoginError};
use super::types::KeySessionResult;

/// How long a code stays open.
pub const CODE_TTL: u64 = 60;
/// How long the registry's answer for an app is kept.
pub const REGISTRY_TTL: Duration = Duration::from_secs(300);
/// The registry's most cached apps; a bigger cache drops its expired entries first.
const REGISTRY_MAX_APPS: usize = 256;
/// The platform an executor reads its app registry from when `identity.registryUrl` is unset.
pub const DEFAULT_REGISTRY_URL: &str = "https://api.coasys.app";
/// A registry read takes at most this long.
const REGISTRY_TIMEOUT: Duration = Duration::from_secs(10);

const B64: base64::engine::GeneralPurpose = base64::engine::general_purpose::URL_SAFE_NO_PAD;

/// `codeChallenge` for `verifier` (RFC 7636 S256): `base64url(SHA-256(verifier))`.
pub fn s256(verifier: &str) -> String {
    B64.encode(Sha256::digest(verifier.as_bytes()))
}

/// Whether `challenge` is the S256 of some verifier: a SHA-256 in base64url without padding.
pub fn is_s256_challenge(challenge: &str) -> bool {
    B64.decode(challenge).is_ok_and(|bytes| bytes.len() == 32) && challenge.len() == 43
}

/// RFC 7636 section 4.1: 43 to 128 characters of `[A-Za-z0-9-._~]`.
fn is_verifier(verifier: &str) -> bool {
    (43..=128).contains(&verifier.len())
        && verifier
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'-' | b'.' | b'_' | b'~'))
}

/// A login waiting for its code to be redeemed.
struct Pending {
    code_challenge: String,
    origin: String,
    expires: u64,
    /// The session the code opens; its token is not released before the redeem.
    session: KeySessionResult,
    /// The grant of `session`: ended when this is dropped unredeemed.
    grant: EndUnlessAnswered,
}

/// The open codes. In memory only: a restart drops them (and the grants stay ended, as their
/// guards drop).
#[derive(Clone, Default)]
pub struct Codes {
    open: Arc<Mutex<HashMap<String, Pending>>>,
}

impl Codes {
    /// A one-time code for the login `session` (whose grant `grant` is not yet answered), bound
    /// to `code_challenge` and the app's `origin`, open until the expiry it answers with.
    pub(super) fn issue(
        &self,
        session: KeySessionResult,
        grant: EndUnlessAnswered,
        code_challenge: String,
        origin: String,
        now: u64,
    ) -> Result<(String, u64), KeyLoginError> {
        let mut bytes = [0u8; 32];
        rand::rngs::OsRng.fill_bytes(&mut bytes);
        let code = B64.encode(bytes);
        let expires = now + CODE_TTL;
        let mut open = self.lock()?;
        // Expired codes go first, and end their grants as they drop.
        open.retain(|_, pending| pending.expires >= now);
        open.insert(
            code.clone(),
            Pending {
                code_challenge,
                origin,
                expires,
                session,
                grant,
            },
        );
        drop(open);
        // A code nobody redeems ends its grant at its expiry, not at the next call.
        let codes = self.clone();
        let sweep = code.clone();
        tokio::spawn(async move {
            tokio::time::sleep(Duration::from_secs(CODE_TTL + 1)).await;
            if let Ok(mut open) = codes.lock() {
                open.remove(&sweep);
            }
        });
        Ok((code, expires))
    }

    /// `user.redeemCode`: the session `code` opened, when `verifier` is the one whose S256 the
    /// login named and the code has not expired. The code is used up whatever the answer, and a
    /// refusal ends the grant: a wrong guess spends the code.
    pub fn redeem(
        &self,
        code: &str,
        verifier: &str,
        now: u64,
    ) -> Result<KeySessionResult, KeyLoginError> {
        let pending = self.lock()?.remove(code).ok_or(KeyLoginError::Code)?;
        if now > pending.expires
            || !is_verifier(verifier)
            || s256(verifier) != pending.code_challenge
        {
            return Err(KeyLoginError::Code);
        }
        pending.grant.keep();
        Ok(pending.session)
    }

    /// The origin `code` was issued for, while it is open.
    #[cfg(test)]
    pub fn origin_of(&self, code: &str) -> Option<String> {
        self.lock().ok()?.get(code).map(|p| p.origin.clone())
    }

    fn lock(&self) -> Result<std::sync::MutexGuard<'_, HashMap<String, Pending>>, KeyLoginError> {
        self.open
            .lock()
            .map_err(|_| KeyLoginError::Internal("the code store is poisoned".into()))
    }
}

/// The state of web logins in an [`super::service::IdentityService`].
#[derive(Default)]
pub struct Connect {
    pub codes: Codes,
    pub registry: AppRegistry,
}

impl Connect {
    /// Web login state reading the registry at `registry_url`, else the default platform.
    pub fn new(registry_url: Option<&str>) -> Self {
        Connect {
            codes: Codes::default(),
            registry: registry_url.map_or_else(AppRegistry::default, AppRegistry::new),
        }
    }
}

/// What the registry says of an app (`GET /v1/connect/apps/{appId}`).
#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct RegisteredApp {
    /// The origins the app's pages are served from, exactly.
    pub origins: Vec<String>,
    /// A Coasys app: it may hold the operator role of an operator's own executor.
    pub first_party: bool,
}

impl RegisteredApp {
    /// Whether `origin` (normalised) is one of the app's origins.
    pub fn has_origin(&self, origin: &str) -> bool {
        self.origins
            .iter()
            .any(|o| normalise_origin(o).is_ok_and(|o| o == origin))
    }
}

/// The platform's app registry, read with a short cache.
pub struct AppRegistry {
    base: String,
    client: reqwest::Client,
    cache: Mutex<HashMap<String, (Instant, RegisteredApp)>>,
}

impl Default for AppRegistry {
    fn default() -> Self {
        AppRegistry::new(DEFAULT_REGISTRY_URL)
    }
}

impl AppRegistry {
    /// A registry read at `base` (`identity.registryUrl`).
    pub fn new(base: &str) -> Self {
        AppRegistry {
            base: base.trim_end_matches('/').to_string(),
            client: reqwest::Client::builder()
                .timeout(REGISTRY_TIMEOUT)
                .redirect(reqwest::redirect::Policy::none())
                .build()
                .unwrap_or_default(),
            cache: Mutex::default(),
        }
    }

    /// The app `app_id`: from the cache while its answer is fresh, else from the platform.
    /// `Origin` for an app the registry does not know; `RegistryUnavailable` when it cannot be
    /// read (nothing is answered from a stale entry: a revoked origin must not linger).
    pub async fn app(&self, app_id: &str) -> Result<RegisteredApp, KeyLoginError> {
        if app_id.is_empty()
            || app_id.len() > 64
            || !app_id
                .bytes()
                .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'.'))
        {
            return Err(KeyLoginError::BadRequest(format!(
                "{app_id:?} is not an app id"
            )));
        }
        if let Some(app) = self.cached(app_id) {
            return Ok(app);
        }
        let unavailable = |why: String| {
            log::warn!("connect: the app registry did not answer for {app_id}: {why}");
            KeyLoginError::RegistryUnavailable
        };
        let response = self
            .client
            .get(format!("{}/v1/connect/apps/{app_id}", self.base))
            .send()
            .await
            .map_err(|e| unavailable(e.to_string()))?;
        match response.status() {
            s if s == reqwest::StatusCode::NOT_FOUND => return Err(KeyLoginError::Origin),
            s if !s.is_success() => return Err(unavailable(format!("status {s}"))),
            _ => {}
        }
        let app: RegisteredApp = response
            .json()
            .await
            .map_err(|e| unavailable(e.to_string()))?;
        let mut cache = self
            .cache
            .lock()
            .map_err(|_| KeyLoginError::Internal("the registry cache is poisoned".into()))?;
        if cache.len() >= REGISTRY_MAX_APPS {
            cache.retain(|_, (at, _)| at.elapsed() < REGISTRY_TTL);
        }
        if cache.len() < REGISTRY_MAX_APPS {
            cache.insert(app_id.to_string(), (Instant::now(), app.clone()));
        }
        Ok(app)
    }

    fn cached(&self, app_id: &str) -> Option<RegisteredApp> {
        let cache = self.cache.lock().ok()?;
        cache
            .get(app_id)
            .filter(|(at, _)| at.elapsed() < REGISTRY_TTL)
            .map(|(_, app)| app.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// RFC 7636 appendix B.
    #[test]
    fn s256_is_the_rfc_7636_transform() {
        let verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk";
        assert_eq!(
            s256(verifier),
            "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
        );
        assert!(is_s256_challenge(&s256(verifier)));
        assert!(is_verifier(verifier));
    }

    #[test]
    fn a_challenge_is_a_sha256_and_a_verifier_is_43_to_128_unreserved_characters() {
        assert!(!is_s256_challenge("short"));
        assert!(!is_s256_challenge(&"A".repeat(44)));
        assert!(!is_s256_challenge(&format!("{}=", "A".repeat(42))));
        assert!(!is_verifier(&"a".repeat(42)));
        assert!(!is_verifier(&"a".repeat(129)));
        assert!(!is_verifier(&format!("{} ", "a".repeat(42))));
    }

    #[test]
    fn an_app_matches_its_origins_after_normalising() {
        let app = RegisteredApp {
            origins: vec!["https://Coasys.app/".into()],
            first_party: true,
        };
        assert!(app.has_origin("https://coasys.app"));
        assert!(!app.has_origin("https://evil.example"));
    }

    #[tokio::test]
    async fn a_registry_refuses_an_app_id_that_is_a_path() {
        let registry = AppRegistry::new("http://127.0.0.1:1");
        for bad in ["", "a/b", "../x", "a?b", &"a".repeat(65)] {
            assert!(
                matches!(registry.app(bad).await, Err(KeyLoginError::BadRequest(_))),
                "{bad:?}"
            );
        }
    }
}
