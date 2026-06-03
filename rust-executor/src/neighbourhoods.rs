use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use uuid::Uuid;

use crate::agent::{did_for_context, AgentContext};
use crate::languages::LanguageController;
use crate::perspectives::{add_perspective, all_perspectives, get_perspective, update_perspective};
use crate::types::*;
use crate::types::{Neighbourhood, Perspective, PerspectiveHandle, PerspectiveState};

/// Spike package identity for the holograph-link Language. The
/// canonical AD4M content-address (`hash("@coasys/holograph-link@<v>")`)
/// is the address every neighborhood that defaults to holograph-link
/// will reference. v1 uses 0.1.0 to match
/// `bootstrap-languages/holograph-link/package.json`.
pub const HOLOGRAPH_LINK_PACKAGE_ID: &str = "@coasys/holograph-link@0.1.0";

/// Compute the canonical AD4M address for the holograph-link Language.
/// Matches the `hash()` host function in `js_core/utils_extension.rs`
/// (SHA-256 -> CIDv1 -> base58btc with the `Qm` prefix), so the
/// address is the same whether produced from Rust here or from the
/// JS-side `hash(...)` call.
pub fn holograph_link_default_address() -> String {
    use cid::Cid;
    use multibase::Base;
    use multihash::{Code, MultihashDigest};
    let multihash = Code::Sha2_256.digest(HOLOGRAPH_LINK_PACKAGE_ID.as_bytes());
    let cid = Cid::new_v1(0, multihash);
    let encoded = multibase::encode(Base::Base58Btc, cid.to_bytes());
    format!("Qm{}", encoded)
}

/// True when the runtime should substitute the holograph-link Language
/// for neighborhoods published without an explicit `link_language`.
/// Gated by the `HOLOGRAPH_DEFAULT_NEIGHBORHOOD=1` env flag per
/// SPIKE.md §2.2 Step 6.
pub fn holograph_default_enabled() -> bool {
    std::env::var("HOLOGRAPH_DEFAULT_NEIGHBORHOOD")
        .map(|v| v.trim() == "1")
        .unwrap_or(false)
}

/// Resolve the effective link-language address for a publish request.
///
/// - `Some(addr)` non-empty: caller-supplied address wins.
/// - empty or `None`: substitute the holograph-link default if and
///   only if `HOLOGRAPH_DEFAULT_NEIGHBORHOOD=1`. Otherwise return an
///   `Err` so the caller can surface "link_language required" to the
///   client (matching pre-Step-6 behavior).
pub fn resolve_link_language(requested: Option<String>) -> Result<String, AnyError> {
    let trimmed = requested
        .as_deref()
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string());
    if let Some(addr) = trimmed {
        return Ok(addr);
    }
    if holograph_default_enabled() {
        let addr = holograph_link_default_address();
        log::info!(
            "[holograph] Substituting holograph-link as default link_language: {}",
            addr
        );
        return Ok(addr);
    }
    Err(anyhow!(
        "link_language is required (set HOLOGRAPH_DEFAULT_NEIGHBORHOOD=1 to default to holograph-link)"
    ))
}

pub async fn _neighbourhood_publish_from_perspective(
    uuid: &str,
    link_language: String,
    meta: Perspective,
) -> Result<String, AnyError> {
    neighbourhood_publish_from_perspective_with_context(
        uuid,
        link_language,
        meta,
        &AgentContext::main_agent(),
    )
    .await
}

pub async fn neighbourhood_publish_from_perspective_with_context(
    uuid: &str,
    link_language: String,
    meta: Perspective,
    context: &AgentContext,
) -> Result<String, AnyError> {
    let perspective = get_perspective(uuid).ok_or(anyhow!("Perspective not found"))?;

    LanguageController::install_language(link_language.clone()).await?;

    let neighbourhood = Neighbourhood {
        link_language,
        meta,
    };

    // Create neighbourhood with context
    let neighbourhood_address =
        LanguageController::create_neighbourhood_with_context(neighbourhood, context).await?;

    let neighbourhood_url = format!("neighbourhood://{}", neighbourhood_address);
    let neighbourhood_exp = LanguageController::get_neighbourhood(neighbourhood_address)
        .await?
        .ok_or(anyhow!("Could not retrieve NeigbourhoodExpression which was just created. Problem with Neighbourhood language"))?;

    let mut perspective_handle = perspective.persisted.lock().await.clone();
    // Add shared perspective to original perspective and then update controller
    perspective_handle.shared_url = Some(neighbourhood_url.clone());
    perspective_handle.neighbourhood = Some(neighbourhood_exp);
    perspective_handle.state = PerspectiveState::NeighbourhoodCreationInitiated;

    // Initialize owners list with the creator's DID if not already set
    let creator_did = did_for_context(context)?;
    if perspective_handle.owners.is_none() {
        perspective_handle.owners = Some(vec![creator_did.clone()]);
    } else if !perspective_handle.is_owned_by(&creator_did) {
        perspective_handle.add_owner(&creator_did);
    }

    update_perspective(&perspective_handle)
        .await
        .map_err(|e| anyhow!(e))?;

    // Ensure any existing shared links are committed to the link language
    // This is critical for early links created before neighbourhood sharing
    // We need to do this after the neighbourhood is created but before other agents join
    perspective.ensure_public_links_are_shared().await;
    Ok(neighbourhood_url)
}

pub async fn _install_neighbourhood(url: String) -> Result<PerspectiveHandle, AnyError> {
    install_neighbourhood_with_context(url, &crate::agent::AgentContext::main_agent()).await
}

pub async fn install_neighbourhood_with_context(
    url: String,
    context: &crate::agent::AgentContext,
) -> Result<PerspectiveHandle, AnyError> {
    let perspectives = all_perspectives();

    // Check if neighbourhood already exists
    for p in perspectives.iter() {
        let mut handle = p.persisted.lock().await.clone();
        if handle.shared_url == Some(url.clone()) {
            // Neighbourhood exists - add this user as owner if it's a user context
            log::info!(
                "Adding user {:?} to existing neighbourhood {}",
                context.user_email,
                url
            );
            if let Some(user_email) = &context.user_email {
                let user_did = crate::agent::AgentService::get_user_did_by_email(user_email)?;

                // Update database
                crate::db::Ad4mDb::with_global_instance(|db| {
                    db.add_owner_to_neighbourhood(&url, &user_did)
                })?;

                // Add user to owners list
                // Update in-memory handle
                handle.add_owner(&user_did);

                update_perspective(&handle).await.map_err(|e| anyhow!(e))?;

                // Update link language with new owners list
                if let Some(owners) = &handle.owners {
                    p.update_local_agents(owners.clone()).await;
                }

                log::info!(
                    "Added user {} to existing neighbourhood {}. Link language has been updated with new owners.",
                    user_email,
                    url
                );
                return Ok(handle.clone());
            } else {
                // Main agent trying to join existing neighbourhood
                // Add main agent to owners list for access control
                let main_agent_did = crate::agent::did_for_context(context)?;

                // Update database
                crate::db::Ad4mDb::with_global_instance(|db| {
                    db.add_owner_to_neighbourhood(&url, &main_agent_did)
                })?;

                // Add main agent to owners list in memory
                handle.add_owner(&main_agent_did);

                update_perspective(&handle).await.map_err(|e| anyhow!(e))?;

                // Update link language with new owners list
                if let Some(owners) = &handle.owners {
                    p.update_local_agents(owners.clone()).await;
                }

                log::info!(
                    "Added main agent to existing neighbourhood {}. Link language has been updated with new owners.",
                    url
                );
                return Ok(handle.clone());
            }
        }
    }

    let expression_ref = ExpressionRef::try_from(url.to_string())?;
    let neighbourhood_exp =
        LanguageController::get_neighbourhood(expression_ref.expression).await?;
    if neighbourhood_exp.is_none() {
        return Err(anyhow!("Could not find neighbourhood with URL {}", url));
    }
    log::info!(
        "Core.install_neighbourhood(): Got neighbourhood {:?}",
        neighbourhood_exp
    );
    let neighbourhood = neighbourhood_exp.unwrap();

    // Install the link language before checking its availability.
    // This fetches the bundle from the language-language and loads it on the JS side.
    if let Err(e) =
        LanguageController::install_language(neighbourhood.data.link_language.clone()).await
    {
        log::warn!(
            "Failed to install link language {}: {}",
            neighbourhood.data.link_language,
            e
        );
    }

    let state = if LanguageController::language_by_address(neighbourhood.data.link_language.clone())
        .await?
        .is_some()
    {
        PerspectiveState::LinkLanguageInstalledButNotSynced
    } else {
        PerspectiveState::LinkLanguageFailedToInstall
    };

    log::info!(
        "Core.install_neighbourhood(): Creating perspective {}, {:?}, {:?}",
        url,
        neighbourhood,
        state
    );

    let owner_did = did_for_context(context)?;

    let handle = PerspectiveHandle {
        uuid: Uuid::new_v4().to_string(),
        name: Some(url.clone()),
        shared_url: Some(url.clone()),
        neighbourhood: Some(neighbourhood.clone()),
        state,
        owners: Some(vec![owner_did.clone()]), // Initialize owners list with creator
    };
    add_perspective(handle.clone(), Some(true))
        .await
        .map_err(|e| anyhow!(e))?;

    log::info!(
        "Created new perspective for neighbourhood {}. Link language will handle DID mapping when accessed by user {}",
        url,
        owner_did
    );

    Ok(handle)
}

#[cfg(test)]
mod tests {
    use super::*;

    // ----- Helper to scope env mutations to one test --------------
    // std::env::set_var is process-global; if we run tests in
    // parallel, the env state interleaves. Cargo's default test
    // harness runs in parallel; these tests must run with
    // --test-threads=1. The Step-6 cargo command does that.

    fn with_env<F: FnOnce()>(key: &str, value: Option<&str>, f: F) {
        let prev = std::env::var(key).ok();
        match value {
            Some(v) => std::env::set_var(key, v),
            None => std::env::remove_var(key),
        }
        f();
        match prev {
            Some(v) => std::env::set_var(key, v),
            None => std::env::remove_var(key),
        }
    }

    #[test]
    fn holograph_link_default_address_is_stable_qm() {
        let addr = holograph_link_default_address();
        assert!(
            addr.starts_with("Qm"),
            "expected Qm-prefixed CID, got {addr}"
        );
        // Stable across runs because the input string is fixed.
        let addr2 = holograph_link_default_address();
        assert_eq!(addr, addr2);
    }

    #[test]
    fn holograph_default_disabled_by_default() {
        with_env("HOLOGRAPH_DEFAULT_NEIGHBORHOOD", None, || {
            assert!(!holograph_default_enabled());
        });
    }

    #[test]
    fn holograph_default_enabled_with_flag_one() {
        with_env("HOLOGRAPH_DEFAULT_NEIGHBORHOOD", Some("1"), || {
            assert!(holograph_default_enabled());
        });
    }

    #[test]
    fn holograph_default_disabled_with_flag_other_value() {
        with_env("HOLOGRAPH_DEFAULT_NEIGHBORHOOD", Some("0"), || {
            assert!(!holograph_default_enabled());
        });
        with_env("HOLOGRAPH_DEFAULT_NEIGHBORHOOD", Some("true"), || {
            assert!(!holograph_default_enabled());
        });
    }

    #[test]
    fn resolve_passes_through_explicit_address() {
        with_env("HOLOGRAPH_DEFAULT_NEIGHBORHOOD", Some("1"), || {
            // Even with the env flag on, an explicit address wins.
            let addr = resolve_link_language(Some("QmExplicit123".to_string())).unwrap();
            assert_eq!(addr, "QmExplicit123");
        });
    }

    #[test]
    fn resolve_substitutes_default_when_flag_set_and_empty_input() {
        with_env("HOLOGRAPH_DEFAULT_NEIGHBORHOOD", Some("1"), || {
            let addr = resolve_link_language(None).unwrap();
            assert_eq!(addr, holograph_link_default_address());

            let addr2 = resolve_link_language(Some("".to_string())).unwrap();
            assert_eq!(addr2, holograph_link_default_address());

            let addr3 = resolve_link_language(Some("   ".to_string())).unwrap();
            assert_eq!(addr3, holograph_link_default_address());
        });
    }

    #[test]
    fn resolve_errors_when_flag_unset_and_empty_input() {
        with_env("HOLOGRAPH_DEFAULT_NEIGHBORHOOD", None, || {
            let err = resolve_link_language(None).unwrap_err().to_string();
            assert!(err.contains("link_language is required"), "got: {err}");

            let err2 = resolve_link_language(Some("".to_string()))
                .unwrap_err()
                .to_string();
            assert!(err2.contains("link_language is required"));
        });
    }
}
