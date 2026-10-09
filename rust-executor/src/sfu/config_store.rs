//! A neighbourhood's call config, stored as links in the neighbourhood.
//!
//! The config is a link `<neighbourhood url> ad4m://sfu_config <literal:json:…>`,
//! shared like any other link, so every member's executor reads the same
//! config and it survives restarts. Any member can add links, so only one
//! author counts: the neighbourhood's creator — the author of its
//! neighbourhood expression. Of the creator's validly signed config links
//! the newest wins; anyone else's are ignored.

use crate::agent::AgentContext;
use crate::perspectives::all_perspectives;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, Link, LinkQuery, LinkStatus};

use super::types::SfuConfig;
use super::MAX_MESH_PARTICIPANTS;

/// Predicate of the config link. Its source is the neighbourhood URL.
pub const SFU_CONFIG_PREDICATE: &str = "ad4m://sfu_config";

/// This node's perspective for `neighbourhood_url`, if it has joined it.
async fn neighbourhood_perspective(neighbourhood_url: &str) -> Option<PerspectiveInstance> {
    for perspective in all_perspectives() {
        if perspective.persisted.lock().await.shared_url.as_deref() == Some(neighbourhood_url) {
            return Some(perspective);
        }
    }
    None
}

/// The DID that created the neighbourhood, from its neighbourhood expression.
/// `None` for a perspective that carries no expression (a synthetic test
/// neighbourhood): such a neighbourhood has no config author.
pub async fn neighbourhood_creator(neighbourhood_url: &str) -> Option<String> {
    let perspective = neighbourhood_perspective(neighbourhood_url).await?;
    let handle = perspective.persisted.lock().await;
    handle.neighbourhood.as_ref().map(|n| n.author.clone())
}

/// `literal:json:<RFC 3986-encoded JSON>`, as the SDK's `Literal` writes it.
fn to_literal(config: &SfuConfig) -> Result<String, String> {
    let json = serde_json::to_string(config).map_err(|e| e.to_string())?;
    Ok(format!("literal:json:{}", urlencoding::encode(&json)))
}

fn from_literal(target: &str) -> Option<SfuConfig> {
    let encoded = target.strip_prefix("literal:json:")?;
    serde_json::from_str(&urlencoding::decode(encoded).ok()?).ok()
}

/// The config that counts among `links`: the newest validly signed one by
/// `creator` that parses. Timestamps are RFC 3339 in UTC, so they order as
/// strings. `maxMeshParticipants` is clamped to the range `setConfig`
/// accepts: a link written past that check (by hand, or by another build)
/// must not set a room's capacity.
fn creators_latest(links: &[DecoratedLinkExpression], creator: &str) -> Option<SfuConfig> {
    links
        .iter()
        .filter(|l| l.author == creator && l.proof.valid == Some(true))
        .filter_map(|l| Some((l.timestamp.as_str(), from_literal(&l.data.target)?)))
        .max_by(|a, b| a.0.cmp(b.0))
        .map(|(_, mut config)| {
            config.max_mesh_participants =
                config.max_mesh_participants.clamp(2, MAX_MESH_PARTICIPANTS);
            config
        })
}

/// The neighbourhood's call config, or `None` when it has none (not joined
/// here, no creator, or no config link from the creator yet).
pub async fn stored_config(neighbourhood_url: &str) -> Option<SfuConfig> {
    let perspective = neighbourhood_perspective(neighbourhood_url).await?;
    let creator = {
        let handle = perspective.persisted.lock().await;
        handle.neighbourhood.as_ref()?.author.clone()
    };
    let query = LinkQuery {
        source: Some(neighbourhood_url.to_string()),
        predicate: Some(SFU_CONFIG_PREDICATE.to_string()),
        ..Default::default()
    };
    match perspective.get_links(&query).await {
        Ok(links) => creators_latest(&links, &creator),
        Err(e) => {
            log::warn!("SFU config: reading {} failed: {}", neighbourhood_url, e);
            None
        }
    }
}

/// [`stored_config`], or the default config.
pub async fn read_config(neighbourhood_url: &str) -> SfuConfig {
    stored_config(neighbourhood_url).await.unwrap_or_default()
}

/// Store `config` as a shared link in the neighbourhood, signed as `context`.
/// The caller checks that `context` is the creator: a link from anyone else
/// would be stored and then ignored by every reader.
pub async fn write_config(
    neighbourhood_url: &str,
    config: &SfuConfig,
    context: &AgentContext,
) -> Result<(), String> {
    let mut perspective = neighbourhood_perspective(neighbourhood_url)
        .await
        .ok_or_else(|| "This node has not joined that neighbourhood".to_string())?;
    let link = Link {
        source: neighbourhood_url.to_string(),
        predicate: Some(SFU_CONFIG_PREDICATE.to_string()),
        target: to_literal(config)?,
    };
    perspective
        .add_link(link, LinkStatus::Shared, None, context)
        .await
        .map_err(|e| format!("Storing the call config failed: {}", e))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::DecoratedExpressionProof;

    fn link(
        author: &str,
        timestamp: &str,
        config: &SfuConfig,
        valid: bool,
    ) -> DecoratedLinkExpression {
        DecoratedLinkExpression {
            author: author.into(),
            timestamp: timestamp.into(),
            data: Link {
                source: "neighbourhood://n".into(),
                predicate: Some(SFU_CONFIG_PREDICATE.into()),
                target: to_literal(config).unwrap(),
            },
            proof: DecoratedExpressionProof {
                key: String::new(),
                signature: String::new(),
                valid: Some(valid),
                invalid: Some(!valid),
            },
            status: None,
        }
    }

    fn mode(m: &str) -> SfuConfig {
        SfuConfig {
            mode: m.into(),
            ..Default::default()
        }
    }

    #[test]
    fn the_literal_round_trips_in_the_sdks_format() {
        let config = SfuConfig {
            mode: "cascaded".into(),
            sfu_peers: vec!["did:a".into(), "did:b c".into()],
            ..Default::default()
        };
        let literal = to_literal(&config).unwrap();
        assert!(literal.starts_with("literal:json:%7B"), "{literal}");
        assert_eq!(from_literal(&literal).unwrap().sfu_peers, config.sfu_peers);
        assert!(from_literal("literal:string:x").is_none());
    }

    #[test]
    fn only_the_creators_newest_valid_config_counts() {
        let links = [
            link(
                "did:creator",
                "2026-10-01T00:00:00.000Z",
                &mode("designated"),
                true,
            ),
            link(
                "did:creator",
                "2026-10-02T00:00:00.000Z",
                &mode("cascaded"),
                true,
            ),
            // Newer, but not the creator's.
            link(
                "did:member",
                "2026-10-03T00:00:00.000Z",
                &mode("gateway"),
                true,
            ),
            // Newer and claims the creator, but its signature does not hold.
            link(
                "did:creator",
                "2026-10-04T00:00:00.000Z",
                &mode("mesh"),
                false,
            ),
        ];
        assert_eq!(
            creators_latest(&links, "did:creator").unwrap().mode,
            "cascaded"
        );
        assert!(creators_latest(&links, "did:nobody").is_none());
    }

    #[test]
    fn a_stored_mesh_limit_outside_the_accepted_range_is_clamped() {
        let limit = |n: u32| SfuConfig {
            max_mesh_participants: n,
            ..Default::default()
        };
        let read = |n: u32| {
            let links = [link(
                "did:creator",
                "2026-10-01T00:00:00.000Z",
                &limit(n),
                true,
            )];
            creators_latest(&links, "did:creator")
                .unwrap()
                .max_mesh_participants
        };
        assert_eq!(read(0), 2);
        assert_eq!(read(4_000_000_000), MAX_MESH_PARTICIPANTS);
        assert_eq!(read(8), 8);
    }

    /// The creator's write lands as a link in the neighbourhood's perspective,
    /// and every read takes it from there.
    #[tokio::test]
    async fn the_creators_config_is_stored_as_a_link_and_read_back() {
        use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
        use crate::types::DecoratedNeighbourhoodExpression;

        let (perspective, _shapes, context) = setup_perspective_no_llm(&[]).await;
        let creator = crate::agent::did();
        let url = format!("neighbourhood://sfu-config-test/{}", uuid::Uuid::new_v4());
        let uuid = {
            let mut handle = perspective.persisted.lock().await;
            handle.shared_url = Some(url.clone());
            handle.neighbourhood = Some(DecoratedNeighbourhoodExpression {
                author: creator.clone(),
                ..Default::default()
            });
            handle.uuid.clone()
        };
        crate::perspectives::register_perspective(uuid.clone(), perspective);

        assert_eq!(neighbourhood_creator(&url).await, Some(creator));
        assert!(stored_config(&url).await.is_none());
        assert_eq!(read_config(&url).await.mode, SfuConfig::default().mode);

        let config = SfuConfig {
            mode: "cascaded".into(),
            sfu_peers: vec!["did:node-1".into()],
            ..Default::default()
        };
        write_config(&url, &config, &context).await.unwrap();
        let stored = stored_config(&url).await.expect("config link");
        assert_eq!(stored.mode, "cascaded");
        assert_eq!(stored.sfu_peers, vec!["did:node-1".to_string()]);

        crate::perspectives::unregister_perspective(&uuid);
    }
}
