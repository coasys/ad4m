//! A neighbourhood's call policy, stored as a link in the neighbourhood.
//!
//! The policy is the part of the call config every member's client must
//! agree on: `mode` and `maxMeshParticipants`. It is a link
//! `<neighbourhood url> ad4m://sfu_config <literal:json:…>`, shared like any
//! other link, so every member's executor reads the same policy and it
//! survives restarts. Any member can add links, so only one author counts:
//! the neighbourhood's creator — the author of its neighbourhood expression.
//! Of the creator's validly signed config links the newest wins; anyone
//! else's are ignored. A new write removes the creator's earlier ones.
//!
//! Nothing else of `SfuConfig` is stored here. Which nodes run an SFU and
//! how big they are is executor config (`SfuService::get_config` adds it),
//! and ICE credentials never go into a link every member syncs. A link an
//! older build wrote with those fields is read for its policy only.

use serde::{Deserialize, Serialize};

use crate::agent::{did_for_context, AgentContext};
use crate::perspectives::all_perspectives;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::{DecoratedLinkExpression, Link, LinkExpression, LinkQuery, LinkStatus};

use super::types::SfuConfig;
use super::MAX_MESH_PARTICIPANTS;

/// What the config link holds.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CallPolicy {
    pub mode: String,
    pub max_mesh_participants: u32,
}

impl Default for CallPolicy {
    fn default() -> Self {
        let defaults = SfuConfig::default();
        CallPolicy {
            mode: defaults.mode,
            max_mesh_participants: defaults.max_mesh_participants,
        }
    }
}

impl CallPolicy {
    /// The policy as a config with every executor-side field unset.
    pub fn into_config(self) -> SfuConfig {
        SfuConfig {
            mode: self.mode,
            max_mesh_participants: self.max_mesh_participants,
            ..Default::default()
        }
    }
}

/// Predicate of the config link. Its source is the neighbourhood URL.
pub const SFU_CONFIG_PREDICATE: &str = "ad4m://sfu_config";

/// This node's perspective for `neighbourhood_url`, if it has joined it.
pub(crate) async fn neighbourhood_perspective(
    neighbourhood_url: &str,
) -> Option<PerspectiveInstance> {
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
fn to_literal(policy: &CallPolicy) -> Result<String, String> {
    let json = serde_json::to_string(policy).map_err(|e| e.to_string())?;
    Ok(format!("literal:json:{}", urlencoding::encode(&json)))
}

/// The policy in a config link. Any other field is ignored; a missing one
/// takes its default.
fn from_literal(target: &str) -> Option<CallPolicy> {
    let encoded = target.strip_prefix("literal:json:")?;
    let config: SfuConfig = serde_json::from_str(&urlencoding::decode(encoded).ok()?).ok()?;
    Some(CallPolicy {
        mode: config.mode,
        max_mesh_participants: config.max_mesh_participants,
    })
}

/// The policy that counts among `links`: the newest validly signed one by
/// `creator` that parses. Timestamps are RFC 3339 in UTC, so they order as
/// strings. `maxMeshParticipants` is clamped to the range `setConfig`
/// accepts: a link written past that check (by hand, or by another build)
/// must not set a room's capacity.
fn creators_latest(links: &[DecoratedLinkExpression], creator: &str) -> Option<CallPolicy> {
    links
        .iter()
        .filter(|l| l.author == creator && l.proof.valid == Some(true))
        .filter_map(|l| Some((l.timestamp.as_str(), from_literal(&l.data.target)?)))
        .max_by(|a, b| a.0.cmp(b.0))
        .map(|(_, mut policy)| {
            policy.max_mesh_participants =
                policy.max_mesh_participants.clamp(2, MAX_MESH_PARTICIPANTS);
            policy
        })
}

fn config_links_query(neighbourhood_url: &str) -> LinkQuery {
    LinkQuery {
        source: Some(neighbourhood_url.to_string()),
        predicate: Some(SFU_CONFIG_PREDICATE.to_string()),
        ..Default::default()
    }
}

/// The neighbourhood's call policy, or `None` when it has none (not joined
/// here, no creator, or no config link from the creator yet).
pub async fn stored_policy(neighbourhood_url: &str) -> Option<CallPolicy> {
    let perspective = neighbourhood_perspective(neighbourhood_url).await?;
    let creator = {
        let handle = perspective.persisted.lock().await;
        handle.neighbourhood.as_ref()?.author.clone()
    };
    match perspective
        .get_links(&config_links_query(neighbourhood_url))
        .await
    {
        Ok(links) => creators_latest(&links, &creator),
        Err(e) => {
            log::warn!("SFU config: reading {} failed: {}", neighbourhood_url, e);
            None
        }
    }
}

/// [`stored_policy`], or the default policy.
pub async fn read_policy(neighbourhood_url: &str) -> CallPolicy {
    stored_policy(neighbourhood_url).await.unwrap_or_default()
}

/// Store `policy` as a shared link in the neighbourhood, signed as `context`,
/// and remove the config links `context` wrote before, so members do not
/// keep syncing every policy ever set. The caller checks that `context` is
/// the creator: a link from anyone else would be stored and then ignored by
/// every reader.
pub async fn write_policy(
    neighbourhood_url: &str,
    policy: &CallPolicy,
    context: &AgentContext,
) -> Result<(), String> {
    let mut perspective = neighbourhood_perspective(neighbourhood_url)
        .await
        .ok_or_else(|| "This node has not joined that neighbourhood".to_string())?;
    let author = did_for_context(context).map_err(|e| e.to_string())?;
    let earlier: Vec<LinkExpression> = perspective
        .get_links(&config_links_query(neighbourhood_url))
        .await
        .map_err(|e| format!("Reading the call config failed: {}", e))?
        .into_iter()
        .filter(|l| l.author == author)
        .map(LinkExpression::from)
        .collect();
    let link = Link {
        source: neighbourhood_url.to_string(),
        predicate: Some(SFU_CONFIG_PREDICATE.to_string()),
        target: to_literal(policy)?,
    };
    perspective
        .add_link(link, LinkStatus::Shared, None, context)
        .await
        .map_err(|e| format!("Storing the call config failed: {}", e))?;
    if !earlier.is_empty() {
        perspective
            .remove_links(earlier, None)
            .await
            .map_err(|e| format!("Removing the earlier call config failed: {}", e))?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::DecoratedExpressionProof;

    fn link(author: &str, timestamp: &str, target: String, valid: bool) -> DecoratedLinkExpression {
        DecoratedLinkExpression {
            author: author.into(),
            timestamp: timestamp.into(),
            data: Link {
                source: "neighbourhood://n".into(),
                predicate: Some(SFU_CONFIG_PREDICATE.into()),
                target,
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

    fn policy(mode: &str, max_mesh_participants: u32) -> CallPolicy {
        CallPolicy {
            mode: mode.into(),
            max_mesh_participants,
        }
    }

    fn mode(m: &str) -> String {
        to_literal(&policy(m, 4)).unwrap()
    }

    #[test]
    fn the_literal_round_trips_in_the_sdks_format() {
        let literal = to_literal(&policy("cascaded", 6)).unwrap();
        assert!(literal.starts_with("literal:json:%7B"), "{literal}");
        assert_eq!(from_literal(&literal), Some(policy("cascaded", 6)));
        assert!(from_literal("literal:string:x").is_none());
    }

    /// A link an older build wrote carries topology and ICE credentials too.
    /// Only the policy is read from it; the rest never reaches a config.
    #[test]
    fn a_link_with_executor_fields_yields_only_its_policy() {
        let old = serde_json::json!({
            "mode": "designated",
            "maxMeshParticipants": 5,
            "designatedPeer": "did:node",
            "sfuPeers": ["did:a"],
            "preferredSfuDid": "did:a",
            "maxParticipantsPerNode": 9,
            "iceServers": [{"urls": ["turn:t"], "username": "u", "credential": "c"}],
        });
        let target = format!("literal:json:{}", urlencoding::encode(&old.to_string()));
        let read = from_literal(&target).unwrap();
        assert_eq!(read, policy("designated", 5));
        let config = read.into_config();
        assert!(config.designated_peer.is_none());
        assert!(config.sfu_peers.is_empty());
        assert!(config.preferred_sfu_did.is_none());
        assert!(config.max_participants_per_node.is_none());
        assert!(config.ice_servers.is_empty());
    }

    #[test]
    fn only_the_creators_newest_valid_config_counts() {
        let links = [
            link(
                "did:creator",
                "2026-10-01T00:00:00.000Z",
                mode("designated"),
                true,
            ),
            link(
                "did:creator",
                "2026-10-02T00:00:00.000Z",
                mode("cascaded"),
                true,
            ),
            // Newer, but not the creator's.
            link(
                "did:member",
                "2026-10-03T00:00:00.000Z",
                mode("gateway"),
                true,
            ),
            // Newer and claims the creator, but its signature does not hold.
            link(
                "did:creator",
                "2026-10-04T00:00:00.000Z",
                mode("mesh"),
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
        let read = |n: u32| {
            let target = to_literal(&policy("mesh", n)).unwrap();
            let links = [link(
                "did:creator",
                "2026-10-01T00:00:00.000Z",
                target,
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
    /// every read takes it from there, and a new write replaces the old link.
    #[tokio::test]
    async fn the_creators_policy_is_one_link_replaced_on_each_write() {
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
        crate::perspectives::register_perspective(uuid.clone(), perspective.clone());

        assert_eq!(neighbourhood_creator(&url).await, Some(creator));
        assert!(stored_policy(&url).await.is_none());
        assert_eq!(read_policy(&url).await, CallPolicy::default());

        write_policy(&url, &policy("cascaded", 6), &context)
            .await
            .unwrap();
        assert_eq!(stored_policy(&url).await, Some(policy("cascaded", 6)));

        write_policy(&url, &policy("mesh", 3), &context)
            .await
            .unwrap();
        assert_eq!(stored_policy(&url).await, Some(policy("mesh", 3)));
        let links = perspective
            .get_links(&config_links_query(&url))
            .await
            .unwrap();
        assert_eq!(links.len(), 1, "the earlier config link was not removed");

        crate::perspectives::unregister_perspective(&uuid);
    }
}
