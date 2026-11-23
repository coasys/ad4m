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

use deno_core::error::AnyError;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::sync::Mutex;
// tokio's clock, so a paused test clock ages the cache.
use tokio::time::Instant;

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

/// How long a resolved policy is trusted before the membership check runs
/// again. The check waits on the link language — the same serial queue the
/// 2026-11-19 call's `getConfig`s died behind — so a member that joined or
/// left can take this long to be noticed.
const POLICY_CACHE_TTL: std::time::Duration = std::time::Duration::from_secs(60);

/// How long `read_policy` waits on the membership check once the cached
/// policy has expired. Past it the check counts as failed, and the last
/// policy answers: an expired entry must not put `getConfig` back behind a
/// stalled language.
const MEMBERSHIP_CHECK_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(2);

/// The last policy `read_policy` resolved for a neighbourhood, and — when
/// it came from a membership check rather than a stored config link —
/// whether the check found every member hosted here. `send_broadcast`
/// reads [`every_member_local`] as its cheap "no remote members" signal.
struct PolicyCacheEntry {
    policy: CallPolicy,
    at: Instant,
    every_member_local: Option<bool>,
    /// Set by [`saw_member`]: a member this executor does not host showed
    /// up, so the next `read_policy` checks again whatever the entry's age.
    stale: bool,
}

/// The members [`saw_member`] has seen per neighbourhood, and whether this
/// executor hosts each. A member stays one, so a sighting is kept: a
/// membership check that was already running when it came in reads it
/// when it returns, instead of caching an "every member local" it no
/// longer is.
#[derive(Default)]
struct Sightings {
    seen: HashSet<String>,
    remote: bool,
}

lazy_static! {
    static ref POLICY_CACHE: Mutex<HashMap<String, PolicyCacheEntry>> = Mutex::new(HashMap::new());
    static ref SIGHTINGS: Mutex<HashMap<String, Sightings>> = Mutex::new(HashMap::new());
}

/// The resolved policy when it is less than [`POLICY_CACHE_TTL`] old.
fn cached_policy(neighbourhood_url: &str) -> Option<CallPolicy> {
    POLICY_CACHE
        .lock()
        .expect("policy cache lock")
        .get(neighbourhood_url)
        .filter(|entry| !entry.stale && entry.at.elapsed() < POLICY_CACHE_TTL)
        .map(|entry| entry.policy.clone())
}

/// The resolved policy however old it is: what a failed membership check
/// falls back to, which must never become "mesh" by default.
fn last_policy(neighbourhood_url: &str) -> Option<CallPolicy> {
    POLICY_CACHE
        .lock()
        .expect("policy cache lock")
        .get(neighbourhood_url)
        .map(|entry| entry.policy.clone())
}

fn cache_policy(neighbourhood_url: &str, policy: &CallPolicy, every_member_local: Option<bool>) {
    POLICY_CACHE.lock().expect("policy cache lock").insert(
        neighbourhood_url.to_string(),
        PolicyCacheEntry {
            policy: policy.clone(),
            at: Instant::now(),
            every_member_local,
            stale: false,
        },
    );
}

/// `true` when the last membership check a `read_policy` ran found every
/// member hosted on this executor, while that verdict is still fresh:
/// remote delivery could then reach nobody the local delivery did not, so
/// `send_broadcast` skips the link-language hop.
pub(crate) fn every_member_local(neighbourhood_url: &str) -> bool {
    POLICY_CACHE
        .lock()
        .expect("policy cache lock")
        .get(neighbourhood_url)
        .is_some_and(|entry| {
            entry.every_member_local == Some(true) && entry.at.elapsed() < POLICY_CACHE_TTL
        })
}

/// `did` is a member of the neighbourhood (it signalled there, or the link
/// language listed it online). When this executor does not host it, an
/// "every member local" verdict is wrong: it ends at once, so the next
/// broadcast goes out remotely instead of being skipped for the rest of
/// [`POLICY_CACHE_TTL`], and the next policy read checks the members again
/// (a failed check answers mesh). The sighting is also kept for a check
/// that is running now ([`Sightings`]).
/// Costs a map lookup for a member seen before.
pub(crate) fn saw_member(neighbourhood_url: &str, did: &str) {
    {
        let mut sightings = SIGHTINGS.lock().expect("sightings lock");
        let known = sightings.entry(neighbourhood_url.to_string()).or_default();
        if !known.seen.insert(did.to_string()) {
            return;
        }
        if hosted_dids().iter().any(|h| h == did) {
            return;
        }
        known.remote = true;
    }
    if let Some(entry) = POLICY_CACHE
        .lock()
        .expect("policy cache lock")
        .get_mut(neighbourhood_url)
        .filter(|entry| entry.every_member_local == Some(true))
    {
        // Only a computed policy has a verdict, and "every member local"
        // computed "designated": with a member elsewhere it is mesh, also
        // for a check that fails before it can say so.
        entry.policy.mode = "mesh".to_string();
        entry.every_member_local = Some(false);
        entry.stale = true;
    }
}

/// Whether [`saw_member`] has seen a member this executor does not host.
fn saw_remote_member(neighbourhood_url: &str) -> bool {
    SIGHTINGS
        .lock()
        .expect("sightings lock")
        .get(neighbourhood_url)
        .is_some_and(|known| known.remote)
}

/// [`stored_policy`], or the policy of a neighbourhood with none: this
/// executor's SFU (`designated`) when it hosts every member, so all of them
/// reach the same room, and mesh otherwise. SFUs on different executors meet
/// only through a cascade an operator sets up, so members on two executors
/// would each sit alone in their own executor's room.
///
/// The membership check waits on the link language's serial runtime, so the
/// resolved policy is cached for [`POLICY_CACHE_TTL`]. A check that fails
/// answers the last resolved policy, however old, and an error when there
/// is none — never a silent "mesh" a different neighbour would compute
/// differently (2026-11-19: a blocked language turned a designated-policy
/// call to mesh mid-call).
pub async fn read_policy(neighbourhood_url: &str) -> Result<CallPolicy, AnyError> {
    if let Some(policy) = stored_policy(neighbourhood_url).await {
        cache_policy(neighbourhood_url, &policy, None);
        return Ok(policy);
    }
    let perspective = match neighbourhood_perspective(neighbourhood_url).await {
        Some(perspective) => perspective,
        // Not joined here: this executor cannot host the neighbourhood's
        // members, so this is an honest "mesh", not a failed check.
        None => {
            return Ok(CallPolicy {
                mode: "mesh".to_string(),
                ..Default::default()
            })
        }
    };
    if let Some(policy) = cached_policy(neighbourhood_url) {
        return Ok(policy);
    }
    let checked = tokio::time::timeout(MEMBERSHIP_CHECK_TIMEOUT, hosts_every_member(&perspective))
        .await
        .unwrap_or_else(|_| {
            Err(deno_core::anyhow::anyhow!(
                "the membership check did not answer within {MEMBERSHIP_CHECK_TIMEOUT:?}"
            ))
        });
    match checked {
        Ok(hosted) => {
            // Read after the check: a remote member that signalled while it
            // ran is one its member list may not have had yet.
            let hosted = hosted && !saw_remote_member(neighbourhood_url);
            let policy = CallPolicy {
                mode: if hosted { "designated" } else { "mesh" }.to_string(),
                ..Default::default()
            };
            cache_policy(neighbourhood_url, &policy, Some(hosted));
            Ok(policy)
        }
        Err(e) => {
            if let Some(policy) = last_policy(neighbourhood_url) {
                log::warn!(
                    "SFU config: the membership check for {neighbourhood_url} failed ({e}); answering the last policy {policy:?}"
                );
                Ok(policy)
            } else if !perspective.has_link_language().await {
                // No link language is no remote members to check: the same
                // "mesh" a perspective without one has always got, not a
                // check turned into one.
                Ok(CallPolicy {
                    mode: "mesh".to_string(),
                    ..Default::default()
                })
            } else {
                Err(e)
            }
        }
    }
}

/// Whether every member of the neighbourhood is hosted on this executor, so
/// all of them can join its SFU. Members are the DIDs the neighbourhood's
/// link language knows plus its local owners (`PerspectiveInstance::others`);
/// hosted are the main agent and the managed users. A neighbourhood with no
/// known member is not.
pub async fn hosts_every_member(perspective: &PerspectiveInstance) -> Result<bool, AnyError> {
    let members = perspective.others().await?;
    Ok(all_hosted(&members, &hosted_dids()))
}

/// The agents this executor hosts: its users and the main agent.
fn hosted_dids() -> Vec<String> {
    let mut hosted: Vec<String> = crate::db_backend::db_backend()
        .list_users()
        .unwrap_or_default()
        .into_iter()
        .map(|user| user.did)
        .collect();
    hosted.extend(did_for_context(&AgentContext::main_agent()).ok());
    hosted
}

fn all_hosted(members: &[String], hosted: &[String]) -> bool {
    !members.is_empty() && members.iter().all(|did| hosted.contains(did))
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
                anchor: None,
            },
            status: None,
            verdict: None,
            person: None,
            by_bot: None,
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
    fn every_member_is_hosted_only_when_none_is_elsewhere() {
        let s = |v: &[&str]| v.iter().map(|d| d.to_string()).collect::<Vec<_>>();
        let hosted = s(&["did:main", "did:alice", "did:bob"]);
        assert!(all_hosted(&s(&["did:alice", "did:bob"]), &hosted));
        assert!(!all_hosted(&s(&["did:alice", "did:carol"]), &hosted));
        assert!(!all_hosted(&[], &hosted));
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
        // No policy and no remote members to check (no link language here):
        // mesh, as a space with a member on another executor.
        assert_eq!(read_policy(&url).await.unwrap(), policy("mesh", 4));

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
