//! Per-space configuration for the Holograph substrate.
//!
//! `SpaceConfig` is the single knob passed into `HolographSpace` construction.
//! v1 always uses `SpaceConfig::full_replication_single_doc()`; v1.5 will
//! pass shard-aware configs without any change to the substrate code that
//! consumes this struct.
//!
//! Sharding-ready commitments honored here (SPIKE §1.5):
//!
//! 1. Arc policy is explicit, not hardcoded "yes."  v1 default is `Full`.
//! 2. Loc-fn policy is wired through; v1 default is `HashLoc` (K2's default).
//! 6. `HolographSpace` accepts a `SpaceConfig` (this struct) with arc policy
//!    + loc_fn + validation regime.

use kitsune2_api::DhtArc;
use serde::{Deserialize, Serialize};

/// How a space chooses its current storage arc.
///
/// v1 is always `Full` (every node holds everything). v1.5 will plug
/// `Sharded` configurations in; the arc value itself is then computed by
/// K2's arc-management code at runtime — the policy here is the input.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ArcPolicy {
    /// Full replication — `DhtArc::FULL`. v1 default.
    Full,
    /// Sharded — store only ops whose location falls within this arc.
    /// Reserved for v1.5; not exercised in v1.
    Sharded(DhtArc),
}

impl ArcPolicy {
    /// The target storage arc for this policy.
    ///
    /// Storage decisions in the OpStore consult this — they do NOT
    /// hardcode "yes." Even in v1 we go through this path so the v1.5
    /// `Sharded` variant lights up without touching the OpStore.
    pub fn target_arc(&self) -> DhtArc {
        match self {
            ArcPolicy::Full => DhtArc::FULL,
            ArcPolicy::Sharded(arc) => *arc,
        }
    }
}

/// How an op's K2 location is derived.
///
/// v1 keeps K2's default xor-based loc (`HashLoc`). v1.5 will register a
/// `DocIdLoc` callback that routes ops by `doc_id` into hot sectors. The
/// callback registration itself lives in the host (`OpId::set_loc_callback`
/// is a process-global one-shot) — this enum just records the policy
/// declared by the space config.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum LocFnPolicy {
    /// Default xor-based location derivation. v1 default.
    HashLoc,
    /// Route by `doc_id` field on the envelope. Reserved for v1.5.
    DocIdLoc,
}

/// The validation pipeline an incoming op must pass before being stored.
///
/// v1 only does signature + parent-presence; richer regimes are deferred
/// to `SHARDED_MODE.md` (see SPIKE §1.4) and v1.5.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ValidationRegime {
    /// Verify the envelope signature and the presence of all declared
    /// parents in the local op store. v1 default.
    SignatureAndParentsOnly,
}

/// Per-space configuration for a Holograph space.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SpaceConfig {
    pub arc_policy: ArcPolicy,
    pub loc_fn_policy: LocFnPolicy,
    pub validation_regime: ValidationRegime,
    /// Override for K2's gossip-initiation cadence. None means use K2's
    /// default (~120s). v1 spike uses 5_000ms — see SPIKE §1.1.
    pub gossip_initiate_interval_ms: Option<u32>,
}

impl SpaceConfig {
    /// The v1 default — full arc, single-doc, signature+parent validation,
    /// 5s gossip cadence.
    pub fn full_replication_single_doc() -> Self {
        Self {
            arc_policy: ArcPolicy::Full,
            loc_fn_policy: LocFnPolicy::HashLoc,
            validation_regime: ValidationRegime::SignatureAndParentsOnly,
            gossip_initiate_interval_ms: Some(5_000),
        }
    }

    /// The current target storage arc, derived from `arc_policy`.
    pub fn target_arc(&self) -> DhtArc {
        self.arc_policy.target_arc()
    }
}

impl Default for SpaceConfig {
    fn default() -> Self {
        Self::full_replication_single_doc()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn full_replication_single_doc_is_full_arc() {
        let cfg = SpaceConfig::full_replication_single_doc();
        assert_eq!(cfg.target_arc(), DhtArc::FULL);
        assert_eq!(cfg.loc_fn_policy, LocFnPolicy::HashLoc);
        assert_eq!(
            cfg.validation_regime,
            ValidationRegime::SignatureAndParentsOnly
        );
        assert_eq!(cfg.gossip_initiate_interval_ms, Some(5_000));
    }

    #[test]
    fn default_matches_full_replication_single_doc() {
        assert_eq!(
            SpaceConfig::default(),
            SpaceConfig::full_replication_single_doc()
        );
    }

    #[test]
    fn sharded_policy_round_trips_arc() {
        let arc = DhtArc::Arc(100, 200);
        let cfg = SpaceConfig {
            arc_policy: ArcPolicy::Sharded(arc),
            ..SpaceConfig::full_replication_single_doc()
        };
        assert_eq!(cfg.target_arc(), arc);
    }
}
