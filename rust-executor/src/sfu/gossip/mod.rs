//! Cluster gossip abstraction for cascade signalling.
//!
//! **Security boundary**: the TCP gossip port accepts JSON-line frames
//! from any connecting peer.  Deployments MUST restrict this port to
//! cluster-internal networks (VPN, loopback, firewall rule).  Exposing
//! it to untrusted networks enables denial-of-service via oversized
//! frames or malicious cascade signals.
//!
//! The SFU's cascade layer needs to exchange [`CascadeSignal`] messages
//! between SFU nodes — announce/leave for discovery + load updates, and
//! pipe-offer/pipe-answer for inter-node media bridges.
//!
//! How those messages travel is intentionally pluggable so production
//! deployments can wrap whatever the host application's signalling
//! layer is (Holochain neighbourhood signals for the AD4M case,
//! libp2p/gossipsub for a different runtime, etc.), and so test
//! harnesses can hand-roll an in-memory or TCP transport.
//!
//! The trait surface is deliberately small:
//!
//! - `send(target, signal)` — broadcast or point-to-point.
//! - `take_inbound()` — exactly one consumer pulls the mpsc receiver
//!   the transport feeds.
//!
//! The wind tunnel ships [`tcp::TcpGossip`] (this module's sibling)
//! for multi-process clusters on one host or across machines, and
//! [`noop::NoopGossip`] for single-node executors that don't want
//! cascade at all.  AD4M-side production will live in a higher layer
//! that wraps neighbourhood signals.

use async_trait::async_trait;
use tokio::sync::mpsc;

use super::cascade::CascadeSignal;

pub mod signal;
pub mod tcp;

/// Where to direct an outgoing signal.
#[derive(Debug, Clone)]
pub enum GossipTarget {
    /// Send to every peer the transport currently knows about.
    Broadcast,
    /// Send to one specific peer keyed by DID.  Drop silently if the
    /// transport has no route for that DID (the cascade announce flow
    /// is best-effort; pipe negotiation surfaces errors at a higher
    /// layer).
    PeerDid(String),
}

/// Pluggable cluster gossip transport.
#[async_trait]
pub trait CascadeGossip: Send + Sync {
    /// Publish a `CascadeSignal` to either every known peer or a
    /// single named peer.  Returns `Ok(())` once handed to the
    /// transport — *delivery* is best-effort, like UDP.
    async fn send(&self, target: GossipTarget, signal: CascadeSignal) -> Result<(), String>;

    /// Take the inbound channel.  The SFU calls this once during
    /// `SfuService::start`; subsequent calls return `None`.
    fn take_inbound(&self) -> Option<mpsc::Receiver<CascadeSignal>>;

    /// Local node identity that other peers should reach us by.
    fn local_did(&self) -> &str;

    /// Capacity hint published in every Announce.  Stored on the
    /// transport so callers don't have to re-pass it.
    fn max_participants_per_node(&self) -> u32;
}

/// A no-op gossip transport for single-node executors.  Send is a
/// success that goes nowhere; `take_inbound` returns `None` so the
/// SfuService doesn't spawn an inbound loop.  Useful as the default
/// when cascade is disabled.
pub struct NoopGossip {
    local_did: String,
    max_participants_per_node: u32,
}

impl NoopGossip {
    pub fn new(local_did: String, max_participants_per_node: u32) -> Self {
        Self {
            local_did,
            max_participants_per_node,
        }
    }
}

#[async_trait]
impl CascadeGossip for NoopGossip {
    async fn send(&self, _target: GossipTarget, _signal: CascadeSignal) -> Result<(), String> {
        Ok(())
    }
    fn take_inbound(&self) -> Option<mpsc::Receiver<CascadeSignal>> {
        None
    }
    fn local_did(&self) -> &str {
        &self.local_did
    }
    fn max_participants_per_node(&self) -> u32 {
        self.max_participants_per_node
    }
}
