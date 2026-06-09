//! SFU (Selective Forwarding Unit) service for the AD4M executor.
//!
//! Embeds a str0m-based WebRTC SFU as a built-in executor service,
//! following the same pattern as the Holochain conductor, Prolog, and SurrealDB services.
//!
//! The SFU receives each participant's media stream once and selectively forwards
//! it to all other participants in the room, reducing per-peer upload from O(N) to O(1).
//!
//! Cascaded multi-node SFU is supported via [`cascade`]: SFU nodes form a
//! cluster, announce their capacity, and redirect new joins to the
//! least-loaded peer.  Inter-node media flows over str0m-to-str0m pipe
//! transports.

pub mod cascade;
pub mod gossip;
pub mod relay;
pub mod room;
pub mod server;
pub mod types;

mod service;

pub use gossip::{tcp::GossipPeer, tcp::TcpGossip, CascadeGossip, GossipTarget, NoopGossip};
pub use service::{get_sfu_service, SfuService};
pub use types::{CallSessionInfo, SfuConfig, SfuParticipantInfo, SfuRoomInfo};
