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
pub mod reachability;
pub mod relay;
pub mod room;
pub mod server;
pub mod turn;
pub mod types;

mod service;

pub use gossip::{
    signal::{SignalGossip, SignalSender},
    tcp::GossipPeer,
    tcp::TcpGossip,
    CascadeGossip, GossipTarget, NoopGossip,
};
pub use reachability::{is_private_ip, SfuReachability};
pub use service::{get_sfu_service, SfuService};
pub use types::{
    CallSessionInfo, IceServer, SfuCallRenegotiationOffer, SfuConfig, SfuMigrateEvent,
    SfuParticipantInfo, SfuPipeRenegotiationAnswer, SfuPipeRenegotiationOffer, SfuRoomInfo,
    TrackMapEntry,
};

/// Detect the default outbound IP address of this machine.
///
/// Creates a UDP socket and connects it to a public address (8.8.8.8:80).
/// The OS routing table selects the appropriate interface — no packet
/// leaves the machine.  Returns the local IP the OS chose, which on a
/// single-NIC server equals the publicly reachable address.
///
/// Falls back to `127.0.0.1` when detection fails (no network, no
/// default route, containerised environment with no outbound).
pub fn detect_outbound_ip() -> std::net::IpAddr {
    let fallback = std::net::IpAddr::V4(std::net::Ipv4Addr::LOCALHOST);
    let Ok(socket) = std::net::UdpSocket::bind("0.0.0.0:0") else {
        return fallback;
    };
    // connect() on a UDP socket sets the default destination without
    // sending anything.  The kernel selects the outbound interface.
    if socket.connect("8.8.8.8:80").is_err() {
        return fallback;
    }
    socket.local_addr().map(|a| a.ip()).unwrap_or(fallback)
}
