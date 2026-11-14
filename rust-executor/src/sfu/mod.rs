//! SFU (Selective Forwarding Unit) service for the AD4M executor.
//!
//! Embeds a str0m-based WebRTC SFU as a built-in executor service,
//! following the same pattern as the Holochain conductor, Prolog, and SurrealDB services.
//!
//! The SFU receives each participant's media stream once and selectively forwards
//! it to all other participants in the room, reducing per-peer upload from O(N) to O(1).
//!
//! Cascaded multi-node SFU is supported via [`cascade`]: SFU nodes form a
//! cluster and announce their capacity, and inter-node media flows over
//! str0m-to-str0m pipe transports. A caller that can reach other nodes
//! (`acceptRedirect`) may be redirected to the least-loaded peer; an SDK
//! client reaches only its own executor and is always served there.

pub mod cascade;
pub mod config_store;
pub mod gossip;
pub mod hosts;
pub(crate) mod pipe_bus;
pub mod reachability;
pub mod relay;
pub mod room;
pub mod server;
pub mod types;

mod service;
mod simulcast;

// `SignalGossip` is not constructed yet: see `gossip::signal`.
pub use gossip::{
    signal::{SignalGossip, SignalSender},
    tcp::GossipPeer,
    tcp::TcpGossip,
    CascadeGossip, GossipTarget, NoopGossip,
};
pub use reachability::{is_private_ip, SfuReachability};
pub use service::{get_sfu_service, CallJoinError, SfuService, MAX_MESH_PARTICIPANTS};
pub use types::{
    CallSessionInfo, IceServer, SfuCallRenegotiationOffer, SfuConfig, SfuDataMessage,
    SfuMigrateEvent, SfuParticipantInfo, SfuPipeRenegotiationAnswer, SfuPipeRenegotiationOffer,
    SfuRoomInfo, SfuRoomRoster, SfuRosterParticipant, SfuSessionReplaced, TrackMapEntry,
};

/// Detect this machine's LAN address: the IPv4 address of the interface its
/// default route leaves from.
///
/// Creates a UDP socket and connects it to a public address (8.8.8.8:80).
/// The OS routing table selects the appropriate interface — no packet
/// leaves the machine.  Returns the local IP the OS chose, which on a
/// single-NIC server equals the publicly reachable address.
///
/// Fails, saying why, when there is none: no network, no default route, a
/// containerised environment with no outbound, or a route that leaves from
/// loopback.
pub fn detect_outbound_ip() -> Result<std::net::IpAddr, String> {
    let socket = std::net::UdpSocket::bind("0.0.0.0:0")
        .map_err(|e| format!("no IPv4 socket for the route lookup: {}", e))?;
    // connect() on a UDP socket sets the default destination without
    // sending anything.  The kernel selects the outbound interface.
    socket
        .connect("8.8.8.8:80")
        .map_err(|e| format!("no default route: {}", e))?;
    let ip = socket
        .local_addr()
        .map_err(|e| format!("the route lookup gave no address: {}", e))?
        .ip();
    if ip.is_unspecified() || ip.is_loopback() {
        return Err(format!("the default route leaves from {}", ip));
    }
    Ok(ip)
}
