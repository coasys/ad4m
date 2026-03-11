//! Generic Iroh-to-ICE bridge for AD4M WebRTC connections.
//!
//! Provides ICE candidates derived from the Holochain/Iroh transport layer,
//! replacing the need for external STUN/TURN servers.
//!
//! This module is transport-agnostic — it works with socket addresses from
//! any source. It has no knowledge of whether the consumer is a P2P mesh
//! call, an SFU connection, or a pipe transport.
//!
//! See: <https://github.com/coasys/ad4m/issues/719>

use std::net::SocketAddr;

/// A WebRTC ICE candidate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IceCandidate {
    /// The ICE candidate string in SDP format (RFC 8445).
    /// e.g. "candidate:1 1 udp 2130706431 192.168.1.5 12345 typ host"
    pub candidate: String,
    /// The candidate type.
    pub typ: IceCandidateType,
    /// The underlying socket address.
    pub addr: SocketAddr,
}

/// ICE candidate types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IceCandidateType {
    /// Local/LAN address.
    Host,
    /// Server-reflexive (public/NAT-mapped) address, replaces STUN.
    ServerReflexive,
}

/// Generic Iroh-to-ICE bridge.
///
/// Takes socket addresses from the Holochain/kitsune2 transport layer
/// and converts them to WebRTC ICE candidate format.
pub struct IrohIce {
    /// Cached local addresses.
    local_addrs: Vec<SocketAddr>,
}

impl IrohIce {
    /// Create a new IrohIce instance with the given local socket addresses.
    ///
    /// In practice, these come from `conductor.holochain_p2p().local_socket_addrs().await`.
    pub fn new(local_addrs: Vec<SocketAddr>) -> Self {
        Self { local_addrs }
    }

    /// Update the local addresses (e.g. when the transport discovers new addresses).
    pub fn update_addrs(&mut self, addrs: Vec<SocketAddr>) {
        self.local_addrs = addrs;
    }

    /// Get ICE candidates for the local endpoint.
    ///
    /// Classifies addresses as:
    /// - `host` — private/LAN addresses (RFC 1918, link-local, loopback)
    /// - `srflx` — public/server-reflexive addresses (everything else)
    pub fn local_candidates(&self) -> Vec<IceCandidate> {
        self.local_addrs
            .iter()
            .enumerate()
            .map(|(i, addr)| {
                let typ = classify_addr(addr);
                let priority = compute_priority(&typ, i);
                IceCandidate {
                    candidate: format_ice_candidate(i + 1, addr, &typ, priority),
                    typ,
                    addr: *addr,
                }
            })
            .collect()
    }

    /// Get ICE candidates formatted for use in an SDP answer/offer.
    ///
    /// Returns the candidate strings ready for `RTCPeerConnection.addIceCandidate()`.
    pub fn candidate_strings(&self) -> Vec<String> {
        self.local_candidates()
            .into_iter()
            .map(|c| c.candidate)
            .collect()
    }

    /// Check if any public (server-reflexive) addresses are available.
    /// If false, connections will only work on the local network.
    pub fn has_public_addr(&self) -> bool {
        self.local_addrs.iter().any(|a| !is_private(a))
    }
}

/// Classify a socket address as host (private) or server-reflexive (public).
fn classify_addr(addr: &SocketAddr) -> IceCandidateType {
    if is_private(addr) {
        IceCandidateType::Host
    } else {
        IceCandidateType::ServerReflexive
    }
}

/// Check if a socket address is private/LAN.
fn is_private(addr: &SocketAddr) -> bool {
    match addr {
        SocketAddr::V4(v4) => {
            let ip = v4.ip();
            ip.is_private()
                || ip.is_loopback()
                || ip.is_link_local()
                || ip.is_unspecified()
        }
        SocketAddr::V6(v6) => {
            let ip = v6.ip();
            ip.is_loopback() || ip.is_unspecified()
            // Note: is_unique_local() (fc00::/7) is nightly-only,
            // so we check the first byte manually.
                || {
                    let octets = ip.octets();
                    octets[0] == 0xfc || octets[0] == 0xfd // ULA
                    || (octets[0] == 0xfe && (octets[1] & 0xc0) == 0x80) // link-local
                }
        }
    }
}

/// Compute ICE candidate priority per RFC 8445 Section 5.1.2.1.
///
/// priority = (2^24) * type_preference + (2^8) * local_preference + (256 - component_id)
///
/// type_preference: host=126, srflx=100
/// component_id: always 1 (RTP)
fn compute_priority(typ: &IceCandidateType, index: usize) -> u32 {
    let type_pref: u32 = match typ {
        IceCandidateType::Host => 126,
        IceCandidateType::ServerReflexive => 100,
    };
    // local_preference: prefer lower-index addresses (first discovered)
    let local_pref: u32 = 65535u32.saturating_sub(index as u32);
    let component_id: u32 = 1;

    (type_pref << 24) + (local_pref << 8) + (256 - component_id)
}

/// Format an ICE candidate string per RFC 8445.
///
/// Format: candidate:{foundation} {component} udp {priority} {ip} {port} typ {type}
fn format_ice_candidate(
    foundation: usize,
    addr: &SocketAddr,
    typ: &IceCandidateType,
    priority: u32,
) -> String {
    let type_str = match typ {
        IceCandidateType::Host => "host",
        IceCandidateType::ServerReflexive => "srflx",
    };
    format!(
        "candidate:{foundation} 1 udp {priority} {ip} {port} typ {type_str}",
        foundation = foundation,
        priority = priority,
        ip = addr.ip(),
        port = addr.port(),
        type_str = type_str,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::{Ipv4Addr, Ipv6Addr};

    #[test]
    fn test_classify_private_v4() {
        let cases = vec![
            ("192.168.1.5:1234", IceCandidateType::Host),
            ("10.0.0.1:5000", IceCandidateType::Host),
            ("172.16.0.1:8080", IceCandidateType::Host),
            ("127.0.0.1:3000", IceCandidateType::Host),
            ("169.254.1.1:4000", IceCandidateType::Host),
            ("8.8.8.8:443", IceCandidateType::ServerReflexive),
            ("203.0.113.5:12345", IceCandidateType::ServerReflexive),
        ];
        for (addr_str, expected) in cases {
            let addr: SocketAddr = addr_str.parse().unwrap();
            assert_eq!(
                classify_addr(&addr),
                expected,
                "wrong classification for {addr_str}"
            );
        }
    }

    #[test]
    fn test_classify_private_v6() {
        let loopback: SocketAddr = "[::1]:1234".parse().unwrap();
        assert_eq!(classify_addr(&loopback), IceCandidateType::Host);

        let ula: SocketAddr = "[fd12::1]:1234".parse().unwrap();
        assert_eq!(classify_addr(&ula), IceCandidateType::Host);

        let public: SocketAddr = "[2001:db8::1]:1234".parse().unwrap();
        assert_eq!(classify_addr(&public), IceCandidateType::ServerReflexive);
    }

    #[test]
    fn test_format_ice_candidate() {
        let addr: SocketAddr = "192.168.1.5:12345".parse().unwrap();
        let candidate = format_ice_candidate(1, &addr, &IceCandidateType::Host, 2130706431);
        assert_eq!(
            candidate,
            "candidate:1 1 udp 2130706431 192.168.1.5 12345 typ host"
        );
    }

    #[test]
    fn test_format_srflx_candidate() {
        let addr: SocketAddr = "203.0.113.5:54321".parse().unwrap();
        let candidate =
            format_ice_candidate(2, &addr, &IceCandidateType::ServerReflexive, 1694498815);
        assert_eq!(
            candidate,
            "candidate:2 1 udp 1694498815 203.0.113.5 54321 typ srflx"
        );
    }

    #[test]
    fn test_priority_ordering() {
        // Host candidates should have higher priority than srflx
        let host_priority = compute_priority(&IceCandidateType::Host, 0);
        let srflx_priority = compute_priority(&IceCandidateType::ServerReflexive, 0);
        assert!(
            host_priority > srflx_priority,
            "host ({host_priority}) should have higher priority than srflx ({srflx_priority})"
        );

        // Earlier candidates should have higher priority than later ones
        let first = compute_priority(&IceCandidateType::Host, 0);
        let second = compute_priority(&IceCandidateType::Host, 1);
        assert!(
            first > second,
            "first ({first}) should have higher priority than second ({second})"
        );
    }

    #[test]
    fn test_iroh_ice_local_candidates() {
        let addrs = vec![
            "192.168.1.5:12345".parse().unwrap(),
            "203.0.113.5:54321".parse().unwrap(),
        ];
        let ice = IrohIce::new(addrs);
        let candidates = ice.local_candidates();

        assert_eq!(candidates.len(), 2);
        assert_eq!(candidates[0].typ, IceCandidateType::Host);
        assert_eq!(candidates[1].typ, IceCandidateType::ServerReflexive);
        assert!(candidates[0].candidate.contains("typ host"));
        assert!(candidates[1].candidate.contains("typ srflx"));
    }

    #[test]
    fn test_has_public_addr() {
        let private_only = IrohIce::new(vec!["192.168.1.5:1234".parse().unwrap()]);
        assert!(!private_only.has_public_addr());

        let with_public = IrohIce::new(vec![
            "192.168.1.5:1234".parse().unwrap(),
            "8.8.8.8:5678".parse().unwrap(),
        ]);
        assert!(with_public.has_public_addr());
    }

    #[test]
    fn test_empty_addrs() {
        let ice = IrohIce::new(vec![]);
        assert!(ice.local_candidates().is_empty());
        assert!(ice.candidate_strings().is_empty());
        assert!(!ice.has_public_addr());
    }

    #[test]
    fn test_update_addrs() {
        let mut ice = IrohIce::new(vec![]);
        assert!(ice.local_candidates().is_empty());

        ice.update_addrs(vec!["10.0.0.1:5000".parse().unwrap()]);
        assert_eq!(ice.local_candidates().len(), 1);
    }
}
