//! SFU reachability detection.
//!
//! Determines whether the SFU server's bound address can accept
//! inbound connections from the public internet.  Runs once at
//! startup; the result decides whether this executor should
//! advertise SFU capability to neighbourhoods its agents join.
//!
//! Two layers:
//!
//! 1. **RFC 1918 check** — instant, no I/O.  Catches private,
//!    loopback, link-local, and CGNAT ranges.
//! 2. **STUN self-check** — sends a single STUN binding request to
//!    a public STUN server and compares the server-reflexive address
//!    to the bind address.  Confirms public reachability or detects
//!    a NAT that the RFC 1918 check cannot see (carrier-grade NAT
//!    with a public-looking IP, firewall rewriting, etc.).

use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};
use std::time::Duration;

use log::debug;
use tokio::net::UdpSocket;
use tokio::time::timeout;

/// Outcome of the SFU reachability probe.
#[derive(Debug, Clone)]
pub enum SfuReachability {
    /// Bind IP matches the STUN server-reflexive address — publicly
    /// reachable.  Remote clients can send UDP directly to this IP.
    Public { reflexive_addr: SocketAddr },

    /// Behind NAT — the STUN server sees a different IP than the
    /// one the executor bound to.  Remote clients cannot reach the
    /// SFU without port-forwarding or a relay.
    Nat {
        bind_ip: IpAddr,
        reflexive_ip: IpAddr,
    },

    /// Reachability could not be determined (STUN timeout, DNS
    /// failure, no default route).  Treated as not-public.
    Unknown { reason: String },
}

impl SfuReachability {
    /// Short label for log lines and RPC responses.
    pub fn label(&self) -> &'static str {
        match self {
            SfuReachability::Public { .. } => "public",
            SfuReachability::Nat { .. } => "nat",
            SfuReachability::Unknown { .. } => "unknown",
        }
    }

    /// Whether this executor should advertise SFU capability.
    pub fn is_public(&self) -> bool {
        matches!(self, SfuReachability::Public { .. })
    }
}

impl std::fmt::Display for SfuReachability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SfuReachability::Public { reflexive_addr } => {
                write!(f, "public (reflexive {})", reflexive_addr)
            }
            SfuReachability::Nat {
                bind_ip,
                reflexive_ip,
            } => write!(f, "nat (bind {}, reflexive {})", bind_ip, reflexive_ip),
            SfuReachability::Unknown { reason } => write!(f, "unknown ({})", reason),
        }
    }
}

// ── Private-address detection ────────────────────────────────────

/// Returns `true` when the IP falls in a non-publicly-routable range.
///
/// Covers RFC 1918 (`10/8`, `172.16/12`, `192.168/16`), RFC 6598
/// CGNAT (`100.64/10`), loopback (`127/8`), link-local
/// (`169.254/16`), and the IPv6 equivalents (`::1`, `fe80::/10`,
/// `fc00::/7`).
pub fn is_private_ip(ip: IpAddr) -> bool {
    match ip {
        IpAddr::V4(v4) => is_private_v4(v4),
        IpAddr::V6(v6) => is_private_v6(v6),
    }
}

fn is_private_v4(ip: Ipv4Addr) -> bool {
    let o = ip.octets();
    o[0] == 127                                  // 127.0.0.0/8     loopback
        || o[0] == 10                            // 10.0.0.0/8      RFC 1918
        || (o[0] == 172 && (o[1] & 0xF0) == 16) // 172.16.0.0/12   RFC 1918
        || (o[0] == 192 && o[1] == 168)          // 192.168.0.0/16  RFC 1918
        || (o[0] == 100 && (o[1] & 0xC0) == 64) // 100.64.0.0/10   CGNAT
        || (o[0] == 169 && o[1] == 254)          // 169.254.0.0/16  link-local
        || ip.is_unspecified()                   // 0.0.0.0
}

fn is_private_v6(ip: Ipv6Addr) -> bool {
    if ip.is_loopback() || ip.is_unspecified() {
        return true;
    }
    let s = ip.segments();
    (s[0] & 0xFFC0) == 0xFE80  // fe80::/10  link-local
        || (s[0] & 0xFE00) == 0xFC00 // fc00::/7   ULA
}

// ── STUN binding probe ───────────────────────────────────────────

const MAGIC_COOKIE: u32 = 0x2112_A442;
const BINDING_REQUEST: u16 = 0x0001;
const BINDING_RESPONSE: u16 = 0x0101;
const ATTR_XOR_MAPPED: u16 = 0x0020;
const ATTR_MAPPED: u16 = 0x0001;
const HEADER_LEN: usize = 20;
const FAMILY_V4: u8 = 0x01;

const STUN_SERVER: &str = "stun.l.google.com:19302";
const PROBE_TIMEOUT: Duration = Duration::from_secs(3);
const PROBE_RETRIES: u32 = 2;

/// Build a 20-byte STUN Binding Request (no attributes).
/// Returns the packet and the 12-byte transaction ID.
fn binding_request() -> ([u8; HEADER_LEN], [u8; 12]) {
    let mut buf = [0u8; HEADER_LEN];
    buf[0..2].copy_from_slice(&BINDING_REQUEST.to_be_bytes());
    // length = 0
    buf[4..8].copy_from_slice(&MAGIC_COOKIE.to_be_bytes());

    // Transaction ID — unique per request, not a secret.
    let txn = {
        use std::time::SystemTime;
        let ns = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        let pid = std::process::id() as u128;
        let seed = ns ^ (pid << 64);
        let mut id = [0u8; 12];
        id[..8].copy_from_slice(&(seed as u64).to_ne_bytes());
        id[8..].copy_from_slice(&((seed >> 64) as u32).to_ne_bytes());
        id
    };
    buf[8..20].copy_from_slice(&txn);
    (buf, txn)
}

/// Extract the reflexive address from a STUN Binding Response.
fn parse_response(data: &[u8], txn: &[u8; 12]) -> Option<SocketAddr> {
    if data.len() < HEADER_LEN {
        return None;
    }
    if u16::from_be_bytes([data[0], data[1]]) != BINDING_RESPONSE {
        return None;
    }
    if u32::from_be_bytes([data[4], data[5], data[6], data[7]]) != MAGIC_COOKIE {
        return None;
    }
    if &data[8..20] != txn {
        return None;
    }

    let msg_len = u16::from_be_bytes([data[2], data[3]]) as usize;
    let end = HEADER_LEN + msg_len.min(data.len().saturating_sub(HEADER_LEN));
    let mut pos = HEADER_LEN;
    let mut mapped: Option<SocketAddr> = None;

    while pos + 4 <= end {
        let attr_type = u16::from_be_bytes([data[pos], data[pos + 1]]);
        let attr_len = u16::from_be_bytes([data[pos + 2], data[pos + 3]]) as usize;
        let start = pos + 4;
        if start + attr_len > end {
            break;
        }
        let attr = &data[start..start + attr_len];

        match attr_type {
            ATTR_XOR_MAPPED => {
                if let Some(a) = decode_xor(attr) {
                    return Some(a);
                }
            }
            ATTR_MAPPED => {
                mapped = mapped.or_else(|| decode_plain(attr));
            }
            _ => {}
        }
        pos = start + ((attr_len + 3) & !3); // 4-byte aligned
    }
    mapped
}

fn decode_xor(attr: &[u8]) -> Option<SocketAddr> {
    if attr.len() < 8 || attr[1] != FAMILY_V4 {
        return None;
    }
    let port = u16::from_be_bytes([attr[2], attr[3]]) ^ (MAGIC_COOKIE >> 16) as u16;
    let ip = u32::from_be_bytes([attr[4], attr[5], attr[6], attr[7]]) ^ MAGIC_COOKIE;
    Some(SocketAddr::new(IpAddr::V4(Ipv4Addr::from(ip)), port))
}

fn decode_plain(attr: &[u8]) -> Option<SocketAddr> {
    if attr.len() < 8 || attr[1] != FAMILY_V4 {
        return None;
    }
    let port = u16::from_be_bytes([attr[2], attr[3]]);
    let ip = u32::from_be_bytes([attr[4], attr[5], attr[6], attr[7]]);
    Some(SocketAddr::new(IpAddr::V4(Ipv4Addr::from(ip)), port))
}

// ── Public API ───────────────────────────────────────────────────

/// Probe the SFU server's public reachability.
///
/// When the bind IP already falls in a private range, returns
/// [`SfuReachability::Nat`] immediately.  Otherwise sends a STUN
/// binding request and compares the server-reflexive address.
///
/// Timeout: 3 s per attempt, up to 3 attempts (one initial + two
/// retries).  Non-blocking.
pub async fn check_reachability(bind_ip: IpAddr) -> SfuReachability {
    // Fast path — private IP can never be publicly reachable.
    if is_private_ip(bind_ip) {
        return SfuReachability::Nat {
            bind_ip,
            reflexive_ip: bind_ip,
        };
    }

    // Resolve the STUN server.
    let stun_addr: SocketAddr = match tokio::net::lookup_host(STUN_SERVER).await {
        Ok(mut addrs) => match addrs.next() {
            Some(a) => a,
            None => {
                return SfuReachability::Unknown {
                    reason: format!("{} resolved to no addresses", STUN_SERVER),
                }
            }
        },
        Err(e) => {
            return SfuReachability::Unknown {
                reason: format!("DNS lookup failed: {}", e),
            }
        }
    };

    // Bind a temporary probe socket on the same interface.
    let socket = match UdpSocket::bind(SocketAddr::new(bind_ip, 0)).await {
        Ok(s) => s,
        Err(e) => {
            return SfuReachability::Unknown {
                reason: format!("probe bind failed: {}", e),
            }
        }
    };

    let (pkt, txn) = binding_request();

    for attempt in 0..=PROBE_RETRIES {
        if attempt > 0 {
            debug!("STUN reachability retry {}/{}", attempt, PROBE_RETRIES);
        }
        if let Err(e) = socket.send_to(&pkt, stun_addr).await {
            debug!("STUN send failed: {}", e);
            continue;
        }

        let mut buf = [0u8; 512];
        match timeout(PROBE_TIMEOUT, socket.recv_from(&mut buf)).await {
            Ok(Ok((n, _))) => {
                if let Some(reflexive) = parse_response(&buf[..n], &txn) {
                    if reflexive.ip() == bind_ip {
                        return SfuReachability::Public {
                            reflexive_addr: reflexive,
                        };
                    }
                    return SfuReachability::Nat {
                        bind_ip,
                        reflexive_ip: reflexive.ip(),
                    };
                }
                debug!("STUN response unparseable ({} bytes)", n);
            }
            Ok(Err(e)) => debug!("STUN recv error: {}", e),
            Err(_) => debug!("STUN timeout ({}ms)", PROBE_TIMEOUT.as_millis()),
        }
    }

    SfuReachability::Unknown {
        reason: format!(
            "no response from {} after {} attempts",
            STUN_SERVER,
            PROBE_RETRIES + 1
        ),
    }
}

// ── Tests ────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn private_v4_ranges() {
        assert!(is_private_ip("127.0.0.1".parse().unwrap()));
        assert!(is_private_ip("127.255.255.255".parse().unwrap()));
        assert!(is_private_ip("10.0.0.1".parse().unwrap()));
        assert!(is_private_ip("10.255.255.255".parse().unwrap()));
        assert!(is_private_ip("172.16.0.1".parse().unwrap()));
        assert!(is_private_ip("172.31.255.255".parse().unwrap()));
        assert!(is_private_ip("192.168.0.1".parse().unwrap()));
        assert!(is_private_ip("192.168.255.255".parse().unwrap()));
        assert!(is_private_ip("100.64.0.1".parse().unwrap()));
        assert!(is_private_ip("100.127.255.255".parse().unwrap()));
        assert!(is_private_ip("169.254.1.1".parse().unwrap()));
        assert!(is_private_ip("0.0.0.0".parse().unwrap()));
    }

    #[test]
    fn public_v4() {
        assert!(!is_private_ip("8.8.8.8".parse().unwrap()));
        assert!(!is_private_ip("203.0.113.5".parse().unwrap()));
        assert!(!is_private_ip("1.1.1.1".parse().unwrap()));
        assert!(!is_private_ip("100.128.0.1".parse().unwrap())); // just outside CGNAT
        assert!(!is_private_ip("172.32.0.1".parse().unwrap()));  // just outside 172.16/12
    }

    #[test]
    fn private_v6() {
        assert!(is_private_ip("::1".parse().unwrap()));
        assert!(is_private_ip("fe80::1".parse().unwrap()));
        assert!(is_private_ip("fd00::1".parse().unwrap()));
        assert!(is_private_ip("fc00::1".parse().unwrap()));
        assert!(is_private_ip("::".parse().unwrap()));
    }

    #[test]
    fn public_v6() {
        assert!(!is_private_ip("2001:db8::1".parse().unwrap()));
        assert!(!is_private_ip("2606:4700::1".parse().unwrap()));
    }

    #[test]
    fn stun_request_format() {
        let (buf, txn) = binding_request();
        assert_eq!(buf.len(), 20);
        assert_eq!(u16::from_be_bytes([buf[0], buf[1]]), BINDING_REQUEST);
        assert_eq!(u16::from_be_bytes([buf[2], buf[3]]), 0); // length
        assert_eq!(
            u32::from_be_bytes([buf[4], buf[5], buf[6], buf[7]]),
            MAGIC_COOKIE
        );
        assert_eq!(&buf[8..20], &txn);
    }

    #[test]
    fn parse_xor_mapped_address() {
        let txn = [0xAA; 12];
        let mut resp = vec![0u8; 32];
        resp[0..2].copy_from_slice(&BINDING_RESPONSE.to_be_bytes());
        resp[2..4].copy_from_slice(&12u16.to_be_bytes());
        resp[4..8].copy_from_slice(&MAGIC_COOKIE.to_be_bytes());
        resp[8..20].copy_from_slice(&txn);
        // XOR-MAPPED-ADDRESS attribute
        resp[20..22].copy_from_slice(&ATTR_XOR_MAPPED.to_be_bytes());
        resp[22..24].copy_from_slice(&8u16.to_be_bytes());
        resp[24] = 0; // reserved
        resp[25] = FAMILY_V4;
        let xor_port = 12345u16 ^ (MAGIC_COOKIE >> 16) as u16;
        resp[26..28].copy_from_slice(&xor_port.to_be_bytes());
        let xor_ip = u32::from_be_bytes([203, 0, 113, 5]) ^ MAGIC_COOKIE;
        resp[28..32].copy_from_slice(&xor_ip.to_be_bytes());

        let addr = parse_response(&resp, &txn).unwrap();
        assert_eq!(addr.ip(), IpAddr::V4(Ipv4Addr::new(203, 0, 113, 5)));
        assert_eq!(addr.port(), 12345);
    }

    #[test]
    fn parse_mapped_address_fallback() {
        let txn = [0xBB; 12];
        let mut resp = vec![0u8; 32];
        resp[0..2].copy_from_slice(&BINDING_RESPONSE.to_be_bytes());
        resp[2..4].copy_from_slice(&12u16.to_be_bytes());
        resp[4..8].copy_from_slice(&MAGIC_COOKIE.to_be_bytes());
        resp[8..20].copy_from_slice(&txn);
        // Plain MAPPED-ADDRESS
        resp[20..22].copy_from_slice(&ATTR_MAPPED.to_be_bytes());
        resp[22..24].copy_from_slice(&8u16.to_be_bytes());
        resp[24] = 0;
        resp[25] = FAMILY_V4;
        resp[26..28].copy_from_slice(&8080u16.to_be_bytes());
        resp[28..32].copy_from_slice(&[198, 51, 100, 1]);

        let addr = parse_response(&resp, &txn).unwrap();
        assert_eq!(addr.ip(), IpAddr::V4(Ipv4Addr::new(198, 51, 100, 1)));
        assert_eq!(addr.port(), 8080);
    }

    #[test]
    fn rejects_wrong_transaction() {
        let txn = [0xCC; 12];
        let wrong = [0xDD; 12];
        let mut resp = vec![0u8; 32];
        resp[0..2].copy_from_slice(&BINDING_RESPONSE.to_be_bytes());
        resp[2..4].copy_from_slice(&12u16.to_be_bytes());
        resp[4..8].copy_from_slice(&MAGIC_COOKIE.to_be_bytes());
        resp[8..20].copy_from_slice(&wrong);
        resp[20..22].copy_from_slice(&ATTR_XOR_MAPPED.to_be_bytes());
        resp[22..24].copy_from_slice(&8u16.to_be_bytes());

        assert!(parse_response(&resp, &txn).is_none());
    }
}
