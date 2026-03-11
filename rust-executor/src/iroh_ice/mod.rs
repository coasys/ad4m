//! Iroh-to-ICE bridge for AD4M WebRTC connections.
//!
//! Provides ICE candidates derived from Holochain/Iroh transport peer URLs,
//! replacing the need for external STUN/TURN servers.

pub mod demux;
pub mod stun_responder;


use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// ICE candidate info used by the executor.
#[derive(Debug, Clone)]
pub struct IceCandidate {
    pub candidate: String,
    pub candidate_type: String,
    pub address: String,
    pub port: u16,
}

/// Parse a peer URL to extract host and port.
fn parse_url_addr(url_str: &str) -> Option<(String, u16)> {
    if let Ok(parsed) = url::Url::parse(url_str) {
        if let Some(host) = parsed.host_str() {
            let port = parsed.port_or_known_default().unwrap_or(443);
            return Some((host.to_string(), port));
        }
    }
    if let Ok(addr) = url_str.parse::<std::net::SocketAddr>() {
        return Some((addr.ip().to_string(), addr.port()));
    }
    None
}

/// Determine candidate type from address string.
fn candidate_type_for(addr: &str) -> &'static str {
    if let Ok(ip) = addr.parse::<std::net::IpAddr>() {
        match ip {
            std::net::IpAddr::V4(v4) if v4.is_loopback() || v4.is_private() => "host",
            std::net::IpAddr::V6(v6) if v6.is_loopback() => "host",
            _ => "srflx",
        }
    } else {
        "srflx"
    }
}

/// Convert peer URL strings into ICE candidate representations.
pub fn candidates_from_urls(urls: &[String]) -> Vec<IceCandidate> {
    urls.iter()
        .enumerate()
        .filter_map(|(i, url)| {
            let (host, port) = parse_url_addr(url)?;
            let ctype = candidate_type_for(&host);

            let mut hasher = DefaultHasher::new();
            url.hash(&mut hasher);
            let foundation = hasher.finish() % 10000;

            let priority = if ctype == "host" {
                2130706431u32.saturating_sub(i as u32)
            } else {
                1694498815u32.saturating_sub(i as u32)
            };

            let candidate_str = format!(
                "candidate:{} 1 udp {} {} {} typ {}",
                foundation, priority, host, port, ctype
            );

            Some(IceCandidate {
                candidate: candidate_str,
                candidate_type: ctype.to_string(),
                address: host,
                port,
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_candidates_from_urls_empty() {
        let result = candidates_from_urls(&[]);
        assert!(result.is_empty());
    }

    #[test]
    fn test_candidates_from_urls_wss() {
        let urls = vec!["wss://192.168.1.1:4000".to_string()];
        let candidates = candidates_from_urls(&urls);
        assert_eq!(candidates.len(), 1);
        assert_eq!(candidates[0].candidate_type, "host");
        assert_eq!(candidates[0].address, "192.168.1.1");
        assert_eq!(candidates[0].port, 4000);
        assert!(candidates[0].candidate.contains("typ host"));
    }

    #[test]
    fn test_candidates_from_urls_public() {
        let urls = vec!["wss://8.8.8.8:5000".to_string()];
        let candidates = candidates_from_urls(&urls);
        assert_eq!(candidates[0].candidate_type, "srflx");
    }

    #[test]
    fn test_candidates_from_urls_unparseable() {
        let urls = vec!["not-a-url".to_string()];
        let candidates = candidates_from_urls(&urls);
        assert!(candidates.is_empty());
    }
}

/// Handle to a running STUN responder, holding the channel sender
/// for the demuxer to tee STUN packets into.
pub struct StunHandle {
    /// Send STUN packets here from the demuxer.
    pub stun_tx: tokio::sync::mpsc::Sender<stun_responder::StunPacket>,
    /// Receive STUN responses to send back over the network.
    pub response_rx: tokio::sync::mpsc::Receiver<stun_responder::StunResponse>,
}

/// Start a STUN responder task and return handles for the demuxer.
pub fn start_stun_responder(channel_size: usize) -> StunHandle {
    let (stun_tx, stun_rx) = tokio::sync::mpsc::channel(channel_size);
    let (response_tx, response_rx) = tokio::sync::mpsc::channel(channel_size);
    tokio::spawn(stun_responder::run_stun_responder(stun_rx, response_tx));
    StunHandle { stun_tx, response_rx }
}
