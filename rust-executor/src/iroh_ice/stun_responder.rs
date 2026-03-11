//! Minimal RFC 5389 STUN Binding Request/Response handler.
//!
//! Handles only STUN Binding Requests and responds with XOR-MAPPED-ADDRESS.
//! Designed to run as an async task, receiving packets via mpsc channel.

use std::net::SocketAddr;
use tokio::sync::mpsc;

/// STUN magic cookie (RFC 5389 §6)
const MAGIC_COOKIE: u32 = 0x2112_A442;

/// STUN message types
const BINDING_REQUEST: u16 = 0x0001;
const BINDING_RESPONSE: u16 = 0x0101;

/// STUN attribute types
const ATTR_XOR_MAPPED_ADDRESS: u16 = 0x0020;

/// Address family constants
const FAMILY_IPV4: u8 = 0x01;
const FAMILY_IPV6: u8 = 0x02;

/// STUN header size
const STUN_HEADER_SIZE: usize = 20;

/// A STUN packet received from the demuxer, tagged with its source address.
#[derive(Debug, Clone)]
pub struct StunPacket {
    pub data: Vec<u8>,
    pub source: SocketAddr,
}

/// A STUN response to be sent back.
#[derive(Debug, Clone)]
pub struct StunResponse {
    pub data: Vec<u8>,
    pub dest: SocketAddr,
}

/// Parsed STUN message header.
#[derive(Debug, Clone)]
struct StunHeader {
    msg_type: u16,
    #[allow(dead_code)]
    msg_length: u16,
    transaction_id: [u8; 12],
}

/// Parse a STUN message header from raw bytes.
///
/// Returns `None` if the packet is too short, has wrong magic cookie, or
/// the top two bits are not zero (per RFC 5389 §6).
pub fn parse_stun_header(data: &[u8]) -> Option<StunHeader> {
    if data.len() < STUN_HEADER_SIZE {
        return None;
    }

    // Top two bits of first byte must be 0
    if data[0] & 0xC0 != 0 {
        return None;
    }

    let msg_type = u16::from_be_bytes([data[0], data[1]]);
    let msg_length = u16::from_be_bytes([data[2], data[3]]);
    let cookie = u32::from_be_bytes([data[4], data[5], data[6], data[7]]);

    if cookie != MAGIC_COOKIE {
        return None;
    }

    let mut transaction_id = [0u8; 12];
    transaction_id.copy_from_slice(&data[8..20]);

    Some(StunHeader {
        msg_type,
        msg_length,
        transaction_id,
    })
}

/// Build a XOR-MAPPED-ADDRESS attribute for the given address.
///
/// Per RFC 5389 §15.2:
/// - Port is XORed with top 16 bits of magic cookie
/// - IPv4 address is XORed with magic cookie
/// - IPv6 address is XORed with magic cookie + transaction ID (16 bytes)
pub fn build_xor_mapped_address(addr: &SocketAddr, transaction_id: &[u8; 12]) -> Vec<u8> {
    let cookie_bytes = MAGIC_COOKIE.to_be_bytes();
    let xored_port = (addr.port() ^ (MAGIC_COOKIE >> 16) as u16).to_be_bytes();

    match addr {
        SocketAddr::V4(v4) => {
            let ip_bytes = v4.ip().octets();
            let xored_ip = [
                ip_bytes[0] ^ cookie_bytes[0],
                ip_bytes[1] ^ cookie_bytes[1],
                ip_bytes[2] ^ cookie_bytes[2],
                ip_bytes[3] ^ cookie_bytes[3],
            ];

            // Attribute: type (2) + length (2) + reserved (1) + family (1) + port (2) + addr (4) = 12
            let mut attr = Vec::with_capacity(12);
            attr.extend_from_slice(&ATTR_XOR_MAPPED_ADDRESS.to_be_bytes());
            attr.extend_from_slice(&8u16.to_be_bytes()); // value length
            attr.push(0x00); // reserved
            attr.push(FAMILY_IPV4);
            attr.extend_from_slice(&xored_port);
            attr.extend_from_slice(&xored_ip);
            attr
        }
        SocketAddr::V6(v6) => {
            let ip_bytes = v6.ip().octets();
            // XOR with magic cookie (4 bytes) + transaction ID (12 bytes) = 16 bytes
            let mut xor_key = [0u8; 16];
            xor_key[..4].copy_from_slice(&cookie_bytes);
            xor_key[4..16].copy_from_slice(transaction_id);

            let mut xored_ip = [0u8; 16];
            for i in 0..16 {
                xored_ip[i] = ip_bytes[i] ^ xor_key[i];
            }

            // Attribute: type (2) + length (2) + reserved (1) + family (1) + port (2) + addr (16) = 24
            let mut attr = Vec::with_capacity(24);
            attr.extend_from_slice(&ATTR_XOR_MAPPED_ADDRESS.to_be_bytes());
            attr.extend_from_slice(&20u16.to_be_bytes()); // value length
            attr.push(0x00); // reserved
            attr.push(FAMILY_IPV6);
            attr.extend_from_slice(&xored_port);
            attr.extend_from_slice(&xored_ip);
            attr
        }
    }
}

/// Build a complete STUN Binding Response for a given request.
///
/// Returns `None` if the request isn't a valid STUN Binding Request.
pub fn build_binding_response(request: &[u8], source_addr: &SocketAddr) -> Option<Vec<u8>> {
    let header = parse_stun_header(request)?;

    if header.msg_type != BINDING_REQUEST {
        return None;
    }

    let xma = build_xor_mapped_address(source_addr, &header.transaction_id);
    
    let msg_len = xma.len() as u16;

    let mut response = Vec::with_capacity(STUN_HEADER_SIZE + xma.len());
    // Header
    response.extend_from_slice(&BINDING_RESPONSE.to_be_bytes());
    response.extend_from_slice(&msg_len.to_be_bytes());
    response.extend_from_slice(&MAGIC_COOKIE.to_be_bytes());
    response.extend_from_slice(&header.transaction_id);
    // Attribute
    response.extend_from_slice(&xma);

    Some(response)
}

/// Run the STUN responder task.
///
/// Reads STUN packets from `rx`, processes Binding Requests, and sends
/// responses to `response_tx`.
pub async fn run_stun_responder(
    mut rx: mpsc::Receiver<StunPacket>,
    response_tx: mpsc::Sender<StunResponse>,
) {
    while let Some(pkt) = rx.recv().await {
        if let Some(response_data) = build_binding_response(&pkt.data, &pkt.source) {
            let resp = StunResponse {
                data: response_data,
                dest: pkt.source,
            };
            // Best-effort send; if the response channel is full, drop
            let _ = response_tx.try_send(resp);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::{Ipv4Addr, Ipv6Addr, SocketAddrV4, SocketAddrV6};

    /// Helper: build a minimal STUN Binding Request with a given transaction ID.
    fn make_binding_request(transaction_id: &[u8; 12]) -> Vec<u8> {
        let mut pkt = Vec::with_capacity(20);
        pkt.extend_from_slice(&BINDING_REQUEST.to_be_bytes());
        pkt.extend_from_slice(&0u16.to_be_bytes()); // length = 0 (no attributes)
        pkt.extend_from_slice(&MAGIC_COOKIE.to_be_bytes());
        pkt.extend_from_slice(transaction_id);
        pkt
    }

    #[test]
    fn test_parse_valid_binding_request() {
        let tid = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
        let pkt = make_binding_request(&tid);
        let header = parse_stun_header(&pkt).unwrap();
        assert_eq!(header.msg_type, BINDING_REQUEST);
        assert_eq!(header.transaction_id, tid);
        assert_eq!(header.msg_length, 0);
    }

    #[test]
    fn test_parse_truncated_packet() {
        assert!(parse_stun_header(&[0; 10]).is_none());
        assert!(parse_stun_header(&[]).is_none());
    }

    #[test]
    fn test_parse_wrong_magic_cookie() {
        let mut pkt = make_binding_request(&[0; 12]);
        // Corrupt the magic cookie
        pkt[4] = 0xFF;
        assert!(parse_stun_header(&pkt).is_none());
    }

    #[test]
    fn test_parse_top_bits_set() {
        let mut pkt = make_binding_request(&[0; 12]);
        pkt[0] |= 0x80; // set top bit
        assert!(parse_stun_header(&pkt).is_none());
    }

    #[test]
    fn test_binding_response_ipv4() {
        let tid = [0xAA, 0xBB, 0xCC, 0xDD, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];
        let request = make_binding_request(&tid);
        let source = SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::new(192, 168, 1, 100), 12345));
        let response = build_binding_response(&request, &source).unwrap();

        // Verify response header
        let resp_type = u16::from_be_bytes([response[0], response[1]]);
        assert_eq!(resp_type, BINDING_RESPONSE);

        let resp_cookie = u32::from_be_bytes([response[4], response[5], response[6], response[7]]);
        assert_eq!(resp_cookie, MAGIC_COOKIE);

        // Verify transaction ID echoed
        assert_eq!(&response[8..20], &tid);

        // Verify XOR-MAPPED-ADDRESS attribute
        let attr_type = u16::from_be_bytes([response[20], response[21]]);
        assert_eq!(attr_type, ATTR_XOR_MAPPED_ADDRESS);

        // Verify family
        assert_eq!(response[25], FAMILY_IPV4);

        // Decode XOR'd port
        let xored_port = u16::from_be_bytes([response[26], response[27]]);
        let port = xored_port ^ (MAGIC_COOKIE >> 16) as u16;
        assert_eq!(port, 12345);

        // Decode XOR'd address
        let cookie = MAGIC_COOKIE.to_be_bytes();
        let ip = Ipv4Addr::new(
            response[28] ^ cookie[0],
            response[29] ^ cookie[1],
            response[30] ^ cookie[2],
            response[31] ^ cookie[3],
        );
        assert_eq!(ip, Ipv4Addr::new(192, 168, 1, 100));
    }

    #[test]
    fn test_binding_response_ipv6() {
        let tid = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C];
        let request = make_binding_request(&tid);
        let ipv6 = Ipv6Addr::new(0x2001, 0x0db8, 0, 0, 0, 0, 0, 1);
        let source = SocketAddr::V6(SocketAddrV6::new(ipv6, 9999, 0, 0));
        let response = build_binding_response(&request, &source).unwrap();

        // Verify family
        assert_eq!(response[25], FAMILY_IPV6);

        // Decode XOR'd port
        let xored_port = u16::from_be_bytes([response[26], response[27]]);
        let port = xored_port ^ (MAGIC_COOKIE >> 16) as u16;
        assert_eq!(port, 9999);

        // Decode XOR'd IPv6 address
        let cookie = MAGIC_COOKIE.to_be_bytes();
        let mut xor_key = [0u8; 16];
        xor_key[..4].copy_from_slice(&cookie);
        xor_key[4..16].copy_from_slice(&tid);

        let mut decoded_ip = [0u8; 16];
        for i in 0..16 {
            decoded_ip[i] = response[28 + i] ^ xor_key[i];
        }
        let decoded_addr = Ipv6Addr::from(decoded_ip);
        assert_eq!(decoded_addr, ipv6);
    }

    #[test]
    fn test_reject_non_binding_request() {
        let tid = [0; 12];
        // Build a STUN Indication (type 0x0011) instead of Binding Request
        let mut pkt = Vec::with_capacity(20);
        pkt.extend_from_slice(&0x0011u16.to_be_bytes());
        pkt.extend_from_slice(&0u16.to_be_bytes());
        pkt.extend_from_slice(&MAGIC_COOKIE.to_be_bytes());
        pkt.extend_from_slice(&tid);

        let source = SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::LOCALHOST, 1234));
        assert!(build_binding_response(&pkt, &source).is_none());
    }

    #[test]
    fn test_malformed_packet_doesnt_crash() {
        let source = SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::LOCALHOST, 1234));
        // Various malformed inputs
        assert!(build_binding_response(&[], &source).is_none());
        assert!(build_binding_response(&[0xFF; 5], &source).is_none());
        assert!(build_binding_response(&[0; 19], &source).is_none());
        // Valid length but wrong cookie
        assert!(build_binding_response(&[0; 20], &source).is_none());
    }

    #[test]
    fn test_transaction_id_echo() {
        let tid = [0xDE, 0xAD, 0xBE, 0xEF, 0xCA, 0xFE, 0xBA, 0xBE, 0x12, 0x34, 0x56, 0x78];
        let request = make_binding_request(&tid);
        let source = SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::new(10, 0, 0, 1), 5000));
        let response = build_binding_response(&request, &source).unwrap();
        assert_eq!(&response[8..20], &tid);
    }

    #[tokio::test]
    async fn test_stun_responder_roundtrip() {
        let (stun_tx, stun_rx) = mpsc::channel(16);
        let (resp_tx, mut resp_rx) = mpsc::channel(16);

        tokio::spawn(run_stun_responder(stun_rx, resp_tx));

        let tid = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
        let request = make_binding_request(&tid);
        let source = SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::new(203, 0, 113, 50), 54321));

        stun_tx
            .send(StunPacket {
                data: request,
                source,
            })
            .await
            .unwrap();

        let resp = resp_rx.recv().await.unwrap();
        assert_eq!(resp.dest, source);

        // Verify it's a valid Binding Response
        let header = parse_stun_header(&resp.data).unwrap();
        assert_eq!(header.msg_type, BINDING_RESPONSE);
        assert_eq!(header.transaction_id, tid);
    }

    #[tokio::test]
    async fn test_stun_responder_ignores_non_binding() {
        let (stun_tx, stun_rx) = mpsc::channel(16);
        let (resp_tx, mut resp_rx) = mpsc::channel(16);

        tokio::spawn(run_stun_responder(stun_rx, resp_tx));

        // Send a non-binding STUN message
        let mut pkt = Vec::with_capacity(20);
        pkt.extend_from_slice(&0x0011u16.to_be_bytes()); // Indication
        pkt.extend_from_slice(&0u16.to_be_bytes());
        pkt.extend_from_slice(&MAGIC_COOKIE.to_be_bytes());
        pkt.extend_from_slice(&[0u8; 12]);

        let source = SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::LOCALHOST, 1234));
        stun_tx
            .send(StunPacket {
                data: pkt,
                source,
            })
            .await
            .unwrap();

        // Then send a valid one to prove the responder is still running
        let tid = [0xFF; 12];
        let valid_request = make_binding_request(&tid);
        stun_tx
            .send(StunPacket {
                data: valid_request,
                source,
            })
            .await
            .unwrap();

        let resp = resp_rx.recv().await.unwrap();
        let header = parse_stun_header(&resp.data).unwrap();
        assert_eq!(header.msg_type, BINDING_RESPONSE);
        assert_eq!(header.transaction_id, tid);
    }
}
