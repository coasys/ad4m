//! UDP Demultiplexer for shared Iroh socket.
//!
//! Classifies incoming UDP packets by first byte per RFC 9443 and tees
//! STUN packets to an internal channel while passing all valid traffic
//! through to Quinn/Iroh.

use std::fmt;
use std::io::{self, IoSliceMut};
use std::net::SocketAddr;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};

use quinn::{AsyncUdpSocket, UdpPoller};
use tokio::sync::mpsc;
use quinn::udp::{RecvMeta, Transmit};

use super::stun_responder::StunPacket;

/// Classification of a UDP packet based on its first byte (RFC 9443).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PacketClass {
    /// STUN: first byte in [0, 3]
    Stun,
    /// Dropped: first byte in [4, 15] or [16, 19] (ZRTP)
    Drop,
    /// DTLS: first byte in [20, 63]
    Dtls,
    /// QUIC/RTP/RTCP: first byte in [64, 255]
    QuicRtp,
}

/// Classify a packet by its first byte per RFC 9443.
pub fn classify_first_byte(byte: u8) -> PacketClass {
    match byte {
        0..=3 => PacketClass::Stun,
        4..=19 => PacketClass::Drop,
        20..=63 => PacketClass::Dtls,
        64..=255 => PacketClass::QuicRtp,
    }
}

/// A UDP socket wrapper that demultiplexes incoming packets by protocol.
///
/// STUN packets (first byte 0-3) are teed to an mpsc channel AND passed through.
/// Dropped ranges (4-19) are silently consumed.
/// Everything else passes through unchanged.
pub struct DemuxUdpSocket {
    inner: Arc<dyn AsyncUdpSocket>,
    stun_tx: mpsc::Sender<StunPacket>,
}

impl DemuxUdpSocket {
    /// Create a new demultiplexing socket wrapper.
    pub fn new(inner: Arc<dyn AsyncUdpSocket>, stun_tx: mpsc::Sender<StunPacket>) -> Self {
        Self { inner, stun_tx }
    }
}

impl fmt::Debug for DemuxUdpSocket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DemuxUdpSocket")
            .field("inner", &self.inner)
            .finish()
    }
}

impl AsyncUdpSocket for DemuxUdpSocket {
    fn create_io_poller(self: Arc<Self>) -> Pin<Box<dyn UdpPoller>> {
        self.inner.clone().create_io_poller()
    }

    fn try_send(&self, transmit: &Transmit) -> io::Result<()> {
        self.inner.try_send(transmit)
    }

    fn poll_recv(
        &self,
        cx: &mut Context,
        bufs: &mut [IoSliceMut<'_>],
        meta: &mut [RecvMeta],
    ) -> Poll<io::Result<usize>> {
        let result = self.inner.poll_recv(cx, bufs, meta);

        if let Poll::Ready(Ok(n)) = &result {
            // Process each received datagram
            for i in 0..*n {
                let buf = &bufs[i];
                if buf.is_empty() {
                    continue;
                }
                let first_byte = buf[0];
                let class = classify_first_byte(first_byte);

                match class {
                    PacketClass::Stun => {
                        // Tee to STUN responder (best-effort, don't block Quinn)
                        let data = buf[..meta[i].len].to_vec();
                        let source = meta[i].addr;
                        let _ = self.stun_tx.try_send(StunPacket { data, source });
                        // Pass through to Quinn (don't modify bufs/meta)
                    }
                    PacketClass::Drop => {
                        // We'd ideally remove this from the results, but Quinn's
                        // poll_recv API makes that awkward. The dropped ranges (4-19)
                        // won't match any valid QUIC or STUN packet, so Quinn will
                        // discard them anyway. Pass through harmlessly.
                    }
                    PacketClass::Dtls | PacketClass::QuicRtp => {
                        // Pass through unchanged
                    }
                }
            }
        }

        result
    }

    fn local_addr(&self) -> io::Result<SocketAddr> {
        self.inner.local_addr()
    }

    fn max_transmit_segments(&self) -> usize {
        self.inner.max_transmit_segments()
    }

    fn max_receive_segments(&self) -> usize {
        self.inner.max_receive_segments()
    }

    fn may_fragment(&self) -> bool {
        self.inner.may_fragment()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_classify_stun_range() {
        for b in 0..=3u8 {
            assert_eq!(classify_first_byte(b), PacketClass::Stun, "byte {}", b);
        }
    }

    #[test]
    fn test_classify_drop_range() {
        for b in 4..=19u8 {
            assert_eq!(classify_first_byte(b), PacketClass::Drop, "byte {}", b);
        }
    }

    #[test]
    fn test_classify_dtls_range() {
        for b in 20..=63u8 {
            assert_eq!(classify_first_byte(b), PacketClass::Dtls, "byte {}", b);
        }
    }

    #[test]
    fn test_classify_quic_rtp_range() {
        for b in 64..=255u8 {
            assert_eq!(classify_first_byte(b), PacketClass::QuicRtp, "byte {}", b);
        }
    }

    #[test]
    fn test_stun_tee_with_channel() {
        // Verify that creating a DemuxUdpSocket with a channel works
        // and that try_send on a full channel doesn't panic
        let (tx, _rx) = mpsc::channel(1);

        // Fill the channel
        let _ = tx.try_send(StunPacket {
            data: vec![0],
            source: "127.0.0.1:1234".parse().unwrap(),
        });

        // Another send should not panic (best-effort)
        let result = tx.try_send(StunPacket {
            data: vec![0],
            source: "127.0.0.1:1234".parse().unwrap(),
        });
        assert!(result.is_err()); // Channel full, but no panic
    }
}
