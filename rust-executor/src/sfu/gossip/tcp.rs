//! TCP + JSON-line gossip transport.
//!
//! Each SFU node binds a local TCP listener; every other node in the
//! configured peer set opens an outbound TCP connection to that
//! listener.  Messages are length-prefixed by newline:
//!
//! ```text
//! { "type": "sfu-announce", "did": "did:key:...",
//!   "room_id": "windtunnel://t3:t3-sfu-cascade-2node",
//!   "participant_count": 4, "capacity_hint": 4 }\n
//! ```
//!
//! On the receive side the transport spawns one tokio task per
//! inbound socket; each frame is JSON-decoded into a [`CascadeSignal`]
//! and pushed onto a shared mpsc channel that `SfuService` consumes.
//!
//! Only configured peers are heard. The listener drops a connection from
//! an address no peer is configured at, and a connection's first frame
//! binds it to one of the DIDs configured at that address; a later frame
//! claiming another DID closes it. Peers that share an IP (a single-host
//! cluster) can still claim each other's DIDs: frames are not signed, so
//! run the cascade listener only on a network its peers alone can reach.
//!
//! Discovery is static — the peer list is configuration provided at
//! startup.  The transport doesn't try to maintain global mesh
//! topology; if a peer goes down its outbound TcpStream errors and
//! sends silently drop until the operator restarts.  That's
//! acceptable for this layer because cascade decisions tolerate
//! stale views (`pick_redirect_node` falls back gracefully).

use std::collections::{HashMap, HashSet};
use std::net::{IpAddr, SocketAddr};
use std::sync::{Arc, Mutex as StdMutex};
use std::time::Duration;

use async_trait::async_trait;
use log::{debug, info, warn};
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::{mpsc, Mutex};
use tokio::task::JoinHandle;
use tokio::time::sleep;

use super::super::cascade::CascadeSignal;
use super::{CascadeGossip, GossipTarget};

/// One peer reachable over TCP gossip.
#[derive(Debug, Clone)]
pub struct GossipPeer {
    /// Remote node DID — what `GossipTarget::PeerDid` matches on.
    pub did: String,
    /// Address to connect to.
    pub addr: SocketAddr,
}

/// Internal sender map: peer DID → outbound message sender.  The
/// outbound write task pulls frames from the per-peer sender and
/// writes them to the TcpStream.
type OutboundMap = Arc<Mutex<HashMap<String, mpsc::Sender<CascadeSignal>>>>;

pub struct TcpGossip {
    local_did: String,
    max_participants_per_node: u32,
    outbound: OutboundMap,
    /// `take_inbound` is called once from the synchronous trait
    /// method during `SfuService::start`; std Mutex avoids requiring
    /// an async context.
    inbound_rx: StdMutex<Option<mpsc::Receiver<CascadeSignal>>>,
    /// Spawned tasks — retained so they keep running for the
    /// lifetime of the gossip transport.
    _tasks: Vec<JoinHandle<()>>,
}

impl TcpGossip {
    /// Bind on `bind_addr`, dial each configured peer.  Returns once
    /// the listener is up and the dial tasks have been spawned;
    /// dials retry forever in the background.
    pub async fn start(
        local_did: String,
        max_participants_per_node: u32,
        bind_addr: SocketAddr,
        peers: Vec<GossipPeer>,
    ) -> Result<Arc<Self>, String> {
        let listener = TcpListener::bind(bind_addr)
            .await
            .map_err(|e| format!("TcpGossip: bind {} failed: {}", bind_addr, e))?;
        let actual_bind = listener
            .local_addr()
            .map_err(|e| format!("TcpGossip: local_addr: {}", e))?;
        info!("TcpGossip[{}] listening on {}", local_did, actual_bind);

        let (inbound_tx, inbound_rx) = mpsc::channel::<CascadeSignal>(256);
        let outbound: OutboundMap = Arc::new(Mutex::new(HashMap::new()));

        let mut tasks: Vec<JoinHandle<()>> = Vec::new();

        // Which DIDs may speak from which address. Inbound connections come
        // from an ephemeral port, so only the IP identifies a peer — in
        // canonical form, as a dual-stack listener reports an IPv4 peer as
        // `::ffff:a.b.c.d`.
        let mut allowed: HashMap<IpAddr, HashSet<String>> = HashMap::new();
        for peer in peers.iter().filter(|p| p.did != local_did) {
            allowed
                .entry(peer.addr.ip().to_canonical())
                .or_default()
                .insert(peer.did.clone());
        }

        // Accept-loop: every inbound TCP stream from a configured peer
        // address spawns its own reader.
        {
            let inbound_tx = inbound_tx.clone();
            tasks.push(tokio::spawn(async move {
                loop {
                    match listener.accept().await {
                        Ok((socket, peer_addr)) => {
                            let Some(dids) = allowed.get(&peer_addr.ip().to_canonical()) else {
                                warn!(
                                    "TcpGossip: refusing {}, no peer is configured there",
                                    peer_addr
                                );
                                continue;
                            };
                            debug!("TcpGossip accept from {}", peer_addr);
                            let tx = inbound_tx.clone();
                            tokio::spawn(reader_loop(socket, tx, dids.clone()));
                        }
                        Err(e) => {
                            warn!("TcpGossip accept error: {}", e);
                            sleep(Duration::from_millis(500)).await;
                        }
                    }
                }
            }));
        }

        // Per-peer outbound: open a connection and own a writer task.
        for peer in peers {
            if peer.did == local_did {
                continue;
            }
            let outbound = Arc::clone(&outbound);
            let did = peer.did.clone();
            let addr = peer.addr;
            tasks.push(tokio::spawn(async move {
                connect_loop(did, addr, outbound).await;
            }));
        }

        Ok(Arc::new(Self {
            local_did,
            max_participants_per_node,
            outbound,
            inbound_rx: StdMutex::new(Some(inbound_rx)),
            _tasks: tasks,
        }))
    }
}

#[async_trait]
impl CascadeGossip for TcpGossip {
    async fn send(&self, target: GossipTarget, signal: CascadeSignal) -> Result<(), String> {
        let outbound = self.outbound.lock().await;
        let variant = signal.variant_name();
        match target {
            GossipTarget::Broadcast => {
                let recipients: Vec<&String> = outbound.keys().collect();
                debug!(
                    "TcpGossip[{}] broadcast {} to {} peer(s): {:?}",
                    self.local_did,
                    variant,
                    recipients.len(),
                    recipients
                );
                for tx in outbound.values() {
                    let _ = tx.try_send(signal.clone());
                }
            }
            GossipTarget::PeerDid(did) => {
                if let Some(tx) = outbound.get(&did) {
                    debug!(
                        "TcpGossip[{}] directed {} to {}",
                        self.local_did, variant, did
                    );
                    let _ = tx.try_send(signal);
                } else {
                    debug!(
                        "TcpGossip[{}] no outbound for {} — {} dropped",
                        self.local_did, did, variant
                    );
                }
            }
        }
        Ok(())
    }

    fn take_inbound(&self) -> Option<mpsc::Receiver<CascadeSignal>> {
        let mut guard = self.inbound_rx.lock().expect("inbound_rx mutex poisoned");
        guard.take()
    }

    fn local_did(&self) -> &str {
        &self.local_did
    }

    fn max_participants_per_node(&self) -> u32 {
        self.max_participants_per_node
    }
}

/// Maximum line length the gossip reader accepts.  Lines exceeding
/// this limit get dropped to prevent memory exhaustion from a
/// malicious or misbehaving sender.
///
/// SDP payloads with multiple renegotiation m-lines (pipe answers
/// carrying audio + video per remote peer) routinely reach 20-30 KiB.
/// 128 KiB gives comfortable headroom for large rooms while still
/// capping memory from a misbehaving peer.
const MAX_GOSSIP_LINE_BYTES: usize = 131_072;

/// Reader: pull JSON lines off the socket, dispatch into the shared
/// inbound channel.  Exits when the stream closes — its task
/// terminates cleanly so the listener can accept the peer again
/// when it reconnects.
///
/// `allowed` holds the DIDs configured at this connection's address. The
/// first frame binds the connection to its sender, which must be one of
/// them; the connection closes on any frame from another sender.
async fn reader_loop(
    socket: TcpStream,
    inbound_tx: mpsc::Sender<CascadeSignal>,
    allowed: HashSet<String>,
) {
    let reader = BufReader::with_capacity(MAX_GOSSIP_LINE_BYTES, socket);
    // Wrap in a length-limited reader so a malicious peer cannot send an
    // unbounded line that grows the String beyond MAX_GOSSIP_LINE_BYTES.
    let mut reader = reader.take(MAX_GOSSIP_LINE_BYTES as u64);
    let mut line = String::new();
    let mut bound: Option<String> = None;
    loop {
        line.clear();
        // Reset the remaining byte limit before each read so every line
        // gets the full allowance.
        reader.set_limit(MAX_GOSSIP_LINE_BYTES as u64);
        match reader.read_line(&mut line).await {
            Ok(0) => return, // peer closed or limit hit without newline
            Ok(_) => {
                // If the line used the entire limit without a newline,
                // the peer sent an oversized frame — close the connection.
                if !line.ends_with('\n') {
                    warn!(
                        "TcpGossip reader: frame exceeds {} bytes without newline, closing",
                        MAX_GOSSIP_LINE_BYTES,
                    );
                    return;
                }
                match serde_json::from_str::<CascadeSignal>(line.trim()) {
                    Ok(signal) => {
                        let sender = signal.sender_did();
                        let accepted = match &bound {
                            Some(did) => did == sender,
                            None => allowed.contains(sender),
                        };
                        if !accepted {
                            warn!(
                                "TcpGossip reader: {} frame from unexpected sender {}, closing",
                                signal.variant_name(),
                                sender
                            );
                            return;
                        }
                        bound.get_or_insert_with(|| sender.to_string());
                        debug!("TcpGossip reader: inbound {} signal", signal.variant_name());
                        if inbound_tx.send(signal).await.is_err() {
                            return; // SfuService dropped the receiver
                        }
                    }
                    Err(e) => {
                        debug!("TcpGossip drop malformed frame: {} -- {:?}", e, line.trim());
                    }
                }
            }
            Err(e) => {
                debug!("TcpGossip reader error: {}", e);
                return;
            }
        }
    }
}

/// Connector: retry-forever loop that maintains an outbound
/// connection to one peer.  When connected, registers an mpsc sender
/// in the shared outbound map; on disconnect, removes itself and
/// retries.
async fn connect_loop(did: String, addr: SocketAddr, outbound: OutboundMap) {
    loop {
        match TcpStream::connect(addr).await {
            Ok(mut socket) => {
                debug!("TcpGossip connected to {} @ {}", did, addr);
                let (tx, mut rx) = mpsc::channel::<CascadeSignal>(64);
                {
                    let mut guard = outbound.lock().await;
                    guard.insert(did.clone(), tx);
                }
                // Drain the channel into the socket; on error, drop the
                // entry and retry.
                while let Some(signal) = rx.recv().await {
                    let mut bytes = match serde_json::to_vec(&signal) {
                        Ok(b) => b,
                        Err(e) => {
                            warn!("TcpGossip serialize error: {}", e);
                            continue;
                        }
                    };
                    bytes.push(b'\n');
                    if let Err(e) = socket.write_all(&bytes).await {
                        debug!("TcpGossip write to {} failed: {}", did, e);
                        break;
                    }
                }
                {
                    let mut guard = outbound.lock().await;
                    guard.remove(&did);
                }
            }
            Err(_) => {
                // Peer not up yet — try again shortly.
            }
        }
        sleep(Duration::from_millis(500)).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::time::timeout;

    fn peer(did: &str, ip: &str) -> GossipPeer {
        GossipPeer {
            did: did.into(),
            addr: SocketAddr::new(ip.parse().unwrap(), 9),
        }
    }

    fn announce(did: &str) -> Vec<u8> {
        let mut frame = serde_json::to_vec(&CascadeSignal::Announce {
            did: did.into(),
            room_id: "n:r".into(),
            participant_count: 1,
            capacity_hint: 4,
        })
        .unwrap();
        frame.push(b'\n');
        frame
    }

    /// A listener for `peers`, a raw connection to it from 127.0.0.1, and
    /// the inbound channel.
    async fn listen(
        peers: Vec<GossipPeer>,
    ) -> (TcpStream, mpsc::Receiver<CascadeSignal>, Arc<TcpGossip>) {
        let port = std::net::TcpListener::bind("127.0.0.1:0")
            .unwrap()
            .local_addr()
            .unwrap()
            .port();
        let addr: SocketAddr = format!("127.0.0.1:{}", port).parse().unwrap();
        let gossip = TcpGossip::start("did:me".into(), 4, addr, peers)
            .await
            .unwrap();
        let rx = gossip.take_inbound().unwrap();
        (TcpStream::connect(addr).await.unwrap(), rx, gossip)
    }

    async fn received(rx: &mut mpsc::Receiver<CascadeSignal>) -> Option<String> {
        timeout(Duration::from_millis(300), rx.recv())
            .await
            .ok()
            .flatten()
            .map(|s| s.sender_did().to_string())
    }

    #[tokio::test]
    async fn a_configured_peer_is_heard_and_bound_to_its_did() {
        let (mut conn, mut rx, _g) =
            listen(vec![peer("did:b", "127.0.0.1"), peer("did:c", "127.0.0.1")]).await;
        conn.write_all(&announce("did:b")).await.unwrap();
        assert_eq!(received(&mut rx).await.as_deref(), Some("did:b"));
        // The connection is did:b's now: did:c, configured at the same
        // address, cannot speak on it.
        conn.write_all(&announce("did:c")).await.unwrap();
        conn.write_all(&announce("did:b")).await.ok();
        assert_eq!(received(&mut rx).await, None);
    }

    #[tokio::test]
    async fn an_unconfigured_did_is_not_heard() {
        let (mut conn, mut rx, _g) = listen(vec![peer("did:b", "127.0.0.1")]).await;
        conn.write_all(&announce("did:evil")).await.unwrap();
        conn.write_all(&announce("did:b")).await.ok();
        assert_eq!(received(&mut rx).await, None);
    }

    #[tokio::test]
    async fn an_address_with_no_configured_peer_is_refused() {
        let (mut conn, mut rx, _g) = listen(vec![peer("did:b", "10.0.0.9")]).await;
        conn.write_all(&announce("did:b")).await.ok();
        assert_eq!(received(&mut rx).await, None);
    }
}
