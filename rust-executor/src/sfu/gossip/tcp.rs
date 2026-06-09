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
//! Discovery is static — the peer list is configuration provided at
//! startup.  The transport doesn't try to maintain global mesh
//! topology; if a peer goes down its outbound TcpStream errors and
//! sends silently drop until the operator restarts.  That's
//! acceptable for this layer because cascade decisions tolerate
//! stale views (`pick_redirect_node` falls back gracefully).

use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex as StdMutex};
use std::time::Duration;

use async_trait::async_trait;
use log::{debug, info, warn};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
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

        // Accept-loop: every inbound TCP stream spawns its own reader.
        {
            let inbound_tx = inbound_tx.clone();
            tasks.push(tokio::spawn(async move {
                loop {
                    match listener.accept().await {
                        Ok((socket, peer_addr)) => {
                            debug!("TcpGossip accept from {}", peer_addr);
                            let tx = inbound_tx.clone();
                            tokio::spawn(reader_loop(socket, tx));
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
        match target {
            GossipTarget::Broadcast => {
                for tx in outbound.values() {
                    let _ = tx.try_send(signal.clone());
                }
            }
            GossipTarget::PeerDid(did) => {
                if let Some(tx) = outbound.get(&did) {
                    let _ = tx.try_send(signal);
                } else {
                    debug!(
                        "TcpGossip[{}] no outbound for {} — dropping",
                        self.local_did, did
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

/// Reader: pull JSON lines off the socket, dispatch into the shared
/// inbound channel.  Exits when the stream closes — its task
/// terminates cleanly so the listener can accept the peer again
/// when it reconnects.
async fn reader_loop(socket: TcpStream, inbound_tx: mpsc::Sender<CascadeSignal>) {
    let mut reader = BufReader::new(socket);
    let mut line = String::new();
    loop {
        line.clear();
        match reader.read_line(&mut line).await {
            Ok(0) => return, // peer closed
            Ok(_) => match serde_json::from_str::<CascadeSignal>(line.trim()) {
                Ok(signal) => {
                    if inbound_tx.send(signal).await.is_err() {
                        return; // SfuService dropped the receiver
                    }
                }
                Err(e) => {
                    debug!("TcpGossip drop malformed frame: {} -- {:?}", e, line.trim());
                }
            },
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
