//! Generic signal-based gossip transport.
//!
//! Designed for host applications that already have a signalling
//! channel between nodes — AD4M's telepresence API, libp2p gossipsub,
//! a WebSocket relay, or any other mechanism that can carry JSON
//! payloads point-to-point or broadcast.
//!
//! The transport works through two interfaces:
//!
//! **Outbound**: when the cascade layer calls `send()`, the transport
//! serialises the [`CascadeSignal`] to JSON and invokes a user-
//! provided callback (`SignalSender`).  The host application routes
//! that JSON to the target peer(s) through whatever channel it owns.
//!
//! **Inbound**: the host application calls `inject_signal()` whenever
//! it receives a cascade signal from another node.  The transport
//! deserialises and pushes it onto the shared mpsc channel that
//! `SfuService` consumes.
//!
//! No TCP sockets, no connection management, no retry logic — the
//! host application's signalling layer handles all of that.

use std::sync::{Arc, Mutex as StdMutex};

use async_trait::async_trait;
use log::{debug, warn};
use tokio::sync::mpsc;

use super::super::cascade::CascadeSignal;
use super::{CascadeGossip, GossipTarget};

/// Callback the host application provides for outbound signals.
///
/// `target` — the intended recipient: either a DID string for
///   point-to-point delivery, or `None` for broadcast.
/// `payload` — the JSON-serialised cascade signal.
///
/// The callback should return `Ok(())` once the signal has been
/// handed to the transport.  Delivery remains best-effort.
pub trait SignalSender: Send + Sync + 'static {
    fn send_signal(&self, target: Option<&str>, payload: &str) -> Result<(), String>;
}

/// Blanket impl: any `Fn(Option<&str>, &str) -> Result<(), String>`
/// works as a `SignalSender`.
impl<F> SignalSender for F
where
    F: Fn(Option<&str>, &str) -> Result<(), String> + Send + Sync + 'static,
{
    fn send_signal(&self, target: Option<&str>, payload: &str) -> Result<(), String> {
        self(target, payload)
    }
}

pub struct SignalGossip {
    local_did: String,
    max_participants_per_node: u32,
    sender: Arc<dyn SignalSender>,
    inbound_tx: mpsc::Sender<CascadeSignal>,
    inbound_rx: StdMutex<Option<mpsc::Receiver<CascadeSignal>>>,
}

impl SignalGossip {
    /// Create a new signal-based gossip transport.
    ///
    /// `sender` — the outbound callback that routes JSON payloads
    ///   through the host application's signalling channel.
    pub fn new(
        local_did: String,
        max_participants_per_node: u32,
        sender: Arc<dyn SignalSender>,
    ) -> Arc<Self> {
        let (inbound_tx, inbound_rx) = mpsc::channel::<CascadeSignal>(256);
        Arc::new(Self {
            local_did,
            max_participants_per_node,
            sender,
            inbound_tx,
            inbound_rx: StdMutex::new(Some(inbound_rx)),
        })
    }

    /// Inject an inbound signal received from another node.
    ///
    /// The host application calls this whenever it receives a JSON
    /// payload tagged as a cascade signal (e.g. from a telepresence
    /// callback).  The method deserialises the JSON and pushes the
    /// signal onto the internal channel for `SfuService` to consume.
    ///
    /// Returns `Ok(())` on success, `Err` if the JSON does not parse
    /// as a valid `CascadeSignal` or the internal channel has closed.
    pub fn inject_signal(&self, json_payload: &str) -> Result<(), String> {
        let signal: CascadeSignal = serde_json::from_str(json_payload)
            .map_err(|e| format!("SignalGossip: invalid JSON: {}", e))?;

        let variant = match &signal {
            CascadeSignal::Announce { .. } => "Announce",
            CascadeSignal::Leave { .. } => "Leave",
            CascadeSignal::PipeOffer { .. } => "PipeOffer",
            CascadeSignal::PipeAnswer { .. } => "PipeAnswer",
        };
        debug!("SignalGossip[{}] injected inbound {}", self.local_did, variant);

        self.inbound_tx
            .try_send(signal)
            .map_err(|e| format!("SignalGossip: inbound channel: {}", e))
    }
}

#[async_trait]
impl CascadeGossip for SignalGossip {
    async fn send(&self, target: GossipTarget, signal: CascadeSignal) -> Result<(), String> {
        let json = serde_json::to_string(&signal)
            .map_err(|e| format!("SignalGossip: serialise: {}", e))?;

        let variant = match &signal {
            CascadeSignal::Announce { .. } => "Announce",
            CascadeSignal::Leave { .. } => "Leave",
            CascadeSignal::PipeOffer { .. } => "PipeOffer",
            CascadeSignal::PipeAnswer { .. } => "PipeAnswer",
        };

        match target {
            GossipTarget::Broadcast => {
                debug!(
                    "SignalGossip[{}] broadcast {} ({} bytes)",
                    self.local_did,
                    variant,
                    json.len()
                );
                if let Err(e) = self.sender.send_signal(None, &json) {
                    warn!("SignalGossip broadcast {} failed: {}", variant, e);
                }
            }
            GossipTarget::PeerDid(did) => {
                debug!(
                    "SignalGossip[{}] directed {} to {} ({} bytes)",
                    self.local_did,
                    variant,
                    did,
                    json.len()
                );
                if let Err(e) = self.sender.send_signal(Some(&did), &json) {
                    warn!(
                        "SignalGossip directed {} to {} failed: {}",
                        variant, did, e
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn test_inject_and_take() {
        let counter = Arc::new(AtomicUsize::new(0));
        let c = counter.clone();
        let sender: Arc<dyn SignalSender> = Arc::new(move |_target: Option<&str>, _payload: &str| {
            c.fetch_add(1, Ordering::Relaxed);
            Ok(())
        });

        let gossip = SignalGossip::new("did:test:local".into(), 8, sender);

        // Inject a well-formed signal
        let json = r#"{"type":"sfu-announce","did":"did:test:remote","room_id":"r1","participant_count":2,"capacity_hint":8}"#;
        assert!(gossip.inject_signal(json).is_ok());

        // Take the inbound channel and verify the signal arrived
        let mut rx = gossip.take_inbound().expect("should yield receiver");
        let signal = rx.try_recv().expect("should have one signal");
        match signal {
            CascadeSignal::Announce { did, .. } => assert_eq!(did, "did:test:remote"),
            _ => panic!("expected Announce"),
        }

        // Second take returns None
        assert!(gossip.take_inbound().is_none());

        // Bad JSON returns Err
        assert!(gossip.inject_signal("not json").is_err());
    }

    #[tokio::test]
    async fn test_send_broadcast() {
        let payloads: Arc<StdMutex<Vec<(Option<String>, String)>>> =
            Arc::new(StdMutex::new(Vec::new()));
        let p = payloads.clone();
        let sender: Arc<dyn SignalSender> =
            Arc::new(move |target: Option<&str>, payload: &str| {
                p.lock()
                    .unwrap()
                    .push((target.map(|s| s.to_string()), payload.to_string()));
                Ok(())
            });

        let gossip = SignalGossip::new("did:test:local".into(), 4, sender);

        let signal = CascadeSignal::Leave {
            did: "did:test:local".into(),
            room_id: "r1".into(),
        };
        gossip.send(GossipTarget::Broadcast, signal).await.unwrap();

        let captured = payloads.lock().unwrap();
        assert_eq!(captured.len(), 1);
        assert!(captured[0].0.is_none()); // broadcast → None target
        assert!(captured[0].1.contains("sfu-leave"));
    }

    #[tokio::test]
    async fn test_send_directed() {
        let payloads: Arc<StdMutex<Vec<(Option<String>, String)>>> =
            Arc::new(StdMutex::new(Vec::new()));
        let p = payloads.clone();
        let sender: Arc<dyn SignalSender> =
            Arc::new(move |target: Option<&str>, payload: &str| {
                p.lock()
                    .unwrap()
                    .push((target.map(|s| s.to_string()), payload.to_string()));
                Ok(())
            });

        let gossip = SignalGossip::new("did:test:local".into(), 4, sender);

        let signal = CascadeSignal::Leave {
            did: "did:test:local".into(),
            room_id: "r1".into(),
        };
        gossip
            .send(GossipTarget::PeerDid("did:test:remote".into()), signal)
            .await
            .unwrap();

        let captured = payloads.lock().unwrap();
        assert_eq!(captured.len(), 1);
        assert_eq!(
            captured[0].0.as_deref(),
            Some("did:test:remote")
        );
    }
}
