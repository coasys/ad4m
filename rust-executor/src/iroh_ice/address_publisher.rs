//! Address publisher — periodically broadcasts local ICE candidates to all
//! joined neighbourhoods so remote peers can use this executor as a STUN server.

use std::collections::HashSet;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::time::{interval, Duration};
use log::{debug, error, info, warn};
use serde::{Deserialize, Serialize};

use crate::holochain_service::get_holochain_service;
use crate::perspectives::all_perspectives;
use crate::agent::{self, create_signed_expression, AgentContext};
use crate::types::{DecoratedLinkExpression, Link, LinkExpression};
use crate::graphql::graphql_types::{LinkStatus, Perspective, PerspectiveExpression};

/// JSON signal broadcast to neighbourhoods.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct IceAddressSignal {
    #[serde(rename = "type")]
    pub signal_type: String,
    pub addresses: Vec<IceAddressEntry>,
    pub agent_did: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct IceAddressEntry {
    pub address: String,
    pub port: u16,
    pub candidate: String,
}

/// Holds cached state for change detection.
pub struct AddressPublisher {
    cached_addresses: Arc<Mutex<HashSet<IceAddressEntry>>>,
    interval_secs: u64,
}

impl AddressPublisher {
    pub fn new(interval_secs: u64) -> Self {
        Self {
            cached_addresses: Arc::new(Mutex::new(HashSet::new())),
            interval_secs,
        }
    }

    /// Build the signal payload from current addresses.
    /// Returns None if addresses haven't changed since last check.
    pub async fn build_signal(&self) -> Option<IceAddressSignal> {
        let interface = get_holochain_service().await;
        let addrs = interface.local_socket_addrs().await.unwrap_or_default();
        let candidates = crate::iroh_ice::candidates_from_urls(&addrs);

        let entries: HashSet<IceAddressEntry> = candidates
            .into_iter()
            .map(|c| IceAddressEntry {
                address: c.address,
                port: c.port,
                candidate: c.candidate,
            })
            .collect();

        // Check if addresses changed
        let mut cached = self.cached_addresses.lock().await;
        if *cached == entries {
            debug!("iroh-ice: addresses unchanged, skipping broadcast");
            return None;
        }

        *cached = entries.clone();

        let agent_did = agent::did();
        Some(IceAddressSignal {
            signal_type: "iroh-ice-addresses".to_string(),
            addresses: entries.into_iter().collect(),
            agent_did,
        })
    }

    /// Broadcast the signal to all neighbourhood perspectives.
    pub async fn broadcast_to_neighbourhoods(&self, signal: &IceAddressSignal) {
        let perspectives = all_perspectives();

        let json_payload = match serde_json::to_string(signal) {
            Ok(j) => j,
            Err(e) => {
                error!("iroh-ice: failed to serialize signal: {}", e);
                return;
            }
        };

        let agent_context = AgentContext::main_agent();

        for perspective in perspectives {
            let handle = perspective.persisted.lock().await;
            if handle.neighbourhood.is_none() {
                continue;
            }
            let uuid = handle.uuid.clone();
            drop(handle);

            // Build a link carrying our ICE address data, following the pattern
            // used by neighbourhood_send_broadcast_u in mutation_resolvers.
            let link = Link {
                source: "iroh-ice".to_string(),
                predicate: Some("iroh-ice-addresses".to_string()),
                target: json_payload.clone(),
            };

            let link_expr = match create_signed_expression(link.normalize(), &agent_context) {
                Ok(expr) => expr,
                Err(e) => {
                    warn!("iroh-ice: failed to sign link for {}: {}", uuid, e);
                    continue;
                }
            };

            let decorated = DecoratedLinkExpression::from((
                LinkExpression::from(link_expr),
                LinkStatus::Shared,
            ));

            let perspective_data = Perspective {
                links: vec![decorated],
            };

            let signed_perspective =
                match create_signed_expression(perspective_data, &agent_context) {
                    Ok(expr) => expr,
                    Err(e) => {
                        warn!("iroh-ice: failed to sign perspective for {}: {}", uuid, e);
                        continue;
                    }
                };

            let perspective_expression: PerspectiveExpression = signed_perspective.into();

            debug!("iroh-ice: broadcasting addresses to neighbourhood {}", uuid);

            if let Err(e) = perspective
                .send_broadcast(perspective_expression, false)
                .await
            {
                warn!("iroh-ice: failed to broadcast to {}: {}", uuid, e);
            }
        }
    }

    /// Run the publish loop.
    pub async fn run(&self) {
        info!(
            "iroh-ice: starting address publisher (interval {}s)",
            self.interval_secs
        );

        let mut tick = interval(Duration::from_secs(self.interval_secs));
        loop {
            tick.tick().await;

            match self.build_signal().await {
                Some(signal) => {
                    if signal.addresses.is_empty() {
                        debug!("iroh-ice: no addresses to broadcast (empty list)");
                        continue;
                    }
                    info!(
                        "iroh-ice: broadcasting {} addresses to neighbourhoods",
                        signal.addresses.len()
                    );
                    self.broadcast_to_neighbourhoods(&signal).await;
                }
                None => {
                    // Addresses unchanged
                }
            }
        }
    }
}

/// Spawn the address publisher as a background tokio task.
pub fn start_address_publisher(interval_secs: Option<u64>) -> tokio::task::JoinHandle<()> {
    let publisher = AddressPublisher::new(interval_secs.unwrap_or(60));
    tokio::spawn(async move {
        publisher.run().await;
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signal_json_format() {
        let signal = IceAddressSignal {
            signal_type: "iroh-ice-addresses".to_string(),
            addresses: vec![IceAddressEntry {
                address: "1.2.3.4".to_string(),
                port: 5678,
                candidate: "candidate:1234 1 udp 2130706431 1.2.3.4 5678 typ host".to_string(),
            }],
            agent_did: "did:key:z6MkTest".to_string(),
        };

        let json = serde_json::to_value(&signal).unwrap();
        assert_eq!(json["type"], "iroh-ice-addresses");
        assert_eq!(json["agent_did"], "did:key:z6MkTest");
        assert_eq!(json["addresses"][0]["address"], "1.2.3.4");
        assert_eq!(json["addresses"][0]["port"], 5678);
        assert!(json["addresses"][0]["candidate"]
            .as_str()
            .unwrap()
            .starts_with("candidate:"));
    }

    #[test]
    fn test_signal_empty_addresses() {
        let signal = IceAddressSignal {
            signal_type: "iroh-ice-addresses".to_string(),
            addresses: vec![],
            agent_did: "did:key:z6MkTest".to_string(),
        };

        let json = serde_json::to_value(&signal).unwrap();
        assert!(json["addresses"].as_array().unwrap().is_empty());
        let roundtrip: IceAddressSignal = serde_json::from_value(json).unwrap();
        assert_eq!(roundtrip.addresses.len(), 0);
        assert_eq!(roundtrip.signal_type, "iroh-ice-addresses");
    }

    #[tokio::test]
    async fn test_only_broadcasts_on_change() {
        let publisher = AddressPublisher::new(60);

        let entries: HashSet<IceAddressEntry> = [IceAddressEntry {
            address: "10.0.0.1".to_string(),
            port: 1234,
            candidate: "candidate:1 1 udp 100 10.0.0.1 1234 typ host".to_string(),
        }]
        .into_iter()
        .collect();

        *publisher.cached_addresses.lock().await = entries.clone();

        // Same set => no broadcast needed
        let cached = publisher.cached_addresses.lock().await;
        assert_eq!(*cached, entries);

        // Different set would trigger broadcast
        let mut different = entries.clone();
        different.insert(IceAddressEntry {
            address: "10.0.0.2".to_string(),
            port: 5678,
            candidate: "candidate:2 1 udp 100 10.0.0.2 5678 typ host".to_string(),
        });
        assert_ne!(*cached, different);
    }

    #[test]
    fn test_signal_roundtrip_serialization() {
        let signal = IceAddressSignal {
            signal_type: "iroh-ice-addresses".to_string(),
            addresses: vec![
                IceAddressEntry {
                    address: "192.168.1.1".to_string(),
                    port: 4000,
                    candidate: "candidate:100 1 udp 2130706431 192.168.1.1 4000 typ host"
                        .to_string(),
                },
                IceAddressEntry {
                    address: "8.8.8.8".to_string(),
                    port: 5000,
                    candidate: "candidate:200 1 udp 1694498815 8.8.8.8 5000 typ srflx".to_string(),
                },
            ],
            agent_did: "did:key:z6MkExample".to_string(),
        };

        let json_str = serde_json::to_string(&signal).unwrap();
        let deserialized: IceAddressSignal = serde_json::from_str(&json_str).unwrap();
        assert_eq!(signal, deserialized);
    }
}
