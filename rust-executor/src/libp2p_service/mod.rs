use std::hash::{Hash, Hasher};

use libp2p::{
    futures::StreamExt,
    gossipsub::{self, MessageAuthenticity},
    noise,
    request_response::{self, json, ProtocolSupport},
    swarm::{NetworkBehaviour, Swarm, SwarmEvent},
    tcp,
    yamux,
    mdns,
    PeerId,
    StreamProtocol,
    //NetworkBehaviour as _,
};
//use libp2p::swarm::derive::NetworkBehaviour;
use crate::{agent, types::PerspectiveExpression, graphql::graphql_types::OnlineAgent};
use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

lazy_static! {
    static ref LIBP2P_SERVICE: Arc<Mutex<Option<Libp2pService>>> = Arc::new(Mutex::new(None));
}

// Topic for each neighbourhood's telepresence signals
const TELEPRESENCE_TOPIC_PREFIX: &str = "/ad4m/telepresence/";
const REQUEST_RESPONSE_PROTOCOL: StreamProtocol = StreamProtocol::new("/ad4m/telepresence/1.0.0");

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TelepresenceMessage {
    OnlineStatusRequest,
    OnlineStatusResponse(PerspectiveExpression),
    Signal(PerspectiveExpression),
}

#[derive(Debug)]
pub enum MyBehaviourEvent {
    Gossipsub(gossipsub::Event),
    RequestResponse(request_response::Event<TelepresenceMessage, TelepresenceMessage>),
    Mdns(mdns::Event),
}

impl From<gossipsub::Event> for MyBehaviourEvent {
    fn from(event: gossipsub::Event) -> Self {
        MyBehaviourEvent::Gossipsub(event)
    }
}

impl From<request_response::Event<TelepresenceMessage, TelepresenceMessage>> for MyBehaviourEvent {
    fn from(event: request_response::Event<TelepresenceMessage, TelepresenceMessage>) -> Self {
        MyBehaviourEvent::RequestResponse(event)
    }
}

impl From<mdns::Event> for MyBehaviourEvent {
    fn from(event: mdns::Event) -> Self {
        MyBehaviourEvent::Mdns(event)
    }
}

#[derive(NetworkBehaviour)]
#[behaviour(out_event = "MyBehaviourEvent")]
struct MyBehaviour {
    gossipsub: gossipsub::Behaviour,
    request_response: json::Behaviour<TelepresenceMessage, TelepresenceMessage>,
    mdns: mdns::tokio::Behaviour,
}

#[derive(Clone)]
pub struct Libp2pService {
    swarm: Arc<Mutex<Swarm<MyBehaviour>>>,
    online_agents: Arc<Mutex<HashMap<String, HashMap<String, OnlineAgent>>>>, // neighbourhood_id -> agent_did -> OnlineAgent
    bootstrap_nodes: Vec<String>,
    signal_callbacks:
        Arc<Mutex<HashMap<String, Vec<Box<dyn Fn(PerspectiveExpression) + Send + Sync>>>>>,
    peer_to_did: Arc<Mutex<HashMap<PeerId, String>>>, // Map peer IDs to agent DIDs
    topic_to_neighbourhood: Arc<Mutex<HashMap<gossipsub::TopicHash, String>>>, // Map topic hashes to neighbourhood IDs
    my_online_status: Arc<Mutex<HashMap<String, PerspectiveExpression>>>, // neighbourhood_id -> status
    known_peers: Arc<Mutex<Vec<PeerId>>>, // neighbourhood_id -> list of peer IDs
}

impl Libp2pService {
    pub async fn new(bootstrap_nodes: Vec<String>) -> Result<Self> {
        // Create a swarm with a custom network behaviour
        let swarm = libp2p::SwarmBuilder::with_new_identity()
            .with_tokio()
            .with_tcp(
                tcp::Config::default(),
                noise::Config::new,
                yamux::Config::default,
            )?
            .with_quic()
            .with_behaviour(|key| {
                // Set up gossipsub
                let message_id_fn = |message: &gossipsub::Message| {
                    let mut s = std::collections::hash_map::DefaultHasher::new();
                    message.source.hash(&mut s);
                    message.data.hash(&mut s);
                    gossipsub::MessageId::from(s.finish().to_string())
                };

                let gossipsub_config = gossipsub::ConfigBuilder::default()
                    .heartbeat_interval(std::time::Duration::from_millis(200))
                    .validation_mode(gossipsub::ValidationMode::Strict)
                    .message_id_fn(message_id_fn)
                    // .mesh_outbound_min(1)
                    // .mesh_n_low(2)  // Lower bound for mesh size
                    // .mesh_n(2)  // Minimum number of peers in mesh
                    // .mesh_n_high(3)  // Upper bound for mesh size
                    // .gossip_lazy(1)  // Number of peers to gossip to
                    .build()
                    .map_err(anyhow::Error::msg)?;

                let gossipsub = gossipsub::Behaviour::new(
                    MessageAuthenticity::Signed(key.clone()),
                    gossipsub_config,
                )?;

                // Set up request-response
                let request_response = json::Behaviour::new(
                    [(REQUEST_RESPONSE_PROTOCOL, ProtocolSupport::Full)],
                    request_response::Config::default(),
                );

                // Set up mDNS
                let mdns = mdns::tokio::Behaviour::new(mdns::Config::default(), PeerId::from_public_key(&key.public()))?;

                Ok(MyBehaviour {
                    gossipsub,
                    request_response,
                    mdns,
                })
            })?
            .build();

        Ok(Self {
            swarm: Arc::new(Mutex::new(swarm)),
            online_agents: Arc::new(Mutex::new(HashMap::new())),
            bootstrap_nodes,
            signal_callbacks: Arc::new(Mutex::new(HashMap::new())),
            peer_to_did: Arc::new(Mutex::new(HashMap::new())),
            topic_to_neighbourhood: Arc::new(Mutex::new(HashMap::new())),
            my_online_status: Arc::new(Mutex::new(HashMap::new())),
            known_peers: Arc::new(Mutex::new(Vec::new())),
        })
    }

    pub async fn start(&self) -> Result<()> {
        let mut swarm = self.swarm.lock().await;

        // Connect to bootstrap nodes
        for node in &self.bootstrap_nodes {
            if let Ok(addr) = node.parse::<libp2p::core::multiaddr::Multiaddr>() {
                (*swarm).dial(addr)?;
            }
        }

        // Start listening on all interfaces
        (*swarm).listen_on("/ip4/0.0.0.0/tcp/0".parse()?)?;

        // Start processing events
        let swarm_clone = self.swarm.clone();
        let signal_callbacks_clone = self.signal_callbacks.clone();
        //let peer_to_did_clone = self.peer_to_did.clone();
        let topic_to_neighbourhood_clone = self.topic_to_neighbourhood.clone();
        let my_online_status_clone = self.my_online_status.clone();
        let online_agents_clone = self.online_agents.clone();
        let self_clone = self.clone();

        tokio::spawn(async move {
            loop {
                let mut swarm = swarm_clone.lock().await;
                let sleep = tokio::time::sleep(tokio::time::Duration::from_millis(500));
                tokio::pin!(sleep);

                tokio::select! {
                    Some(event) = swarm.next() => {
                        match event {
                            SwarmEvent::Behaviour(MyBehaviourEvent::Gossipsub(
                                gossipsub::Event::Message {
                                    message,
                                    ..
                                },
                            )) => {
                                let topic_hash = message.topic;
                                let mapping = topic_to_neighbourhood_clone.lock().await;
                                if let Some(neighbourhood_id) = mapping.get(&topic_hash) {
                                    if let Ok(msg) = serde_json::from_slice::<TelepresenceMessage>(&message.data) {
                                        match msg {
                                            TelepresenceMessage::Signal(payload) => {
                                                let callbacks = signal_callbacks_clone.lock().await;
                                                if let Some(neighbourhood_callbacks) = callbacks.get(neighbourhood_id) {
                                                    for callback in neighbourhood_callbacks {
                                                        callback(payload.clone());
                                                    }
                                                }
                                            }
                                            _ => {} // Other message types handled by request-response
                                        }
                                    }
                                }
                            }
                            SwarmEvent::Behaviour(MyBehaviourEvent::RequestResponse(
                                request_response::Event::Message {
                                    message: request_response::Message::Request { request, channel, .. },
                                    peer: peer_id,
                                    ..
                                },
                            )) => {
                                match request {
                                    TelepresenceMessage::OnlineStatusRequest => {
                                        // Get the neighbourhood ID from the topic
                                        let topic = swarm.behaviour().gossipsub.topics().next()
                                            .expect("Should have at least one topic");
                                        let neighbourhood_id = topic_to_neighbourhood_clone.lock().await
                                            .get(topic)
                                            .cloned()
                                            .expect("Topic should be mapped to a neighbourhood");

                                        if let Some(status) = my_online_status_clone.lock().await.get(&neighbourhood_id) {
                                            if let Err(e) = swarm.behaviour_mut().request_response.send_response(
                                                channel,
                                                TelepresenceMessage::OnlineStatusResponse(status.clone()),
                                            ) {
                                                log::error!("Failed to send online status response: {:?}", e);
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            SwarmEvent::Behaviour(MyBehaviourEvent::RequestResponse(
                                request_response::Event::Message {
                                    message: request_response::Message::Response { response, .. },
                                    peer: peer_id,
                                    ..
                                },
                            )) => {
                                if let TelepresenceMessage::OnlineStatusResponse(status) = response {
                                    log::info!("Received online status response from peer: {}", peer_id);
                                    // Get the neighbourhood ID from the topic
                                    let topic = swarm.behaviour().gossipsub.topics().next()
                                        .expect("Should have at least one topic");
                                    log::info!("Topic: {}", topic);
                                    let neighbourhood_id = topic_to_neighbourhood_clone.lock().await
                                        .get(topic)
                                        .cloned()
                                        .expect("Topic should be mapped to a neighbourhood");
                                    log::info!("Neighbourhood ID: {}", neighbourhood_id);
                                    let mut online_agents = online_agents_clone.lock().await;
                                    log::info!("Online agents: {:?}", online_agents);
                                    let neighbourhood_agents = online_agents.entry(neighbourhood_id).or_insert_with(HashMap::new);
                                    log::info!("Neighbourhood agents: {:?}", neighbourhood_agents);
                                    let agent_did = agent::did();
                                    
                                    neighbourhood_agents.insert(
                                        agent_did.clone(),
                                        OnlineAgent {
                                            did: agent_did,
                                            status: status.into(),
                                        },
                                    );
                                    log::info!("Inserted agent: {:?}", neighbourhood_agents);
                                }
                            }
                            SwarmEvent::Behaviour(MyBehaviourEvent::Mdns(mdns::Event::Discovered(list))) => {
                                for (peer_id, multiaddr) in list {
                                    log::info!("mDNS discovered peer {} at {}", peer_id, multiaddr);
                                    if let Err(e) = swarm.dial(multiaddr) {
                                        log::error!("Failed to dial mDNS peer: {}", e);
                                    } else {
                                        log::info!("Dialed mDNS peer: {}", peer_id);
                                        self_clone.known_peers.lock().await.push(peer_id);
                                    }
                                }
                            }
                            SwarmEvent::Behaviour(MyBehaviourEvent::Mdns(mdns::Event::Expired(list))) => {
                                for (peer_id, multiaddr) in list {
                                    log::info!("mDNS peer {} at {} expired", peer_id, multiaddr);
                                }
                            }
                            SwarmEvent::NewListenAddr { address, .. } => {
                                log::info!("Listening on: {}", address);
                            }
                            SwarmEvent::Behaviour(MyBehaviourEvent::Gossipsub(gossipsub::Event::GossipsubNotSupported { peer_id })) => {
                                log::info!("Peer {} doesn't support gossip", peer_id);
                            }
                            SwarmEvent::Behaviour(MyBehaviourEvent::Gossipsub(gossipsub::Event::Subscribed { peer_id, topic })) => {
                                log::info!("Peer {} grafted to topic {}", peer_id, topic);
                            }
                            SwarmEvent::Behaviour(MyBehaviourEvent::Gossipsub(gossipsub::Event::Unsubscribed { peer_id, topic })) => {
                                log::info!("Peer {} pruned from topic {}", peer_id, topic);
                            }
                            _ => {}
                        }
                    }
                    _ = &mut sleep => {
                        // Timeout reached, release the lock
                        drop(swarm);
                        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
                        continue;
                    }
                }
            }
        });

        Ok(())
    }

    pub async fn subscribe_to_neighbourhood(&self, neighbourhood_id: String) -> Result<()> {
        log::info!("Subscribing to neighbourhood: {}", neighbourhood_id);
        let topic = gossipsub::IdentTopic::new(format!("{}{}", TELEPRESENCE_TOPIC_PREFIX, neighbourhood_id));
        let topic_hash = topic.hash();
        
        let mut swarm = self.swarm.lock().await;
        swarm.behaviour_mut().gossipsub.subscribe(&topic)?;
        for peer_id in self.known_peers.lock().await.iter() {
            swarm.behaviour_mut().gossipsub.add_explicit_peer(peer_id);
        }
        
        // Store the mapping
        let mut mapping = self.topic_to_neighbourhood.lock().await;
        mapping.insert(topic_hash, neighbourhood_id);

        // Log subscription info
        let peers = swarm.behaviour().gossipsub.all_peers().collect::<Vec<_>>();
        log::info!("Subscribed to topic {} with {} peers", topic, peers.len());
        
        // Publish a dummy message to trigger mesh re-evaluation
        let dummy_data = b"mesh_eval";
        let mut result = swarm.behaviour_mut().gossipsub.publish(topic.clone(), dummy_data);
        let mut retries = 0;
        while result.is_err() && retries < 5 {
            let error = result.err().unwrap();
            log::error!("Failed to publish dummy message: {}", error);
            log::error!("Retrying...");
            // Wait a bit for the mesh to form
            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
            result = swarm.behaviour_mut().gossipsub.publish(topic.clone(), dummy_data);
            retries += 1;
        }
        
        // Release the lock to allow mesh formation
        drop(swarm);
        
        if result.is_err() {
            log::error!("Failed to publish dummy message after 5 retries");
        } else {
            log::info!("Published dummy message to trigger mesh re-evaluation");
        }

        Ok(())
    }

    pub async fn set_online_status(
        &self,
        neighbourhood_id: String,
        status: PerspectiveExpression,
    ) {        
        // Store our status for this neighbourhood
        self.my_online_status.lock().await.insert(neighbourhood_id.clone(), status.clone());
    }

    pub async fn get_online_agents(&self, neighbourhood_id: String) -> Result<Vec<OnlineAgent>> {
        log::info!("Getting online agents for neighbourhood: {}", neighbourhood_id);
        
        // Clear existing agents for this neighbourhood
        {
            let mut online_agents = self.online_agents.lock().await;
            online_agents.remove(&neighbourhood_id);
        }
        log::info!("Removed existing agents for neighbourhood: {}", neighbourhood_id);

        // Request status from all others in the neighbourhood
        let topic = gossipsub::IdentTopic::new(format!("{}{}", TELEPRESENCE_TOPIC_PREFIX, neighbourhood_id));
        let request = TelepresenceMessage::OnlineStatusRequest;
        let data = serde_json::to_vec(&request)?;

        log::info!("Publishing online status request for neighbourhood: {}", neighbourhood_id);

        // Only lock swarm for publishing
        {
            let mut swarm = self.swarm.lock().await;
            let peers = swarm.behaviour().gossipsub.all_peers().collect::<Vec<_>>();
            log::info!("Current peers for topic {}: {:?}", topic, peers);
            swarm.behaviour_mut().gossipsub.publish(topic, data)?;
        }
        
        log::info!("Published online status request for neighbourhood: {}", neighbourhood_id);
        // Wait for 3 seconds to collect responses
        tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;

        // Return whatever agents we have in the map
        let online_agents = self.online_agents.lock().await;
        let agents = online_agents.get(&neighbourhood_id)
            .map(|agents| agents.values().cloned().collect())
            .unwrap_or_default();

        Ok(agents)
    }

    pub async fn send_signal(
        &self,
        neighbourhood_id: String,
        remote_agent_did: String,
        payload: PerspectiveExpression,
    ) -> Result<()> {
        let topic = gossipsub::IdentTopic::new(format!(
            "{}{}",
            TELEPRESENCE_TOPIC_PREFIX, neighbourhood_id
        ));
        let message = TelepresenceMessage::Signal(payload);
        let data = serde_json::to_vec(&message)?;

        let mut swarm = self.swarm.lock().await;
        swarm.behaviour_mut().gossipsub.publish(topic, data)?;

        Ok(())
    }

    pub async fn send_broadcast(
        &self,
        neighbourhood_id: String,
        payload: PerspectiveExpression,
    ) -> Result<()> {
        let topic = gossipsub::IdentTopic::new(format!(
            "{}{}",
            TELEPRESENCE_TOPIC_PREFIX, neighbourhood_id
        ));
        let message = TelepresenceMessage::Signal(payload);
        let data = serde_json::to_vec(&message)?;

        let mut swarm = self.swarm.lock().await;
        swarm.behaviour_mut().gossipsub.publish(topic, data)?;

        Ok(())
    }

    pub async fn register_signal_callback<F>(
        &self,
        neighbourhood_id: String,
        callback: F,
    )
    where
        F: Fn(PerspectiveExpression) + Send + Sync + 'static,
    {
        let mut callbacks = self.signal_callbacks.lock().await;
        let neighbourhood_callbacks = callbacks
            .entry(neighbourhood_id.to_string())
            .or_insert_with(Vec::new);
        neighbourhood_callbacks.push(Box::new(callback));
    }

    pub async fn init_global_instance(bootstrap_nodes: Vec<String>) -> Result<()> {
        let service = Libp2pService::new(bootstrap_nodes).await?;
        service.start().await?;

        let mut global_service = LIBP2P_SERVICE.lock().await;
        *global_service = Some(service);

        Ok(())
    }

    pub async fn global_instance() -> Result<Libp2pService> {
        LIBP2P_SERVICE
            .lock()
            .await
            .clone()
            .ok_or(anyhow!("Libp2p service not initialized"))
    }

    pub async fn with_global_instance<F, R>(func: F) -> Result<R>
    where
        F: FnOnce(&Libp2pService) -> R,
    {
        let global_instance_arc = LIBP2P_SERVICE.lock().await;
        let service_ref = global_instance_arc
            .as_ref()
            .ok_or(anyhow!("Libp2p service not initialized"))?;
        Ok(func(service_ref))
    }

    pub async fn with_mutable_global_instance<F, R>(func: F) -> Result<R>
    where
        F: FnOnce(&mut Libp2pService) -> R,
    {
        let mut global_instance_arc = LIBP2P_SERVICE.lock().await;
        let service_mut = global_instance_arc
            .as_mut()
            .ok_or(anyhow!("Libp2p service not initialized"))?;
        Ok(func(service_mut))
    }
}
