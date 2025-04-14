use std::{
    hash::{Hash, Hasher},
};

use libp2p::{
    core::upgrade,
    futures::StreamExt,
    swarm::{Swarm, SwarmEvent, NetworkBehaviour},
    Transport, PeerId,
    gossipsub::{self, IdentTopic, MessageAuthenticity, Event as GossipsubEvent},
    request_response::{self, ProtocolSupport, json, Event as RequestResponseEvent, Message as RequestResponseMessage},
    noise, tcp, yamux,
    StreamProtocol,
    //NetworkBehaviour as _,
};
//use libp2p::swarm::derive::NetworkBehaviour;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use anyhow::{Result, anyhow};
use crate::types::Expression;
use std::io;

// Define PerspectiveExpression as an alias for Expression<serde_json::Value>
pub type PerspectiveExpression = Expression<serde_json::Value>;

// Topic for each neighbourhood's telepresence signals
const TELEPRESENCE_TOPIC_PREFIX: &str = "/ad4m/telepresence/";
const REQUEST_RESPONSE_PROTOCOL: StreamProtocol = StreamProtocol::new("/ad4m/telepresence/1.0.0");

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TelepresenceMessage {
    OnlineStatusRequest,
    OnlineStatusResponse(PerspectiveExpression),
    Signal(PerspectiveExpression),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OnlineAgent {
    pub did: String,
    pub status: PerspectiveExpression,
}

#[derive(Debug)]
pub enum MyBehaviourEvent {
    Gossipsub(gossipsub::Event),
    RequestResponse(request_response::Event<TelepresenceMessage, TelepresenceMessage>),
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

#[derive(NetworkBehaviour)]
#[behaviour(out_event = "MyBehaviourEvent")]
struct MyBehaviour {
    gossipsub: gossipsub::Behaviour,
    request_response: json::Behaviour<TelepresenceMessage, TelepresenceMessage>,
}

pub struct Libp2pService {
    swarm: Arc<Mutex<Swarm<MyBehaviour>>>,
    online_agents: Arc<Mutex<HashMap<String, HashMap<String, OnlineAgent>>>>, // neighbourhood_id -> agent_did -> OnlineAgent
    bootstrap_nodes: Vec<String>,
    signal_callbacks: Arc<Mutex<HashMap<String, Vec<Box<dyn Fn(PerspectiveExpression) + Send + Sync>>>>>,
    peer_to_did: Arc<Mutex<HashMap<PeerId, String>>>, // Map peer IDs to agent DIDs
}

impl Libp2pService {
    pub async fn new(bootstrap_nodes: Vec<String>) -> Result<Self> {
        // Create a swarm with a custom network behaviour
        let swarm = libp2p::SwarmBuilder::with_new_identity()
            .with_tokio()
            .with_tcp(
                tcp::Config::default(),
                noise::Config::new,
                yamux::Config::default
            )?
            .with_quic()
            .with_behaviour(|key| {
                // Set up gossipsub
                let message_id_fn = |message: &gossipsub::Message| {
                    let mut s = std::collections::hash_map::DefaultHasher::new();
                    message.data.hash(&mut s);
                    gossipsub::MessageId::from(s.finish().to_string())
                };

                let gossipsub_config = gossipsub::ConfigBuilder::default()
                    .heartbeat_interval(std::time::Duration::from_secs(1))
                    .validation_mode(gossipsub::ValidationMode::Strict)
                    .message_id_fn(message_id_fn)
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

                Ok(MyBehaviour {
                    gossipsub,
                    request_response,
                })
            })?
            .build();

        Ok(Self {
            swarm: Arc::new(Mutex::new(swarm)),
            online_agents: Arc::new(Mutex::new(HashMap::new())),
            bootstrap_nodes,
            signal_callbacks: Arc::new(Mutex::new(HashMap::new())),
            peer_to_did: Arc::new(Mutex::new(HashMap::new())),
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
        let online_agents_clone = self.online_agents.clone();
        let signal_callbacks_clone = self.signal_callbacks.clone();
        let peer_to_did_clone = self.peer_to_did.clone();

        tokio::spawn(async move {
            let mut swarm = swarm_clone.lock().await;
            
            while let Some(event) = swarm.next().await {
                match event {
                    SwarmEvent::Behaviour(MyBehaviourEvent::Gossipsub(gossipsub::Event::Message { 
                        message, 
                        propagation_source: peer_id,
                        ..
                    })) => {
                        // Extract neighbourhood_id from topic
                        let topic_str = message.topic.to_string();
                        if let Some(neighbourhood_id) = topic_str.strip_prefix(TELEPRESENCE_TOPIC_PREFIX) {
                            if let Ok(msg) = serde_json::from_slice::<TelepresenceMessage>(&message.data) {
                                match msg {
                                    TelepresenceMessage::Signal(payload) => {
                                        let callbacks = signal_callbacks_clone.lock().await;
                                        if let Some(neighbourhood_callbacks) = callbacks.get(neighbourhood_id) {
                                            for callback in neighbourhood_callbacks {
                                                callback(payload.clone());
                                            }
                                        }
                                    },
                                    _ => {} // Other message types handled by request-response
                                }
                            }
                        }
                    },
                    SwarmEvent::Behaviour(MyBehaviourEvent::RequestResponse(request_response::Event::Message { 
                        message: request_response::Message::Request { request, channel, .. },
                        peer: peer_id,
                        ..
                    })) => {
                        match request {
                            TelepresenceMessage::OnlineStatusRequest => {
                                let online_agents = online_agents_clone.lock().await;
                                let peer_to_did = peer_to_did_clone.lock().await;
                                
                                // TODO: Get neighbourhood_id from somewhere
                                let neighbourhood_id = "placeholder";
                                
                                if let Some(agents) = online_agents.get(neighbourhood_id) {
                                    if let Some(agent_did) = peer_to_did.get(&peer_id) {
                                        if let Some(agent) = agents.get(agent_did) {
                                            swarm.behaviour_mut().request_response.send_response(channel, TelepresenceMessage::OnlineStatusResponse(agent.status.clone()))
                                                .expect("Failed to send response");
                                        }
                                    }
                                }
                            },
                            _ => {}
                        }
                    },
                    SwarmEvent::Behaviour(MyBehaviourEvent::RequestResponse(request_response::Event::Message { 
                        message: request_response::Message::Response { response, .. },
                        peer: peer_id,
                        ..
                    })) => {
                        if let TelepresenceMessage::OnlineStatusResponse(status) = response {
                            // Update the agent's status in our local map
                            let mut online_agents = online_agents_clone.lock().await;
                            let peer_to_did = peer_to_did_clone.lock().await;
                            
                            // TODO: Get neighbourhood_id from somewhere
                            let neighbourhood_id = "placeholder";
                            
                            if let Some(agent_did) = peer_to_did.get(&peer_id) {
                                let neighbourhood_agents = online_agents.entry(neighbourhood_id.to_string())
                                    .or_insert_with(HashMap::new);
                                
                                neighbourhood_agents.insert(agent_did.clone(), OnlineAgent {
                                    did: agent_did.clone(),
                                    status,
                                });
                            }
                        }
                    },
                    _ => {}
                }
            }
        });

        Ok(())
    }

    pub async fn set_online_status(&self, neighbourhood_id: &str, status: PerspectiveExpression) -> Result<()> {
        let mut online_agents = self.online_agents.lock().await;
        // TODO: Get agent DID from somewhere
        let agent_did = "placeholder_did".to_string();
        
        let neighbourhood_agents = online_agents.entry(neighbourhood_id.to_string())
            .or_insert_with(HashMap::new);
        
        neighbourhood_agents.insert(agent_did.clone(), OnlineAgent {
            did: agent_did,
            status,
        });

        // Subscribe to the neighbourhood's telepresence topic if not already subscribed
        let topic = gossipsub::IdentTopic::new(format!("{}{}", TELEPRESENCE_TOPIC_PREFIX, neighbourhood_id));
        let mut swarm = self.swarm.lock().await;
        swarm.behaviour_mut().gossipsub.subscribe(&topic)?;

        Ok(())
    }

    pub async fn get_online_agents(&self, neighbourhood_id: &str) -> Result<Vec<OnlineAgent>> {
        // First get local agents
        let mut online_agents = self.online_agents.lock().await;
        let neighbourhood_agents = online_agents.entry(neighbourhood_id.to_string())
            .or_insert_with(HashMap::new);
        let mut agents = neighbourhood_agents.values().cloned().collect::<Vec<_>>();

        // Then request status from all others in the neighbourhood
        let topic = gossipsub::IdentTopic::new(format!("{}{}", TELEPRESENCE_TOPIC_PREFIX, neighbourhood_id));
        let request = TelepresenceMessage::OnlineStatusRequest;
        let data = serde_json::to_vec(&request)?;
        
        let mut swarm = self.swarm.lock().await;
        swarm.behaviour_mut().gossipsub.publish(topic, data)?;

        // TODO: Wait for responses and add them to agents list
        // This would require implementing a timeout and response handling

        Ok(agents)
    }

    pub async fn send_signal(&self, neighbourhood_id: &str, remote_agent_did: &str, payload: PerspectiveExpression) -> Result<()> {
        let topic = gossipsub::IdentTopic::new(format!("{}{}", TELEPRESENCE_TOPIC_PREFIX, neighbourhood_id));
        let message = TelepresenceMessage::Signal(payload);
        let data = serde_json::to_vec(&message)?;
        
        let mut swarm = self.swarm.lock().await;
        swarm.behaviour_mut().gossipsub.publish(topic, data)?;

        Ok(())
    }

    pub async fn send_broadcast(&self, neighbourhood_id: &str, payload: PerspectiveExpression) -> Result<()> {
        let topic = gossipsub::IdentTopic::new(format!("{}{}", TELEPRESENCE_TOPIC_PREFIX, neighbourhood_id));
        let message = TelepresenceMessage::Signal(payload);
        let data = serde_json::to_vec(&message)?;
        
        let mut swarm = self.swarm.lock().await;
        swarm.behaviour_mut().gossipsub.publish(topic, data)?;

        Ok(())
    }

    pub async fn register_signal_callback<F>(&self, neighbourhood_id: &str, callback: F) -> Result<()>
    where
        F: Fn(PerspectiveExpression) + Send + Sync + 'static,
    {
        let mut callbacks = self.signal_callbacks.lock().await;
        let neighbourhood_callbacks = callbacks.entry(neighbourhood_id.to_string())
            .or_insert_with(Vec::new);
        neighbourhood_callbacks.push(Box::new(callback));
        Ok(())
    }
} 