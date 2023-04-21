use futures::Stream;
use futures::StreamExt;
use juniper::{graphql_value, FieldError, FieldResult};
use log::error;
use serde::de::DeserializeOwned;
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::Arc;
use tokio::sync::watch;
use tokio::sync::Mutex;
use tokio_stream::wrappers::WatchStream;

type Topic = String;
type Message = String;

pub struct PubSub {
    subscribers: Mutex<HashMap<Topic, Vec<watch::Sender<Message>>>>,
}

impl PubSub {
    pub fn new() -> Self {
        Self {
            subscribers: Mutex::new(HashMap::new()),
        }
    }

    pub async fn subscribe(&self, topic: &Topic) -> watch::Receiver<Message> {
        let (tx, rx) = watch::channel("".to_owned());
        let mut subscribers = self.subscribers.lock().await;
        subscribers
            .entry(topic.to_owned())
            .or_insert_with(Vec::new)
            .push(tx);
        rx
    }

    pub async fn publish(&self, topic: &Topic, message: &Message) {
        let subscribers = self.subscribers.lock().await;
        if let Some(subscribers) = subscribers.get(topic) {
            for tx in subscribers {
                let send_res = tx.send(message.to_owned());
                if send_res.is_err() {
                    error!("Failed to send message to subscriber: {:?}", send_res);
                }
            }
        }
    }
}

pub(crate) async fn subscribe_and_process<T: DeserializeOwned + Send + 'static>(
    pubsub: Arc<PubSub>,
    topic: Topic,
) -> Pin<Box<dyn Stream<Item = FieldResult<T>> + Send>> {
    let receiver = pubsub.subscribe(&topic).await;
    let receiver_stream = WatchStream::from_changes(receiver);

    let mapped_stream = receiver_stream.map(|msg| {
        println!("subscribe_and_process: Received message: {:?}", msg);
        match serde_json::from_str::<T>(&msg) {
            Ok(agent_status) => Ok(agent_status),
            Err(e) => Err(FieldError::new(
                e,
                graphql_value!({ "type": "INTERNAL_ERROR_COULD_NOT_SERIALIZE" }),
            )),
        }
    });

    Box::pin(mapped_stream)
}

lazy_static::lazy_static! {
    static ref GLOBAL_PUB_SUB: Arc<PubSub> = Arc::new(PubSub::new());

    pub static ref AGENT_STATUS_CHANGED_TOPIC: String = "agent-status-changed".to_owned();
    pub static ref AGENT_UPDATED_TOPIC: String = "agent-updated".to_owned();
    pub static ref EXCEPTION_OCCURRED_TOPIC: String = "exception-occurred".to_owned();
    pub static ref NEIGHBOURHOOD_SIGNAL_TOPIC: String = "neighbourhood-signal".to_owned();
    pub static ref PERSPECTIVE_ADDED_TOPIC: String = "perspective-added".to_owned();
    pub static ref PERSPECTIVE_LINK_ADDED_TOPIC: String = "perspective-link-added".to_owned();
    pub static ref PERSPECTIVE_LINK_REMOVED_TOPIC: String = "perspective-link-removed".to_owned();
    pub static ref PERSPECTIVE_LINK_UPDATED_TOPIC: String = "perspective-link-updated".to_owned();
    pub static ref PERSPECTIVE_REMOVED_TOPIC: String = "perspective-removed".to_owned();
    pub static ref PERSPECTIVE_UPDATED_TOPIC: String = "perspective-updated".to_owned();
    pub static ref PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC: String = "perspective-sync-state-change".to_owned();
    pub static ref RUNTIME_MESSAGED_RECEIVED_TOPIC: String = "runtime-messaged-received".to_owned();
}

pub async fn get_global_pubsub() -> Arc<PubSub> {
    GLOBAL_PUB_SUB.clone()
}
