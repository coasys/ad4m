use crate::graphql::graphql_types::GetFilter;
use crate::graphql::graphql_types::GetValue;
use coasys_juniper::{graphql_value, FieldError, FieldResult};
use futures::Stream;
use futures::StreamExt;
use log::{debug, error, warn};
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

    pub async fn remove_dead_subscribers(&self) {
        let mut subscribers = self.subscribers.lock().await;
        for (_, subscribers_vec) in subscribers.iter_mut() {
            let mut i = 0;
            while i < subscribers_vec.len() {
                if subscribers_vec[i].is_closed() {
                    warn!("Found closed subscriber, removing...");
                    subscribers_vec.remove(i);
                } else {
                    i += 1;
                }
            }
        }
    }

    pub async fn publish(&self, topic: &Topic, message: &Message) {
        let mut subscribers = self.subscribers.lock().await;

        if let Some(subscribers_vec) = subscribers.get_mut(topic) {
            let mut i = 0;
            while i < subscribers_vec.len() {
                let send_res = subscribers_vec[i].send(message.to_owned());
                if send_res.is_err() {
                    warn!("Failed to send message to subscriber: {:?} on topic: {:?}, with subscribers, len: {:?}", send_res, topic, subscribers_vec.len());
                    warn!("Removing subscriber from topic: {:?}", topic);
                    subscribers_vec.remove(i);
                } else {
                    i += 1;
                }
            }
        }
    }
}

pub(crate) async fn subscribe_and_process<
    T: DeserializeOwned + Send + 'static + std::fmt::Debug + GetValue + GetFilter,
>(
    pubsub: Arc<PubSub>,
    topic: Topic,
    filter: Option<String>,
) -> Pin<Box<dyn Stream<Item = FieldResult<T::Value>> + Send>> {
    debug!("Subscribing to topic: {}", topic);
    pubsub.remove_dead_subscribers().await;
    let receiver = pubsub.subscribe(&topic).await;
    let receiver_stream = WatchStream::from_changes(receiver);

    let mapped_stream = receiver_stream.filter_map(move |msg| {
        match serde_json::from_str::<T>(&msg) {
            Ok(data) => {
                if let Some(filter) = &filter {
                    if &data
                        .get_filter()
                        .expect("Could not get filter on T where we expected to filter")
                        != filter
                    {
                        return futures::future::ready(None);
                    }
                }
                let value = data.get_value(); // Get the underlying value using the GetValue trait
                futures::future::ready(Some(Ok(value)))
            }
            Err(e) => {
                let type_name = std::any::type_name::<T>();
                error!("Failed to deserialize pubsub message: {:?}", e);
                error!("Type: {}", type_name);
                error!("Message: {:?}", msg);

                let field_error = FieldError::new(
                    e,
                    graphql_value!({ "type": "INTERNAL_ERROR_COULD_NOT_SERIALIZE" }),
                );
                futures::future::ready(Some(Err(field_error)))
            }
        }
    });

    Box::pin(mapped_stream)
}

lazy_static::lazy_static! {
    static ref GLOBAL_PUB_SUB: Arc<PubSub> = Arc::new(PubSub::new());

    pub static ref AGENT_STATUS_CHANGED_TOPIC: String = "agent-status-changed-topic".to_owned();
    pub static ref APPS_CHANGED: String = "apps-changed".to_owned();
    pub static ref AGENT_UPDATED_TOPIC: String = "agent-updated-topic".to_owned();
    pub static ref EXCEPTION_OCCURRED_TOPIC: String = "exception-occurred-topic".to_owned();
    pub static ref NEIGHBOURHOOD_SIGNAL_TOPIC: String = "neighbourhood-signal-received-topic".to_owned();
    pub static ref PERSPECTIVE_ADDED_TOPIC: String = "perspective-added-topic".to_owned();
    pub static ref PERSPECTIVE_LINK_ADDED_TOPIC: String = "perspective-link-added-topic".to_owned();
    pub static ref PERSPECTIVE_LINK_REMOVED_TOPIC: String = "perspective-link-removed-topic".to_owned();
    pub static ref PERSPECTIVE_LINK_UPDATED_TOPIC: String = "perspective-link-updated-topic".to_owned();
    pub static ref PERSPECTIVE_REMOVED_TOPIC: String = "perspective-removed-topic".to_owned();
    pub static ref PERSPECTIVE_UPDATED_TOPIC: String = "perspective-updated-topic".to_owned();
    pub static ref PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC: String = "perspective-sync-state-change-topic".to_owned();
    pub static ref RUNTIME_MESSAGED_RECEIVED_TOPIC: String = "runtime-messaged-received-topic".to_owned();
    pub static ref RUNTIME_NOTIFICATION_TRIGGERED_TOPIC: String = "runtime-notification-triggered-topic".to_owned();
    pub static ref AI_TRANSCRIPTION_TEXT_TOPIC: String = "ai-transcription-text-topic".to_owned();
    pub static ref AI_MODEL_LOADING_STATUS: String = "ai-model-loading-status".to_owned();
}

pub async fn get_global_pubsub() -> Arc<PubSub> {
    GLOBAL_PUB_SUB.clone()
}
