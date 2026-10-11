use crate::events::Event;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;
use std::sync::LazyLock;
use tokio::sync::broadcast;
use tokio::sync::Mutex;

type Topic = String;
type Message = String;

pub struct PubSub {
    /// String topics. One is left: [`PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC`].
    subscribers: Mutex<HashMap<Topic, broadcast::Sender<Message>>>,
    /// Typed events, every topic on one channel: each socket decides per event.
    events: broadcast::Sender<Arc<Event>>,
}

impl PubSub {
    pub fn new() -> Self {
        Self {
            subscribers: Mutex::new(HashMap::new()),
            events: broadcast::channel(10000).0,
        }
    }

    /// Sends `event` to every event stream. Takes no lock and needs no runtime, so sync
    /// and async publishers call the same function.
    pub fn publish_event(&self, event: Event) {
        let _ = self.events.send(Arc::new(event)); // Ignore if no receivers
    }

    /// Every typed event published from now on.
    pub fn subscribe_events(&self) -> broadcast::Receiver<Arc<Event>> {
        self.events.subscribe()
    }

    pub async fn subscribe(&self, topic: &Topic) -> broadcast::Receiver<Message> {
        let mut subscribers = self.subscribers.lock().await;
        let sender = subscribers
            .entry(topic.to_owned())
            .or_insert_with(|| broadcast::channel(10000).0);
        sender.subscribe()
    }

    pub async fn publish(&self, topic: &Topic, message: &Message) {
        let subscribers = self.subscribers.lock().await;
        if let Some(sender) = subscribers.get(topic) {
            let _ = sender.send(message.clone()); // Ignore if no receivers
        }
    }
}

lazy_static::lazy_static! {
    static ref GLOBAL_PUB_SUB: Arc<PubSub> = Arc::new(PubSub::new());

    /// The one string topic: live query updates. They go out to the perspective's owners
    /// (`events_ws::matches_perspective_owner`) until live queries belong to a connection
    /// (#1207), and then become a typed event to `Audience::Connection`.
    pub static ref PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC: String = "perspective-query-subscription-topic".to_owned();
}

/// Per-user dirty set for batched credit change notifications.
/// When credits are mutated, the affected user's email is inserted.
/// A background flush loop periodically drains the set and publishes
/// updated HostingUserInfo only for the affected users.
pub static DIRTY_CREDIT_USERS: LazyLock<std::sync::Mutex<HashSet<String>>> =
    LazyLock::new(|| std::sync::Mutex::new(HashSet::new()));

pub fn mark_credits_dirty(email: &str) {
    if let Ok(mut set) = DIRTY_CREDIT_USERS.lock() {
        set.insert(email.to_owned());
    }
}

pub async fn get_global_pubsub() -> Arc<PubSub> {
    GLOBAL_PUB_SUB.clone()
}

pub fn get_global_pubsub_sync() -> Arc<PubSub> {
    GLOBAL_PUB_SUB.clone()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::events::{Audience, Topic};

    #[test]
    fn a_typed_event_reaches_every_subscriber_once_without_a_runtime() {
        let pubsub = PubSub::new();
        let mut first = pubsub.subscribe_events();
        let mut second = pubsub.subscribe_events();
        let payload = serde_json::json!({ "model": "m" });
        pubsub.publish_event(Event::new(
            Topic::AiModelLoadingStatus,
            Audience::Everyone,
            &payload,
        ));
        for rx in [&mut first, &mut second] {
            let event = rx.try_recv().expect("the event arrives");
            assert_eq!(event.topic, Topic::AiModelLoadingStatus);
            assert_eq!(event.payload, payload);
            assert!(rx.try_recv().is_err(), "exactly once");
        }
    }
}
