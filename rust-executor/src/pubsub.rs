use crate::events::Event;
use std::collections::HashSet;
use std::sync::Arc;
use std::sync::LazyLock;
use tokio::sync::broadcast;

pub struct PubSub {
    /// Typed events, every topic on one channel: each socket decides per event.
    events: broadcast::Sender<Arc<Event>>,
}

impl PubSub {
    pub fn new() -> Self {
        Self {
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
}

lazy_static::lazy_static! {
    static ref GLOBAL_PUB_SUB: Arc<PubSub> = Arc::new(PubSub::new());
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
