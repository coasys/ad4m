//! The SFU's own channel for pipe renegotiation between SFU nodes.
//!
//! The SFU event loop publishes an offer (a pipe gained outbound tracks) or an answer (it
//! applied a remote node's offer); the service's cascade router relays each over gossip to
//! the remote node. Nothing here reaches a client socket, so these are not executor events
//! ([`crate::events::Event`]) and need no audience.

use std::sync::LazyLock;
use tokio::sync::broadcast;

use super::types::{SfuPipeRenegotiationAnswer, SfuPipeRenegotiationOffer};

/// How many signals a slow router may fall behind before it skips some.
const CAPACITY: usize = 1024;

static OFFERS: LazyLock<broadcast::Sender<SfuPipeRenegotiationOffer>> =
    LazyLock::new(|| broadcast::channel(CAPACITY).0);
static ANSWERS: LazyLock<broadcast::Sender<SfuPipeRenegotiationAnswer>> =
    LazyLock::new(|| broadcast::channel(CAPACITY).0);

/// Sends `offer` to the cascade router. Needs no runtime: the event loop is synchronous.
pub(crate) fn publish_offer(offer: SfuPipeRenegotiationOffer) {
    let _ = OFFERS.send(offer); // No router yet: nothing to relay to.
}

/// Sends `answer` to the cascade router.
pub(crate) fn publish_answer(answer: SfuPipeRenegotiationAnswer) {
    let _ = ANSWERS.send(answer);
}

/// Every pipe offer published from now on.
pub(crate) fn subscribe_offers() -> broadcast::Receiver<SfuPipeRenegotiationOffer> {
    OFFERS.subscribe()
}

/// Every pipe answer published from now on.
pub(crate) fn subscribe_answers() -> broadcast::Receiver<SfuPipeRenegotiationAnswer> {
    ANSWERS.subscribe()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn a_published_signal_reaches_each_subscriber() {
        let mut offers = subscribe_offers();
        let mut answers = subscribe_answers();
        publish_offer(SfuPipeRenegotiationOffer {
            remote_did: "did:remote".into(),
            room_id: "room".into(),
            sdp_offer: "{}".into(),
        });
        publish_answer(SfuPipeRenegotiationAnswer {
            remote_did: "did:remote".into(),
            room_id: "room".into(),
            sdp_answer: "{}".into(),
        });
        // Other tests may publish too: look for ours.
        loop {
            let offer = offers.recv().await.expect("an offer");
            if offer.remote_did == "did:remote" {
                break;
            }
        }
        loop {
            let answer = answers.recv().await.expect("an answer");
            if answer.remote_did == "did:remote" {
                break;
            }
        }
    }
}
