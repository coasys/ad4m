//! Sending a telepresence broadcast to remote members through the link
//! language, off the RPC's critical path.
//!
//! A language runtime serves one call at a time; on 2026-11-19 a job in one
//! held it for 20 s at a time, and every `sendBroadcast` — presence beats,
//! mesh offers, ICE candidates — queued behind it and died waiting. So the
//! remote send is not part of the RPC: `send_broadcast` answers once local
//! delivery is done and leaves the send to a per-perspective queue drained
//! by a background task, one send at a time and at most
//! `MAX_PENDING_BROADCASTS` deep. Past the bound a send is refused so the
//! caller learns it did not go out, and a queued presence beat is dropped
//! the moment a newer one of the same sender, device and call arrives —
//! it is superseded anyway.

use std::collections::VecDeque;
use std::sync::atomic::Ordering;

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;

use super::PerspectiveInstance;
use crate::languages::capability::Capability;
use crate::languages::language::Language;
use crate::types::PerspectiveExpression;

/// How many broadcasts wait behind the send in flight, per perspective.
/// Bounded, so a language that stalls still fails the send instead of
/// growing a backlog the RPC reported as sent.
pub(crate) const MAX_PENDING_BROADCASTS: usize = 8;

/// The predicate the call protocol's messages go out on
/// (`core/src/neighbourhood/MeshManager.ts`'s `RTC_PREDICATE`).
const RTC_PREDICATE: &str = "ad4m://session/rtc";

/// Remote broadcasts waiting on the link language, and whether a task is
/// draining them.
#[derive(Default)]
pub(crate) struct BroadcastQueue {
    pending: VecDeque<PerspectiveExpression>,
    draining: bool,
}

/// Who a presence beat speaks for: `(sender DID, device, call)`. `Some`
/// only when every link of `payload` is an `ad4m://session/rtc` link whose
/// `source` parses to `{ device, message: { kind: "beat", call } }` (the
/// JSON the client's telepresence channel writes) and they all name the
/// same device and call. A payload with anything else in it is not a beat:
/// dropping it would drop that too.
fn beat_key(payload: &PerspectiveExpression) -> Option<(String, String, String)> {
    let mut key: Option<(String, String, String)> = None;
    for link in &payload.data.links {
        if link.data.predicate.as_deref() != Some(RTC_PREDICATE) {
            return None;
        }
        let source: serde_json::Value = serde_json::from_str(&link.data.source).ok()?;
        let message = source.get("message")?;
        if message.get("kind")?.as_str()? != "beat" {
            return None;
        }
        let this = (
            payload.author.clone(),
            source.get("device")?.as_str()?.to_string(),
            message.get("call")?.as_str()?.to_string(),
        );
        match &key {
            Some(key) if *key != this => return None,
            _ => key = Some(this),
        }
    }
    key
}

impl PerspectiveInstance {
    /// Queue `payload` for remote delivery through `language` — the part of
    /// `send_broadcast` that must not make the RPC wait on the language's
    /// serial runtime.
    ///
    /// Local delivery is already done when this runs. The send is skipped
    /// when the language has no broadcast adapter, and when the
    /// neighbourhood's members were all verified local
    /// ([`crate::sfu::config_store::every_member_local`]): remote delivery
    /// could then reach nobody local delivery did not.
    ///
    /// Errors only when the queue is full — the send was refused rather
    /// than piled onto a blocked language.
    pub(crate) fn broadcast_remotely(
        &self,
        language: Language,
        payload: PerspectiveExpression,
        shared_url: Option<&str>,
    ) -> Result<(), AnyError> {
        if !language.has(Capability::TelepresenceSendBroadcast) {
            return Ok(());
        }
        if let Some(url) = shared_url {
            if crate::sfu::config_store::every_member_local(url) {
                return Ok(());
            }
        }

        let mut queue = self.language_broadcasts.lock().unwrap();
        if let Some(key) = beat_key(&payload) {
            queue
                .pending
                .retain(|queued| beat_key(queued).as_ref() != Some(&key));
        }
        if queue.pending.len() >= MAX_PENDING_BROADCASTS {
            return Err(anyhow!(
                "The link-language broadcast queue is full; the broadcast was dropped"
            ));
        }
        queue.pending.push_back(payload);
        if queue.draining {
            return Ok(());
        }
        queue.draining = true;
        drop(queue);

        let this = self.clone();
        tokio::spawn(async move {
            let mut language = language;
            loop {
                let next = {
                    let mut queue = this.language_broadcasts.lock().unwrap();
                    match queue.pending.pop_front() {
                        Some(payload) => payload,
                        None => {
                            queue.draining = false;
                            break;
                        }
                    }
                };
                if this.is_teardown.load(Ordering::Acquire) {
                    break;
                }
                // A slow language call holds this task, never the RPCs that
                // queued behind it — they already answered.
                if let Err(e) = language.send_broadcast(next).await {
                    log::warn!("Telepresence broadcast via the link language failed: {e}");
                }
            }
        });
        Ok(())
    }
}
