//! Every 30 s, one `[call-diag] event=language.queue` line per language with calls waiting:
//! how many, how long the oldest has waited, how long the running call has run. A language
//! runs one call at a time (`language_runtime.rs`), so one slow call (a key-grant job's HTTP
//! time-outs) holds every caller behind it; these lines show which language and since when.
//! Nothing is logged while every queue is empty.

use std::time::Duration;

use super::LanguageController;

/// How often the queues are reported.
pub const QUEUE_REPORT_INTERVAL: Duration = Duration::from_secs(30);

impl LanguageController {
    /// Log one line per language with calls waiting. Returns how many it logged.
    pub(crate) async fn report_queues(&self) -> usize {
        let handles: Vec<_> = self.runtimes.lock().await.values().cloned().collect();
        let mut logged = 0;
        for handle in handles {
            let Some(queue) = handle.queue_snapshot() else {
                continue;
            };
            crate::call_diag!(
                log::Level::Info,
                "language.queue",
                "language" => handle.language_address,
                "name" => handle.language_name.clone().unwrap_or_default(),
                "depth" => queue.depth,
                "oldest_wait_ms" => queue.oldest_wait.as_millis(),
                "running_ms" => queue.running_for.map_or("-".to_string(), |d| d.as_millis().to_string()),
            );
            logged += 1;
        }
        logged
    }
}

/// Start the 30 s report. Called once at startup.
pub fn start_queue_report() {
    tokio::spawn(async {
        let mut every = tokio::time::interval(QUEUE_REPORT_INTERVAL);
        every.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
        every.tick().await;
        loop {
            every.tick().await;
            LanguageController::global_instance().report_queues().await;
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::call_diag::capture;
    use crate::languages::language_runtime_handle::LanguageRuntimeHandle;

    /// A language with two calls waiting gets one line with its depth and the age of the
    /// oldest; once they are answered it gets none.
    #[tokio::test]
    async fn a_language_with_calls_waiting_is_reported_and_an_idle_one_is_not() {
        let controller = LanguageController::global_instance();
        let address = format!("QmQueueReport{}", uuid::Uuid::new_v4().simple());
        let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel();
        let handle = LanguageRuntimeHandle::from_channel(address.clone(), tx);
        controller
            .set_runtime_for_test(&address, Some(handle.clone()))
            .await;
        let language = format!("language={address}");

        let calls: Vec<_> = (0..2)
            .map(|i| {
                let handle = handle.clone();
                tokio::spawn(async move { handle.execute(format!("call {i}")).await })
            })
            .collect();
        let mut taken = Vec::new();
        for _ in 0..2 {
            taken.push(requests.recv().await.unwrap());
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
        controller.report_queues().await;
        let lines = capture::lines("language.queue", &[&language]);
        assert_eq!(lines.len(), 1, "{lines:?}");
        capture::assert_format(&lines[0].line);
        assert_eq!(
            capture::field(&lines[0].line, "depth").as_deref(),
            Some("2")
        );
        let oldest: u64 = capture::field(&lines[0].line, "oldest_wait_ms")
            .unwrap()
            .parse()
            .unwrap();
        assert!(oldest >= 50, "{}", lines[0].line);

        for request in taken {
            let _ = request.response_tx.send(Ok("done".to_string()));
        }
        for call in calls {
            assert_eq!(call.await.unwrap().unwrap(), "done");
        }
        controller.report_queues().await;
        assert_eq!(capture::lines("language.queue", &[&language]).len(), 1);
        controller.set_runtime_for_test(&address, None).await;
    }

    /// A caller that stops waiting (its future dropped: a timeout, a cancelled RPC) takes its
    /// call off the queue report, though the runtime never answered it.
    #[tokio::test]
    async fn a_cancelled_call_leaves_the_queue_report() {
        let address = format!("QmCancelledCall{}", uuid::Uuid::new_v4().simple());
        let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel();
        let handle = LanguageRuntimeHandle::from_channel(address, tx);
        let waited =
            tokio::time::timeout(Duration::from_millis(50), handle.execute("hang()".into())).await;
        assert!(waited.is_err(), "the call was answered");
        // The runtime still holds the request; nobody waits for it any more.
        let _held = requests.recv().await.unwrap();
        assert!(
            handle.queue_snapshot().is_none(),
            "a dropped call still counts as waiting: {:?}",
            handle.queue_snapshot()
        );
    }
}
