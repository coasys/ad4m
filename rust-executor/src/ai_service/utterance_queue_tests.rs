//! Admission to a transcription stream.
//!
//! Eight waiting utterances per stream, plus at most one being processed.
//! That is not eight in flight in total. A ninth waiting utterance is refused
//! at once. Audio above the shared 10 MiB float32 ceiling is refused before
//! it is queued.

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use tokio::sync::Notify;

use super::providers::audio::{AudioFile, RemoteTranscribe, TranscribeReply};
use super::*;
use crate::billing::test_seam;
use crate::db::Ad4mDb;
use crate::events::Topic;
use crate::pubsub::get_global_pubsub;

/// The first `n` samples of the speech recording. `n` stays inside one extra
/// partial window of 64_000, so each length has the same speech-gate result.
fn speech_prefix(n: usize) -> Vec<f32> {
    let speech = transcription::speech_fixture();
    assert!(
        speech.len() >= n,
        "the speech fixture has {} samples, the test asked for {n}",
        speech.len()
    );
    speech[..n].to_vec()
}

/// Holds the first upstream call until [`HoldUpstream::release`] is notified.
/// Later calls answer at once. Each call records the sample count.
struct HoldUpstream {
    entered: Arc<Notify>,
    release: Arc<Notify>,
    calls: Arc<AtomicUsize>,
    lengths: Arc<std::sync::Mutex<Vec<usize>>>,
}

#[async_trait::async_trait]
impl RemoteTranscribe for HoldUpstream {
    async fn transcribe(
        &self,
        model: &str,
        audio: AudioFile,
        fields: Vec<(String, String)>,
    ) -> Result<TranscribeReply> {
        assert_eq!(model, "remote-model");
        assert!(fields.is_empty());
        let samples = (audio.bytes.len() - 44) / 2;
        let n = self.calls.fetch_add(1, Ordering::SeqCst);
        self.lengths.lock().unwrap().push(samples);
        if n == 0 {
            // Signal after the count, before the wait, so the test observes
            // "in progress" and not merely "queued".
            self.entered.notify_one();
            self.release.notified().await;
        }
        let text = format!("{samples} samples");
        Ok(TranscribeReply {
            body: text.clone().into_bytes(),
            text,
            content_type: "text/plain".into(),
        })
    }
}

struct Hold {
    entered: Arc<Notify>,
    release: Arc<Notify>,
    calls: Arc<AtomicUsize>,
    lengths: Arc<std::sync::Mutex<Vec<usize>>>,
}

impl Hold {
    fn new() -> Self {
        Self {
            entered: Arc::new(Notify::new()),
            release: Arc::new(Notify::new()),
            calls: Arc::new(AtomicUsize::new(0)),
            lengths: Arc::new(std::sync::Mutex::new(Vec::new())),
        }
    }

    fn client(&self) -> Arc<HoldUpstream> {
        Arc::new(HoldUpstream {
            entered: self.entered.clone(),
            release: self.release.clone(),
            calls: self.calls.clone(),
            lengths: self.lengths.clone(),
        })
    }
}

async fn open_held(service: &AIService, hold: &Hold, payer: Option<&str>) -> String {
    let _ = Ad4mDb::init_global_instance(":memory:");
    service
        .open_remote_stream(
            "m".into(),
            "remote-model".into(),
            "e".into(),
            hold.client(),
            payer.map(str::to_string),
            "did:key:owner".into(),
        )
        .await
}

/// One utterance is in progress. Eight more wait. The next is refused, and
/// that audio is not transcribed, billed or published. The nine that were
/// accepted then run in order.
#[tokio::test(start_paused = true)]
async fn a_stream_admits_eight_waiting_utterances_and_refuses_the_next() {
    let service = AIService::empty();
    let hold = Hold::new();
    let id = open_held(&service, &hold, Some("did:key:payer")).await;
    Ad4mDb::with_global_instance(|db| db.set_host_rates(&[("m".to_string(), 0.5)])).unwrap();
    test_seam::reset();
    test_seam::force_result(test_seam::ForcedResult::Success);
    let operator = RequestContext::operator_for_test();
    let mut text = service
        .subscribe_transcription_text(&id, &operator)
        .await
        .unwrap();
    let mut events = get_global_pubsub().await.subscribe_events();

    let feed_a = service.feed_transcription_stream(&id, speech_prefix(64_000), &operator);
    tokio::pin!(feed_a);
    tokio::select! {
        biased;
        result = &mut feed_a => panic!("the held utterance finished before the test released it: {result:?}"),
        _ = hold.entered.notified() => {}
    }
    assert_eq!(hold.calls.load(Ordering::SeqCst), 1);

    let mut waiting = Vec::new();
    for n in 1..=8 {
        waiting.push(
            service
                .queue_utterance(&id, speech_prefix(64_000 + n), &operator)
                .await
                .expect("a waiting slot was refused"),
        );
    }
    let refused = service
        .queue_utterance(&id, speech_prefix(64_009), &operator)
        .await;
    // A feed answer is not `Debug`, so this cannot use `unwrap_err`.
    let Err(error) = refused else {
        panic!("a ninth waiting utterance was accepted");
    };
    let message = error.to_string();
    assert!(
        message.to_ascii_lowercase().contains("full"),
        "the refusal did not say the queue is full: {message}"
    );
    assert_eq!(
        hold.calls.load(Ordering::SeqCst),
        1,
        "refused audio was sent upstream"
    );
    assert_eq!(
        test_seam::calls().len(),
        0,
        "refused audio was billed before it could run"
    );

    hold.release.notify_one();
    feed_a.await.expect("the utterance in progress");
    for answer in waiting {
        answer.await.expect("a waiting utterance");
    }

    assert_eq!(hold.calls.load(Ordering::SeqCst), 9);
    assert!(
        !hold.lengths.lock().unwrap().contains(&64_009),
        "refused audio was transcribed: {:?}",
        hold.lengths.lock().unwrap()
    );
    let got: Vec<String> = {
        let mut out = Vec::new();
        for _ in 0..9 {
            out.push(text.recv().await.unwrap());
        }
        out
    };
    let expected: Vec<String> = (0..9).map(|n| format!("{} samples", 64_000 + n)).collect();
    assert_eq!(got, expected);
    assert!(text.try_recv().is_err(), "a tenth transcript was published");

    let mut published = 0;
    while let Ok(event) = events.try_recv() {
        if event.topic == Topic::AiTranscriptionText && event.payload["streamId"] == id.as_str() {
            published += 1;
            assert_ne!(event.payload["text"], "64009 samples");
        }
    }
    assert_eq!(published, 9, "refused audio published a text event");
    assert_eq!(test_seam::calls().len(), 9, "refused audio was billed");
}

/// The service ceiling is the same 10 MiB of float32 the HTTP feed allows.
/// An utterance at the ceiling is queued. One sample past it is not.
#[tokio::test]
async fn an_utterance_above_the_sample_ceiling_is_refused_before_enqueue() {
    const MAX_SAMPLES: usize = 10 * 1024 * 1024 / 4;
    assert_eq!(MAX_SAMPLES, transcription::MAX_UTTERANCE_SAMPLES);
    let service = AIService::empty();
    let _ = Ad4mDb::init_global_instance(":memory:");
    let (session, mut queue) = TranscriptionSession::for_test(None);
    let id = "ceiling".to_string();
    service
        .transcription_streams
        .lock()
        .await
        .insert(id.clone(), session);
    let operator = RequestContext::operator_for_test();

    let accepted = service
        .queue_utterance(&id, vec![0.0; MAX_SAMPLES], &operator)
        .await;
    assert!(
        accepted.is_ok(),
        "an utterance at the 10 MiB ceiling was refused"
    );
    let refused = service
        .queue_utterance(&id, vec![0.0; MAX_SAMPLES + 1], &operator)
        .await;
    let Err(error) = refused else {
        panic!("an utterance above the 10 MiB ceiling was accepted");
    };
    assert!(
        error.to_string().contains(&MAX_SAMPLES.to_string()),
        "the refusal did not name the ceiling: {error}"
    );

    let queued = queue
        .try_recv()
        .expect("the at-limit utterance was not queued");
    assert_eq!(queued.samples.len(), MAX_SAMPLES);
    assert!(
        queue.try_recv().is_err(),
        "the over-limit utterance was queued"
    );
}

/// One utterance is in the worker and eight wait.
async fn fill_to_capacity(
    service: &AIService,
    hold: &Hold,
    id: &String,
    operator: &RequestContext,
) -> (FeedAnswer, Vec<FeedAnswer>) {
    // `queue_utterance` returns an owned answer. The feed future borrows the service.
    let mut feed_a = service
        .queue_utterance(id, speech_prefix(64_000), operator)
        .await
        .expect("the first utterance was refused");
    tokio::select! {
        biased;
        _ = &mut feed_a => panic!("the held utterance finished before the test released it"),
        _ = hold.entered.notified() => {}
    }
    let mut waiting = Vec::new();
    for n in 1..=8 {
        waiting.push(
            service
                .queue_utterance(id, speech_prefix(64_000 + n), operator)
                .await
                .expect("a waiting slot was refused"),
        );
    }
    (feed_a, waiting)
}

struct Immediate {
    calls: Arc<AtomicUsize>,
}

#[async_trait::async_trait]
impl RemoteTranscribe for Immediate {
    async fn transcribe(
        &self,
        model: &str,
        audio: AudioFile,
        fields: Vec<(String, String)>,
    ) -> Result<TranscribeReply> {
        assert_eq!(model, "remote-model");
        assert!(fields.is_empty());
        let _ = audio;
        self.calls.fetch_add(1, Ordering::SeqCst);
        let text = "accepted".to_string();
        Ok(TranscribeReply {
            body: text.clone().into_bytes(),
            text,
            content_type: "text/plain".into(),
        })
    }
}

/// The first call waits. The next call sleeps three seconds, then answers.
struct StepUpstream {
    entered: Arc<Notify>,
    release: Arc<Notify>,
    calls: Arc<AtomicUsize>,
}

#[async_trait::async_trait]
impl RemoteTranscribe for StepUpstream {
    async fn transcribe(
        &self,
        model: &str,
        audio: AudioFile,
        fields: Vec<(String, String)>,
    ) -> Result<TranscribeReply> {
        assert_eq!(model, "remote-model");
        assert!(fields.is_empty());
        let samples = (audio.bytes.len() - 44) / 2;
        let n = self.calls.fetch_add(1, Ordering::SeqCst);
        if n == 0 {
            self.entered.notify_one();
            self.release.notified().await;
        } else {
            tokio::time::sleep(std::time::Duration::from_secs(3)).await;
        }
        let text = format!("{samples} samples");
        Ok(TranscribeReply {
            body: text.clone().into_bytes(),
            text,
            content_type: "text/plain".into(),
        })
    }
}

struct FailUpstream {
    calls: Arc<AtomicUsize>,
}

#[async_trait::async_trait]
impl RemoteTranscribe for FailUpstream {
    async fn transcribe(
        &self,
        _model: &str,
        _audio: AudioFile,
        _fields: Vec<(String, String)>,
    ) -> Result<TranscribeReply> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        tokio::time::sleep(std::time::Duration::from_secs(2)).await;
        Err(anyhow::anyhow!("upstream failed"))
    }
}

async fn open_client(
    service: &AIService,
    client: Arc<dyn RemoteTranscribe>,
    payer: Option<&str>,
    completions: Option<tokio::sync::mpsc::UnboundedSender<transcription::UtteranceCompletion>>,
) -> String {
    let _ = Ad4mDb::init_global_instance(":memory:");
    match completions {
        Some(completions) => {
            service
                .open_remote_stream_observed(
                    "m".into(),
                    "remote-model".into(),
                    "e".into(),
                    client,
                    payer.map(str::to_string),
                    "did:key:owner".into(),
                    completions,
                )
                .await
        }
        None => {
            service
                .open_remote_stream(
                    "m".into(),
                    "remote-model".into(),
                    "e".into(),
                    client,
                    payer.map(str::to_string),
                    "did:key:owner".into(),
                )
                .await
        }
    }
}

/// A feed, a close and an open on another stream finish while this queue is full.
#[tokio::test]
async fn another_stream_proceeds_while_one_utterance_queue_is_full() {
    let service = AIService::empty();
    let hold = Hold::new();
    let full = open_held(&service, &hold, None).await;
    let operator = RequestContext::operator_for_test();
    let (_in_progress, _waiting) = fill_to_capacity(&service, &hold, &full, &operator).await;

    let fast_calls = Arc::new(AtomicUsize::new(0));
    let fast = open_client(
        &service,
        Arc::new(Immediate {
            calls: fast_calls.clone(),
        }),
        None,
        None,
    )
    .await;
    let fed = tokio::time::timeout(
        std::time::Duration::from_secs(15),
        service.feed_transcription_stream(&fast, speech_prefix(64_000), &operator),
    )
    .await
    .expect("the other stream waited on the full queue");
    fed.expect("the other stream was refused");
    assert_eq!(fast_calls.load(Ordering::SeqCst), 1);

    tokio::time::timeout(
        std::time::Duration::from_secs(5),
        service.close_transcription_stream(&fast, &operator),
    )
    .await
    .expect("close waited on the full queue")
    .expect("close");
    let opened = tokio::time::timeout(std::time::Duration::from_secs(5), async {
        open_held(&service, &Hold::new(), None).await
    })
    .await
    .expect("open waited on the full queue");
    assert_ne!(opened, full);
}

/// Closing a full stream still answers every utterance it had accepted.
#[tokio::test(start_paused = true)]
async fn closing_a_full_stream_drains_every_accepted_utterance() {
    let service = AIService::empty();
    let hold = Hold::new();
    let id = open_held(&service, &hold, None).await;
    let operator = RequestContext::operator_for_test();
    let (feed_a, waiting) = fill_to_capacity(&service, &hold, &id, &operator).await;
    service
        .close_transcription_stream(&id, &operator)
        .await
        .unwrap();
    hold.release.notify_one();
    feed_a.await.expect("the utterance in progress");
    for answer in waiting {
        answer.await.expect("an accepted utterance");
    }
    assert_eq!(hold.calls.load(Ordering::SeqCst), 9);
    assert!(
        service
            .queue_utterance(&id, speech_prefix(64_000), &operator)
            .await
            .is_err(),
        "a closed stream accepted more audio"
    );
}

/// Dropping the feed future does not free the queue slot. The audio stays
/// queued and is still transcribed.
#[tokio::test]
async fn a_dropped_feed_future_keeps_its_queue_slot() {
    // A broken queue bound can leave a feed waiting forever; fail instead of hanging CI.
    tokio::time::timeout(std::time::Duration::from_secs(60), async {
        let service = AIService::empty();
        let hold = Hold::new();
        let id = open_held(&service, &hold, None).await;
        let operator = RequestContext::operator_for_test();
        let (feed_a, mut waiting) = fill_to_capacity(&service, &hold, &id, &operator).await;
        drop(waiting.pop().unwrap());
        let refused = service
            .queue_utterance(&id, speech_prefix(64_010), &operator)
            .await;
        let Err(error) = refused else {
            panic!("dropping a feed future freed a queue slot");
        };
        assert!(
            error.to_string().to_ascii_lowercase().contains("full"),
            "{error}"
        );
        hold.release.notify_one();
        feed_a.await.expect("the utterance in progress");
        for answer in waiting {
            answer.await.expect("a waiting utterance");
        }
        // The dropped utterance is last. Its answer is gone, so the worker takes it
        // after the answers this test still holds.
        tokio::time::timeout(std::time::Duration::from_secs(15), async {
            while hold.calls.load(Ordering::SeqCst) < 9 {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("the dropped feed's audio was not transcribed");
    })
    .await
    .expect("the test did not finish within 60 s");
}

/// A dead worker fails the utterances it had accepted. A later feed hears that it closed.
#[tokio::test(start_paused = true)]
async fn a_stopped_worker_fails_the_utterances_it_had_accepted() {
    let service = AIService::empty();
    let hold = Hold::new();
    let id = open_held(&service, &hold, None).await;
    let operator = RequestContext::operator_for_test();
    let (feed_a, waiting) = fill_to_capacity(&service, &hold, &id, &operator).await;
    service.abort_transcription_worker(&id).await;
    assert!(
        feed_a.await.is_err(),
        "the utterance in progress was answered"
    );
    for answer in waiting {
        assert!(answer.await.is_err(), "a queued utterance was answered");
    }
    let Err(again) = service
        .queue_utterance(&id, speech_prefix(64_000), &operator)
        .await
    else {
        panic!("the closed stream accepted audio");
    };
    assert!(
        again.to_string().to_ascii_lowercase().contains("closed"),
        "{again}"
    );
    assert!(!AIServiceError::is_utterance_queue_full(&again));
}

/// An unauthorized caller is refused before admission, including when the queue is full.
#[tokio::test(start_paused = true)]
async fn an_unauthorized_caller_does_not_learn_that_the_utterance_queue_is_full() {
    let service = AIService::empty();
    let hold = Hold::new();
    let id = open_held(&service, &hold, Some("did:key:alice")).await;
    let _multi = crate::test_utils::MultiUserMode::on();
    let operator = RequestContext::operator_for_test();
    let _filled = fill_to_capacity(&service, &hold, &id, &operator).await;
    let stranger = RequestContext::user_for_test("did:key:stranger", vec![]);
    let Err(refused) = service
        .queue_utterance(&id, speech_prefix(64_000), &stranger)
        .await
    else {
        panic!("a stranger fed the stream");
    };
    let message = refused.to_string();
    assert!(message.contains("Authorization error"), "{message}");
    assert!(!message.to_ascii_lowercase().contains("full"), "{message}");
}

/// B waits five seconds behind A, then the upstream call takes three seconds.
/// The queue wait includes the five seconds. The upstream time does not.
#[tokio::test(start_paused = true)]
async fn queue_wait_is_apart_from_upstream_time() {
    let service = AIService::empty();
    let entered = Arc::new(Notify::new());
    let release = Arc::new(Notify::new());
    let calls = Arc::new(AtomicUsize::new(0));
    let (tx, mut completions) = tokio::sync::mpsc::unbounded_channel();
    let id = open_client(
        &service,
        Arc::new(StepUpstream {
            entered: entered.clone(),
            release: release.clone(),
            calls: calls.clone(),
        }),
        None,
        Some(tx),
    )
    .await;
    let operator = RequestContext::operator_for_test();
    let mut feed_a = service
        .queue_utterance(&id, speech_prefix(64_000), &operator)
        .await
        .expect("A was refused");
    tokio::select! {
        biased;
        _ = &mut feed_a => panic!("the held utterance finished early"),
        _ = entered.notified() => {}
    }
    let feed_b = service
        .queue_utterance(&id, speech_prefix(64_001), &operator)
        .await
        .expect("B was refused");
    tokio::time::sleep(std::time::Duration::from_secs(5)).await;
    release.notify_one();
    feed_a.await.expect("A");
    feed_b.await.expect("B");

    let a = completions.recv().await.expect("A's completion");
    let b = completions.recv().await.expect("B's completion");
    assert_eq!(a.outcome, transcription::UtteranceOutcome::Transcribed);
    // A was dequeued before the five seconds. Those seconds are inside A's call.
    assert!(a.queue_wait_ms < 5_000, "{a:?}");
    assert!(a.upstream_ms.expect("A called upstream") >= 5_000, "{a:?}");
    assert_eq!(b.outcome, transcription::UtteranceOutcome::Transcribed);
    assert!(b.queue_wait_ms >= 5_000, "{b:?}");
    let upstream = b.upstream_ms.expect("B called upstream");
    assert!(upstream >= 3_000, "{b:?}");
    assert!(
        upstream < 5_000,
        "upstream time includes the queue wait: {b:?}"
    );
    assert!(b.total_ms >= b.queue_wait_ms, "{b:?}");
    assert!(b.total_ms >= b.queue_wait_ms + upstream, "{b:?}");
}

#[tokio::test(start_paused = true)]
async fn completion_records_cover_empty_silence_failure_and_credits() {
    let operator = RequestContext::operator_for_test();

    let service = AIService::empty();
    let calls = Arc::new(AtomicUsize::new(0));
    let (tx, mut completions) = tokio::sync::mpsc::unbounded_channel();
    let id = open_client(
        &service,
        Arc::new(Immediate {
            calls: calls.clone(),
        }),
        None,
        Some(tx),
    )
    .await;
    service
        .feed_transcription_stream(&id, Vec::new(), &operator)
        .await
        .expect("empty audio");
    let empty = completions.recv().await.unwrap();
    assert_eq!(empty.outcome, transcription::UtteranceOutcome::Empty);
    assert!(empty.upstream_ms.is_none(), "{empty:?}");
    assert!(empty.total_ms >= empty.queue_wait_ms, "{empty:?}");
    assert_eq!(calls.load(Ordering::SeqCst), 0);

    if crate::ai_service::local::onnx_runtime::ready() {
        service
            .feed_transcription_stream(&id, vec![0.0; 16_000], &operator)
            .await
            .expect("silence");
        let silence = completions.recv().await.unwrap();
        assert_eq!(silence.outcome, transcription::UtteranceOutcome::Silence);
        assert!(silence.upstream_ms.is_none(), "{silence:?}");
        assert!(silence.total_ms >= silence.queue_wait_ms, "{silence:?}");
        assert_eq!(calls.load(Ordering::SeqCst), 0, "silence was sent upstream");
    }

    let fail_calls = Arc::new(AtomicUsize::new(0));
    let (tx, mut completions) = tokio::sync::mpsc::unbounded_channel();
    let id = open_client(
        &service,
        Arc::new(FailUpstream {
            calls: fail_calls.clone(),
        }),
        None,
        Some(tx),
    )
    .await;
    let failed = service
        .feed_transcription_stream(&id, speech_prefix(64_000), &operator)
        .await;
    assert!(failed.is_err(), "the upstream failure was accepted");
    let failed = completions.recv().await.unwrap();
    assert_eq!(
        failed.outcome,
        transcription::UtteranceOutcome::UpstreamFailed
    );
    let upstream = failed.upstream_ms.expect("the failed call was timed");
    assert!(upstream >= 2_000, "{failed:?}");
    assert!(failed.queue_wait_ms < 2_000, "{failed:?}");
    assert!(failed.total_ms >= upstream, "{failed:?}");
    assert!(failed.total_ms >= failed.queue_wait_ms, "{failed:?}");
    assert_eq!(fail_calls.load(Ordering::SeqCst), 1);

    let calls = Arc::new(AtomicUsize::new(0));
    let (tx, mut completions) = tokio::sync::mpsc::unbounded_channel();
    let id = open_client(
        &service,
        Arc::new(Immediate {
            calls: calls.clone(),
        }),
        Some("did:key:broke"),
        Some(tx),
    )
    .await;
    Ad4mDb::with_global_instance(|db| {
        db.add_user("broke@example.com", "did:key:broke", "password")?;
        db.set_user_credits("broke@example.com", 0.0)?;
        db.set_free_hosting_enabled(false)
    })
    .unwrap();
    let refused = service
        .feed_transcription_stream(&id, speech_prefix(64_000), &operator)
        .await
        .unwrap_err();
    Ad4mDb::with_global_instance(|db| db.set_free_hosting_enabled(true)).unwrap();
    assert!(refused.to_string().contains("credits"), "{refused}");
    let credits = completions.recv().await.unwrap();
    assert_eq!(
        credits.outcome,
        transcription::UtteranceOutcome::InsufficientCredits
    );
    assert!(credits.upstream_ms.is_none(), "{credits:?}");
    assert!(credits.total_ms >= credits.queue_wait_ms, "{credits:?}");
    assert_eq!(
        calls.load(Ordering::SeqCst),
        0,
        "the credit refusal was sent"
    );
}

/// One target is full and one is not. The feed names the full stream. The other is fed once.
#[tokio::test]
async fn a_feed_names_the_stream_whose_utterance_queue_is_full() {
    let service = AIService::empty();
    AIService::install_global_for_test(Some(service.clone())).await;
    let hold = Hold::new();
    let full = open_held(&service, &hold, None).await;
    let operator = RequestContext::operator_for_test();
    let _filled = fill_to_capacity(&service, &hold, &full, &operator).await;
    let fast_calls = Arc::new(AtomicUsize::new(0));
    let accepting = open_client(
        &service,
        Arc::new(Immediate {
            calls: fast_calls.clone(),
        }),
        None,
        None,
    )
    .await;

    let samples = speech_prefix(64_000);
    let mut body = Vec::with_capacity(samples.len() * 4);
    for sample in samples {
        body.extend(sample.to_le_bytes());
    }
    let mut headers = axum::http::HeaderMap::new();
    headers.insert(
        "x-stream-ids",
        format!("{full},{accepting}").parse().unwrap(),
    );
    let result = crate::api::ai_ws::feed_transcription_stream(
        axum::extract::State(crate::api::auth::AppState {
            admin_credential: None,
        }),
        operator,
        headers,
        axum::body::Bytes::from(body),
    )
    .await;
    AIService::install_global_for_test(None).await;

    let message = match result {
        Err(crate::api::errors::ApiError::Internal(message)) => message,
        other => panic!("expected the aggregate feed error, got {other:?}"),
    };
    assert!(message.contains(&full), "{message}");
    assert!(message.to_ascii_lowercase().contains("full"), "{message}");
    assert!(message.contains("the others were fed"), "{message}");
    assert!(!message.contains(&accepting), "{message}");
    assert_eq!(fast_calls.load(Ordering::SeqCst), 1);
    assert_eq!(hold.calls.load(Ordering::SeqCst), 1);
}
