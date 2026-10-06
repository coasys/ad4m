//! `feed_outcome`: what POST /ai/transcription/feed answers when some streams fail.

use crate::api::errors::ApiError;
use crate::api::transcription_feed::feed_outcome;

fn message(result: Result<(), ApiError>) -> String {
    match result {
        Err(ApiError::Internal(msg)) => msg,
        Err(_) => panic!("expected an internal error"),
        Ok(()) => panic!("expected an error, got success"),
    }
}

#[test]
fn every_stream_fed_is_success() {
    assert!(feed_outcome(&[], 3).is_ok());
}

#[test]
fn one_failed_stream_of_several_fails_the_request() {
    // This used to answer "true": only a feed where every stream failed was an error.
    let msg = message(feed_outcome(&["s2: stream not found".to_string()], 3));
    assert!(msg.contains("1 of 3"), "got {msg}");
    assert!(msg.contains("the others were fed"), "got {msg}");
    assert!(msg.contains("s2: stream not found"), "got {msg}");
}

#[test]
fn every_stream_failed_keeps_its_message() {
    let errors = vec!["s1: gone".to_string(), "s2: gone".to_string()];
    let msg = message(feed_outcome(&errors, 2));
    assert!(msg.starts_with("All streams failed"), "got {msg}");
}
