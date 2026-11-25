//! Bound mount reads before any mirror writes (initial mount, sync and restore).
use std::{future::Future, time::Duration};

use anyhow::Result;
use serde::Serialize;

use super::error::LanguageError;

/// The language API does not expose a cell id. Name the language, not a guessed cell.
#[derive(Debug, Serialize, thiserror::Error)]
#[serde(
    tag = "kind",
    rename_all = "camelCase",
    rename_all_fields = "camelCase"
)]
pub(crate) enum MountError {
    #[error("CellError(InitTimeout): language {language_address} did not initialize after {after_ms} ms ({attempts} attempts)")]
    CellInitTimeout {
        language_address: String,
        after_ms: u64,
        attempts: usize,
    },
    #[error("MountReadTimeout: language {language_address} timed out reading the mount after {after_ms} ms")]
    ReadTimeout {
        language_address: String,
        after_ms: u64,
    },
}

// One end-to-end budget for shapes plus snapshot, on every mount read path.
const MOUNT_READ_TIMEOUT: Duration = Duration::from_secs(75);

pub(crate) async fn read_mount<T, F, Fut>(language: &str, read: F) -> Result<T>
where
    F: FnMut() -> Fut,
    Fut: Future<Output = Result<T, LanguageError>>,
{
    read_with_policy(language, read, MOUNT_READ_TIMEOUT, Duration::from_secs(1)).await
}

async fn read_with_policy<T, F, Fut>(
    language: &str,
    mut read: F,
    budget: Duration,
    backoff: Duration,
) -> Result<T>
where
    F: FnMut() -> Fut,
    Fut: Future<Output = Result<T, LanguageError>>,
{
    use tokio::time::{sleep, timeout_at, Instant};

    let start = Instant::now();
    let deadline = start + budget;
    let mut attempts = 0;
    let mut saw_init_timeout = false;
    let result = timeout_at(deadline, async {
        loop {
            attempts += 1;
            match read().await {
                Ok(value) => return Ok(value),
                // Holochain errors cross the JS bridge as strings. Match the precise
                // upstream variant, not arbitrary timeout/network/database failures.
                Err(LanguageError::RuntimeError { message, .. })
                    if message.contains("CellError(InitTimeout)") =>
                {
                    saw_init_timeout = true;
                    if attempts == 3 {
                        break;
                    }
                    sleep(backoff * attempts as u32).await;
                }
                Err(error) => return Err(error.into()),
            }
        }
        Err(MountError::CellInitTimeout {
            language_address: language.to_owned(),
            after_ms: start.elapsed().as_millis() as u64,
            attempts,
        }
        .into())
    })
    .await;

    match result {
        Ok(result) => result,
        Err(_) => {
            // Dropping the wait does NOT cancel a running language/HC call. Never
            // automatically retry an unsettled call. No mirror writes have begun;
            // the runtime retains its ordered-call/busy protection until it settles.
            let after_ms = budget.as_millis() as u64;
            Err(if saw_init_timeout {
                MountError::CellInitTimeout {
                    language_address: language.to_owned(),
                    after_ms,
                    attempts,
                }
            } else {
                MountError::ReadTimeout {
                    language_address: language.to_owned(),
                    after_ms,
                }
            }
            .into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{cell::Cell, future::pending};
    use tokio::time::{sleep, timeout, Instant};

    fn init_error() -> LanguageError {
        LanguageError::RuntimeError {
            address: "acorn".into(),
            message: "Conductor API error: CellError(InitTimeout)".into(),
        }
    }

    #[tokio::test(start_paused = true)]
    async fn pending_cell_read_returns_typed_deadline() {
        let calls = Cell::new(0);
        let result = timeout(
            Duration::from_secs(2),
            read_with_policy(
                "acorn",
                || {
                    calls.set(calls.get() + 1);
                    pending::<Result<(), LanguageError>>()
                },
                Duration::from_secs(1),
                Duration::from_millis(10),
            ),
        )
        .await
        .expect("mount read must settle before the outer watchdog");
        assert_eq!(calls.get(), 1, "an unsettled call must never be retried");
        assert!(matches!(
            result.unwrap_err().downcast_ref::<MountError>(),
            Some(MountError::ReadTimeout { after_ms: 1000, .. })
        ));
    }

    #[tokio::test(start_paused = true)]
    async fn transient_init_timeout_retries_then_succeeds() {
        let calls = Cell::new(0);
        let start = Instant::now();
        let result = read_with_policy(
            "acorn",
            || {
                calls.set(calls.get() + 1);
                std::future::ready(if calls.get() < 3 {
                    Err(init_error())
                } else {
                    Ok(42)
                })
            },
            Duration::from_secs(10),
            Duration::from_secs(1),
        )
        .await
        .unwrap();
        assert_eq!(result, 42);
        assert_eq!(calls.get(), 3);
        assert_eq!(start.elapsed(), Duration::from_secs(3));
    }

    #[tokio::test(start_paused = true)]
    async fn init_timeout_exhaustion_is_typed_and_capped() {
        let calls = Cell::new(0);
        let error = read_with_policy::<(), _, _>(
            "acorn",
            || {
                calls.set(calls.get() + 1);
                std::future::ready(Err(init_error()))
            },
            Duration::from_secs(10),
            Duration::from_secs(1),
        )
        .await
        .unwrap_err();
        assert_eq!(calls.get(), 3);
        let typed = error.downcast_ref::<MountError>().unwrap();
        assert_eq!(
            serde_json::to_value(typed).unwrap(),
            serde_json::json!({
                "kind": "cellInitTimeout", "languageAddress": "acorn", "afterMs": 3000, "attempts": 3
            })
        );
    }

    #[tokio::test(start_paused = true)]
    async fn deadline_is_shared_by_attempts_and_backoff() {
        let calls = Cell::new(0);
        let start = Instant::now();
        let error = read_with_policy::<(), _, _>(
            "acorn",
            || {
                calls.set(calls.get() + 1);
                async {
                    sleep(Duration::from_secs(2)).await;
                    Err(init_error())
                }
            },
            Duration::from_secs(4),
            Duration::from_secs(1),
        )
        .await
        .unwrap_err();
        assert!(matches!(
            error.downcast_ref::<MountError>(),
            Some(MountError::CellInitTimeout {
                after_ms: 4000,
                attempts: 2,
                ..
            })
        ));
        assert_eq!(calls.get(), 2);
        assert_eq!(start.elapsed(), Duration::from_secs(4));
    }

    #[tokio::test(start_paused = true)]
    async fn other_failures_are_not_retried_or_reclassified() {
        let calls = Cell::new(0);
        let error = read_with_policy::<(), _, _>(
            "acorn",
            || {
                calls.set(calls.get() + 1);
                std::future::ready(Err(LanguageError::SerializationError {
                    message: "invalid snapshot".into(),
                }))
            },
            Duration::from_secs(10),
            Duration::from_secs(1),
        )
        .await
        .unwrap_err();
        assert_eq!(calls.get(), 1);
        assert!(matches!(
            error.downcast_ref::<LanguageError>(),
            Some(LanguageError::SerializationError { .. })
        ));
    }
}
