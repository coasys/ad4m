//! Expression reads and local-first writes.
//!
//! Reads check the expression cache first and fetch every miss for one
//! language in a single call into its runtime. A runtime serves one request
//! at a time, so fetching misses one call each made N network round trips
//! strictly sequential; inside one call they run concurrently.
//!
//! The cache only ever holds expressions their language reported immutable,
//! so a hit needs no `isImmutableExpression` call: only misses ask, in the
//! same call that fetches them.
//!
//! Writes to a language that exports `expressionPrepare` / `expressionPublish`
//! cache the prepared expression before publishing it, and queue the publish
//! if it fails; `run_publish_worker` retries the queue with backoff. The
//! expression is readable locally at once and `create` succeeds offline.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use log::{info, warn};
use serde::Deserialize;
use serde_json::Value as JsonValue;

use super::error::LanguageError;
use super::LanguageController;
use crate::agent::AgentContext;
use crate::db::Ad4mDb;
use crate::types::Expression;

/// Concurrent fetches inside one runtime call.
const FETCH_CONCURRENCY: usize = 8;
/// Addresses per prefetch call. Small, so a large sync interleaves with the
/// reads someone is waiting on instead of holding the runtime for minutes.
const PREFETCH_CHUNK: usize = 8;
const PUBLISH_POLL_INTERVAL: Duration = Duration::from_secs(10);
const PUBLISH_BATCH: u32 = 32;
const PUBLISH_BACKOFF_BASE_MS: i64 = 10_000;
const PUBLISH_BACKOFF_MAX_MS: i64 = 15 * 60 * 1000;

static PUBLISH_WORKER_STARTED: AtomicBool = AtomicBool::new(false);

/// The cache and publish-queue key: the language's hash, never an alias.
fn expression_url(resolved_language: &str, expression_address: &str) -> String {
    format!("{}://{}", resolved_language, expression_address)
}

fn now_ms() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0)
}

/// Delay before the next publish attempt, after `attempts` failed ones.
fn publish_backoff_ms(attempts: u32) -> i64 {
    let factor = 1i64 << attempts.min(16);
    (PUBLISH_BACKOFF_BASE_MS.saturating_mul(factor)).min(PUBLISH_BACKOFF_MAX_MS)
}

#[derive(Deserialize)]
struct FetchOutcome {
    immutable: bool,
    expression: JsonValue,
    error: Option<String>,
}

#[derive(Deserialize)]
struct Prepared {
    address: String,
    expression: JsonValue,
    immutable: bool,
}

/// One runtime call that fetches `addresses` with bounded concurrency. Each
/// result says whether its address is immutable; with `only_immutable`,
/// mutable addresses are not fetched at all (prefetch has no use for them).
fn fetch_script(addresses: &[String], only_immutable: bool) -> String {
    let addresses = serde_json::to_string(addresses).unwrap_or_else(|_| "[]".to_string());
    format!(
        r#"JSON.stringify(await (async () => {{
            const addresses = {addresses};
            const onlyImmutable = {only_immutable};
            const fetchOne = async (address) => {{
                let immutable = false;
                if (typeof language.isImmutableExpression === "function") {{
                    try {{ immutable = !!(await language.isImmutableExpression(address)); }} catch (_e) {{}}
                }}
                if ((onlyImmutable && !immutable) || typeof language.expressionGet !== "function") {{
                    return {{ immutable, expression: null }};
                }}
                try {{
                    return {{ immutable, expression: (await language.expressionGet(address)) ?? null }};
                }} catch (e) {{
                    return {{ immutable, expression: null, error: String((e && e.message) || e) }};
                }}
            }};
            const results = new Array(addresses.length);
            let next = 0;
            const worker = async () => {{
                while (next < addresses.length) {{
                    const i = next++;
                    results[i] = await fetchOne(addresses[i]);
                }}
            }};
            await Promise.all(Array.from({{ length: Math.min({FETCH_CONCURRENCY}, addresses.length) }}, worker));
            return results;
        }})())"#
    )
}

impl LanguageController {
    async fn resolve_language_alias(&self, lang_address: &str) -> String {
        let aliases = self.language_aliases.lock().await;
        aliases
            .get(lang_address)
            .cloned()
            .unwrap_or_else(|| lang_address.to_string())
    }

    /// Get expressions from one language, one result per address, in order.
    /// Not for `literal` — `get_expression` decodes those itself.
    pub async fn get_expressions(
        &self,
        lang_address: &str,
        addresses: &[String],
    ) -> Vec<Result<Option<JsonValue>, LanguageError>> {
        let lang = self.resolve_language_alias(lang_address).await;
        let urls: Vec<String> = addresses.iter().map(|a| expression_url(&lang, a)).collect();
        let cached = Ad4mDb::with_global_instance(|db| db.cached_expressions(&urls))
            .unwrap_or_else(|e| {
                warn!("Expression cache read failed, fetching instead: {}", e);
                vec![None; urls.len()]
            });

        let mut results: Vec<Option<Result<Option<JsonValue>, LanguageError>>> =
            Vec::with_capacity(addresses.len());
        let mut misses = Vec::new();
        for (i, hit) in cached.into_iter().enumerate() {
            match hit {
                Some(expr) => {
                    let mut json = serde_json::to_value(&expr).unwrap_or(JsonValue::Null);
                    Self::verify_expression_proof(&mut json);
                    results.push(Some(Ok(Some(json))));
                }
                None => {
                    results.push(None);
                    misses.push(i);
                }
            }
        }

        if !misses.is_empty() {
            let miss_addresses: Vec<String> =
                misses.iter().map(|&i| addresses[i].clone()).collect();
            let fetched = self.fetch_uncached(&lang, &miss_addresses, false).await;
            for (&i, outcome) in misses.iter().zip(fetched) {
                results[i] = Some(outcome);
            }
        }

        results.into_iter().map(|r| r.unwrap_or(Ok(None))).collect()
    }

    /// Get expressions by URL across languages, one result per URL, in
    /// order. A URL that does not parse, names a language that is not
    /// loaded (and `load_missing` is false or loading fails), or fails to
    /// fetch reads as `None`.
    pub async fn get_expressions_by_url(
        &self,
        urls: &[String],
        load_missing: bool,
    ) -> Vec<Option<JsonValue>> {
        let mut results: Vec<Option<JsonValue>> = vec![None; urls.len()];
        let mut by_language: HashMap<String, Vec<(usize, String)>> = HashMap::new();
        for (i, url) in urls.iter().enumerate() {
            let Ok((lang, address)) = Self::parse_expr_url(url) else {
                continue;
            };
            if lang == "literal" {
                results[i] = self.get_expression(&lang, &address).await.ok().flatten();
            } else {
                by_language.entry(lang).or_default().push((i, address));
            }
        }

        for (lang, entries) in by_language {
            if !self.is_language_loaded(&lang).await {
                if !load_missing {
                    continue;
                }
                if let Err(e) = self.language_by_ref(&lang).await {
                    warn!(
                        "Could not load language {} to resolve expressions: {}",
                        lang, e
                    );
                    continue;
                }
            }
            let addresses: Vec<String> = entries.iter().map(|(_, a)| a.clone()).collect();
            let fetched = self.get_expressions(&lang, &addresses).await;
            for ((i, _), outcome) in entries.iter().zip(fetched) {
                results[*i] = outcome.ok().flatten();
            }
        }
        results
    }

    /// Fetch and cache the immutable expressions among `urls` that are not
    /// cached yet, so they are readable offline later. Mutable expressions
    /// and languages that are not loaded are skipped; failures are dropped.
    pub async fn prefetch_expressions(&self, urls: &[String]) {
        let mut by_language: HashMap<String, Vec<String>> = HashMap::new();
        for url in urls {
            if let Ok((lang, address)) = Self::parse_expr_url(url) {
                if lang != "literal" && lang != "did" {
                    by_language.entry(lang).or_default().push(address);
                }
            }
        }

        for (lang, mut addresses) in by_language {
            if !self.is_language_loaded(&lang).await {
                continue;
            }
            let lang = self.resolve_language_alias(&lang).await;
            addresses.sort();
            addresses.dedup();
            let urls: Vec<String> = addresses.iter().map(|a| expression_url(&lang, a)).collect();
            let Ok(cached) = Ad4mDb::with_global_instance(|db| db.cached_expressions(&urls)) else {
                continue;
            };
            let missing: Vec<String> = addresses
                .into_iter()
                .zip(cached)
                .filter_map(|(a, hit)| hit.is_none().then_some(a))
                .collect();
            for chunk in missing.chunks(PREFETCH_CHUNK) {
                self.fetch_uncached(&lang, chunk, true).await;
            }
        }
    }

    /// One runtime call fetching `addresses`; caches every immutable result.
    async fn fetch_uncached(
        &self,
        lang: &str,
        addresses: &[String],
        only_immutable: bool,
    ) -> Vec<Result<Option<JsonValue>, LanguageError>> {
        let script = fetch_script(addresses, only_immutable);
        let outcomes: Vec<FetchOutcome> = match self.execute_on_language(lang, &script).await {
            Ok(raw) => match serde_json::from_str(raw.trim()) {
                Ok(outcomes) => outcomes,
                Err(e) => {
                    let error = LanguageError::SerializationError {
                        message: format!("Failed to parse expressions: {}", e),
                    };
                    return vec![Err(error); addresses.len()];
                }
            },
            Err(e) => return vec![Err(e); addresses.len()],
        };

        addresses
            .iter()
            .zip(outcomes)
            .map(|(address, outcome)| {
                if let Some(message) = outcome.error {
                    return Err(LanguageError::RuntimeError {
                        address: lang.to_string(),
                        message,
                    });
                }
                let mut json = outcome.expression;
                if json.is_null() {
                    return Ok(None);
                }
                if outcome.immutable {
                    if let Ok(expr) = serde_json::from_value::<Expression<JsonValue>>(json.clone())
                    {
                        let url = expression_url(lang, address);
                        if let Err(e) =
                            Ad4mDb::with_global_instance(|db| db.cache_expression(&url, &expr))
                        {
                            warn!("Failed to cache expression {}: {}", url, e);
                        }
                    }
                }
                Self::verify_expression_proof(&mut json);
                Ok(Some(json))
            })
            .collect()
    }

    /// Create through `expressionPrepare` / `expressionPublish` when the
    /// language exports both; `Ok(None)` when it does not, and the caller
    /// falls back to `expressionCreate`. Returns the expression address.
    ///
    /// An immutable expression is cached before it is published, and a
    /// failed publish is queued rather than returned: the expression is
    /// already readable here, and the worker publishes it when it can. A
    /// mutable one cannot be served from the cache, so its publish must
    /// succeed now.
    pub(super) async fn create_via_prepare(
        &self,
        lang: &str,
        content_json: &str,
        agent_context: &AgentContext,
    ) -> Result<Option<String>, LanguageError> {
        let script = format!(
            r#"JSON.stringify(await (async () => {{
                if (typeof language.expressionPrepare !== "function"
                    || typeof language.expressionPublish !== "function") {{
                    return null;
                }}
                const prepared = await language.expressionPrepare({content_json});
                if (!prepared || typeof prepared.address !== "string") {{
                    throw new Error("expressionPrepare returned no address");
                }}
                let immutable = false;
                if (typeof language.isImmutableExpression === "function") {{
                    immutable = !!(await language.isImmutableExpression(prepared.address));
                }}
                return {{ address: prepared.address, expression: prepared.expression, immutable }};
            }})())"#
        );
        let raw = self
            .execute_on_language_with_context(lang, &script, agent_context)
            .await?;
        let prepared: Option<Prepared> =
            serde_json::from_str(raw.trim()).map_err(|e| LanguageError::SerializationError {
                message: format!("expressionPrepare returned an unexpected result: {}", e),
            })?;
        let Some(prepared) = prepared else {
            return Ok(None);
        };

        let cacheable = prepared.immutable
            && serde_json::from_value::<Expression<JsonValue>>(prepared.expression.clone()).is_ok();
        if !cacheable {
            self.publish_expression(lang, &prepared.address, &prepared.expression)
                .await?;
            return Ok(Some(prepared.address));
        }

        let url = expression_url(lang, &prepared.address);
        let expr: Expression<JsonValue> = serde_json::from_value(prepared.expression.clone())
            .map_err(|e| LanguageError::SerializationError {
                message: e.to_string(),
            })?;
        Ad4mDb::with_global_instance(|db| db.cache_expression(&url, &expr)).map_err(|e| {
            LanguageError::IoError {
                message: format!("Failed to cache expression {}: {}", url, e),
            }
        })?;

        if let Err(e) = self
            .publish_expression(lang, &prepared.address, &prepared.expression)
            .await
        {
            warn!("Publishing {} failed, queued to retry: {}", url, e);
            Ad4mDb::with_global_instance(|db| {
                db.queue_expression_publish(
                    &url,
                    lang,
                    &prepared.address,
                    &prepared.expression,
                    now_ms() + publish_backoff_ms(0),
                )
            })
            .map_err(|e| LanguageError::IoError {
                message: format!("Failed to queue publish of {}: {}", url, e),
            })?;
        }
        Ok(Some(prepared.address))
    }

    async fn publish_expression(
        &self,
        lang: &str,
        address: &str,
        expression: &JsonValue,
    ) -> Result<(), LanguageError> {
        let address = serde_json::to_string(address).unwrap_or_else(|_| "\"\"".to_string());
        let expression = serde_json::to_string(expression).unwrap_or_else(|_| "null".to_string());
        let script = format!(
            r#"JSON.stringify(await (async () => {{
                await language.expressionPublish({address}, {expression});
                return null;
            }})())"#
        );
        self.execute_on_language(lang, &script).await.map(|_| ())
    }

    /// Retry queued publishes that are due. Each failure doubles that
    /// expression's delay, up to `PUBLISH_BACKOFF_MAX_MS`; nothing is ever
    /// dropped, since the queue may hold the only copy outside this node.
    async fn publish_due(&self) {
        let now = now_ms();
        let due = match Ad4mDb::with_global_instance(|db| {
            db.due_expression_publishes(now, PUBLISH_BATCH)
        }) {
            Ok(due) => due,
            Err(e) => {
                warn!("Could not read the expression publish queue: {}", e);
                return;
            }
        };

        for pending in due {
            // Installed languages load after the agent unlocks; until then
            // the row waits without counting as a failed attempt.
            if !self.is_language_loaded(&pending.language_address).await {
                let deferred = Ad4mDb::with_global_instance(|db| {
                    db.reschedule_expression_publish(
                        &pending.url,
                        pending.attempts,
                        now + PUBLISH_POLL_INTERVAL.as_millis() as i64,
                        "language not loaded",
                    )
                });
                if let Err(e) = deferred {
                    warn!(
                        "Could not update the publish queue for {}: {}",
                        pending.url, e
                    );
                }
                continue;
            }
            let result = self
                .publish_expression(
                    &pending.language_address,
                    &pending.expression_address,
                    &pending.expression,
                )
                .await;

            let outcome = match result {
                Ok(()) => {
                    info!("Published queued expression {}", pending.url);
                    Ad4mDb::with_global_instance(|db| db.complete_expression_publish(&pending.url))
                }
                Err(e) => {
                    let attempts = pending.attempts + 1;
                    Ad4mDb::with_global_instance(|db| {
                        db.reschedule_expression_publish(
                            &pending.url,
                            attempts,
                            now_ms() + publish_backoff_ms(attempts),
                            &e.to_string(),
                        )
                    })
                }
            };
            if let Err(e) = outcome {
                warn!(
                    "Could not update the publish queue for {}: {}",
                    pending.url, e
                );
            }
        }
    }

    /// Start the loop that publishes queued expressions. Idempotent: the
    /// first call starts it, later calls do nothing.
    pub fn start_publish_worker() {
        if PUBLISH_WORKER_STARTED.swap(true, Ordering::SeqCst) {
            return;
        }
        tokio::spawn(async {
            loop {
                LanguageController::global_instance().publish_due().await;
                tokio::time::sleep(PUBLISH_POLL_INTERVAL).await;
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn publish_backoff_doubles_then_caps() {
        assert_eq!(publish_backoff_ms(0), 10_000);
        assert_eq!(publish_backoff_ms(1), 20_000);
        assert_eq!(publish_backoff_ms(3), 80_000);
        assert_eq!(publish_backoff_ms(7), PUBLISH_BACKOFF_MAX_MS);
        assert_eq!(publish_backoff_ms(u32::MAX), PUBLISH_BACKOFF_MAX_MS);
    }

    #[test]
    fn fetch_script_is_ascii_and_escapes_addresses() {
        let script = fetch_script(&["a\"b".to_string()], true);
        assert!(script.is_ascii());
        assert!(script.contains(r#"["a\"b"]"#));
        assert!(script.contains("const onlyImmutable = true;"));
    }
}
