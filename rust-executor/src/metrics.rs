//! Resource counters of the executor, and their Prometheus text form (`GET /metrics`).
//!
//! What a hosted executor needs to see per tenant: which perspective, which user and which
//! language uses the queries, the writes and the language calls.
//!
//! - Per perspective: [`PerspectiveCounters`], one per [`PerspectiveInstance`], on the
//!   instance. A removed perspective leaves the registry, so it leaves the output too.
//! - Per language: [`LanguageCallStats`], one per language runtime handle. An unloaded
//!   language leaves the runtimes map, so it leaves the output too.
//! - Per user (the DID a session acts as): [`UserCounters`] in one map. An entry is made
//!   only for a session that acts as an agent of this executor (the main agent or a managed
//!   agent), so the map holds at most one entry per agent. The executor never deletes an
//!   agent, so no entry is removed. The gauges per user are read from the perspectives at
//!   scrape time.
//! - Executor-wide: the in-flight queries and the open SPARQL stores are two atomics.
//!
//! The hot paths only add to atomics. The user map takes a read lock once per query call
//! and once per write in a request; it takes the write lock only the first time a DID is
//! seen. Gauges that live elsewhere (subscriptions, owners, store size) are read only when
//! the route is scraped.
//!
//! [`PerspectiveInstance`]: crate::perspectives::perspective_instance::PerspectiveInstance

use std::collections::{BTreeMap, HashMap};
use std::fmt::Write as _;
use std::sync::atomic::{AtomicI64, AtomicU64, Ordering};
use std::sync::{Arc, LazyLock, RwLock};
use std::time::{Duration, Instant};

/// Counters of one perspective.
#[derive(Debug, Default)]
pub struct PerspectiveCounters {
    /// Client query calls that ran an evaluation (links, SPARQL, model queries, getters,
    /// the first evaluation of a subscription).
    pub queries: AtomicU64,
    /// Time those calls held their evaluation, in microseconds.
    pub query_micros: AtomicU64,
    /// Subscription re-evaluations after a write.
    pub subscription_rechecks: AtomicU64,
    /// Links written to the store, from any source (local writes and neighbourhood diffs).
    pub links_added: AtomicU64,
    /// Links removed from the store, from any source.
    pub links_removed: AtomicU64,
}

/// Counters of one user: the DID a session acts as.
#[derive(Debug, Default)]
pub struct UserCounters {
    /// Client query calls the user made.
    pub queries: AtomicU64,
    /// Time those calls held their evaluation, in microseconds.
    pub query_micros: AtomicU64,
    /// Links added or removed by the user's requests.
    pub links_written: AtomicU64,
}

/// Counters of one language runtime: the script calls into it.
#[derive(Debug, Default)]
pub struct LanguageCallStats {
    /// Script calls that ended (success or error).
    pub calls: AtomicU64,
    /// Time from send to answer, queue included, in microseconds.
    pub call_micros: AtomicU64,
    /// Calls that ended with an error.
    pub errors: AtomicU64,
    /// Calls sent and not yet answered.
    pub in_flight: AtomicI64,
}

impl LanguageCallStats {
    /// Counts one call as in flight until [`LanguageCall::finish`] or the guard's drop.
    pub fn start(self: &Arc<Self>) -> LanguageCall {
        self.in_flight.fetch_add(1, Ordering::Relaxed);
        LanguageCall {
            stats: self.clone(),
            started: Instant::now(),
            ok: false,
        }
    }
}

/// One language call in flight. A call dropped before [`LanguageCall::finish`] (its caller
/// stopped waiting) counts as an error.
pub struct LanguageCall {
    stats: Arc<LanguageCallStats>,
    started: Instant,
    ok: bool,
}

impl LanguageCall {
    /// Ends the call with its outcome.
    pub fn finish(mut self, ok: bool) {
        self.ok = ok;
    }
}

impl Drop for LanguageCall {
    fn drop(&mut self) {
        let s = &self.stats;
        s.calls.fetch_add(1, Ordering::Relaxed);
        s.call_micros
            .fetch_add(micros(self.started.elapsed()), Ordering::Relaxed);
        if !self.ok {
            s.errors.fetch_add(1, Ordering::Relaxed);
        }
        s.in_flight.fetch_sub(1, Ordering::Relaxed);
    }
}

static IN_FLIGHT_QUERIES: AtomicI64 = AtomicI64::new(0);
static OPEN_STORES: AtomicI64 = AtomicI64::new(0);

static USERS: LazyLock<RwLock<HashMap<String, Arc<UserCounters>>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

tokio::task_local! {
    /// The counters of the user whose request runs on this task (`HandlerMap::dispatch`).
    static CALLER: Option<Arc<UserCounters>>;
}

/// The counters of `did`, made on first use.
pub fn user(did: &str) -> Arc<UserCounters> {
    if let Some(counters) = USERS.read().unwrap_or_else(|e| e.into_inner()).get(did) {
        return counters.clone();
    }
    USERS
        .write()
        .unwrap_or_else(|e| e.into_inner())
        .entry(did.to_string())
        .or_default()
        .clone()
}

/// Runs `request` with `did` as the user its link writes count for.
pub async fn as_caller<F: std::future::Future>(did: Option<&str>, request: F) -> F::Output {
    CALLER.scope(did.map(user), request).await
}

/// Counts a store write on `perspective`, and on the user whose request made it, if any.
pub fn record_link_writes(perspective: &PerspectiveCounters, added: usize, removed: usize) {
    if added == 0 && removed == 0 {
        return;
    }
    perspective
        .links_added
        .fetch_add(added as u64, Ordering::Relaxed);
    perspective
        .links_removed
        .fetch_add(removed as u64, Ordering::Relaxed);
    let _ = CALLER.try_with(|caller| {
        if let Some(user) = caller {
            user.links_written
                .fetch_add((added + removed) as u64, Ordering::Relaxed);
        }
    });
}

/// One client query call, from the moment it holds its slot until its evaluation ends.
/// It rides on the call's `EvalStop`, so a stopped evaluation that still runs on a blocking
/// thread stays in flight until it really ends.
#[derive(Debug)]
pub struct QueryTimer {
    perspective: Arc<PerspectiveCounters>,
    user: Option<Arc<UserCounters>>,
    started: Instant,
}

impl QueryTimer {
    /// Starts a query on `perspective` for the user `did`, if any.
    pub fn start(perspective: Arc<PerspectiveCounters>, did: Option<&str>) -> QueryTimer {
        IN_FLIGHT_QUERIES.fetch_add(1, Ordering::Relaxed);
        QueryTimer {
            perspective,
            user: did.map(user),
            started: Instant::now(),
        }
    }
}

impl Drop for QueryTimer {
    fn drop(&mut self) {
        let spent = micros(self.started.elapsed());
        self.perspective.queries.fetch_add(1, Ordering::Relaxed);
        self.perspective
            .query_micros
            .fetch_add(spent, Ordering::Relaxed);
        if let Some(user) = &self.user {
            user.queries.fetch_add(1, Ordering::Relaxed);
            user.query_micros.fetch_add(spent, Ordering::Relaxed);
        }
        IN_FLIGHT_QUERIES.fetch_sub(1, Ordering::Relaxed);
    }
}

/// Counts one open SPARQL store while it lives. Every clone of a store shares one token, so
/// a store counts until its last clone drops.
#[derive(Debug)]
pub struct OpenStore(());

impl OpenStore {
    pub fn open() -> OpenStore {
        OPEN_STORES.fetch_add(1, Ordering::Relaxed);
        OpenStore(())
    }
}

impl Drop for OpenStore {
    fn drop(&mut self) {
        OPEN_STORES.fetch_sub(1, Ordering::Relaxed);
    }
}

fn micros(d: Duration) -> u64 {
    u64::try_from(d.as_micros()).unwrap_or(u64::MAX)
}

/// Queries in flight across the executor.
pub fn in_flight_queries() -> i64 {
    IN_FLIGHT_QUERIES.load(Ordering::Relaxed)
}

/// SPARQL stores open in this process.
pub fn open_stores() -> i64 {
    OPEN_STORES.load(Ordering::Relaxed)
}

/// One perspective's row of a [`Snapshot`].
#[derive(Debug, Clone, Default, PartialEq)]
pub struct PerspectiveRow {
    pub uuid: String,
    pub queries: u64,
    pub query_micros: u64,
    pub subscriptions: u64,
    pub subscription_rechecks: u64,
    pub links_added: u64,
    pub links_removed: u64,
    /// Bytes of the store's files on disk; `None` for an in-memory store.
    pub store_bytes: Option<u64>,
}

/// One user's row of a [`Snapshot`].
#[derive(Debug, Clone, Default, PartialEq)]
pub struct UserRow {
    pub did: String,
    pub queries: u64,
    pub query_micros: u64,
    pub subscriptions: u64,
    pub links_written: u64,
    pub perspectives: u64,
}

/// One language's row of a [`Snapshot`].
#[derive(Debug, Clone, Default, PartialEq)]
pub struct LanguageRow {
    pub address: String,
    pub name: Option<String>,
    pub calls: u64,
    pub call_micros: u64,
    pub errors: u64,
    pub in_flight: i64,
}

/// Every counter at one moment.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Snapshot {
    pub open_perspectives: u64,
    pub open_stores: i64,
    pub in_flight_queries: i64,
    pub perspectives: Vec<PerspectiveRow>,
    pub users: Vec<UserRow>,
    pub languages: Vec<LanguageRow>,
}

impl LanguageRow {
    pub fn of(address: &str, name: Option<&str>, stats: &LanguageCallStats) -> LanguageRow {
        LanguageRow {
            address: address.to_string(),
            name: name.map(str::to_string),
            calls: stats.calls.load(Ordering::Relaxed),
            call_micros: stats.call_micros.load(Ordering::Relaxed),
            errors: stats.errors.load(Ordering::Relaxed),
            in_flight: stats.in_flight.load(Ordering::Relaxed),
        }
    }
}

/// Bytes of the files directly in `dir` and its subdirectories. A RocksDB store is a flat
/// directory of tens of files, so this is a few `stat`s per perspective.
fn dir_bytes(dir: &std::path::Path) -> Option<u64> {
    let mut total = 0;
    for entry in std::fs::read_dir(dir).ok()?.flatten() {
        let Ok(meta) = entry.metadata() else { continue };
        if meta.is_dir() {
            total += dir_bytes(&entry.path()).unwrap_or(0);
        } else {
            total += meta.len();
        }
    }
    Some(total)
}

/// Reads every counter: the perspective registry, the language runtimes and the user map.
pub async fn snapshot() -> Snapshot {
    let main_did = crate::agent::did_for_context(&crate::agent::AgentContext::main_agent()).ok();
    let app_data = crate::perspectives::get_app_data_path();

    let mut perspectives = Vec::new();
    // Per user: (live subscriptions, perspectives owned).
    let mut gauges: BTreeMap<String, (u64, u64)> = BTreeMap::new();
    let instances = crate::perspectives::all_perspectives();
    for p in &instances {
        let owners = p.persisted.lock().await.owners.clone();
        match owners.filter(|o| !o.is_empty()) {
            Some(owners) => {
                for owner in owners {
                    gauges.entry(owner).or_default().1 += 1;
                }
            }
            None => {
                if let Some(main) = &main_did {
                    gauges.entry(main.clone()).or_default().1 += 1;
                }
            }
        }
        let subscribers = p.subscription_subscribers().await;
        for subscriber in &subscribers {
            if let Some(did) = subscriber.as_ref().or(main_did.as_ref()) {
                gauges.entry(did.clone()).or_default().0 += 1;
            }
        }
        let c = &p.metrics;
        perspectives.push(PerspectiveRow {
            uuid: p.uuid.clone(),
            queries: c.queries.load(Ordering::Relaxed),
            query_micros: c.query_micros.load(Ordering::Relaxed),
            subscriptions: subscribers.len() as u64,
            subscription_rechecks: c.subscription_rechecks.load(Ordering::Relaxed),
            links_added: c.links_added.load(Ordering::Relaxed),
            links_removed: c.links_removed.load(Ordering::Relaxed),
            store_bytes: None,
        });
    }
    if let Some(base) = app_data {
        perspectives = tokio::task::spawn_blocking(move || {
            for row in &mut perspectives {
                let dir = std::path::Path::new(&base)
                    .join("perspectives")
                    .join(&row.uuid)
                    .join("sparql_store");
                row.store_bytes = dir_bytes(&dir);
            }
            perspectives
        })
        .await
        .unwrap_or_default();
    }
    perspectives.sort_by(|a, b| a.uuid.cmp(&b.uuid));

    let mut users: BTreeMap<String, UserRow> = BTreeMap::new();
    for (did, c) in USERS.read().unwrap_or_else(|e| e.into_inner()).iter() {
        users.insert(
            did.clone(),
            UserRow {
                did: did.clone(),
                queries: c.queries.load(Ordering::Relaxed),
                query_micros: c.query_micros.load(Ordering::Relaxed),
                links_written: c.links_written.load(Ordering::Relaxed),
                ..Default::default()
            },
        );
    }
    for (did, (subscriptions, owned)) in gauges {
        let row = users.entry(did.clone()).or_insert_with(|| UserRow {
            did,
            ..Default::default()
        });
        row.subscriptions = subscriptions;
        row.perspectives = owned;
    }

    let mut languages = crate::languages::LanguageController::global_instance()
        .call_stats()
        .await;
    languages.sort_by(|a, b| a.address.cmp(&b.address));

    Snapshot {
        open_perspectives: instances.len() as u64,
        open_stores: open_stores(),
        in_flight_queries: in_flight_queries(),
        perspectives,
        users: users.into_values().collect(),
        languages,
    }
}

/// A label value in the text format: backslash, double quote and newline escaped.
fn label(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
}

fn seconds(micros: u64) -> f64 {
    micros as f64 / 1_000_000.0
}

/// Writes one metric family: its HELP and TYPE lines and one sample per row.
fn family<T>(
    out: &mut String,
    name: &str,
    kind: &str,
    help: &str,
    rows: &[T],
    sample: impl Fn(&T) -> (String, String),
) {
    let _ = writeln!(out, "# HELP {name} {help}");
    let _ = writeln!(out, "# TYPE {name} {kind}");
    for row in rows {
        let (labels, value) = sample(row);
        let _ = writeln!(out, "{name}{{{labels}}} {value}");
    }
}

fn single(out: &mut String, name: &str, help: &str, value: i64) {
    let _ = writeln!(out, "# HELP {name} {help}");
    let _ = writeln!(out, "# TYPE {name} gauge");
    let _ = writeln!(out, "{name} {value}");
}

/// The snapshot in the Prometheus text exposition format (version 0.0.4).
pub fn render(s: &Snapshot) -> String {
    let mut out = String::new();
    single(
        &mut out,
        "ad4m_perspectives_open",
        "Perspectives loaded in the executor.",
        s.open_perspectives as i64,
    );
    single(
        &mut out,
        "ad4m_sparql_stores_open",
        "SPARQL stores open in the process.",
        s.open_stores,
    );
    single(
        &mut out,
        "ad4m_queries_in_flight",
        "Client query calls evaluating now.",
        s.in_flight_queries,
    );

    let p = |r: &PerspectiveRow| format!("perspective=\"{}\"", label(&r.uuid));
    family(
        &mut out,
        "ad4m_perspective_queries_total",
        "counter",
        "Client query calls on the perspective.",
        &s.perspectives,
        |r| (p(r), r.queries.to_string()),
    );
    family(
        &mut out,
        "ad4m_perspective_query_seconds_total",
        "counter",
        "Time client query calls on the perspective evaluated.",
        &s.perspectives,
        |r| (p(r), seconds(r.query_micros).to_string()),
    );
    family(
        &mut out,
        "ad4m_perspective_subscriptions",
        "gauge",
        "Live query subscriptions on the perspective.",
        &s.perspectives,
        |r| (p(r), r.subscriptions.to_string()),
    );
    family(
        &mut out,
        "ad4m_perspective_subscription_rechecks_total",
        "counter",
        "Subscription re-evaluations after writes to the perspective.",
        &s.perspectives,
        |r| (p(r), r.subscription_rechecks.to_string()),
    );
    family(
        &mut out,
        "ad4m_perspective_links_added_total",
        "counter",
        "Links added to the perspective's store, local and synced.",
        &s.perspectives,
        |r| (p(r), r.links_added.to_string()),
    );
    family(
        &mut out,
        "ad4m_perspective_links_removed_total",
        "counter",
        "Links removed from the perspective's store, local and synced.",
        &s.perspectives,
        |r| (p(r), r.links_removed.to_string()),
    );
    let on_disk: Vec<(String, u64)> = s
        .perspectives
        .iter()
        .filter_map(|r| r.store_bytes.map(|b| (r.uuid.clone(), b)))
        .collect();
    family(
        &mut out,
        "ad4m_perspective_store_bytes",
        "gauge",
        "Bytes of the perspective's store files on disk.",
        &on_disk,
        |(uuid, bytes)| {
            (
                format!("perspective=\"{}\"", label(uuid)),
                bytes.to_string(),
            )
        },
    );

    let u = |r: &UserRow| format!("user=\"{}\"", label(&r.did));
    family(
        &mut out,
        "ad4m_user_queries_total",
        "counter",
        "Client query calls the user made.",
        &s.users,
        |r| (u(r), r.queries.to_string()),
    );
    family(
        &mut out,
        "ad4m_user_query_seconds_total",
        "counter",
        "Time the user's query calls evaluated.",
        &s.users,
        |r| (u(r), seconds(r.query_micros).to_string()),
    );
    family(
        &mut out,
        "ad4m_user_subscriptions",
        "gauge",
        "Live query subscriptions the user holds.",
        &s.users,
        |r| (u(r), r.subscriptions.to_string()),
    );
    family(
        &mut out,
        "ad4m_user_links_written_total",
        "counter",
        "Links the user's requests added or removed.",
        &s.users,
        |r| (u(r), r.links_written.to_string()),
    );
    family(
        &mut out,
        "ad4m_user_perspectives",
        "gauge",
        "Loaded perspectives the user owns.",
        &s.users,
        |r| (u(r), r.perspectives.to_string()),
    );

    let l = |r: &LanguageRow| {
        format!(
            "language=\"{}\",name=\"{}\"",
            label(&r.address),
            label(r.name.as_deref().unwrap_or(""))
        )
    };
    family(
        &mut out,
        "ad4m_language_calls_total",
        "counter",
        "Script calls into the language that ended.",
        &s.languages,
        |r| (l(r), r.calls.to_string()),
    );
    family(
        &mut out,
        "ad4m_language_call_seconds_total",
        "counter",
        "Time script calls into the language took, queue included.",
        &s.languages,
        |r| (l(r), seconds(r.call_micros).to_string()),
    );
    family(
        &mut out,
        "ad4m_language_call_errors_total",
        "counter",
        "Script calls into the language that ended with an error.",
        &s.languages,
        |r| (l(r), r.errors.to_string()),
    );
    family(
        &mut out,
        "ad4m_language_calls_in_flight",
        "gauge",
        "Script calls into the language sent and not answered.",
        &s.languages,
        |r| (l(r), r.in_flight.to_string()),
    );
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_query_timer_counts_on_the_perspective_and_the_user_and_leaves_no_query_in_flight() {
        let perspective = Arc::new(PerspectiveCounters::default());
        let did = "did:key:metrics-timer-user";
        let before = in_flight_queries();
        let timer = QueryTimer::start(perspective.clone(), Some(did));
        assert_eq!(in_flight_queries(), before + 1);
        std::thread::sleep(Duration::from_millis(2));
        drop(timer);
        assert_eq!(in_flight_queries(), before);
        assert_eq!(perspective.queries.load(Ordering::Relaxed), 1);
        assert!(perspective.query_micros.load(Ordering::Relaxed) >= 2_000);
        let u = user(did);
        assert_eq!(u.queries.load(Ordering::Relaxed), 1);
        assert!(u.query_micros.load(Ordering::Relaxed) >= 2_000);
    }

    #[test]
    fn a_language_call_counts_and_its_gauge_returns_to_zero() {
        let stats = Arc::new(LanguageCallStats::default());
        let ok = stats.start();
        let failed = stats.start();
        assert_eq!(stats.in_flight.load(Ordering::Relaxed), 2);
        ok.finish(true);
        failed.finish(false);
        // A caller that stopped waiting: an error.
        drop(stats.start());
        assert_eq!(stats.in_flight.load(Ordering::Relaxed), 0);
        assert_eq!(stats.calls.load(Ordering::Relaxed), 3);
        assert_eq!(stats.errors.load(Ordering::Relaxed), 2);
    }

    #[tokio::test]
    async fn link_writes_count_for_the_caller_only_inside_its_request() {
        let perspective = PerspectiveCounters::default();
        let did = "did:key:metrics-writer";
        as_caller(Some(did), async {
            record_link_writes(&perspective, 3, 1);
        })
        .await;
        // Outside a request (a synced diff): the perspective only.
        record_link_writes(&perspective, 2, 0);
        assert_eq!(perspective.links_added.load(Ordering::Relaxed), 5);
        assert_eq!(perspective.links_removed.load(Ordering::Relaxed), 1);
        assert_eq!(user(did).links_written.load(Ordering::Relaxed), 4);
    }

    #[test]
    fn open_stores_count_until_dropped() {
        let before = open_stores();
        let a = OpenStore::open();
        let b = OpenStore::open();
        assert_eq!(open_stores(), before + 2);
        drop(a);
        drop(b);
        assert_eq!(open_stores(), before);
    }

    #[test]
    fn the_text_format_has_one_family_per_metric_and_escaped_labels() {
        let s = Snapshot {
            open_perspectives: 1,
            open_stores: 1,
            in_flight_queries: 0,
            perspectives: vec![PerspectiveRow {
                uuid: "p-1".into(),
                queries: 4,
                query_micros: 1_500_000,
                subscriptions: 2,
                subscription_rechecks: 7,
                links_added: 3,
                links_removed: 1,
                store_bytes: Some(4096),
            }],
            users: vec![UserRow {
                did: "did:key:u\"1".into(),
                queries: 4,
                query_micros: 1_500_000,
                subscriptions: 2,
                links_written: 4,
                perspectives: 1,
            }],
            languages: vec![LanguageRow {
                address: "Qm1".into(),
                name: Some("agent".into()),
                calls: 9,
                call_micros: 250_000,
                errors: 1,
                in_flight: 0,
            }],
        };
        let text = render(&s);
        for line in [
            "ad4m_perspectives_open 1",
            "ad4m_sparql_stores_open 1",
            "ad4m_queries_in_flight 0",
            "ad4m_perspective_queries_total{perspective=\"p-1\"} 4",
            "ad4m_perspective_query_seconds_total{perspective=\"p-1\"} 1.5",
            "ad4m_perspective_subscriptions{perspective=\"p-1\"} 2",
            "ad4m_perspective_subscription_rechecks_total{perspective=\"p-1\"} 7",
            "ad4m_perspective_links_added_total{perspective=\"p-1\"} 3",
            "ad4m_perspective_links_removed_total{perspective=\"p-1\"} 1",
            "ad4m_perspective_store_bytes{perspective=\"p-1\"} 4096",
            "ad4m_user_queries_total{user=\"did:key:u\\\"1\"} 4",
            "ad4m_user_links_written_total{user=\"did:key:u\\\"1\"} 4",
            "ad4m_user_perspectives{user=\"did:key:u\\\"1\"} 1",
            "ad4m_language_calls_total{language=\"Qm1\",name=\"agent\"} 9",
            "ad4m_language_call_seconds_total{language=\"Qm1\",name=\"agent\"} 0.25",
            "ad4m_language_call_errors_total{language=\"Qm1\",name=\"agent\"} 1",
            "ad4m_language_calls_in_flight{language=\"Qm1\",name=\"agent\"} 0",
        ] {
            assert!(
                text.lines().any(|l| l == line),
                "missing {line:?} in\n{text}"
            );
        }
        // Each family declared once.
        let types = text.lines().filter(|l| l.starts_with("# TYPE ")).count();
        assert_eq!(types, 19);
    }
}
