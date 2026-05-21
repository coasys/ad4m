use super::model_query::load_shape_from_store;
use super::model_query::types::{ModelShape, ShapeResolver};
use super::sdna::{generic_link_fact, is_sdna_link};
use super::shacl_parser::parse_shacl_to_links;
use super::update_perspective;
use super::utils::{prolog_get_all_string_bindings, prolog_resolution_to_string};
use crate::agent::AgentContext;
use crate::agent::{create_signed_expression, did_for_context};
use crate::languages::language::Language;
use crate::languages::LanguageController;
use crate::perspectives::utils::{prolog_get_first_binding, prolog_value_to_json_string};
use crate::prolog_service::get_prolog_service;
use crate::prolog_service::types::QueryResolution;
use crate::prolog_service::PrologService;
use crate::prolog_service::{
    engine_pool::FILTERING_THRESHOLD, DEFAULT_POOL_SIZE, DEFAULT_POOL_SIZE_WITH_FILTERING,
};
use crate::prolog_service::{PrologMode, PROLOG_MODE};
use crate::pubsub::{
    get_global_pubsub, NEIGHBOURHOOD_SIGNAL_TOPIC, PERSPECTIVE_LINK_ADDED_TOPIC,
    PERSPECTIVE_LINK_REMOVED_TOPIC, PERSPECTIVE_LINK_UPDATED_TOPIC,
    PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC, PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC,
    RUNTIME_NOTIFICATION_TRIGGERED_TOPIC,
};
use crate::types::{
    DecoratedPerspectiveDiff, LinkMutations, LinkQuery, LinkStatus, NeighbourhoodSignalFilter,
    OnlineAgent, PerspectiveExpression, PerspectiveHandle, PerspectiveLinkUpdatedWithOwner,
    PerspectiveLinkWithOwner, PerspectiveQuerySubscriptionFilter, PerspectiveState,
    PerspectiveStateFilter,
};
use crate::{db::Ad4mDb, types::*};
use ad4m_client::literal::Literal;
use chrono::DateTime;
use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use futures::future;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::future::Future;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, LazyLock};
use std::time::Duration;
use tokio::sync::{Mutex, RwLock};
use tokio::time::{sleep, Instant};
use tokio::{join, time};
use urlencoding;

/// Tracks which predicates have changed since the last subscription check.
#[derive(Debug, Clone)]
enum ChangedPredicates {
    /// No changes recorded yet (initial state)
    NoneRecorded,
    /// A predicate-less diff was seen — must check ALL subscriptions
    CheckAll,
    /// Only specific predicates changed
    Specific(HashSet<String>),
}
use uuid;
use uuid::Uuid;

static MAX_COMMIT_BYTES: usize = 3_000_000; //3MiB
static MAX_PENDING_DIFFS_COUNT: usize = 150;
static MAX_PENDING_SECONDS: u64 = 3;
static IMMEDIATE_COMMITS_COUNT: usize = 20;
static QUERY_SUBSCRIPTION_TIMEOUT: u64 = 60; // 1 minute in seconds (was 5 min)
static QUERY_SUBSCRIPTION_CHECK_INTERVAL: u64 = 200; // 200ms

fn notification_pool_name(uuid: &str) -> String {
    format!("notification_{}", uuid)
}

fn is_sparql_query(query: &str) -> bool {
    let trimmed = query.trim();
    // Match SPARQL keywords followed by whitespace or '{' / '<' to avoid
    // false-positives on Prolog atoms like `base(X).`
    let upper = trimmed.to_ascii_uppercase();
    for keyword in &["SELECT", "ASK", "CONSTRUCT", "DESCRIBE", "PREFIX", "BASE"] {
        if upper.starts_with(keyword) {
            let rest = &trimmed[keyword.len()..];
            if rest.is_empty()
                || rest.starts_with(|c: char| c.is_whitespace() || c == '{' || c == '<')
            {
                return true;
            }
        }
    }
    false
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub enum SdnaType {
    SubjectClass,
    Flow,
    Custom,
}

impl SdnaType {
    pub fn from_string(s: &str) -> Result<Self, AnyError> {
        match s {
            "subject_class" => Ok(SdnaType::SubjectClass),
            "flow" => Ok(SdnaType::Flow),
            "custom" => Ok(SdnaType::Custom),
            _ => Err(anyhow!(
                "Invalid SDNA type: {}. Must one of 'subject_class', 'flow' or 'custom'.",
                s
            )),
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub enum Action {
    #[serde(rename = "addLink")]
    AddLink,
    #[serde(rename = "removeLink")]
    RemoveLink,
    #[serde(rename = "setSingleTarget")]
    SetSingleTarget,
    #[serde(rename = "collectionSetter")]
    CollectionSetter,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct Command {
    source: Option<String>,
    predicate: Option<String>,
    target: Option<String>,
    local: Option<bool>,
    action: Action,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SubjectClass {
    #[serde(rename = "C")]
    c: Option<String>,
    #[serde(rename = "Class")]
    class: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SubjectClassProperty {
    #[serde(rename = "C")]
    c: Option<String>,
    #[serde(rename = "Property")]
    property: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SubjectClassCollection {
    #[serde(rename = "C")]
    c: Option<String>,
    #[serde(rename = "Collection")]
    collection: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SubjectClassActions {
    #[serde(rename = "C")]
    c: Option<String>,
    #[serde(rename = "Actions")]
    actions: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct PorpertyValue {
    #[serde(rename = "C")]
    c: Option<String>,
    #[serde(rename = "Value")]
    value: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct SubjectClassOption {
    #[serde(rename = "className")]
    pub class_name: Option<String>,
    #[serde(rename = "query")]
    pub query: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct Parameter {
    name: String,
    value: serde_json::Value,
}

#[derive(Clone)]
struct SubscribedQuery {
    query: String,
    last_result: String,
    last_keepalive: Instant,
    user_email: Option<String>,
    /// Predicate IRIs extracted from the SPARQL/Prolog query at registration time.
    /// If empty, the subscription is always re-checked (safe fallback for variable predicates).
    predicates: HashSet<String>,
    /// When set, this subscription was registered via `model_subscribe_and_query`.
    /// On trigger, `execute_model_query` is called instead of re-running raw SPARQL.
    model_query_params: Option<ModelSubscriptionParams>,
}

/// Parameters for a model query subscription. Stored alongside the trigger SPARQL
/// so that `check_subscribed_queries` can re-execute the model query in Rust.
/// Shape is resolved from the perspective's cache at each re-evaluation —
/// no JSON metadata is held here.
#[derive(Clone)]
struct ModelSubscriptionParams {
    class_name: String,
    query_json: String,
}

/// Extract predicate IRIs from a SPARQL query by finding triple patterns.
/// Returns an empty set if no fixed predicates are found (e.g. `?s ?p ?o`),
/// which means the subscription should always be re-checked.
///
/// Also returns an empty set if a `GRAPH` pattern uses a variable predicate
/// (e.g. `GRAPH ?g { ?source ?predicate ?target }`). Such patterns match links
/// with ANY predicate, so we cannot narrow the subscription to a fixed set.
/// Compiled regexes for SPARQL predicate extraction — compiled once, reused on every call.
static RE_GRAPH_VAR_PRED: LazyLock<regex::Regex> = LazyLock::new(|| {
    regex::Regex::new(
        r"GRAPH\s+\?\w+\s*\{[^}]*(?:\?\w+|<[^>]+>)\s+(\?\w+)\s+(?:\?\w+|<[^>]+>)[^}]*\}",
    )
    .unwrap()
});
static RE_VAR_PRED: LazyLock<regex::Regex> = LazyLock::new(|| {
    regex::Regex::new(r"(?:\?\w+|<[^>]+>)\s+\?\w+\s+(?:\?\w+|<[^>]+>)\s*\.").unwrap()
});
static RE_IRI_PRED: LazyLock<regex::Regex> = LazyLock::new(|| {
    regex::Regex::new(r"(?:\?\w+|<[^>]+>)\s+(<[^>]+>)\s+(?:\?\w+|<[^>]+>)").unwrap()
});

fn extract_predicates_from_sparql(query: &str) -> HashSet<String> {
    // Detect variable predicate inside GRAPH patterns:
    // GRAPH ?var { ... ?var1 ?varPred ?var2 ... }
    // This is the pattern our model queries use for fetching all links.
    if RE_GRAPH_VAR_PRED.is_match(query) {
        return HashSet::new();
    }

    // Detect variable predicate in regular triple patterns (e.g. ?source ?predicate ?target)
    // When a query uses a variable predicate, it can match any predicate,
    // so we must always re-check.
    if RE_VAR_PRED.is_match(query) {
        return HashSet::new();
    }

    let mut predicates = HashSet::new();
    // Match triple patterns: (var|uri) <uri> (var|uri)
    // The middle <uri> is the predicate
    for cap in RE_IRI_PRED.captures_iter(query) {
        let pred = cap[1].trim_matches(|c| c == '<' || c == '>');
        predicates.insert(pred.to_string());
    }
    predicates
}

#[derive(Clone)]
pub struct PerspectiveInstance {
    pub persisted: Arc<Mutex<PerspectiveHandle>>,
    /// Cached UUID — never changes after construction, avoids locking `persisted`.
    pub uuid: String,

    pub created_from_join: bool,
    pub is_fast_polling: bool,
    pub retries: u32,

    is_teardown: Arc<AtomicBool>,
    sdna_change_mutex: Arc<Mutex<()>>,
    prolog_update_mutex: Arc<RwLock<()>>,
    link_language: Arc<RwLock<Option<Language>>>,
    trigger_notification_check: Arc<AtomicBool>,
    trigger_prolog_subscription_check: Arc<AtomicBool>,
    /// Predicates of links changed since last subscription check.
    changed_predicates: Arc<Mutex<ChangedPredicates>>,
    commit_debounce_timer: Arc<Mutex<Option<tokio::time::Instant>>>,
    immediate_commits_remaining: Arc<Mutex<usize>>,
    subscribed_queries: Arc<Mutex<HashMap<String, SubscribedQuery>>>,
    batch_store: Arc<RwLock<HashMap<String, PerspectiveDiff>>>,
    // Fallback sync tracking for ensure_public_links_are_shared
    last_successful_fallback_sync: Arc<Mutex<Option<tokio::time::Instant>>>,
    fallback_sync_interval: Arc<Mutex<Duration>>,
    pub(crate) sparql_store: Arc<crate::perspectives::sparql_store::SparqlStore>,
    /// In-memory cache of parsed `ModelShape` instances keyed by class name.
    /// Populated lazily from SHACL triples in `sparql_store`; invalidated by
    /// `add_sdna_inner` when SHACL is re-written for a class.  No persistence.
    shape_cache: Arc<std::sync::RwLock<HashMap<String, Arc<ModelShape>>>>,
}

/// Cache-backed `ShapeResolver` borrowed from a `PerspectiveInstance` for the
/// lifetime of a single query.  On miss it parses SHACL from the perspective's
/// store and memoizes the result.
struct PerspectiveShapeResolver<'a> {
    cache: &'a std::sync::RwLock<HashMap<String, Arc<ModelShape>>>,
    store: &'a crate::perspectives::sparql_store::SparqlStore,
}

impl<'a> ShapeResolver for PerspectiveShapeResolver<'a> {
    fn get_shape(&self, class_name: &str) -> Result<Arc<ModelShape>, AnyError> {
        if let Some(shape) = self.cache.read().unwrap().get(class_name).cloned() {
            return Ok(shape);
        }
        let shape = load_shape_from_store(self.store, class_name)?;
        let arc = Arc::new(shape);
        self.cache
            .write()
            .unwrap()
            .insert(class_name.to_string(), arc.clone());
        Ok(arc)
    }
}

impl PerspectiveInstance {
    pub fn new(handle: PerspectiveHandle, created_from_join: Option<bool>) -> Self {
        // Build per-perspective data path for persistent SPARQL store
        let sparql_data_path = crate::perspectives::get_app_data_path().map(|base| {
            let p = std::path::PathBuf::from(&base)
                .join("perspectives")
                .join(&handle.uuid);
            p.to_string_lossy().to_string()
        });

        PerspectiveInstance {
            persisted: Arc::new(Mutex::new(handle.clone())),
            uuid: handle.uuid.clone(),

            created_from_join: created_from_join.unwrap_or(false),
            is_fast_polling: false,
            retries: 0,
            is_teardown: Arc::new(AtomicBool::new(false)),
            sdna_change_mutex: Arc::new(Mutex::new(())),
            prolog_update_mutex: Arc::new(RwLock::new(())),
            link_language: Arc::new(RwLock::new(None)),
            trigger_notification_check: Arc::new(AtomicBool::new(false)),
            trigger_prolog_subscription_check: Arc::new(AtomicBool::new(false)),
            changed_predicates: Arc::new(Mutex::new(ChangedPredicates::NoneRecorded)),
            commit_debounce_timer: Arc::new(Mutex::new(None)),
            immediate_commits_remaining: Arc::new(Mutex::new(IMMEDIATE_COMMITS_COUNT)),
            subscribed_queries: Arc::new(Mutex::new(HashMap::new())),
            batch_store: Arc::new(RwLock::new(HashMap::new())),
            last_successful_fallback_sync: Arc::new(Mutex::new(None)),
            fallback_sync_interval: Arc::new(Mutex::new(Duration::from_secs(30))),
            sparql_store: Arc::new(
                crate::perspectives::sparql_store::SparqlStore::new(sparql_data_path.as_deref())
                    .expect("Failed to create per-perspective SPARQL service"),
            ),
            shape_cache: Arc::new(std::sync::RwLock::new(HashMap::new())),
        }
    }

    /// Look up a cached `ModelShape` for the given class name, loading it
    /// from the perspective's SHACL triples on cache miss.  Returns an
    /// `Arc<ModelShape>` shared across query and subscription re-evaluation.
    pub fn get_shape(&self, class_name: &str) -> Result<Arc<ModelShape>, AnyError> {
        self.shape_resolver().get_shape(class_name)
    }

    /// Drop any cached `ModelShape` for `class_name`.  Called when SHACL is
    /// (re-)written for the class so the next query re-parses fresh state.
    pub fn invalidate_shape(&self, class_name: &str) {
        self.shape_cache.write().unwrap().remove(class_name);
    }

    /// Borrow a cache-backed `ShapeResolver` for the lifetime of a single
    /// query.  Used by `execute_model_query` for include recursion.
    fn shape_resolver(&self) -> PerspectiveShapeResolver<'_> {
        PerspectiveShapeResolver {
            cache: &self.shape_cache,
            store: &self.sparql_store,
        }
    }

    pub async fn start_background_tasks(self) {
        let _ = join!(
            self.ensure_link_language(),
            self.notification_check_loop(),
            self.nh_sync_loop(),
            self.pending_diffs_loop(),
            self.subscribed_queries_loop(),
            self.fallback_sync_loop()
        );
    }

    pub async fn teardown_background_tasks(&self) {
        self.is_teardown.store(true, Ordering::Release);
    }

    /// Sync all existing links to the SPARQL (Oxigraph) store
    pub fn sync_existing_links_to_sparql(
        &self,
        links: &[DecoratedLinkExpression],
    ) -> Result<(), deno_core::anyhow::Error> {
        self.sparql_store.reload(links.to_vec())
    }

    async fn ensure_link_language(&self) {
        let mut interval = time::interval(Duration::from_secs(5));
        while !self.is_teardown.load(Ordering::Acquire) {
            if self.link_language.read().await.is_none()
                && self.persisted.lock().await.neighbourhood.is_some()
            {
                let nh = self
                    .persisted
                    .lock()
                    .await
                    .neighbourhood
                    .as_ref()
                    .expect("must be some")
                    .clone();

                log::debug!(
                    "ensure_link_language: checking language {} for perspective",
                    nh.data.link_language
                );

                match LanguageController::language_by_address(nh.data.link_language.clone()).await {
                    Ok(Some(mut language)) => {
                        // Set local agents before storing the language
                        let agents_to_register = {
                            let handle = self.persisted.lock().await;
                            log::debug!(
                                "🔍 ensure_link_language: perspective {} has owners: {:?}",
                                handle.uuid,
                                handle.owners
                            );
                            if let Some(owners) = &handle.owners {
                                if !owners.is_empty() {
                                    log::debug!("🔍 Using owners list: {:?}", owners);
                                    owners.clone()
                                } else {
                                    // Empty owners list - use main agent
                                    log::debug!("🔍 Owners list is empty, using main agent");
                                    vec![crate::agent::did()]
                                }
                            } else {
                                // No owners set - use main agent
                                log::debug!("🔍 No owners set, using main agent");
                                vec![crate::agent::did()]
                            }
                        };

                        log::info!(
                            "Setting local agents for link language: {:?}",
                            agents_to_register
                        );
                        if let Err(e) = language.set_local_agents(agents_to_register).await {
                            log::error!("Failed to set local agents on link language: {:?}", e);
                        }

                        {
                            let mut link_language_guard = self.link_language.write().await;
                            *link_language_guard = Some(language);
                        }
                        // Cache language→perspective mapping for fast signal routing
                        {
                            let handle = self.persisted.lock().await.clone();
                            crate::perspectives::register_link_language_perspective(
                                nh.data.link_language.clone(),
                                handle,
                            );
                        }
                        if self.persisted.lock().await.state
                            == PerspectiveState::NeighbourhoodCreationInitiated
                        {
                            self.ensure_public_links_are_shared().await;
                        }
                        self.update_perspective_state_log_error(
                            PerspectiveState::LinkLanguageInstalledButNotSynced,
                        )
                        .await;
                        break;
                    }
                    Ok(None) => {
                        // Actively try to install the language on this tick.
                        // `language_by_address` only CHECKS the loaded-runtimes
                        // map; it does not trigger an install. Without the
                        // retry call below, a perspective whose initial
                        // install in `install_neighbourhood_with_context`
                        // failed (e.g. the language language was still
                        // warming up) would stay stuck in
                        // LinkLanguageFailedToInstall forever, because
                        // nothing in this loop ever re-attempted the install
                        // — it just kept asking "is it loaded yet?" to a map
                        // nothing was writing to.
                        log::debug!(
                            "Link language {} not installed yet, attempting install...",
                            nh.data.link_language
                        );
                        if let Err(e) =
                            LanguageController::install_language(nh.data.link_language.clone())
                                .await
                        {
                            log::debug!(
                                "ensure_link_language: install_language({}) failed, will retry in 5s: {}",
                                nh.data.link_language, e
                            );
                            self.update_perspective_state_log_error(
                                PerspectiveState::LinkLanguageFailedToInstall,
                            )
                            .await;
                        }
                        // Loop back around; the next tick will re-enter
                        // language_by_address and either find the freshly
                        // installed runtime or schedule another retry.
                    }
                    Err(e) => {
                        log::error!("Error when calling language_by_address: {:?}", e);
                        self.update_perspective_state_log_error(
                            PerspectiveState::LinkLanguageFailedToInstall,
                        )
                        .await;
                    }
                }
            }
            interval.tick().await;
        }
    }

    async fn nh_sync_loop(&self) {
        // Exponential backoff on sync failure. A perspective whose link
        // language is unreachable (e.g. Holochain timing out because the
        // parent test long since finished) used to hammer sync() every 3
        // seconds forever, piling zome calls onto HC for every leaked
        // instance. Back off up to 5 minutes on repeated failure; reset
        // to the base interval as soon as a sync succeeds.
        const BASE_INTERVAL: Duration = Duration::from_secs(3);
        const MAX_INTERVAL: Duration = Duration::from_secs(300);
        let mut current_interval = BASE_INTERVAL;
        while !self.is_teardown.load(Ordering::Acquire) {
            // Clone the link_language without holding the lock during sync
            let link_language_clone = {
                let link_language_guard = self.link_language.read().await;
                link_language_guard.clone()
            };

            if let Some(mut link_language) = link_language_clone {
                // If the loaded link language does not export a
                // perspective-sync capability, the sync loop has nothing
                // to do — mark the perspective synced and stop polling
                // so we don't burn a task slot on a no-op tick forever.
                if !link_language.has(crate::languages::capability::Capability::PerspectiveSync) {
                    let _ = self
                        .update_perspective_state(PerspectiveState::Synced)
                        .await;
                    return;
                }
                match link_language.sync().await {
                    Ok(_) => {
                        current_interval = BASE_INTERVAL;
                        // Transition to Synced state on successful sync
                        let _ = self
                            .update_perspective_state(PerspectiveState::Synced)
                            .await;
                    }
                    Err(e) => {
                        log::error!("Error calling sync on link language: {:?}", e);
                        current_interval =
                            std::cmp::min(current_interval.saturating_mul(2), MAX_INTERVAL);
                        let _ = self
                            .update_perspective_state(
                                PerspectiveState::LinkLanguageInstalledButNotSynced,
                            )
                            .await;
                    }
                }
            }
            // Sleep in short slices so teardown is observed within ~1s
            // even when the backoff interval is long.
            let mut remaining = current_interval;
            let slice = Duration::from_secs(1);
            while remaining > Duration::from_millis(0) {
                if self.is_teardown.load(Ordering::Acquire) {
                    return;
                }
                let step = std::cmp::min(slice, remaining);
                sleep(step).await;
                remaining = remaining.saturating_sub(step);
            }
        }
    }

    async fn pending_diffs_loop(&self) {
        let uuid = self.uuid.clone();
        let mut interval = time::interval(Duration::from_millis(100));
        let mut last_diff_time = None;

        while !self.is_teardown.load(Ordering::Acquire) {
            interval.tick().await;

            if self.has_link_language().await {
                let (_, ids) = Ad4mDb::with_global_instance(|db| {
                    db.get_pending_diffs(&uuid, Some(MAX_PENDING_DIFFS_COUNT))
                })
                .unwrap_or((PerspectiveDiff::empty(), Vec::new()));

                if ids.is_empty() {
                    continue;
                }

                if last_diff_time.is_none() {
                    // First diff in a burst - start timer
                    last_diff_time = Some(tokio::time::Instant::now());
                }

                // Commit if either:
                // 1. It's been MAX_PENDING_SECONDS since first diff in burst (don't collect longer than MAX_PENDING_SECONDS)
                if last_diff_time.unwrap().elapsed() >= Duration::from_secs(MAX_PENDING_SECONDS) {
                    if self.commit_pending_diffs().await.is_ok() {
                        last_diff_time = None;
                        log::info!("Committed diffs after reaching 10s maximum wait time");
                    }
                // 2. It's been > 1s since last new diff (burst is over)
                } else if !self.has_new_diffs_in_last_second().await {
                    if self.commit_pending_diffs().await.is_ok() {
                        last_diff_time = None;
                        log::info!("Committed diffs after 1s of inactivity");
                    }
                // 3. We have collected more than 100 diffs
                } else if ids.len() >= MAX_PENDING_DIFFS_COUNT
                    && self.commit_pending_diffs().await.is_ok()
                {
                    last_diff_time = None;
                    log::info!("Committed diffs after collecting 100");
                }
            }
        }
    }

    async fn has_new_diffs_in_last_second(&self) -> bool {
        let timer = self.commit_debounce_timer.lock().await;
        timer
            .map(|instant| instant.elapsed() < tokio::time::Duration::from_secs(1))
            .unwrap_or(false)
    }

    async fn has_link_language(&self) -> bool {
        let link_language_guard = self.link_language.read().await;
        link_language_guard.is_some()
    }

    async fn commit_pending_diffs(&self) -> Result<(), AnyError> {
        let uuid = self.uuid.clone();

        let (pending_diffs, pending_ids) = Ad4mDb::with_global_instance(|db| {
            db.get_pending_diffs_by_size(&uuid, MAX_COMMIT_BYTES, Some(MAX_PENDING_DIFFS_COUNT))
        })?;

        if !pending_ids.is_empty() {
            let link_language_clone = {
                let link_language_guard = self.link_language.read().await;
                link_language_guard.clone()
            };

            if let Some(mut link_language) = link_language_clone {
                log::info!("Committing {} pending diffs...", pending_ids.len());
                let commit_result = link_language.commit(pending_diffs).await;
                match commit_result {
                    // Spec §5.2 splits perspective-commit (write) from
                    // perspective-sync (revision read), so a spec-compliant
                    // flat language that implements only perspective-commit
                    // returns Ok(None). Previously we treated Ok(None) as
                    // an error and left the diffs pending forever, so any
                    // language without a perspective-sync capability could
                    // never drain its retry queue. The success signal for
                    // the retry path is "commit didn't throw" — clear the
                    // diffs regardless of whether a revision came back.
                    Ok(_) => {
                        Ad4mDb::with_global_instance(|db| {
                            db.clear_pending_diffs(&uuid, pending_ids)
                        })?;
                        // Reset immediate commits counter after successful commit
                        self.set_immediate_commits(IMMEDIATE_COMMITS_COUNT).await;
                        log::info!("Successfully committed pending diffs");
                        Ok(())
                    }
                    Err(e) => Err(e),
                }
            } else {
                Ok(()) // Keep diffs if no link language
            }
        } else {
            Ok(())
        }
    }

    async fn notification_check_loop(&self) {
        //log::debug!("Starting notification check loop for perspective {}", self.persisted.lock().await.uuid);
        let uuid = self.uuid.clone();
        let mut interval = time::interval(Duration::from_secs(5));
        let mut before = self.notification_trigger_snapshot().await;
        while !self.is_teardown.load(Ordering::Acquire) {
            interval.tick().await;
            let changed = self
                .trigger_notification_check
                .swap(false, Ordering::AcqRel);

            if changed {
                //log::debug!("Notification check loop triggered for perspective {}", uuid);
                //let start = std::time::Instant::now();
                //let snapshot_start = std::time::Instant::now();

                let after = self.notification_trigger_snapshot().await;
                //let snapshot_duration = snapshot_start.elapsed();
                //log::debug!("Notification trigger snapshot took {:?} - for perspective {}", snapshot_duration, uuid);

                //let diff_start = std::time::Instant::now();
                let new_matches = Self::subtract_before_notification_matches(&before, &after);
                //let diff_duration = diff_start.elapsed();
                //log::debug!("Computing notification diff took {:?} - for perspective {}", diff_duration, uuid);

                tokio::spawn(Self::publish_notification_matches(
                    uuid.clone(),
                    new_matches,
                ));
                before = after;
                //let total_duration = start.elapsed();
                //log::debug!("Total notification check iteration took {:?} - for perspective {}", total_duration, uuid);
            }
        }
    }

    pub async fn ensure_public_links_are_shared(&self) -> bool {
        let uuid = self.uuid.clone();

        // Clone link_language without holding the lock
        let link_language_clone = {
            let link_language_guard = self.link_language.read().await;
            link_language_guard.clone()
        };

        if let Some(mut link_language) = link_language_clone {
            // Query SPARQL store for all links
            let decorated_links = match self.sparql_store.get_all_links() {
                Ok(links) => links,
                Err(e) => {
                    log::error!(
                        "Failed to get links from SPARQL store in ensure_public_links_are_shared for perspective {}: {}",
                        uuid, e
                    );
                    return false;
                }
            };

            let mut local_links: Vec<(LinkExpression, LinkStatus)> = decorated_links
                .into_iter()
                .map(|decorated| {
                    let status = decorated.status.clone().unwrap_or(LinkStatus::Local);
                    (LinkExpression::from(decorated), status)
                })
                .collect();

            local_links.retain(|(_, status)| status == &LinkStatus::Shared);

            let remote_links = match link_language.current_revision().await {
                Ok(Some(_)) => {
                    link_language
                        .render()
                        .await
                        .unwrap_or(None)
                        .unwrap_or_default()
                        .links
                }
                _ => vec![],
            };

            let mut links_to_commit = Vec::new();
            for (local_link, _) in &local_links {
                if !remote_links.iter().any(|e| {
                    e.author == local_link.author
                        && e.timestamp == local_link.timestamp
                        && e.data.source == local_link.data.source
                        && e.data.target == local_link.data.target
                        && e.data.predicate == local_link.data.predicate
                }) {
                    links_to_commit.push(local_link.clone());
                }
            }

            if !links_to_commit.is_empty() {
                let links_count = links_to_commit.len();
                let result = link_language
                    .commit(PerspectiveDiff {
                        additions: links_to_commit,
                        removals: vec![],
                    })
                    .await;

                if let Err(e) = result {
                    log::error!("Error calling link language's commit in ensure_public_links_are_shared: {:?}", e);
                    return false;
                }
                log::debug!(
                    "Successfully committed {} links to link language in fallback sync",
                    links_count
                );
            }

            //Ad4mDb::with_global_instance(|db| db.add_many_links(&self.persisted.lock().await.uuid, &remote_links)).unwrap(); // Assuming add_many_links takes a reference to a Vec<LinkExpression> and returns Result<(), AnyError>
            return true;
        }
        false
    }

    pub async fn update_perspective_state(&self, state: PerspectiveState) -> Result<(), AnyError> {
        if self.persisted.lock().await.state != state {
            let mut handle = self.persisted.lock().await.clone();
            handle.state = state.clone();

            update_perspective(&handle).await.map_err(|e| anyhow!(e))?;

            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC,
                    &serde_json::to_string(&PerspectiveStateFilter {
                        perspective: handle,
                        state: serde_json::to_string(&state)
                            .expect("must be able to serialze PerspectiveState"),
                    })
                    .unwrap(),
                )
                .await;
        }
        Ok(())
    }

    async fn update_perspective_state_log_error(&self, state: PerspectiveState) {
        if let Err(e) = self.update_perspective_state(state).await {
            log::error!("Error updating perspective state: {:?}", e);
        }
    }

    pub async fn update_from_handle(&self, handle: PerspectiveHandle) {
        *self.persisted.lock().await = handle;
    }

    pub async fn commit(&self, diff: &PerspectiveDiff) -> Result<(), AnyError> {
        let handle = self.persisted.lock().await.clone();
        if handle.neighbourhood.is_none() {
            return Ok(());
        }

        // Seeing if we already have pending diffs, to not overtake older commits but instead add this one to the queue
        let (_, pending_ids) =
            Ad4mDb::with_global_instance(|db| db.get_pending_diffs(&handle.uuid, Some(1)))
                .unwrap_or((PerspectiveDiff::empty(), Vec::new()));

        let commit_result = if pending_ids.is_empty() {
            // No pending diffs, let's try
            // Clone link_language without holding the lock
            let link_language_clone = {
                let link_language_guard = self.link_language.read().await;
                link_language_guard.clone()
            };

            if let Some(mut link_language) = link_language_clone {
                // Spec §5.2 separates perspective-commit from perspective-sync,
                // so a commit-only flat language has no meaningful notion of
                // "current revision" — the previous `current_revision().is_some()`
                // pre-check would always fail for such a language and force
                // every commit through the pending-diffs retry queue, which is
                // (a) slow and (b) architecturally wrong: the retry queue is
                // for transient failures, not for gating healthy commits.
                //
                // Drop the pre-check. The DoS counter still throttles bursts
                // independently, and link_language.commit() itself is the
                // authoritative signal for whether the commit succeeded — if
                // the underlying language isn't ready, it throws and the
                // error path below queues the diff. Legacy languages that
                // implement perspective-sync still behave correctly because
                // they throw from commit() when not synced.
                let mut immediate_commits_remaining = self.immediate_commits_remaining.lock().await;
                if *immediate_commits_remaining > 0 {
                    *immediate_commits_remaining -= 1;
                    link_language.commit(diff.clone()).await
                } else {
                    Err(anyhow!("Debouncing commit burst"))
                }
            } else {
                Err(anyhow!("LinkLanguage not available"))
            }
        } else {
            Err(anyhow!("Other pending diffs already in queue"))
        };

        let ok = match commit_result {
            Ok(Some(rev)) => {
                if rev.trim().is_empty() {
                    log::warn!("LinkLanguage.commit returned an empty revision string; treating as success");
                    true
                } else {
                    log::info!("Committed to revision: {}", rev);
                    true
                }
            }
            Ok(None) => true,
            Err(e) => {
                log::warn!(
                    "Error trying to commit diff: {:?}\nStoring in pending diffs for later",
                    e
                );
                false
            }
        };

        if !ok {
            // Store diff in DB
            Ad4mDb::with_global_instance(|db| db.add_pending_diff(&handle.uuid, diff))?;
            // Update or start timer
            let mut timer = self.commit_debounce_timer.lock().await;
            *timer = Some(tokio::time::Instant::now());
        }

        Ok(())
    }

    // Add method to configure immediate commits
    pub async fn set_immediate_commits(&self, count: usize) {
        *self.immediate_commits_remaining.lock().await = count;
    }

    fn spawn_commit_and_handle_error(&self, diff: &PerspectiveDiff) {
        let self_clone = self.clone();
        let diff_clone = diff.clone();

        tokio::spawn(async move {
            if let Err(e) = self_clone.commit(&diff_clone).await {
                log::error!("PerspectiveInstance::commit() returned error: {:?}\nStoring in pending diffs for later", e);
                let handle_clone = self_clone.persisted.lock().await.clone();
                Ad4mDb::with_global_instance(|db|
                    db.add_pending_diff(&handle_clone.uuid, &diff_clone)
                ).expect("Couldn't write pending diff. DB should be initialized and usable at this point");
            }
        });
    }

    pub async fn diff_from_link_language(&self, diff: PerspectiveDiff) -> Result<(), AnyError> {
        // Deduplicate by (author, timestamp, source, predicate, target)
        // Use structured keys to avoid delimiter collision issues
        let mut seen_add: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut unique_additions: Vec<LinkExpression> = Vec::new();
        for link in diff.additions.iter() {
            let key_tuple = (
                &link.author,
                &link.timestamp,
                &link.data.source,
                link.data.predicate.as_deref().unwrap_or(""),
                &link.data.target,
            );
            let key = serde_json::to_string(&key_tuple).unwrap_or_else(|_| {
                // Fallback to a simple hash if serialization fails
                format!("{:?}", key_tuple)
            });
            if seen_add.insert(key) {
                unique_additions.push(link.clone());
            }
        }

        let mut seen_rem: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut unique_removals: Vec<LinkExpression> = Vec::new();
        for link in diff.removals.iter() {
            let key_tuple = (
                &link.author,
                &link.timestamp,
                &link.data.source,
                link.data.predicate.as_deref().unwrap_or(""),
                &link.data.target,
            );
            let key = serde_json::to_string(&key_tuple).unwrap_or_else(|_| {
                // Fallback to a simple hash if serialization fails
                format!("{:?}", key_tuple)
            });
            if seen_rem.insert(key) {
                unique_removals.push(link.clone());
            }
        }

        let decorated_diff = DecoratedPerspectiveDiff {
            additions: unique_additions
                .iter()
                .map(|link| DecoratedLinkExpression::from((link.clone(), LinkStatus::Shared)))
                .collect(),
            removals: unique_removals
                .iter()
                .map(|link| DecoratedLinkExpression::from((link.clone(), LinkStatus::Shared)))
                .collect(),
        };

        // Write to SPARQL store (primary storage for links)
        self.persist_link_diff(&decorated_diff).await?;

        // Update both Prolog engines: subscription (immediate) + query (lazy)
        self.update_prolog_engines(decorated_diff.clone()).await;
        self.pubsub_publish_diff(decorated_diff).await;

        Ok(())
    }

    pub async fn telepresence_signal_from_link_language(
        &self,
        mut signal: PerspectiveExpression,
        recipient_did: Option<String>,
    ) {
        signal.verify_signatures();
        let handle = self.persisted.lock().await.clone();

        log::debug!("telepresence_signal_from_link_language: perspective={}, recipient_did={:?}, signal_author={}",
            handle.uuid, recipient_did, signal.author);

        super::publish_telepresence_signal(handle, signal, recipient_did).await;
    }

    pub async fn add_link(
        &mut self,
        link: Link,
        status: LinkStatus,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        if let Some(ref email) = context.user_email {
            crate::billing::check_compute_credits(email)?;
        }
        link.validate()?;
        let link_expr: LinkExpression = create_signed_expression(link.normalize(), context)?.into();
        let result = self
            .add_link_expression(link_expr, status, batch_id)
            .await?;

        if let Some(ref email) = context.user_email {
            let uuid = self.uuid.clone();
            if let Err(e) = crate::billing::bill_compute(
                email,
                crate::billing::get_link_write_rate(),
                "link_write",
                Some(&format!("1 link in perspective {}", uuid)),
            ) {
                log::warn!("Call exceeded compute credits (add_link): {:?}", e);
            }
        }

        Ok(result)
    }

    pub async fn remove_link(
        &mut self,
        link_expression: LinkExpression,
        batch_id: Option<String>,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        if let Some(batch_id) = batch_id {
            let mut batches = self.batch_store.write().await;
            let diff = batches
                .get_mut(&batch_id)
                .ok_or(anyhow!("Batch not found"))?;

            let _handle = self.persisted.lock().await.clone();

            // Query SPARQL store
            let decorated_link = self
                .sparql_store
                .get_link(
                    &link_expression.data.source,
                    link_expression.data.predicate.as_deref(),
                    &link_expression.data.target,
                    &link_expression.author,
                    &link_expression.timestamp,
                )?
                .ok_or(anyhow!("Link not found"))?;

            let link_from_db = LinkExpression::from(decorated_link.clone());
            let status = decorated_link.status.clone().unwrap_or(LinkStatus::Local);

            diff.removals.push(link_from_db.clone());
            Ok(DecoratedLinkExpression::from((link_from_db, status)))
        } else {
            let _handle = self.persisted.lock().await.clone();

            // Query SPARQL store
            if let Some(decorated_link) = self.sparql_store.get_link(
                &link_expression.data.source,
                link_expression.data.predicate.as_deref(),
                &link_expression.data.target,
                &link_expression.author,
                &link_expression.timestamp,
            )? {
                let link_from_db = LinkExpression::from(decorated_link.clone());
                let status = decorated_link.status.clone().unwrap_or(LinkStatus::Local);

                let diff = PerspectiveDiff::from_removals(vec![link_expression.clone()]);
                let decorated_link_result =
                    DecoratedLinkExpression::from((link_from_db, status.clone()));
                let decorated_diff =
                    DecoratedPerspectiveDiff::from_removals(vec![decorated_link_result.clone()]);

                // Remove from SPARQL store (primary storage)
                self.persist_link_diff(&decorated_diff).await?;

                // Update both Prolog engines: subscription (immediate) + query (lazy)
                self.update_prolog_engines(decorated_diff.clone()).await;

                self.pubsub_publish_diff(decorated_diff.clone()).await;

                if status == LinkStatus::Shared {
                    self.spawn_commit_and_handle_error(&diff);
                }

                Ok(decorated_link_result)
            } else {
                Err(anyhow!("Link not found"))
            }
        }
    }

    async fn pubsub_publish_diff(&self, decorated_diff: DecoratedPerspectiveDiff) {
        // Get handle without holding lock during pubsub operations
        let handle = {
            let persisted_guard = self.persisted.lock().await;
            persisted_guard.clone()
        };

        // Publish link added events - one per owner for proper multi-user isolation
        let pubsub = get_global_pubsub().await;
        let owners_list = handle.owners.as_ref().filter(|o| !o.is_empty());

        if let Some(owners) = owners_list {
            for link in &decorated_diff.additions {
                for owner in owners {
                    pubsub
                        .publish(
                            &PERSPECTIVE_LINK_ADDED_TOPIC,
                            &serde_json::to_string(&PerspectiveLinkWithOwner {
                                perspective_uuid: handle.uuid.clone(),
                                link: link.clone(),
                                owner: owner.clone(),
                            })
                            .unwrap(),
                        )
                        .await;
                }
            }

            // Publish link removed events - one per owner for proper multi-user isolation
            for link in &decorated_diff.removals {
                for owner in owners {
                    pubsub
                        .publish(
                            &PERSPECTIVE_LINK_REMOVED_TOPIC,
                            &serde_json::to_string(&PerspectiveLinkWithOwner {
                                perspective_uuid: handle.uuid.clone(),
                                link: link.clone(),
                                owner: owner.clone(),
                            })
                            .unwrap(),
                        )
                        .await;
                }
            }
        } else {
            // For perspectives without explicit owners (main agent), publish with main agent DID
            let main_agent_did = crate::agent::did();

            for link in &decorated_diff.additions {
                pubsub
                    .publish(
                        &PERSPECTIVE_LINK_ADDED_TOPIC,
                        &serde_json::to_string(&PerspectiveLinkWithOwner {
                            perspective_uuid: handle.uuid.clone(),
                            link: link.clone(),
                            owner: main_agent_did.clone(),
                        })
                        .unwrap(),
                    )
                    .await;
            }

            for link in &decorated_diff.removals {
                pubsub
                    .publish(
                        &PERSPECTIVE_LINK_REMOVED_TOPIC,
                        &serde_json::to_string(&PerspectiveLinkWithOwner {
                            perspective_uuid: handle.uuid.clone(),
                            link: link.clone(),
                            owner: main_agent_did.clone(),
                        })
                        .unwrap(),
                    )
                    .await;
            }
        }
    }

    pub async fn add_link_expression(
        &mut self,
        link_expression: LinkExpression,
        status: LinkStatus,
        batch_id: Option<String>,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        link_expression.data.validate()?;
        if let Some(batch_id) = batch_id {
            let mut batches = self.batch_store.write().await;
            let diff = batches
                .get_mut(&batch_id)
                .ok_or(anyhow!("Batch not found"))?;

            let mut link_expr = link_expression.clone();
            link_expr.status = Some(status.clone());
            diff.additions.push(link_expr.clone());

            return Ok(DecoratedLinkExpression::from((
                link_expr.clone(),
                status.clone(),
            )));
        }

        // Store link in SPARQL store
        let diff = PerspectiveDiff::from_additions(vec![link_expression.clone()]);
        let decorated_link_expression =
            DecoratedLinkExpression::from((link_expression.clone(), status.clone()));
        let decorated_perspective_diff =
            DecoratedPerspectiveDiff::from_additions(vec![decorated_link_expression.clone()]);

        // Write to SPARQL store (primary storage for links)
        self.persist_link_diff(&decorated_perspective_diff).await?;

        // Update both Prolog engines: subscription (immediate) + query (lazy)
        self.update_prolog_engines(decorated_perspective_diff.clone())
            .await;

        if status == LinkStatus::Shared {
            self.spawn_commit_and_handle_error(&diff);
        }

        self.pubsub_publish_diff(decorated_perspective_diff).await;
        Ok(decorated_link_expression)
    }

    pub async fn add_links(
        &mut self,
        links: Vec<Link>,
        status: LinkStatus,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        if let Some(ref email) = context.user_email {
            crate::billing::check_compute_credits(email)?;
        }
        for link in &links {
            link.validate()?;
        }
        let link_expressions: Result<Vec<_>, _> = links
            .into_iter()
            .map(|l| create_signed_expression(l.normalize(), context).map(LinkExpression::from))
            .collect();
        let link_expressions = link_expressions?;

        if let Some(batch_id) = batch_id {
            let mut batches = self.batch_store.write().await;
            let diff = batches
                .get_mut(&batch_id)
                .ok_or(anyhow!("Batch not found"))?;

            let mut decorated_expressions = Vec::new();
            for mut link_expr in link_expressions {
                link_expr.status = Some(status.clone());
                diff.additions.push(link_expr.clone());
                decorated_expressions
                    .push(DecoratedLinkExpression::from((link_expr, status.clone())));
            }

            Ok(decorated_expressions)
        } else {
            let decorated_link_expressions = link_expressions
                .clone()
                .into_iter()
                .map(|l| DecoratedLinkExpression::from((l, status.clone())))
                .collect::<Vec<DecoratedLinkExpression>>();

            let perspective_diff = PerspectiveDiff::from_additions(link_expressions.clone());
            let decorated_perspective_diff =
                DecoratedPerspectiveDiff::from_additions(decorated_link_expressions.clone());

            // Write to SPARQL store (primary storage for links)
            self.persist_link_diff(&decorated_perspective_diff).await?;

            self.spawn_prolog_facts_update(decorated_perspective_diff.clone(), None);
            self.pubsub_publish_diff(decorated_perspective_diff).await;

            if status == LinkStatus::Shared {
                self.spawn_commit_and_handle_error(&perspective_diff);
            }

            // Bill for link writes
            if let Some(ref email) = context.user_email {
                let uuid = self.uuid.clone();
                let link_count = decorated_link_expressions.len();
                if let Err(e) = crate::billing::bill_compute(
                    email,
                    link_count as f64 * crate::billing::get_link_write_rate(),
                    "link_write",
                    Some(&format!("{} links in perspective {}", link_count, uuid)),
                ) {
                    log::warn!(
                        "Call exceeded compute credits (add_links, count={}): {:?}",
                        link_count,
                        e
                    );
                }
            }

            Ok(decorated_link_expressions)
        }
    }

    pub async fn link_mutations(
        &mut self,
        mutations: LinkMutations,
        status: LinkStatus,
        context: &AgentContext,
    ) -> Result<DecoratedPerspectiveDiff, AnyError> {
        if !mutations.additions.is_empty() {
            if let Some(ref email) = context.user_email {
                crate::billing::check_compute_credits(email)?;
            }
        }
        let addition_links: Vec<Link> = mutations.additions.into_iter().map(Link::from).collect();
        for link in &addition_links {
            link.validate()?;
        }
        let additions = addition_links
            .into_iter()
            .map(|l| create_signed_expression(l.normalize(), context))
            .map(|r| r.map(LinkExpression::from))
            .collect::<Result<Vec<LinkExpression>, AnyError>>()?;
        let removals = mutations
            .removals
            .into_iter()
            .map(LinkExpression::try_from)
            .collect::<Result<Vec<LinkExpression>, AnyError>>()?;

        let diff = PerspectiveDiff::from(additions.clone(), removals.clone());
        let decorated_diff = DecoratedPerspectiveDiff {
            additions: additions
                .into_iter()
                .map(|l| DecoratedLinkExpression::from((l, status.clone())))
                .collect::<Vec<DecoratedLinkExpression>>(),
            removals: removals
                .clone()
                .into_iter()
                .map(|l| DecoratedLinkExpression::from((l, status.clone())))
                .collect::<Vec<DecoratedLinkExpression>>(),
        };

        // Write to SPARQL store (primary storage for links)
        self.persist_link_diff(&decorated_diff).await?;

        self.spawn_prolog_facts_update(decorated_diff.clone(), None);
        self.pubsub_publish_diff(decorated_diff.clone()).await;

        if status == LinkStatus::Shared {
            self.spawn_commit_and_handle_error(&diff);
            // Reset fallback sync interval when new shared links are added
            self.reset_fallback_sync_interval().await;
        }

        // Bill for additions only (removals are free)
        let additions_count = decorated_diff.additions.len();
        if additions_count > 0 {
            if let Some(ref email) = context.user_email {
                let uuid = self.uuid.clone();
                if let Err(e) = crate::billing::bill_compute(
                    email,
                    additions_count as f64 * crate::billing::get_link_write_rate(),
                    "link_write",
                    Some(&format!(
                        "{} additions in perspective {}",
                        additions_count, uuid
                    )),
                ) {
                    log::warn!(
                        "Call exceeded compute credits (link_mutations, additions={}): {:?}",
                        additions_count,
                        e
                    );
                }
            }
        }

        Ok(decorated_diff)
    }

    pub async fn update_link(
        &mut self,
        old_link: LinkExpression,
        new_link: Link,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<DecoratedLinkExpression, AnyError> {
        if let Some(ref email) = context.user_email {
            crate::billing::check_compute_credits(email)?;
        }
        let handle = self.persisted.lock().await.clone();

        // Query SPARQL store
        let decorated_link_option = self.sparql_store.get_link(
            &old_link.data.source,
            old_link.data.predicate.as_deref(),
            &old_link.data.target,
            &old_link.author,
            &old_link.timestamp,
        )?;

        let (_link, link_status) = match decorated_link_option {
            Some(decorated) => {
                let status = decorated.status.clone().unwrap_or(LinkStatus::Local);
                (LinkExpression::from(decorated), status)
            }
            None => {
                return Err(AnyError::msg(format!(
                    "NH [{}] ({}) Link not found in perspective \"{}\": {:?}",
                    handle
                        .shared_url
                        .clone()
                        .unwrap_or("not-shared".to_string()),
                    handle.name.clone().unwrap_or("<no name>".to_string()),
                    handle.uuid,
                    old_link
                )))
            }
        };

        let new_link_expression =
            LinkExpression::from(create_signed_expression(new_link.normalize(), context)?);

        if let Some(batch_id) = batch_id {
            let mut batches = self.batch_store.write().await;
            let diff = batches
                .get_mut(&batch_id)
                .ok_or(anyhow!("Batch not found"))?;

            diff.removals.push(old_link.clone());
            let mut new_link_expr = new_link_expression.clone();
            new_link_expr.status = Some(link_status.clone());
            diff.additions.push(new_link_expr.clone());

            Ok(DecoratedLinkExpression::from((new_link_expr, link_status)))
        } else {
            let diff =
                PerspectiveDiff::from(vec![new_link_expression.clone()], vec![old_link.clone()]);
            let decorated_new_link_expression =
                DecoratedLinkExpression::from((new_link_expression.clone(), link_status.clone()));
            let decorated_old_link =
                DecoratedLinkExpression::from((old_link.clone(), link_status.clone()));
            let decorated_diff = DecoratedPerspectiveDiff::from(
                vec![decorated_new_link_expression.clone()],
                vec![decorated_old_link.clone()],
            );

            // Write to SPARQL store (primary storage for links)
            self.persist_link_diff(&decorated_diff).await?;

            // Update both Prolog engines: subscription (immediate) + query (lazy)
            self.update_prolog_engines(decorated_diff.clone()).await;

            // Publish link updated events - one per owner for proper multi-user isolation
            let pubsub = get_global_pubsub().await;
            let owners_list = handle.owners.as_ref().filter(|o| !o.is_empty());

            if let Some(owners) = owners_list {
                for owner in owners {
                    pubsub
                        .publish(
                            &PERSPECTIVE_LINK_UPDATED_TOPIC,
                            &serde_json::to_string(&PerspectiveLinkUpdatedWithOwner {
                                perspective_uuid: handle.uuid.clone(),
                                old_link: decorated_old_link.clone(),
                                new_link: decorated_new_link_expression.clone(),
                                owner: owner.clone(),
                            })
                            .unwrap(),
                        )
                        .await;
                }
            } else {
                // For perspectives without explicit owners (main agent), publish with main agent DID
                let main_agent_did = crate::agent::did();
                pubsub
                    .publish(
                        &PERSPECTIVE_LINK_UPDATED_TOPIC,
                        &serde_json::to_string(&PerspectiveLinkUpdatedWithOwner {
                            perspective_uuid: handle.uuid.clone(),
                            old_link: decorated_old_link.clone(),
                            new_link: decorated_new_link_expression.clone(),
                            owner: main_agent_did,
                        })
                        .unwrap(),
                    )
                    .await;
            }

            if link_status == LinkStatus::Shared {
                self.spawn_commit_and_handle_error(&diff);
            }

            // Bill for the replacement link (1 addition; removal is free)
            if let Some(ref email) = context.user_email {
                if let Err(e) = crate::billing::bill_compute(
                    email,
                    crate::billing::get_link_write_rate(),
                    "link_write",
                    Some(&format!("1 link update in perspective {}", handle.uuid)),
                ) {
                    log::warn!("Call exceeded compute credits (update_link): {:?}", e);
                }
            }

            Ok(decorated_new_link_expression)
        }
    }

    pub async fn remove_links(
        &mut self,
        link_expressions: Vec<LinkExpression>,
        batch_id: Option<String>,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let _handle = self.persisted.lock().await.clone();

        // Filter to only existing links and collect their statuses
        let mut existing_links = Vec::new();
        for link in link_expressions {
            // Query SPARQL store
            if let Some(decorated_link) = self.sparql_store.get_link(
                &link.data.source,
                link.data.predicate.as_deref(),
                &link.data.target,
                &link.author,
                &link.timestamp,
            )? {
                let link_from_db = LinkExpression::from(decorated_link.clone());
                let status = decorated_link.status.clone().unwrap_or(LinkStatus::Local);
                existing_links.push((link_from_db, status));
            }
        }

        // Skip if no links found
        if existing_links.is_empty() {
            return Ok(Vec::new());
        }

        if let Some(batch_id) = batch_id {
            let mut batches = self.batch_store.write().await;
            let diff = batches
                .get_mut(&batch_id)
                .ok_or(anyhow!("Batch not found"))?;

            let decorated_links: Vec<_> = existing_links
                .iter()
                .map(|(link, status)| {
                    diff.removals.push(link.clone());
                    DecoratedLinkExpression::from((link.clone(), status.clone()))
                })
                .collect();

            Ok(decorated_links)
        } else {
            // Split into links and statuses
            let (links, statuses): (Vec<_>, Vec<_>) = existing_links.into_iter().unzip();

            // Create decorated versions
            let decorated_links: Vec<DecoratedLinkExpression> = links
                .into_iter()
                .zip(statuses.iter())
                .map(|(link, status)| DecoratedLinkExpression::from((link, status.clone())))
                .collect();

            let decorated_diff = DecoratedPerspectiveDiff::from_removals(decorated_links.clone());

            // Remove from SPARQL store (primary storage)
            self.persist_link_diff(&decorated_diff).await?;

            // Update both Prolog engines: subscription (immediate) + query (lazy)
            self.update_prolog_engines(decorated_diff.clone()).await;
            self.pubsub_publish_diff(decorated_diff).await;

            // Only commit shared links by filtering decorated_links
            let shared_links: Vec<LinkExpression> = decorated_links
                .iter()
                .filter(|link| link.status == Some(LinkStatus::Shared))
                .map(|link| link.clone().into())
                .collect();

            if !shared_links.is_empty() {
                let shared_diff = PerspectiveDiff {
                    additions: vec![],
                    removals: shared_links,
                };
                self.spawn_commit_and_handle_error(&shared_diff);
            }

            Ok(decorated_links)
        }
    }

    /// Helper function to efficiently fetch only SDNA-related links from the database
    /// This makes two targeted queries instead of fetching all links:
    /// 1. Links with source == "ad4m://self" (SDNA declarations)
    /// 2. Links with predicate == "ad4m://sdna" (SDNA code)
    async fn get_sdna_links_local(&self) -> Result<Vec<(LinkExpression, LinkStatus)>, AnyError> {
        // Query 1: Get all links from ad4m://self (SDNA declarations)
        let self_links = self.get_links_local(&LinkQuery {
            source: Some("ad4m://self".to_string()),
            ..Default::default()
        })?;

        // Query 2: Get all links with predicate ad4m://sdna (SDNA code)
        let sdna_code_links = self.get_links_local(&LinkQuery {
            predicate: Some("ad4m://sdna".to_string()),
            ..Default::default()
        })?;

        // Combine both result sets (using a HashSet to avoid duplicates)
        let mut seen = std::collections::HashSet::new();
        let mut all_sdna_links = Vec::new();

        for link in self_links.into_iter().chain(sdna_code_links) {
            let key = (
                link.0.data.source.clone(),
                link.0.data.predicate.clone(),
                link.0.data.target.clone(),
                link.0.author.clone(),
                link.0.timestamp.clone(),
                link.1.clone(), // Include LinkStatus
            );
            if seen.insert(key) {
                all_sdna_links.push(link);
            }
        }

        Ok(all_sdna_links)
    }

    /// Get all subject class names from SHACL links (Prolog-free implementation)
    ///
    /// This queries links with:
    /// - predicate = "rdf://type"
    /// - target = "ad4m://SubjectClass"
    ///
    /// The source of these links is the class URI (e.g., "recipe://Recipe")
    /// We extract the class name from the URI.
    pub async fn get_subject_classes_from_shacl(&self) -> Result<Vec<String>, AnyError> {
        let uuid = self.uuid.clone();
        log::debug!(
            "🔶 get_subject_classes_from_shacl: uuid={}, Querying for SHACL class links",
            uuid
        );
        // Query for SHACL class definition links
        let shacl_class_links = self.get_links_local(&LinkQuery {
            predicate: Some("rdf://type".to_string()),
            target: Some("ad4m://SubjectClass".to_string()),
            ..Default::default()
        })?;
        log::debug!(
            "🔶 get_subject_classes_from_shacl: Found {} links",
            shacl_class_links.len()
        );
        for (link, _status) in &shacl_class_links {
            log::debug!(
                "🔶 get_subject_classes_from_shacl: Link: {} -> {:?} -> {}",
                link.data.source,
                link.data.predicate,
                link.data.target
            );
        }

        // Extract class names from source URIs
        let mut class_names: Vec<String> = shacl_class_links
            .iter()
            .filter_map(|(link, _status)| {
                let source = &link.data.source;
                // Class URI format: "namespace://ClassName" (e.g., "recipe://Recipe")
                // We want to extract "ClassName"
                if let Some(idx) = source.rfind("://") {
                    let after_scheme = &source[idx + 3..];
                    // Handle paths like "namespace://path/ClassName"
                    if let Some(last_slash) = after_scheme.rfind('/') {
                        Some(after_scheme[last_slash + 1..].to_string())
                    } else {
                        Some(after_scheme.to_string())
                    }
                } else {
                    None
                }
            })
            .collect();

        // Remove duplicates
        class_names.sort();
        class_names.dedup();

        Ok(class_names)
    }

    fn get_links_local(
        &self,
        query: &LinkQuery,
    ) -> Result<Vec<(LinkExpression, LinkStatus)>, AnyError> {
        let from_date = query.from_date.as_ref().map(|d| {
            let dt: chrono::DateTime<chrono::Utc> = d.clone().into();
            dt.to_rfc3339()
        });
        let until_date = query.until_date.as_ref().map(|d| {
            let dt: chrono::DateTime<chrono::Utc> = d.clone().into();
            dt.to_rfc3339()
        });

        let decorated_links = self.sparql_store.query_links(
            query.source.as_deref(),
            query.predicate.as_deref(),
            query.target.as_deref(),
            from_date.as_deref(),
            until_date.as_deref(),
            None, // Don't limit here — get_links() applies limit after sorting
        )?;

        let result: Vec<(LinkExpression, LinkStatus)> = decorated_links
            .into_iter()
            .map(|decorated| {
                let status = decorated.status.clone().unwrap_or(LinkStatus::Shared);
                let link_expr = LinkExpression {
                    author: decorated.author,
                    timestamp: decorated.timestamp,
                    data: decorated.data,
                    proof: ExpressionProof {
                        key: decorated.proof.key,
                        signature: decorated.proof.signature,
                    },
                    status: Some(status.clone()),
                };
                (link_expr, status)
            })
            .collect();

        Ok(result)
    }

    pub async fn get_links(&self, q: &LinkQuery) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let mut reverse = false;
        let mut query = q.clone();

        if let Some(until_date) = query.until_date.as_ref() {
            if let Some(from_date) = query.from_date.as_ref() {
                let chrono_from_date: chrono::DateTime<chrono::Utc> = from_date.clone().into();
                let chrono_until_date: chrono::DateTime<chrono::Utc> = until_date.clone().into();
                if chrono_from_date > chrono_until_date {
                    reverse = true;
                    query.from_date.clone_from(&q.until_date);
                    query.until_date.clone_from(&q.from_date);
                }
            }
        }

        let mut links = self.get_links_local(&query)?;

        links.sort_by(|(a, _), (b, _)| {
            let a_time = DateTime::parse_from_rfc3339(&a.timestamp).unwrap_or_default();
            let b_time = DateTime::parse_from_rfc3339(&b.timestamp).unwrap_or_default();
            if reverse {
                b_time.cmp(&a_time)
            } else {
                a_time.cmp(&b_time)
            }
        });

        if let Some(limit) = query.limit {
            let limit = links.len().min(limit as usize);
            links = links[..limit].to_vec();
        }

        Ok(links
            .into_iter()
            .map(|(link, status)| DecoratedLinkExpression::from((link.clone(), status)))
            .collect())
    }

    /// Adds the given Social DNA code to the perspective's SDNA code
    /// If shacl_json is provided, also stores SHACL as queryable RDF links
    pub async fn add_sdna(
        &mut self,
        name: String,
        sdna_code: String,
        sdna_type: SdnaType,
        shacl_json: Option<String>,
        context: &AgentContext,
    ) -> Result<bool, AnyError> {
        let mutex = self.sdna_change_mutex.clone();
        let _guard = mutex.lock().await;
        self.add_sdna_inner(name, sdna_code, sdna_type, shacl_json, context)
            .await
    }

    /// Batch variant: registers multiple SDNA entries under a single mutex acquisition.
    pub async fn add_sdna_batch(
        &mut self,
        entries: Vec<(String, String, SdnaType, Option<String>)>,
        context: &AgentContext,
    ) -> Result<Vec<bool>, AnyError> {
        let mutex = self.sdna_change_mutex.clone();
        let _guard = mutex.lock().await;

        let mut results = Vec::with_capacity(entries.len());
        for (name, sdna_code, sdna_type, shacl_json) in entries {
            let result = self
                .add_sdna_inner(name, sdna_code, sdna_type, shacl_json, context)
                .await?;
            results.push(result);
        }
        Ok(results)
    }

    /// Inner implementation of add_sdna without mutex acquisition.
    async fn add_sdna_inner(
        &mut self,
        name: String,
        mut sdna_code: String,
        sdna_type: SdnaType,
        shacl_json: Option<String>,
        context: &AgentContext,
    ) -> Result<bool, AnyError> {
        let predicate = match sdna_type {
            SdnaType::SubjectClass => "ad4m://has_subject_class",
            SdnaType::Flow => "ad4m://has_flow",
            SdnaType::Custom => "ad4m://has_custom_sdna",
        };

        let literal_name = Literal::from_string(name.clone())
            .to_url()
            .expect("just initialized Literal couldn't be turned into URL");

        let mut sdna_links: Vec<Link> = Vec::new();

        // Check if SHACL definition already exists for this class BEFORE doing anything
        if matches!(sdna_type, SdnaType::SubjectClass) {
            // Check for any existing SubjectClass with this name, regardless of namespace
            // We query by target (ad4m://SubjectClass) and then filter by class name
            let all_class_links = self.get_links_local(&LinkQuery {
                predicate: Some("rdf://type".to_string()),
                target: Some("ad4m://SubjectClass".to_string()),
                ..Default::default()
            })?;

            // Check if any existing class matches this name
            let exists = all_class_links.iter().any(|(link, _)| {
                // Extract class name from source URI (e.g., "flux://Channel" -> "Channel")
                link.data
                    .source
                    .split("://")
                    .last()
                    .and_then(|s| s.split('/').last())
                    .map(|class_name| class_name == name)
                    .unwrap_or(false)
            });

            if exists {
                log::info!(
                    "Class '{}' SHACL definition already exists, skipping duplicate",
                    name
                );
                return Ok(true);
            }
        }

        if (Literal::from_url(sdna_code.clone())).is_err() {
            sdna_code = Literal::from_string(sdna_code)
                .to_url()
                .expect("just initialized Literal couldn't be turned into URL");
        }

        sdna_links.push(Link {
            source: "ad4m://self".to_string(),
            predicate: Some(predicate.to_string()),
            target: literal_name.clone(),
        });

        // Store the Prolog code for backward compatibility with getSdna()
        // SHACL links are the source of truth for schema operations,
        // but Prolog code is still stored for retrieval
        sdna_links.push(Link {
            source: literal_name.clone(),
            predicate: Some("ad4m://sdna".to_string()),
            target: sdna_code.clone(),
        });

        self.add_links(sdna_links, LinkStatus::Shared, None, context)
            .await?;

        // Handle SHACL links if SHACL JSON provided explicitly
        if let Some(shacl) = shacl_json {
            let shacl_links = parse_shacl_to_links(&shacl, &name)?;
            self.add_links(shacl_links, LinkStatus::Shared, None, context)
                .await?;
            // SHACL just changed for this class — drop any cached shape so the
            // next query re-parses against the fresh store state.
            self.invalidate_shape(&name);
        }

        Ok(true)
    }

    async fn ensure_prolog_engine_pool(&self) -> Result<(), AnyError> {
        // Prolog engine pools are only used in Pooled mode
        if PROLOG_MODE != PrologMode::Pooled {
            return Ok(());
        }

        // Get service reference and perspective data BEFORE acquiring write lock
        let service = get_prolog_service().await;
        let (uuid, owner_did, neighbourhood_author) = {
            let persisted = self.persisted.lock().await;
            let uuid = persisted.uuid.clone();
            let owner_did = persisted.get_primary_owner();
            let neighbourhood_author = persisted.neighbourhood.as_ref().map(|n| n.author.clone());
            (uuid, owner_did, neighbourhood_author)
        };

        // Check if initialization is needed WITHOUT holding any locks
        if !service.has_perspective_pool(uuid.clone()).await
            || !service
                .has_perspective_pool(notification_pool_name(&uuid))
                .await
        {
            // Get all links BEFORE acquiring write lock to avoid deadlock
            let all_links = self.get_links(&LinkQuery::default()).await?;

            // NOW take write lock after all async operations that might need locks are done
            let _guard = self.prolog_update_mutex.write().await;

            // Check if pool exists under the write lock
            if !service.has_perspective_pool(uuid.clone()).await {
                let pool_size = if all_links.len() > FILTERING_THRESHOLD {
                    Some(DEFAULT_POOL_SIZE_WITH_FILTERING)
                } else {
                    Some(DEFAULT_POOL_SIZE)
                };
                // Create and initialize new pool
                service
                    .ensure_perspective_pool(uuid.clone(), pool_size)
                    .await?;
                service
                    .update_perspective_links(
                        uuid.clone(),
                        "facts".to_string(),
                        all_links.clone(),
                        neighbourhood_author.clone(),
                        owner_did.clone(),
                    )
                    .await?;
            }

            let notification_pool = format!("notification_{}", uuid);

            if !service
                .has_perspective_pool(notification_pool.clone())
                .await
            {
                // Create and initialize new pool
                service
                    .ensure_perspective_pool(notification_pool.clone(), Some(1))
                    .await?;
                service
                    .update_perspective_links(
                        notification_pool,
                        "facts".to_string(),
                        all_links,
                        neighbourhood_author,
                        owner_did,
                    )
                    .await?;
            }
        }

        Ok(())
    }

    /// Get the appropriate prolog pool ID for the given context
    fn get_pool_id_for_context(&self, perspective_uuid: &str, context: &AgentContext) -> String {
        match &context.user_email {
            Some(user_email) => {
                // User-specific pool: "uuid_user_email"
                format!("{}_{}", perspective_uuid, user_email)
            }
            None => {
                // Main agent pool: just the uuid
                perspective_uuid.to_string()
            }
        }
    }

    /// Common helper for executing prolog queries with configurable pool, lock, and executor
    async fn prolog_query_helper<F, Fut>(
        &self,
        query: String,
        use_lock: bool,
        pool_provider: impl FnOnce(&String) -> String,
        executor: F,
    ) -> Result<QueryResolution, AnyError>
    where
        F: FnOnce(Arc<PrologService>, String, String) -> Fut,
        Fut: Future<Output = Result<Result<QueryResolution, String>, AnyError>> + Send,
    {
        let total_start = std::time::Instant::now();
        log::trace!(
            "🧠🧠 Prolog query starting: {} (chars: {})",
            query.chars().take(100).collect::<String>(),
            query.len()
        );

        let ensure_start = std::time::Instant::now();
        self.ensure_prolog_engine_pool().await?;
        log::trace!("🧠🔧 Engine pool ensured in {:?}", ensure_start.elapsed());

        let uuid = self.uuid.clone();

        let service_start = std::time::Instant::now();
        let service = get_prolog_service().await;
        log::trace!("🧠📞 Service retrieved in {:?}", service_start.elapsed());

        let pool_name = pool_provider(&uuid);

        let query = if !query.ends_with('.') {
            query + "."
        } else {
            query
        };

        // Only acquire the prolog read lock in Pooled mode where write locks
        // are taken during fact updates. In other modes, no writes happen so
        // the lock would only add contention without providing synchronization.
        let lock_start = std::time::Instant::now();
        let _read_lock = if use_lock && PROLOG_MODE == PrologMode::Pooled {
            log::trace!("🧠🔒 Waiting for prolog_update_mutex read lock...");
            let guard = self.prolog_update_mutex.read().await;
            log::trace!(
                "🧠✅ Acquired prolog_update_mutex read lock in {:?}",
                lock_start.elapsed()
            );
            Some(guard)
        } else {
            None
        };

        // Execute query with periodic logging and timeout handling
        log::trace!("🧠⏳ Starting query execution...");
        let execute_start = std::time::Instant::now();

        // Spawn a task that logs every 10 seconds while the query is running
        let query_for_logging = query.clone();
        let execute_start_for_logging = execute_start.clone();
        let logging_handle = tokio::spawn(async move {
            let mut interval = tokio::time::interval(std::time::Duration::from_secs(10));
            interval.tick().await;
            loop {
                interval.tick().await;
                let elapsed = execute_start_for_logging.elapsed();
                log::warn!(
                    "🧠⏰ Prolog query still running after {:?}. Query:\n{}",
                    elapsed,
                    query_for_logging
                );
            }
        });

        // Execute query with a 60-second timeout
        let timeout_duration = std::time::Duration::from_secs(60);
        let service = Arc::new(service);
        let query_clone = query.clone();
        let result_future = executor(service, pool_name, query_clone);
        let result_with_timeout = tokio::time::timeout(timeout_duration, result_future).await;

        // Cancel the logging task since query completed or timed out
        logging_handle.abort();

        let result: Result<Result<QueryResolution, String>, AnyError> = match result_with_timeout {
            Ok(r) => {
                log::trace!(
                    "🧠✅ Query execution succeeded in {:?}",
                    execute_start.elapsed()
                );
                r
            }
            Err(_) => {
                log::error!(
                    "🧠⏱️💥 Prolog query timed out after {:?} (60s limit)\nQuery was: {}",
                    execute_start.elapsed(),
                    query
                );
                return Err(anyhow!(
                    "Prolog query execution timed out after 60 seconds. Query: {}",
                    query
                ));
            }
        };

        let match_result = match result {
            Err(e) => {
                log::error!(
                    "🧠💥 Prolog query failed after {:?}: {:?}\nQuery was: {}",
                    execute_start.elapsed(),
                    e,
                    query
                );
                Err(anyhow!(e))
            }
            Ok(resolution) => resolution.map_err(|e| {
                log::error!(
                    "🧠💥 Prolog query resolution error after {:?}: {}\nQuery was: {}",
                    execute_start.elapsed(),
                    e,
                    query
                );
                anyhow!(e)
            }),
        };

        // Log result count and total time
        if let Ok(ref resolution) = match_result {
            let result_count = match resolution {
                QueryResolution::Matches(matches) => matches.len(),
                QueryResolution::True => 1,
                QueryResolution::False => 0,
            };
            log::trace!(
                "🧠🧠🧠 Prolog query:\n{}\n==>> Result count: {}",
                query,
                result_count
            );
        }
        log::trace!("🧠⏱️ TOTAL query time: {:?}", total_start.elapsed());

        match_result
    }

    /// Executes a Prolog query against the perspective's main pool
    /// locks the prolog_update_mutex
    /// uses run_query_smart
    // pub async fn prolog_query(&self, query: String) -> Result<QueryResolution, AnyError> {
    //     self.prolog_query_helper(
    //         query,
    //         true,
    //         |uuid| uuid.clone(),
    //         |service, pool, q| async move { service.run_query_smart(pool, q).await },
    //     )
    //     .await
    // }

    /// Helper to mark the Prolog engine as dirty (needs update before next query)
    /// Only applies to Simple mode
    /// Note: SdnaOnly mode doesn't use dirty flag - it compares SDNA links directly to avoid rebuilding on non-SDNA changes
    async fn mark_prolog_engine_dirty(&self) {
        if PROLOG_MODE == PrologMode::Simple {
            let perspective_uuid = self.uuid.clone();
            get_prolog_service()
                .await
                .mark_dirty(&perspective_uuid)
                .await;
        }
    }

    /// Combined helper: spawns Prolog facts update AND marks query engine as dirty
    /// This is the common pattern throughout the codebase
    async fn update_prolog_engines(&self, diff: DecoratedPerspectiveDiff) {
        // Update subscription engine (immediate via spawned task)
        self.spawn_prolog_facts_update(diff, None);

        // Mark query engine dirty for lazy update on next query
        self.mark_prolog_engine_dirty().await;
    }

    /// Helper for Simple/SdnaOnly modes: extracts perspective metadata, fetches appropriate links,
    /// and calls the appropriate service method
    async fn execute_simple_mode_query(
        &self,
        query: String,
        use_subscription_engine: bool,
        context: &AgentContext,
    ) -> Result<QueryResolution, AnyError> {
        let service = get_prolog_service().await;

        // Extract perspective metadata (same for Simple and SdnaOnly)
        let (perspective_uuid, neighbourhood_author) = {
            let persisted_guard = self.persisted.lock().await;
            (
                persisted_guard.uuid.clone(),
                persisted_guard
                    .neighbourhood
                    .as_ref()
                    .map(|n| n.author.clone()),
            )
        };

        // Override owner_did with current user's DID if context is provided (for multi-user prolog isolation)
        let user_did = did_for_context(context)?;

        // Fetch links based on mode
        let mut links: Vec<DecoratedLinkExpression> = match PROLOG_MODE {
            PrologMode::Simple => {
                // Get all links for Simple mode
                self.get_links_local(&LinkQuery::default())?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect()
            }
            PrologMode::SdnaOnly => {
                // Get only SDNA links for SdnaOnly mode (efficient query)
                self.get_sdna_links_local()
                    .await?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect()
            }
            _ => Vec::new(), // Should never reach here given the callers
        };

        // Filter to only show SDNA links created by this user
        links.retain(|link| {
            // Keep SDNA links only if authored by this user
            link.data.source == "ad4m://self"
                && (link.author == user_did || Some(&link.author) == neighbourhood_author.as_ref())
                || link.data.predicate.as_ref().map(|p| p.as_str()) == Some("ad4m://sdna")
                    && (link.author == user_did
                        || Some(&link.author) == neighbourhood_author.as_ref())
                || (link.data.source != "ad4m://self"
                    && link.data.predicate.as_ref().map(|p| p.as_str()) != Some("ad4m://sdna"))
        });

        // Execute the query using the appropriate engine
        let result = if use_subscription_engine {
            service
                .run_query_subscription_simple(
                    &perspective_uuid,
                    query,
                    &links,
                    neighbourhood_author,
                    Some(user_did),
                )
                .await
        } else {
            service
                .run_query_simple(
                    &perspective_uuid,
                    query,
                    &links,
                    neighbourhood_author,
                    Some(user_did),
                )
                .await
        };

        result.map_err(|e| anyhow!("{}", e))
    }

    /// Executes a Prolog query with user context - uses context-specific pool
    /// locks the prolog_update_mutex
    /// uses run_query_smart
    pub async fn prolog_query_with_context(
        &self,
        query: String,
        context: &AgentContext,
    ) -> Result<QueryResolution, AnyError> {
        match PROLOG_MODE {
            PrologMode::Simple | PrologMode::SdnaOnly => {
                self.execute_simple_mode_query(query, false, context).await
            }
            PrologMode::Pooled => {
                // Pooled mode: Use the old pool-based approach
                let perspective_uuid = self.uuid.clone();

                // Ensure the user-specific pool exists
                self.ensure_prolog_engine_pool_for_context(context).await?;

                self.prolog_query_helper(
                    query,
                    true,
                    |_uuid| self.get_pool_id_for_context(&perspective_uuid, context),
                    |service, pool, q| async move { service.run_query_smart(pool, q).await },
                )
                .await
            }
            PrologMode::Disabled => {
                // Return empty matches instead of False/Error to allow SHACL-based SDNA to work
                Ok(QueryResolution::Matches(vec![]))
            }
        }
    }

    /// Executes a Prolog subscription query against the perspective's main pool
    /// locks the prolog_update_mutex
    /// uses run_query_subscription
    pub async fn prolog_query_subscription(
        &self,
        query: String,
    ) -> Result<QueryResolution, AnyError> {
        match PROLOG_MODE {
            PrologMode::Simple | PrologMode::SdnaOnly => {
                self.execute_simple_mode_query(query, true, &AgentContext::main_agent())
                    .await
            }
            PrologMode::Pooled => {
                // Pooled mode: Use the old pool-based approach
                self.prolog_query_helper(
                    query,
                    true,
                    |uuid| uuid.clone(),
                    |service, pool, q| async move { service.run_query_subscription(pool, q).await },
                )
                .await
            }
            PrologMode::Disabled => {
                log::warn!(
                    "⚠️ Prolog subscription query received but Prolog is DISABLED (query: {}), returning empty result",
                    query
                );
                // Return empty result instead of error to allow SHACL-based SDNA to work
                Ok(QueryResolution::False)
            }
        }
    }

    /// Executes a Prolog subscription query with user context - uses context-specific pool
    /// locks the prolog_update_mutex
    /// uses run_query_subscription
    pub async fn prolog_query_subscription_with_context(
        &self,
        query: String,
        context: &AgentContext,
    ) -> Result<QueryResolution, AnyError> {
        match PROLOG_MODE {
            PrologMode::Simple | PrologMode::SdnaOnly => {
                // Context is now properly used for SDNA filtering per-user
                self.execute_simple_mode_query(query, true, context).await
            }
            PrologMode::Pooled => {
                // Pooled mode: Use the old pool-based approach with context
                let perspective_uuid = self.uuid.clone();

                self.prolog_query_helper(
                    query,
                    true,
                    |_uuid| self.get_pool_id_for_context(&perspective_uuid, context),
                    |service, pool, q| async move { service.run_query_subscription(pool, q).await },
                )
                .await
            }
            PrologMode::Disabled => {
                log::warn!(
                    "⚠️ Prolog subscription query received but Prolog is DISABLED (query: {}), returning empty result",
                    query
                );
                // Return empty result instead of error to allow SHACL-based SDNA to work
                Ok(QueryResolution::False)
            }
        }
    }

    /// Executes a Prolog query directly on the SDNA pool for maximum performance
    ///
    /// This bypasses all smart routing logic and goes directly to the SDNA pool.
    /// Use this for subject class queries during create_subject flow for best performance.
    /// Only use this for queries that you KNOW should be handled by the SDNA pool.
    ///
    /// does not lock the prolog_update_mutex
    /// uses run_query_sdna
    pub async fn prolog_query_sdna(&self, query: String) -> Result<QueryResolution, AnyError> {
        match PROLOG_MODE {
            PrologMode::Simple => {
                // In Simple mode, route to Simple engine which has SDNA facts
                let service = get_prolog_service().await;
                let (perspective_uuid, owner_did, neighbourhood_author) = {
                    let persisted_guard = self.persisted.lock().await;
                    let perspective_uuid = persisted_guard.uuid.clone();
                    let owner_did = persisted_guard.get_primary_owner();
                    let neighbourhood_author = persisted_guard
                        .neighbourhood
                        .as_ref()
                        .map(|n| n.author.clone());
                    (perspective_uuid, owner_did, neighbourhood_author)
                };

                // Get links for SDNA fact generation
                let links = self
                    .get_links_local(&LinkQuery::default())?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect::<Vec<_>>();

                service
                    .run_query_simple(
                        &perspective_uuid,
                        query,
                        &links,
                        neighbourhood_author,
                        owner_did,
                    )
                    .await
                    .map_err(|e| anyhow!("{}", e))
            }
            PrologMode::SdnaOnly => {
                // In SdnaOnly mode, route to Simple engine with only SDNA links
                let service = get_prolog_service().await;
                let (perspective_uuid, owner_did, neighbourhood_author) = {
                    let persisted_guard = self.persisted.lock().await;
                    let perspective_uuid = persisted_guard.uuid.clone();
                    let owner_did = persisted_guard.get_primary_owner();
                    let neighbourhood_author = persisted_guard
                        .neighbourhood
                        .as_ref()
                        .map(|n| n.author.clone());
                    (perspective_uuid, owner_did, neighbourhood_author)
                };

                // Get only SDNA-related links from database (efficient query)
                let links = self
                    .get_sdna_links_local()
                    .await?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect::<Vec<_>>();

                service
                    .run_query_simple(
                        &perspective_uuid,
                        query,
                        &links,
                        neighbourhood_author,
                        owner_did,
                    )
                    .await
                    .map_err(|e| anyhow!("{}", e))
            }
            PrologMode::Pooled => {
                // In pooled mode, use dedicated SDNA pool
                self.prolog_query_helper(
                    query,
                    false,
                    |uuid| uuid.clone(),
                    |service, pool, q| async move { service.run_query_sdna(pool, q).await },
                )
                .await
            }
            PrologMode::Disabled => Ok(QueryResolution::Matches(vec![])),
        }
    }

    /// Executes a Prolog query directly on the SDNA pool with user context
    /// This ensures the SDNA pool has the correct owner_did for SDNA fact filtering
    ///
    /// does not lock the prolog_update_mutex
    /// uses run_query_sdna
    pub async fn prolog_query_sdna_with_context(
        &self,
        query: String,
        context: &AgentContext,
    ) -> Result<QueryResolution, AnyError> {
        match PROLOG_MODE {
            PrologMode::Simple => {
                // In Simple mode, route to Simple engine (no per-context pools)
                // IMPORTANT: Use context user's DID as owner_did so their SDNA links are included
                let service = get_prolog_service().await;
                let (perspective_uuid, neighbourhood_author) = {
                    let persisted_guard = self.persisted.lock().await;
                    let perspective_uuid = persisted_guard.uuid.clone();
                    let neighbourhood_author = persisted_guard
                        .neighbourhood
                        .as_ref()
                        .map(|n| n.author.clone());
                    (perspective_uuid, neighbourhood_author)
                };

                // Use context DID as owner_did for SDNA filtering
                let owner_did = Some(if let Some(user_email) = &context.user_email {
                    crate::agent::AgentService::get_user_did_by_email(user_email)?
                } else {
                    crate::agent::AgentService::with_global_instance(|service| {
                        service.did.clone().unwrap_or_default()
                    })
                });

                // Get links for SDNA fact generation
                let links = self
                    .get_links_local(&LinkQuery::default())?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect::<Vec<_>>();

                service
                    .run_query_simple(
                        &perspective_uuid,
                        query,
                        &links,
                        neighbourhood_author,
                        owner_did,
                    )
                    .await
                    .map_err(|e| anyhow!("{}", e))
            }
            PrologMode::SdnaOnly => {
                // In SdnaOnly mode, route to Simple engine (no per-context pools), only SDNA links
                // IMPORTANT: Use context user's DID as owner_did so their SDNA links are included
                let service = get_prolog_service().await;
                let (perspective_uuid, neighbourhood_author) = {
                    let persisted_guard = self.persisted.lock().await;
                    let perspective_uuid = persisted_guard.uuid.clone();
                    let neighbourhood_author = persisted_guard
                        .neighbourhood
                        .as_ref()
                        .map(|n| n.author.clone());
                    (perspective_uuid, neighbourhood_author)
                };

                // Use context DID as owner_did for SDNA filtering
                let owner_did = Some(if let Some(user_email) = &context.user_email {
                    crate::agent::AgentService::get_user_did_by_email(user_email)?
                } else {
                    crate::agent::AgentService::with_global_instance(|service| {
                        service.did.clone().unwrap_or_default()
                    })
                });

                // Get only SDNA-related links from database (efficient query)
                let links = self
                    .get_sdna_links_local()
                    .await?
                    .into_iter()
                    .map(|(link, status)| DecoratedLinkExpression::from((link, status)))
                    .collect::<Vec<_>>();

                service
                    .run_query_simple(
                        &perspective_uuid,
                        query,
                        &links,
                        neighbourhood_author,
                        owner_did,
                    )
                    .await
                    .map_err(|e| anyhow!("{}", e))
            }
            PrologMode::Pooled => {
                // In pooled mode, use per-context SDNA pool
                let perspective_uuid = self.uuid.clone();

                // Ensure the user-specific pool exists
                self.ensure_prolog_engine_pool_for_context(context).await?;

                self.prolog_query_helper(
                    query,
                    false,
                    |_uuid| self.get_pool_id_for_context(&perspective_uuid, context),
                    |service, pool, q| async move { service.run_query_sdna(pool, q).await },
                )
                .await
            }
            PrologMode::Disabled => Ok(QueryResolution::Matches(vec![])),
        }
    }

    /// Ensure prolog engine pool exists for the given context with correct owner_did
    pub async fn ensure_prolog_engine_pool_for_context(
        &self,
        context: &AgentContext,
    ) -> Result<(), AnyError> {
        // Prolog engine pools are only used in Pooled mode
        if PROLOG_MODE != PrologMode::Pooled {
            return Ok(());
        }

        let (perspective_uuid, neighbourhood_author) = {
            let persisted_guard = self.persisted.lock().await;
            let neighbourhood_author = persisted_guard
                .neighbourhood
                .as_ref()
                .map(|n| n.author.clone());
            (persisted_guard.uuid.clone(), neighbourhood_author)
        };

        let pool_id = self.get_pool_id_for_context(&perspective_uuid, context);
        let owner_did = if let Some(user_email) = &context.user_email {
            crate::agent::AgentService::get_user_did_by_email(user_email)?
        } else {
            crate::agent::AgentService::with_global_instance(|service| {
                service.did.clone().unwrap_or_default()
            })
        };

        // Ensure pool exists
        let service = get_prolog_service().await;
        service
            .ensure_perspective_pool(pool_id.clone(), None)
            .await?;

        // Initialize user pool with correct neighbourhood author for SDNA governance
        // This ensures users can see SDNA from both themselves and the neighbourhood creator
        let links = self.get_links(&crate::types::LinkQuery::default()).await?;

        service
            .update_perspective_links(
                pool_id,
                "facts".to_string(),  // module_name
                links,                // already DecoratedLinkExpression
                neighbourhood_author, // neighbourhood_author for SDNA governance
                Some(owner_did),      // owner_did for SDNA
            )
            .await?;

        Ok(())
    }

    /// Executes a Prolog query against the notification pool
    /// does not lock the prolog_update_mutex
    /// uses run_query_smart
    pub async fn prolog_query_notification(
        &self,
        query: String,
    ) -> Result<QueryResolution, AnyError> {
        self.prolog_query_helper(
            query,
            false,
            |uuid| notification_pool_name(uuid),
            |service, pool, q| async move { service.run_query_smart(pool, q).await },
        )
        .await
    }

    /// Execute a SPARQL query against this perspective's Oxigraph store
    pub fn sparql_query(&self, query: String) -> Result<String, deno_core::anyhow::Error> {
        self.sparql_store.query(&query)
    }

    /// Execute a model query — the executor-side replacement for
    /// SPARQL-build → hydrate → JS-filter → JS-sort → JS-paginate.
    pub fn model_query(
        &self,
        class_name: &str,
        query_json: &str,
    ) -> Result<String, deno_core::anyhow::Error> {
        let query_input: super::model_query::ModelQueryInput = serde_json::from_str(query_json)
            .map_err(|e| deno_core::anyhow::anyhow!("Failed to parse model query: {}", e))?;

        let resolver = self.shape_resolver();
        let shape = resolver.get_shape(class_name)?;
        let result = super::model_query::execute_model_query(
            &self.sparql_store,
            shape.as_ref(),
            &query_input,
            &resolver,
        )?;

        serde_json::to_string(&result).map_err(|e| {
            deno_core::anyhow::anyhow!("Failed to serialize model query result: {}", e)
        })
    }

    /// Evaluate property getters for a batch of instances in-process.
    ///
    /// Returns a JSON string of `{ instanceId: { prop: value, ... } }`.
    pub fn evaluate_getters(
        &self,
        class_name: &str,
        instance_ids: &[String],
        property_names: Option<&[String]>,
    ) -> Result<String, deno_core::anyhow::Error> {
        let shape = self.get_shape(class_name)?;
        let result = super::model_query::evaluate_getters_batch(
            &self.sparql_store,
            shape.as_ref(),
            instance_ids,
            property_names,
        )?;

        serde_json::to_string(&result).map_err(|e| {
            deno_core::anyhow::anyhow!("Failed to serialize evaluate_getters result: {}", e)
        })
    }

    pub(crate) async fn persist_link_diff(
        &self,
        diff: &DecoratedPerspectiveDiff,
    ) -> Result<(), AnyError> {
        // IMPORTANT: Process removals BEFORE additions!
        // The remove_link function matches by source/predicate/target (not unique ID).
        // If we add first and remove second, we'd delete the newly added links too.

        // Removals first
        for removal in &diff.removals {
            if let Err(e) = self.sparql_store.remove_link(removal) {
                log::warn!("Failed to remove link from SPARQL store: {:?}", e);
            }
        }
        // Additions after
        for addition in &diff.additions {
            if let Err(e) = self.sparql_store.add_link(addition) {
                log::warn!("Failed to add link to SPARQL store: {:?}", e);
            }
        }

        Ok(())
    }

    /// Record the predicates from a diff into `changed_predicates`.
    /// `None` means "all predicates changed" (check everything).
    async fn record_changed_predicates(&self, diff: &DecoratedPerspectiveDiff) {
        let mut changed = self.changed_predicates.lock().await;

        // If already CheckAll, it's sticky — nothing to do
        if matches!(*changed, ChangedPredicates::CheckAll) {
            return;
        }

        // Collect predicates from the diff
        let mut new_preds = HashSet::new();
        let mut has_predicate_less = false;
        for link in diff.additions.iter().chain(diff.removals.iter()) {
            if let Some(ref pred) = link.data.predicate {
                new_preds.insert(pred.clone());
            } else {
                has_predicate_less = true;
                break;
            }
        }

        if has_predicate_less {
            *changed = ChangedPredicates::CheckAll;
        } else {
            match &mut *changed {
                ChangedPredicates::NoneRecorded => {
                    *changed = ChangedPredicates::Specific(new_preds);
                }
                ChangedPredicates::Specific(existing) => {
                    existing.extend(new_preds);
                }
                ChangedPredicates::CheckAll => unreachable!(),
            }
        }
    }

    fn spawn_prolog_facts_update(
        &self,
        diff: DecoratedPerspectiveDiff,
        completion_sender: Option<tokio::sync::oneshot::Sender<()>>,
    ) {
        let self_clone = self.clone();

        tokio::spawn(async move {
            // In Disabled, Simple, or SdnaOnly mode, just trigger subscription checks
            // (Pooled mode prolog updates don't apply - run_query_all only works in Pooled mode)
            if PROLOG_MODE == PrologMode::Disabled
                || PROLOG_MODE == PrologMode::Simple
                || PROLOG_MODE == PrologMode::SdnaOnly
            {
                // Trigger notification, prolog subscription
                self_clone
                    .trigger_notification_check
                    .store(true, Ordering::Release);
                self_clone.record_changed_predicates(&diff).await;
                self_clone
                    .trigger_prolog_subscription_check
                    .store(true, Ordering::Release);

                self_clone.pubsub_publish_diff(diff).await;

                if let Some(sender) = completion_sender {
                    let _ = sender.send(());
                }
                return;
            }

            // Pooled mode: original full update logic
            //let spawn_start = std::time::Instant::now();
            //log::info!("🔧 PROLOG UPDATE: Starting prolog facts update task - {} add, {} rem",
            //    diff.additions.len(), diff.removals.len());

            //let ensure_pool_start = std::time::Instant::now();
            if let Err(e) = self_clone.ensure_prolog_engine_pool().await {
                log::error!("Error spawning Prolog engine pool: {:?}", e);
                if let Some(sender) = completion_sender {
                    let _ = sender.send(());
                }
                return;
            }
            //log::info!("🔧 PROLOG UPDATE: Engine pool ensured in {:?}", ensure_pool_start.elapsed());

            let uuid = self_clone.uuid.clone();

            //let analysis_start = std::time::Instant::now();
            let fact_rebuild_needed = !diff.removals.is_empty()
                || diff.additions.iter().any(|link| is_sdna_link(&link.data));
            //log::info!("🔧 PROLOG UPDATE: Analysis completed in {:?} - rebuild_needed: {}",
            //    analysis_start.elapsed(), fact_rebuild_needed);

            //let mutex_wait_start = std::time::Instant::now();
            //log::info!("🔧 PROLOG UPDATE: Waiting for prolog_update_mutex...");

            let did_update = if !fact_rebuild_needed {
                //log::info!("🔧 PROLOG UPDATE: Using FAST ASSERTION path");
                // For additions only, use assertions - acquire lock only during prolog operations
                //let assertions_start = std::time::Instant::now();
                let mut assertions: Vec<String> = Vec::new();
                for addition in &diff.additions {
                    assertions.push(generic_link_fact("assert_link_and_triple", addition));
                }
                //log::info!("🔧 PROLOG UPDATE: Built {} assertions in {:?}",
                //    assertions.len(), assertions_start.elapsed());

                //let service_start = std::time::Instant::now();
                let service = get_prolog_service().await;
                //log::info!("🔧 PROLOG UPDATE: Got prolog service in {:?}", service_start.elapsed());

                // Acquire write lock only for the prolog operation
                let _write_guard = self_clone.prolog_update_mutex.write().await;
                //log::info!("🔧 PROLOG UPDATE: Acquired prolog_update_mutex after {:?}", mutex_wait_start.elapsed());

                let query_start = std::time::Instant::now();
                let query = format!("{}.", assertions.join(","));
                //log::info!("🔧 PROLOG UPDATE: Running assertion query: {} chars", query.len());

                let service_clone = service.clone();
                let uuid_clone = uuid.clone();
                let query_clone = query.clone();
                tokio::spawn(async move {
                    if let Err(e) = service_clone
                        .run_query_all(notification_pool_name(&uuid_clone), query_clone)
                        .await
                    {
                        log::error!(
                            "Failed to update notification pool for perspective {}: {:?}",
                            uuid_clone,
                            e
                        );
                    }
                });

                match service.run_query_all(uuid, query).await {
                    Ok(()) => {
                        //log::info!("🔧 PROLOG UPDATE: Assertion query completed successfully in {:?}", query_start.elapsed());
                        true
                    }
                    Err(e) => {
                        log::error!(
                            "Error while running assertion query to update Prolog engine facts (took {:?}): {:?}", 
                            query_start.elapsed(), e
                        );
                        false
                    }
                }
            } else {
                //log::info!("🔧 PROLOG UPDATE: Using FULL REBUILD path");
                // For fact rebuild, acquire write lock for the entire operation
                let _write_guard = self_clone.prolog_update_mutex.write().await;
                //log::info!("🔧 PROLOG UPDATE: Acquired prolog_update_mutex after {:?}", mutex_wait_start.elapsed());

                let rebuild_start = std::time::Instant::now();
                match self_clone.update_prolog_engine_facts().await {
                    Ok(()) => {
                        log::trace!(
                            "🔧 PROLOG UPDATE: Full rebuild completed successfully in {:?}",
                            rebuild_start.elapsed()
                        );
                        true
                    }
                    Err(e) => {
                        log::error!(
                            "Error while updating Prolog engine facts (took {:?}): {:?}",
                            rebuild_start.elapsed(),
                            e
                        );
                        false
                    }
                }
            };

            if did_update {
                self_clone.record_changed_predicates(&diff).await;
                self_clone.pubsub_publish_diff(diff).await;

                // Trigger notification and subscription checks after prolog facts are updated
                self_clone
                    .trigger_notification_check
                    .store(true, Ordering::Release);
                self_clone
                    .trigger_prolog_subscription_check
                    .store(true, Ordering::Release);
            }

            //log::info!("🔧 PROLOG UPDATE: Total prolog update task took {:?}", spawn_start.elapsed());

            // Signal completion through the oneshot channel if provided
            if let Some(sender) = completion_sender {
                let _ = sender.send(());
            }
        });
    }

    fn all_notifications_for_perspective_id(uuid: String) -> Result<Vec<Notification>, AnyError> {
        Ok(Ad4mDb::with_global_instance(|db| db.get_notifications())?
            .into_iter()
            .filter(|n| n.perspective_ids.contains(&uuid))
            .collect())
    }

    async fn calc_notification_trigger_matches(
        &self,
    ) -> Result<BTreeMap<Notification, Vec<serde_json::Value>>, AnyError> {
        let uuid = self.uuid.clone();

        let notifications = Self::all_notifications_for_perspective_id(uuid.clone())?;
        //log::info!("🔔 NOTIFICATIONS: Found {} notifications for perspective {}", notifications.len(), uuid);

        //log::info!("🔔 NOTIFICATIONS: All triggers:\n{}", notifications.iter()
        //    .map(|n| n.trigger.clone())
        //    .collect::<Vec<String>>()
        //    .join("\n"));
        let mut result_map = BTreeMap::new();
        // Cache key must include both trigger and user_email for deduplication
        let mut trigger_cache: HashMap<(String, Option<String>), Vec<serde_json::Value>> =
            HashMap::new();

        for n in notifications {
            //log::info!("🔔 NOTIFICATIONS: Processing notification for perspective {}: {}", uuid, n.trigger);
            let cache_key = (n.trigger.clone(), n.user_email.clone());
            if let Some(cached_matches) = trigger_cache.get(&cache_key) {
                //log::info!("🔔 NOTIFICATIONS: Using cached matches for notification for perspective {}: {}", uuid, n.trigger);
                result_map.insert(n.clone(), cached_matches.clone());
            } else {
                //let query_start = std::time::Instant::now();
                //log::info!("🔔 NOTIFICATIONS: not cached - Querying notification for perspective {}", uuid);
                // Handle errors per-notification to prevent one user's DID failure from
                // silencing all notifications. This can happen with orphaned notifications
                // from deleted users or corrupted data.
                match {
                    let query = n.trigger.clone();
                    let result_json = self.sparql_store.query(&query);
                    match result_json {
                        Ok(json) => serde_json::from_str::<Vec<serde_json::Value>>(&json)
                            .map_err(|e| anyhow::anyhow!(e)),
                        Err(e) => Err(e),
                    }
                } {
                    Ok(matches) => {
                        trigger_cache.insert(cache_key, matches.clone());
                        result_map.insert(n.clone(), matches);
                    }
                    Err(e) => {
                        log::error!(
                            "Failed to query notification for user {:?} in perspective {}: {:?}. Query: {}. Skipping this notification.",
                            n.user_email,
                            uuid,
                            e,
                            n.trigger
                        );
                        // Skip this notification but continue processing others
                    }
                }
                //log::info!("🔔 NOTIFICATIONS: Querying notification: {} - took {:?}", n.trigger, query_start.elapsed());
            }
        }

        Ok(result_map)
    }

    async fn notification_trigger_snapshot(
        &self,
    ) -> BTreeMap<Notification, Vec<serde_json::Value>> {
        self.calc_notification_trigger_matches()
            .await
            .unwrap_or_else(|e| {
                log::error!("Error trying to render notification matches: {:?}", e);
                BTreeMap::new()
            })
    }

    fn subtract_before_notification_matches(
        before: &BTreeMap<Notification, Vec<serde_json::Value>>,
        after: &BTreeMap<Notification, Vec<serde_json::Value>>,
    ) -> BTreeMap<Notification, Vec<serde_json::Value>> {
        after
            .iter()
            .filter_map(|(notification, after_matches)| {
                let new_matches: Vec<serde_json::Value> =
                    if let Some(before_matches) = before.get(notification) {
                        // Find matches that exist in "after" but not in "before"
                        after_matches
                            .iter()
                            .filter(|after_match| {
                                !before_matches
                                    .iter()
                                    .any(|before_match| before_match == *after_match)
                            })
                            .cloned()
                            .collect()
                    } else {
                        // No previous matches, so all current matches are new
                        after_matches.clone()
                    };

                if new_matches.is_empty() {
                    None
                } else {
                    Some((notification.clone(), new_matches))
                }
            })
            .collect()
    }

    async fn publish_notification_matches(
        uuid: String,
        match_map: BTreeMap<Notification, Vec<serde_json::Value>>,
    ) {
        for (notification, matches) in match_map {
            if !matches.is_empty() {
                // Convert matches to JSON string
                let trigger_match =
                    serde_json::to_string(&matches).unwrap_or_else(|_| "[]".to_string());

                let payload = TriggeredNotification {
                    notification: notification.clone(),
                    perspective_id: uuid.clone(),
                    trigger_match,
                };

                let message = serde_json::to_string(&payload).unwrap();

                get_global_pubsub()
                    .await
                    .publish(&RUNTIME_NOTIFICATION_TRIGGERED_TOPIC, &message)
                    .await;

                if url::Url::parse(&notification.webhook_url).is_ok() {
                    log::info!(
                        "Notification webhook - posting to {:?}",
                        notification.webhook_url
                    );
                    let client = reqwest::Client::new();
                    let res = client
                        .post(&notification.webhook_url)
                        .bearer_auth(&notification.webhook_auth)
                        .header("Content-Type", "application/json")
                        .body(message.clone())
                        .send()
                        .await;
                    log::info!("Notification webhook response: {:?}", res);
                }
            }
        }
    }

    async fn update_prolog_engine_facts(&self) -> Result<(), AnyError> {
        // Get all required data before making service calls
        let (uuid, owner_did) = {
            let persisted_guard = self.persisted.lock().await;
            (
                persisted_guard.uuid.clone(),
                persisted_guard.get_primary_owner(),
            )
        };

        let all_links = self.get_links(&LinkQuery::default()).await?;

        let neighbourhood_author = {
            let persisted_guard = self.persisted.lock().await;
            persisted_guard
                .neighbourhood
                .as_ref()
                .map(|n| n.author.clone())
        };

        let service = get_prolog_service().await;
        service
            .update_perspective_links(
                uuid.clone(),
                "facts".to_string(),
                all_links.clone(),
                neighbourhood_author.clone(),
                owner_did.clone(),
            )
            .await?;
        let service_clone = service.clone();
        tokio::spawn(async move {
            let _ = service_clone
                .update_perspective_links(
                    notification_pool_name(&uuid),
                    "facts".to_string(),
                    all_links,
                    neighbourhood_author,
                    owner_did,
                )
                .await;
        });
        Ok(())
    }

    async fn no_link_language_error(&self) -> AnyError {
        let (uuid, state) = {
            let handle = self.persisted.lock().await;
            (handle.uuid.clone(), handle.state.clone())
        };
        anyhow!(
            "Perspective {} has no link language installed. State is: {:?}",
            uuid,
            state
        )
    }

    pub async fn others(&self) -> Result<Vec<String>, AnyError> {
        let link_language_clone = self.link_language.read().await.clone();
        let mut all_others = if let Some(mut link_language) = link_language_clone {
            link_language.others().await?
        } else {
            return Err(self.no_link_language_error().await);
        };

        // Add all perspective owners (which includes local managed users)
        let handle = self.persisted.lock().await.clone();
        if let Some(owners) = &handle.owners {
            log::debug!("🔍 others() - Perspective owners: {:?}", owners);

            for owner_did in owners {
                if !all_others.contains(owner_did) {
                    log::debug!("✅ others() - Adding owner to others list: {}", owner_did);
                    all_others.push(owner_did.clone());
                }
            }
        }

        log::debug!("🔍 others() - Final others list: {:?}", all_others);
        Ok(all_others)
    }

    pub async fn has_telepresence_adapter(&self) -> bool {
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            match link_language.has_telepresence_adapter().await {
                Ok(result) => result,
                Err(e) => {
                    log::error!("Error calling has_telepresence_adapter: {:?}", e);
                    false
                }
            }
        } else {
            false
        }
    }

    pub async fn online_agents(&self) -> Result<Vec<OnlineAgent>, AnyError> {
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            Ok(link_language
                .get_online_agents()
                .await?
                .into_iter()
                .map(|mut a| {
                    a.status.verify_signatures();
                    a
                })
                .collect())
        } else {
            Err(self.no_link_language_error().await)
        }
    }

    pub async fn set_online_status(&self, status: PerspectiveExpression) -> Result<(), AnyError> {
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            link_language.set_online_status(status).await
        } else {
            Err(self.no_link_language_error().await)
        }
    }

    pub async fn send_signal(
        &self,
        remote_agent_did: String,
        payload: PerspectiveExpression,
    ) -> Result<(), AnyError> {
        // Check if the recipient is a locally managed user
        use crate::agent::AgentService;

        log::debug!(
            "🔔 SEND SIGNAL: Sending signal to remote agent {}",
            remote_agent_did
        );

        let current_perspective_handle = self.persisted.lock().await.clone();

        // Check if this perspective is part of a neighbourhood
        if current_perspective_handle.shared_url.is_some() {
            // Helper closure: publish a signal locally and return Ok(())
            let publish_local = |handle: PerspectiveHandle,
                                 mut signal: PerspectiveExpression,
                                 recipient: String| async move {
                signal.verify_signatures();
                get_global_pubsub()
                    .await
                    .publish(
                        &NEIGHBOURHOOD_SIGNAL_TOPIC,
                        &serde_json::to_string(&NeighbourhoodSignalFilter {
                            perspective: handle,
                            signal,
                            recipient: Some(recipient),
                        })
                        .unwrap(),
                    )
                    .await;
            };

            // Check if any managed email user is the recipient
            if let Ok(user_emails) = AgentService::list_user_emails() {
                for user_email in user_emails {
                    if let Ok(user_did) = AgentService::get_user_did_by_email(&user_email) {
                        if user_did == remote_agent_did {
                            if let Some(owners) = &current_perspective_handle.owners {
                                if owners.contains(&remote_agent_did) {
                                    log::debug!(
                                        "Routing signal locally to managed user {} in neighbourhood {:?}",
                                        user_email,
                                        current_perspective_handle.shared_url
                                    );
                                    let handle = self.persisted.lock().await.clone();
                                    publish_local(handle, payload, remote_agent_did).await;
                                    return Ok(());
                                }
                            }
                        }
                    }
                }
            }

            // Check if the main agent is the recipient.
            // Treat owners=None or owners=[] as implicit main-agent ownership (legacy perspectives).
            let main_agent_did = AgentService::with_global_instance(|s| s.did.clone());
            if let Some(main_agent_did) = main_agent_did {
                if main_agent_did == remote_agent_did {
                    let is_owner = current_perspective_handle
                        .owners
                        .as_ref()
                        .map_or(true, |o| o.is_empty() || o.contains(&remote_agent_did));
                    if is_owner {
                        log::debug!(
                            "Routing signal locally to main agent in neighbourhood {:?}",
                            current_perspective_handle.shared_url
                        );
                        let handle = self.persisted.lock().await.clone();
                        publish_local(handle, payload, remote_agent_did).await;
                        return Ok(());
                    }
                }
            }
        }

        log::debug!(
            "🔔 SEND SIGNAL: Not a local user in this neighbourhood, sending through link language"
        );

        // If not a local user in this neighbourhood, send through link language
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            log::debug!("🔔 SEND SIGNAL: Sending signal through link language");
            link_language.send_signal(remote_agent_did, payload).await
        } else {
            Err(self.no_link_language_error().await)
        }
    }

    pub async fn send_broadcast(
        &self,
        payload: PerspectiveExpression,
        loopback: bool,
    ) -> Result<(), AnyError> {
        use crate::agent::AgentService;

        let current_perspective_handle = self.persisted.lock().await.clone();

        if loopback {
            // send back to all clients through neighbourhood signal subscription
            let payload_clone = payload.clone();
            let self_clone = self.clone();
            tokio::spawn(async move {
                self_clone
                    .telepresence_signal_from_link_language(payload_clone, None)
                    .await;
            });
        }

        // Route signals to all local agents (managed users + main agent) who are owners
        if current_perspective_handle.shared_url.is_some() {
            // Send to each local managed email user who is an explicit owner
            if let Some(owners) = &current_perspective_handle.owners {
                if let Ok(user_emails) = AgentService::list_user_emails() {
                    for user_email in user_emails {
                        if let Ok(user_did) = AgentService::get_user_did_by_email(&user_email) {
                            if owners.contains(&user_did) {
                                let handle = self.persisted.lock().await.clone();
                                let mut signal = payload.clone();
                                signal.verify_signatures();

                                get_global_pubsub()
                                    .await
                                    .publish(
                                        &NEIGHBOURHOOD_SIGNAL_TOPIC,
                                        &serde_json::to_string(&NeighbourhoodSignalFilter {
                                            perspective: handle,
                                            signal,
                                            recipient: Some(user_did),
                                        })
                                        .unwrap(),
                                    )
                                    .await;
                            }
                        }
                    }
                }
            }

            // Send to the main agent if it is an owner.
            // The main agent is not in list_user_emails(), so it must be handled separately.
            // Treat owners=None or owners=[] as implicit main-agent ownership (legacy perspectives).
            let main_agent_did = AgentService::with_global_instance(|s| s.did.clone());
            if let Some(main_agent_did) = main_agent_did {
                let is_owner = current_perspective_handle
                    .owners
                    .as_ref()
                    .map_or(true, |o| o.is_empty() || o.contains(&main_agent_did));
                // Don't echo the broadcast back to the sender (loopback is handled separately).
                let is_sender = payload.author == main_agent_did;
                if is_owner && !is_sender {
                    let handle = self.persisted.lock().await.clone();
                    let mut signal = payload.clone();
                    signal.verify_signatures();

                    log::debug!(
                        "Broadcasting signal locally to main agent in neighbourhood {:?}",
                        current_perspective_handle.shared_url
                    );

                    get_global_pubsub()
                        .await
                        .publish(
                            &NEIGHBOURHOOD_SIGNAL_TOPIC,
                            &serde_json::to_string(&NeighbourhoodSignalFilter {
                                perspective: handle,
                                signal,
                                recipient: Some(main_agent_did),
                            })
                            .unwrap(),
                        )
                        .await;
                }
            }
        }

        // Also send through link language for remote users
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            link_language.send_broadcast(payload).await
        } else {
            Err(self.no_link_language_error().await)
        }
    }

    pub async fn update_local_agents(&self, agents: Vec<String>) {
        log::debug!("Updating local agents for perspective: {:?}", agents);
        let link_language_clone = self.link_language.read().await.clone();
        if let Some(mut link_language) = link_language_clone {
            if let Err(e) = link_language.set_local_agents(agents).await {
                log::error!("Failed to update local agents on link language: {:?}", e);
            }
        } else {
            log::warn!("Cannot update local agents: link language not initialized");
        }
    }

    pub async fn execute_commands(
        &mut self,
        commands: Vec<Command>,
        expression: String,
        parameters: Vec<Parameter>,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<(), AnyError> {
        //let execute_start = std::time::Instant::now();
        //log::info!("⚙️ EXECUTE COMMANDS: Starting execution of {} commands for expression '{}', batch_id: {:?}",
        //    commands.len(), expression, batch_id);

        let jsvalue_to_string = |value: &Value| -> String {
            match value {
                serde_json::Value::String(s) => s.clone(),
                _ => value.to_string(),
            }
        };

        let replace_this = |input: Option<String>| -> Option<String> {
            if Some(String::from("this")) == input {
                Some(expression.clone())
            } else {
                input
            }
        };

        // Single-pass substitution.
        //
        // The previous implementation did a sequential `String::replace`
        // for each parameter in order, which cascaded: a replacement's
        // output could contain another parameter's name and get
        // re-substituted in the next iteration, silently corrupting data
        // whenever a user-provided value happened to match a later
        // parameter name. It was also order-dependent for parameters
        // whose names were prefixes of each other.
        //
        // Instead, scan the input once left-to-right. At each position,
        // try each parameter name (longest first, so that e.g. "foo_bar"
        // wins over "foo" when both exist) and if one matches, emit its
        // value and skip past the name without re-scanning the emitted
        // text. Otherwise advance one UTF-8 character.
        let mut sorted_params: Vec<&Parameter> = parameters.iter().collect();
        sorted_params.sort_by_key(|p| std::cmp::Reverse(p.name.len()));
        let replace_parameters = |input: Option<String>| -> Option<String> {
            let input = input?;
            if sorted_params.is_empty() {
                return Some(input);
            }
            let mut output = String::with_capacity(input.len());
            let bytes = input.as_bytes();
            let mut i = 0;
            while i < bytes.len() {
                let mut matched = false;
                for p in &sorted_params {
                    if p.name.is_empty() {
                        continue;
                    }
                    let name_bytes = p.name.as_bytes();
                    if bytes[i..].starts_with(name_bytes) {
                        output.push_str(&jsvalue_to_string(&p.value));
                        i += name_bytes.len();
                        matched = true;
                        break;
                    }
                }
                if !matched {
                    let ch_len = input[i..].chars().next().map(|c| c.len_utf8()).unwrap_or(1);
                    output.push_str(&input[i..i + ch_len]);
                    i += ch_len;
                }
            }
            Some(output)
        };

        for command in commands.iter() {
            //let command_start = std::time::Instant::now();
            //log::info!("⚙️ EXECUTE COMMANDS: Processing command {}/{}: {:?}", i + 1, commands.len(), command.action);

            let source = replace_this(replace_parameters(command.source.clone()))
                .ok_or_else(|| anyhow!("Source cannot be None"))?;
            let predicate = replace_this(replace_parameters(command.predicate.clone()));
            let target = (replace_parameters(command.target.clone()))
                .ok_or_else(|| anyhow!("Target cannot be None"))?;
            let local = command.local.unwrap_or(false);
            let status = if local {
                LinkStatus::Local
            } else {
                LinkStatus::Shared
            };

            match command.action {
                Action::AddLink => {
                    self.add_link(
                        Link {
                            source,
                            predicate,
                            target,
                        },
                        status,
                        batch_id.clone(),
                        context,
                    )
                    .await?;
                }
                Action::RemoveLink => {
                    let remove_source = source.clone();
                    let remove_predicate = predicate.clone();
                    let remove_target = if target == "*" { None } else { Some(target) };
                    let link_expressions = self
                        .get_links(&LinkQuery {
                            source: Some(remove_source.clone()),
                            predicate: remove_predicate.clone(),
                            target: remove_target.clone(),
                            from_date: None,
                            until_date: None,
                            limit: None,
                        })
                        .await?;
                    for link_expression in link_expressions {
                        self.remove_link(link_expression.into(), batch_id.clone())
                            .await?;
                    }
                    // Also prune matching links from pending batch additions
                    if let Some(ref bid) = batch_id {
                        let mut batches = self.batch_store.write().await;
                        if let Some(diff) = batches.get_mut(bid) {
                            diff.additions.retain(|link_expr| {
                                let source_match = link_expr.data.source == remove_source;
                                let pred_match = remove_predicate.is_none()
                                    || link_expr.data.predicate == remove_predicate;
                                let target_match = remove_target.is_none()
                                    || link_expr.data.target
                                        == remove_target.as_deref().unwrap_or("");
                                !(source_match && pred_match && target_match)
                            });
                        }
                    }
                }
                Action::SetSingleTarget => {
                    if predicate.is_none() {
                        log::error!(
                            "SetSingleTarget actions with no predicate are not allowed. Skipping."
                        );
                        continue;
                    }
                    // Remove matching persisted links
                    let link_expressions = self
                        .get_links(&LinkQuery {
                            source: Some(source.clone()),
                            predicate: predicate.clone(),
                            target: None,
                            from_date: None,
                            until_date: None,
                            limit: None,
                        })
                        .await?;
                    for link_expression in link_expressions {
                        self.remove_link(link_expression.into(), batch_id.clone())
                            .await?;
                    }
                    // Also prune matching links from pending batch additions so
                    // that a previous add in the same batch doesn't survive and
                    // create a duplicate (e.g. save+update in one transaction).
                    if let Some(ref bid) = batch_id {
                        let mut batches = self.batch_store.write().await;
                        if let Some(diff) = batches.get_mut(bid) {
                            diff.additions.retain(|link_expr| {
                                !(link_expr.data.source == source
                                    && link_expr.data.predicate == predicate)
                            });
                        }
                    }
                    self.add_link(
                        Link {
                            source,
                            predicate,
                            target,
                        },
                        status,
                        batch_id.clone(),
                        context,
                    )
                    .await?;
                }
                Action::CollectionSetter => {
                    // Remove matching persisted links
                    let link_expressions = self
                        .get_links(&LinkQuery {
                            source: Some(source.clone()),
                            predicate: predicate.clone(),
                            target: None,
                            from_date: None,
                            until_date: None,
                            limit: None,
                        })
                        .await?;
                    for link_expression in link_expressions {
                        self.remove_link(link_expression.into(), batch_id.clone())
                            .await?;
                    }
                    // Also prune matching links from pending batch additions
                    if let Some(ref bid) = batch_id {
                        let mut batches = self.batch_store.write().await;
                        if let Some(diff) = batches.get_mut(bid) {
                            diff.additions.retain(|link_expr| {
                                !(link_expr.data.source == source
                                    && link_expr.data.predicate == predicate)
                            });
                        }
                    }
                    self.add_links(
                        parameters
                            .iter()
                            .map(|p| Link {
                                source: source.clone(),
                                predicate: predicate.clone(),
                                target: jsvalue_to_string(&p.value),
                            })
                            .collect(),
                        status,
                        batch_id.clone(),
                        context,
                    )
                    .await?;
                }
            }

            //log::info!("⚙️ EXECUTE COMMANDS: Command {} completed in {:?}", i + 1, command_start.elapsed());
        }

        //log::info!("⚙️ EXECUTE COMMANDS: All {} commands executed in {:?}", commands.len(), execute_start.elapsed());
        Ok(())
    }

    async fn subject_class_option_to_class_name(
        &mut self,
        subject_class: SubjectClassOption,
        _context: &AgentContext,
    ) -> Result<String, AnyError> {
        //let method_start = std::time::Instant::now();
        //log::info!("🔍 SUBJECT CLASS: Starting class name resolution...");

        Ok(if let Some(class_name) = subject_class.class_name {
            class_name
        } else {
            return Err(anyhow!(
                "SubjectClassOption requires `className` to be set. Query-based lookup has been removed; resolve the class name client-side."
            ));
        })
    }

    /// Parse actions JSON from a literal target (format: "literal:string:{json}" or legacy "literal://string:{json}")
    fn parse_actions_from_literal(target: &str) -> Result<Vec<Command>, AnyError> {
        let json_str = if let Some(rest) = target.strip_prefix("literal://string:") {
            rest
        } else if let Some(rest) = target.strip_prefix("literal:string:") {
            rest
        } else {
            return Err(anyhow!("Invalid literal format: {}", target));
        };
        // Decode URL-encoded characters if present
        let decoded = urlencoding::decode(json_str)
            .map(|s| s.to_string())
            .unwrap_or_else(|_| json_str.to_string());
        serde_json::from_str(&decoded)
            .map_err(|e| anyhow!("Failed to parse actions JSON: {} - input: {}", e, decoded))
    }

    /// Get actions from SHACL links for a shape-level predicate (constructor/destructor)
    async fn get_shape_actions_from_shacl(
        &self,
        class_name: &str,
        predicate: &str,
    ) -> Result<Option<Vec<Command>>, AnyError> {
        // Query SPARQL store for links with the given predicate whose source ends with {ClassName}Shape
        let shape_suffix = format!("{}Shape", class_name);
        let _uuid = self.uuid.clone();

        let links = self
            .sparql_store
            .get_links_by_predicate_and_source_suffix(predicate, &shape_suffix)?;

        // Return the first match
        if let Some(link) = links.first() {
            return Self::parse_actions_from_literal(&link.data.target).map(Some);
        }

        Ok(None)
    }

    /// Get actions from SHACL links for a property-level predicate (setter/adder/remover)
    async fn get_property_actions_from_shacl(
        &self,
        class_name: &str,
        property: &str,
        predicate: &str,
    ) -> Result<Option<Vec<Command>>, AnyError> {
        // Property shape URI format: {namespace}{ClassName}.{propertyName}
        let prop_suffix = format!("{}.{}", class_name, property);
        let _uuid = self.uuid.clone();

        let links = self
            .sparql_store
            .get_links_by_predicate_and_source_suffix(predicate, &prop_suffix)?;

        // Return the first match
        if let Some(link) = links.first() {
            return Self::parse_actions_from_literal(&link.data.target).map(Some);
        }

        Ok(None)
    }

    /// Get resolve language from SHACL links
    pub async fn get_resolve_language_from_shacl(
        &self,
        class_name: &str,
        property: &str,
    ) -> Result<Option<String>, AnyError> {
        let prop_suffix = format!("{}.{}", class_name, property);
        let _uuid = self.uuid.clone();

        let links = self
            .sparql_store
            .get_links_by_predicate_and_source_suffix("ad4m://resolveLanguage", &prop_suffix)?;

        if let Some(link) = links.first() {
            // Extract value from literal:string:{value} or legacy literal://string:{value}
            let encoded_value =
                if let Some(rest) = link.data.target.strip_prefix("literal://string:") {
                    rest
                } else if let Some(rest) = link.data.target.strip_prefix("literal:string:") {
                    rest
                } else {
                    return Ok(Some(link.data.target.clone()));
                };
            let decoded = urlencoding::decode(encoded_value)
                .map_err(|e| anyhow!("Failed to decode resolve language value: {}", e))?;
            return Ok(Some(decoded.to_string()));
        }

        Ok(None)
    }

    async fn get_constructor_actions(&self, class_name: &str) -> Result<Vec<Command>, AnyError> {
        self.get_shape_actions_from_shacl(class_name, "ad4m://constructor")
            .await?
            .ok_or(anyhow!(
                "No SHACL constructor found for class: {}. Ensure the class has SHACL definitions.",
                class_name
            ))
    }

    async fn get_property_setter_actions(
        &self,
        class_name: &str,
        property: &str,
    ) -> Result<Option<Vec<Command>>, AnyError> {
        self.get_property_actions_from_shacl(class_name, property, "ad4m://setter")
            .await
    }

    pub async fn resolve_property_value(
        &self,
        class_name: &str,
        property: &str,
        value: &serde_json::Value,
        context: &AgentContext,
    ) -> Result<String, AnyError> {
        // Get resolve language from SHACL links
        let resolve_language = self
            .get_resolve_language_from_shacl(class_name, property)
            .await?;

        if let Some(resolve_language) = resolve_language {
            // Create an expression for the value
            let controller = crate::languages::LanguageController::global_instance();
            let agent_context = context.clone();
            match controller
                .expression_create(&resolve_language, value.clone(), &agent_context)
                .await
            {
                Ok(url) => Ok(url),
                Err(e) => {
                    log::warn!("Failed to create expression on {}: {}", resolve_language, e);
                    Ok(value.to_string())
                }
            }
        } else {
            let uri = match value {
                serde_json::Value::String(s) => {
                    // If the value is already a valid URI (has a scheme), use it directly.
                    // Otherwise wrap it in a literal:// URI so link targets are always valid URIs.
                    static URI_SCHEME_RE: std::sync::OnceLock<regex::Regex> =
                        std::sync::OnceLock::new();
                    let re = URI_SCHEME_RE
                        .get_or_init(|| regex::Regex::new(r"^[a-zA-Z][a-zA-Z0-9+\-._]*:").unwrap());
                    if re.is_match(s) {
                        s.clone()
                    } else {
                        Literal::from_string(s.clone())
                            .to_url()
                            .map_err(|e| anyhow!("Failed to encode string as literal URI: {}", e))?
                    }
                }
                serde_json::Value::Number(n) => {
                    if let Some(f) = n.as_f64() {
                        Literal::from_number(f)
                            .to_url()
                            .map_err(|e| anyhow!("Failed to encode number as literal URI: {}", e))?
                    } else {
                        Literal::from_string(value.to_string())
                            .to_url()
                            .map_err(|e| anyhow!("Failed to encode number as literal URI: {}", e))?
                    }
                }
                _ => value.to_string(),
            };
            Ok(uri)
        }
    }

    pub async fn create_subject(
        &mut self,
        subject_class: SubjectClassOption,
        expression_address: String,
        initial_values: Option<serde_json::Value>,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<(), AnyError> {
        //let create_start = std::time::Instant::now();
        //log::info!("🎯 CREATE SUBJECT: Starting create_subject for expression '{}' - batch_id: {:?}",
        //    expression_address, batch_id);

        //let class_name_start = std::time::Instant::now();
        let class_name = self
            .subject_class_option_to_class_name(subject_class, context)
            .await?;
        //log::info!("🎯 CREATE SUBJECT: Got class name '{}' in {:?}", class_name, class_name_start.elapsed());

        //let constructor_start = std::time::Instant::now();
        let mut commands = self.get_constructor_actions(&class_name).await?;
        //log::info!("🎯 CREATE SUBJECT: Got {} constructor actions in {:?}",
        //    commands.len(), constructor_start.elapsed());

        // Handle initial values if provided
        if let Some(obj) = initial_values {
            //log::info!("🎯 CREATE SUBJECT: Processing initial values...");

            if let serde_json::Value::Object(obj) = obj {
                for (prop, value) in obj.iter() {
                    //let prop_start = std::time::Instant::now();
                    if let Some(setter_commands) =
                        self.get_property_setter_actions(&class_name, prop).await?
                    {
                        let target_value = self
                            .resolve_property_value(&class_name, prop, value, context)
                            .await?;

                        //log::info!("🎯 CREATE SUBJECT: Property '{}' setter resolved in {:?}",
                        //    prop, prop_start.elapsed());

                        // Compare predicates between setter and constructor commands
                        for setter_cmd in setter_commands.iter() {
                            let mut overwritten = false;
                            if let Some(setter_pred) = &setter_cmd.predicate {
                                for cmd in commands.iter_mut() {
                                    if let Some(pred) = &cmd.predicate {
                                        if pred == setter_pred {
                                            cmd.target = Some(target_value.clone());
                                            overwritten = true;
                                            break;
                                        }
                                    }
                                }
                            }
                            if !overwritten {
                                commands.push(Command {
                                    target: Some(target_value.clone()),
                                    ..setter_cmd.clone()
                                });
                            }
                        }
                    }
                }
            }
        }

        //let execute_start = std::time::Instant::now();
        //log::info!("🎯 CREATE SUBJECT: Executing {} commands...", commands.len());
        // Execute the merged commands
        self.execute_commands(
            commands,
            expression_address.clone(),
            vec![],
            batch_id.clone(),
            context,
        )
        .await?;

        //log::info!("🎯 CREATE SUBJECT: Commands executed in {:?}", execute_start.elapsed());
        //log::info!("🎯 CREATE SUBJECT: Total create_subject took {:?}", create_start.elapsed());

        Ok(())
    }

    pub async fn get_subject_data(
        &mut self,
        subject_class: SubjectClassOption,
        base_expression: String,
        context: &AgentContext,
    ) -> Result<String, AnyError> {
        let mut object: HashMap<String, String> = HashMap::new();

        // Get author and timestamp from the first link mentioning base as source
        let base_query = LinkQuery {
            source: Some(base_expression.clone()),
            ..Default::default()
        };
        let base_links = self.get_links(&base_query).await?;
        let first_link = base_links
            .first()
            .ok_or_else(|| anyhow!("No links found for base expression: {}", base_expression))?;
        object.insert(
            String::from("author"),
            format!("\"{}\"", first_link.author.clone()),
        );
        object.insert(
            String::from("timestamp"),
            format!("\"{}\"", first_link.timestamp.clone()),
        );

        let class_name = self
            .subject_class_option_to_class_name(subject_class, context)
            .await?;
        let result = self
            .prolog_query_with_context(
                format!(
                    "subject_class(\"{}\", C), instance(C, \"{}\").",
                    class_name, base_expression
                ),
                context,
            )
            .await?;

        if let QueryResolution::False = result {
            log::error!(
                "No instance found for class: {} with id: {}",
                class_name,
                base_expression
            );
            return Err(anyhow!(
                "No instance found for class: {} with id: {}",
                class_name,
                base_expression
            ));
        }

        let properties_result = self
            .prolog_query_with_context(
                format!(
                    r#"subject_class("{}", C), property(C, Property)."#,
                    class_name
                ),
                context,
            )
            .await?;
        let properties: Vec<String> =
            prolog_get_all_string_bindings(&properties_result, "Property");

        for p in &properties {
            let property_values_result = self
                .prolog_query_with_context(
                    format!(
                        r#"subject_class("{}", C), property_getter(C, "{}", "{}", Value)"#,
                        class_name, base_expression, p
                    ),
                    context,
                )
                .await?;
            if let Some(property_value) = prolog_get_first_binding(&property_values_result, "Value")
            {
                let result = self
                    .prolog_query_with_context(
                        format!(
                            r#"subject_class("{}", C), property_resolve(C, "{}")"#,
                            class_name, p
                        ),
                        context,
                    )
                    .await?;
                //println!("resolve query result for {}: {:?}", p, result);
                let resolve_expression_uri = QueryResolution::False != result;
                //println!("resolve_expression_uri for {}: {:?}", p, resolve_expression_uri);
                let value = if resolve_expression_uri {
                    match &property_value {
                        scryer_prolog::Term::String(s) => {
                            let controller =
                                crate::languages::LanguageController::global_instance();
                            if let Ok((lang_address, expression_address)) =
                                crate::languages::LanguageController::parse_expr_url(s)
                            {
                                match controller
                                    .get_expression(&lang_address, &expression_address)
                                    .await
                                {
                                    Ok(Some(expr_json)) => {
                                        let rendered = crate::helpers::build_expression_rendered(
                                            &expr_json,
                                            &lang_address,
                                        );
                                        rendered.data
                                    }
                                    _ => prolog_value_to_json_string(property_value.clone()),
                                }
                            } else {
                                prolog_value_to_json_string(property_value.clone())
                            }
                        }
                        _x => prolog_value_to_json_string(property_value.clone()),
                    }
                } else {
                    prolog_value_to_json_string(property_value.clone())
                };
                object.insert(p.clone(), value);
            } else {
                //log::error!("Couldn't get a property value for class: `{}`, property: `{}`, base: `{}`\nProlog query result was: {:?}", class_name, p, base_expression, property_values_result);
                object.insert(p.clone(), "null".to_string());
            };
        }

        let collections_results = self
            .prolog_query_with_context(
                format!(
                    r#"subject_class("{}", C), collection(C, Collection)"#,
                    class_name
                ),
                context,
            )
            .await?;
        let collections: Vec<String> =
            prolog_get_all_string_bindings(&collections_results, "Collection");

        for c in collections {
            let collection_values_result = self
                .prolog_query_with_context(
                    format!(
                        r#"subject_class("{}", C), collection_getter(C, "{}", "{}", Value)"#,
                        class_name, base_expression, c
                    ),
                    context,
                )
                .await?;
            if let Some(collection_value) =
                prolog_get_first_binding(&collection_values_result, "Value")
            {
                object.insert(c.clone(), prolog_value_to_json_string(collection_value));
            } else {
                //log::error!("Couldn't get a collection value for class: `{}`, collection: `{}`, base: `{}`\nProlog query result was: {:?}", class_name, c, base_expression, collection_values_result);
                object.insert(c.clone(), "[]".to_string());
            }
        }

        let stringified = object
            .into_iter()
            .map(|(k, v)| format!(r#""{}": {}"#, k, v))
            .collect::<Vec<String>>()
            .join(", ");

        Ok(format!("{{ {} }}", stringified))
    }

    async fn send_subscription_update(
        &self,
        subscription_id: String,
        result: String,
        delay: Option<Duration>,
    ) {
        let uuid = self.uuid.clone();
        tokio::spawn(async move {
            if let Some(delay) = delay {
                sleep(delay).await;
            }
            let filter = PerspectiveQuerySubscriptionFilter {
                uuid,
                subscription_id,
                result,
            };
            get_global_pubsub()
                .await
                .publish(
                    &PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC,
                    &serde_json::to_string(&filter).unwrap(),
                )
                .await;
        });
    }

    pub async fn subscribe_and_query(
        &self,
        query: String,
        user_email: Option<String>,
    ) -> Result<(String, String), AnyError> {
        // Check if we already have a subscription with the same query and user
        let existing_subscription = {
            let queries = self.subscribed_queries.lock().await;
            queries
                .iter()
                .find(|(_, q)| q.query == query && q.user_email == user_email)
                .map(|(id, _)| id.clone())
        };

        // Return existing subscription if found
        if let Some(existing_id) = existing_subscription {
            let existing_result = {
                let queries = self.subscribed_queries.lock().await;
                queries.get(&existing_id).map(|q| q.last_result.clone())
            };

            if let Some(last_result) = existing_result {
                return Ok((existing_id, last_result));
            }
        }

        let subscription_id = uuid::Uuid::new_v4().to_string();

        // Execute prolog query with user context
        let agent_context = if let Some(email) = user_email.as_ref() {
            crate::agent::AgentContext::for_user_email(email.clone())
        } else {
            crate::agent::AgentContext::main_agent()
        };
        let result_string = if is_sparql_query(&query) {
            self.sparql_query(query.clone())?
        } else {
            let initial_result = self
                .prolog_query_subscription_with_context(query.clone(), &agent_context)
                .await?;
            prolog_resolution_to_string(initial_result)
        };

        let predicates = if is_sparql_query(&query) {
            extract_predicates_from_sparql(&query)
        } else {
            HashSet::new() // Prolog queries: always re-check
        };

        let subscribed_query = SubscribedQuery {
            query,
            last_result: result_string.clone(),
            last_keepalive: Instant::now(),
            user_email,
            predicates,
            model_query_params: None,
        };

        // Now insert the subscription
        self.subscribed_queries
            .lock()
            .await
            .insert(subscription_id.clone(), subscribed_query);

        Ok((subscription_id, result_string))
    }

    /// Subscribe to model query changes. Builds trigger SPARQL from the model shape,
    /// registers a subscription, and runs the initial model query — all in one call.
    /// When link changes match the trigger predicates, `execute_model_query` is
    /// re-run in Rust and the updated results are pushed to the client.
    pub async fn model_subscribe_and_query(
        &self,
        class_name: String,
        query_json: String,
        user_email: Option<String>,
    ) -> Result<(String, String), AnyError> {
        // 1. Run the initial model query
        let initial_result = self.model_query(&class_name, &query_json)?;

        // 2. Build trigger SPARQL from shape predicates resolved through the cache.
        let trigger_predicates =
            self.build_model_trigger_predicates(&class_name, Some(&query_json));

        let trigger_sparql = if trigger_predicates.is_empty() {
            // Fallback: match any triple (always re-check)
            "SELECT ?s ?p ?o WHERE { ?s ?p ?o . } LIMIT 1".to_string()
        } else {
            let patterns: Vec<String> = trigger_predicates
                .iter()
                .enumerate()
                .map(|(i, p)| format!("{{ ?s <{}> ?o{} . }}", p, i))
                .collect();
            format!(
                "SELECT ?s ?p ?o WHERE {{ {} }} LIMIT 1",
                patterns.join(" UNION ")
            )
        };

        let predicate_set: HashSet<String> = trigger_predicates.into_iter().collect();

        // 3. Check for existing subscription with same params
        let existing_subscription = {
            let queries = self.subscribed_queries.lock().await;
            queries
                .iter()
                .find(|(_, q)| {
                    if let Some(ref params) = q.model_query_params {
                        params.class_name == class_name
                            && params.query_json == query_json
                            && q.user_email == user_email
                    } else {
                        false
                    }
                })
                .map(|(id, _)| id.clone())
        };

        if let Some(existing_id) = existing_subscription {
            // Update last_result and trigger metadata with fresh data
            {
                let mut queries = self.subscribed_queries.lock().await;
                if let Some(q) = queries.get_mut(&existing_id) {
                    q.query = trigger_sparql.clone();
                    q.predicates = predicate_set.clone();
                    q.last_result = initial_result.clone();
                    q.last_keepalive = Instant::now();
                }
            }
            return Ok((existing_id, initial_result));
        }

        // 4. Register new subscription
        let subscription_id = uuid::Uuid::new_v4().to_string();
        let subscribed_query = SubscribedQuery {
            query: trigger_sparql,
            last_result: initial_result.clone(),
            last_keepalive: Instant::now(),
            user_email,
            predicates: predicate_set,
            model_query_params: Some(ModelSubscriptionParams {
                class_name,
                query_json,
            }),
        };

        self.subscribed_queries
            .lock()
            .await
            .insert(subscription_id.clone(), subscribed_query);

        Ok((subscription_id, initial_result))
    }

    /// Extract predicates from a model shape for subscription trigger matching.
    /// Reads the shape from the cache (which warms from SHACL triples on miss)
    /// so subscription re-evaluation matches the same predicate set queries do.
    fn build_model_trigger_predicates(
        &self,
        class_name: &str,
        query_json: Option<&str>,
    ) -> Vec<String> {
        let mut predicates = Vec::new();

        if let Ok(shape) = self.get_shape(class_name) {
            predicates.extend(shape.predicates());
        }

        // Extract parent predicate from query JSON (parent-scoped subscriptions
        // must trigger when the parent link is added/removed)
        if let Some(qj) = query_json {
            if let Ok(query) = serde_json::from_str::<serde_json::Value>(qj) {
                if let Some(pred) = query
                    .get("parent")
                    .and_then(|p| p.get("predicate"))
                    .and_then(|p| p.as_str())
                {
                    predicates.push(pred.to_string());
                }
            }
        }

        predicates.sort();
        predicates.dedup();

        predicates
    }

    pub async fn keepalive_query(&self, subscription_id: String) -> Result<(), AnyError> {
        let mut queries = self.subscribed_queries.lock().await;
        if let Some(query) = queries.get_mut(&subscription_id) {
            query.last_keepalive = Instant::now();
            Ok(())
        } else {
            Err(anyhow!("Subscription not found"))
        }
    }

    pub async fn dispose_query_subscription(
        &self,
        subscription_id: String,
    ) -> Result<bool, AnyError> {
        let removed_query = {
            let mut queries = self.subscribed_queries.lock().await;
            queries.remove(&subscription_id)
        };

        if let Some(query) = removed_query {
            // Notify prolog service that subscription ended
            let uuid = self.uuid.clone();
            if let Err(e) = get_prolog_service()
                .await
                .subscription_ended(uuid, query.query)
                .await
            {
                log::warn!("Failed to notify prolog service of subscription end: {}", e);
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    async fn check_subscribed_queries(&self, changed_predicates: ChangedPredicates) {
        let mut queries_to_remove = Vec::new();
        let mut query_futures: Vec<
            std::pin::Pin<Box<dyn Future<Output = Option<(String, String)>> + Send>>,
        > = Vec::new();
        let now = Instant::now();

        // Collect only the minimal data needed: ID, query string, user_email, keepalive time, predicates,
        // and model_query_params (if this is a model subscription).
        // DON'T clone the potentially huge last_result string
        let queries = {
            let queries = self.subscribed_queries.lock().await;
            queries
                .iter()
                .map(|(id, query)| {
                    (
                        id.clone(),
                        query.query.clone(),
                        query.user_email.clone(),
                        query.last_keepalive,
                        query.predicates.clone(),
                        query.model_query_params.clone(),
                    )
                })
                .collect::<Vec<_>>()
        };

        // Create futures for each query check
        for (id, query_string, user_email, last_keepalive, sub_predicates, model_params) in queries
        {
            // Check for timeout
            if now.duration_since(last_keepalive).as_secs() > QUERY_SUBSCRIPTION_TIMEOUT {
                queries_to_remove.push(id);
                continue;
            }

            // Skip subscription if its predicates don't overlap with changed predicates
            // sub_predicates empty => always check (variable predicate in query)
            // ChangedPredicates::CheckAll => always check (couldn't determine changed predicates)
            // ChangedPredicates::NoneRecorded => should not happen here, but skip if it does
            if !sub_predicates.is_empty() {
                if let ChangedPredicates::Specific(ref changed) = changed_predicates {
                    if sub_predicates.is_disjoint(changed) {
                        log::debug!(
                            "⏭️ Skipping subscription {} — predicates {:?} disjoint from changed {:?}",
                            id, sub_predicates, changed
                        );
                        continue; // No overlap — skip this subscription
                    }
                } else if matches!(changed_predicates, ChangedPredicates::NoneRecorded) {
                    continue;
                }
            } else if matches!(changed_predicates, ChangedPredicates::NoneRecorded) {
                continue;
            }

            // Each future returns Option<(id, new_result)> instead of locking individually.
            // This avoids a lock convoy where N futures all contend on subscribed_queries.
            let self_clone = self.clone();
            let query_future = Box::pin(async move {
                let _agent_context = if let Some(email) = user_email {
                    crate::agent::AgentContext::for_user_email(email)
                } else {
                    crate::agent::AgentContext::main_agent()
                };

                // Model subscriptions: re-run execute_model_query instead of raw SPARQL
                let result_string = if let Some(ref params) = model_params {
                    match self_clone.model_query(&params.class_name, &params.query_json) {
                        Ok(r) => r,
                        Err(e) => {
                            log::error!("Model subscription query failed: {}", e);
                            return None;
                        }
                    }
                } else if is_sparql_query(&query_string) {
                    match self_clone.sparql_query(query_string) {
                        Ok(r) => r,
                        Err(e) => {
                            log::error!("SPARQL subscription query failed: {}", e);
                            return None;
                        }
                    }
                } else {
                    match self_clone
                        .prolog_query_subscription_with_context(query_string, &_agent_context)
                        .await
                    {
                        Ok(result) => prolog_resolution_to_string(result),
                        Err(e) => {
                            log::error!("Prolog subscription query failed: {}", e);
                            return None;
                        }
                    }
                };
                Some((id, result_string))
            });
            query_futures.push(query_future);
        }

        // Run all queries concurrently — no lock contention during execution
        let results = future::join_all(query_futures).await;

        // Single lock acquisition to compare and update all results
        let mut updates_to_send = Vec::new();
        {
            let mut queries = self.subscribed_queries.lock().await;
            for result in results.into_iter().flatten() {
                let (id, result_string) = result;
                if let Some(stored_query) = queries.get_mut(&id) {
                    let changed = result_string != stored_query.last_result;
                    if changed {
                        let old_len = stored_query.last_result.len();
                        let new_len = result_string.len();
                        log::debug!(
                            "📤 Subscription {} result changed (old_len={}, new_len={})",
                            id,
                            old_len,
                            new_len
                        );
                        stored_query.last_result = result_string.clone();
                        updates_to_send.push((id, result_string));
                    } else {
                        log::trace!(
                            "📭 Subscription {} result unchanged (len={})",
                            id,
                            result_string.len()
                        );
                    }
                }
            }
        }

        // Send updates outside the lock
        for (id, result_string) in updates_to_send {
            self.send_subscription_update(id, result_string, None).await;
        }

        // Remove timed out queries and notify prolog service
        if !queries_to_remove.is_empty() {
            let removed_queries = {
                let mut queries = self.subscribed_queries.lock().await;
                queries_to_remove
                    .iter()
                    .filter_map(|id| queries.remove(id).map(|q| (id.clone(), q.query)))
                    .collect::<Vec<_>>()
            };

            // Notify prolog service for each timed out subscription
            let uuid = self.uuid.clone();
            for (_id, query) in removed_queries {
                if let Err(e) = get_prolog_service()
                    .await
                    .subscription_ended(uuid.clone(), query)
                    .await
                {
                    log::warn!(
                        "Failed to notify prolog service of subscription timeout: {}",
                        e
                    );
                }
            }
        }
    }

    async fn subscribed_queries_loop(&self) {
        // Note: the subscription loop must run even when Prolog is disabled,
        // because SPARQL subscriptions don't use Prolog at all.

        let mut log_counter = 0;
        const LOG_INTERVAL: u32 = 300; // Log every ~60 seconds (300 * 200ms)
        const BATCH_WINDOW_MS: u64 = 50; // Debounce window for batching rapid link changes

        while !self.is_teardown.load(Ordering::Acquire) {
            // Check trigger without holding lock during the operation
            let should_check = self
                .trigger_prolog_subscription_check
                .load(Ordering::Acquire);

            if should_check {
                // Batch debounce: wait a short window for more changes to accumulate
                sleep(Duration::from_millis(BATCH_WINDOW_MS)).await;

                // Atomically reset trigger AFTER the sleep, so we catch any
                // triggers that arrived during the debounce window.
                self.trigger_prolog_subscription_check
                    .swap(false, Ordering::AcqRel);
                let changed_preds = std::mem::replace(
                    &mut *self.changed_predicates.lock().await,
                    ChangedPredicates::NoneRecorded,
                );

                log::debug!(
                    "🔔 Subscription check triggered for perspective {} with changed_preds: {:?}",
                    self.uuid,
                    changed_preds
                );
                self.check_subscribed_queries(changed_preds).await;
            }

            // Periodic subscription logging
            log_counter += 1;
            if log_counter >= LOG_INTERVAL {
                log_counter = 0;
                // Get perspective_uuid FIRST before acquiring subscribed_queries lock to avoid deadlock
                let perspective_uuid = self.uuid.clone();
                let queries = self.subscribed_queries.lock().await;
                if !queries.is_empty() {
                    log::info!(
                        "📊 Prolog subscriptions [{}]: {} active",
                        perspective_uuid,
                        queries.len()
                    );
                    for (id, query) in queries.iter() {
                        let query_preview = if query.query.len() > 100 {
                            format!("{}...", &query.query[..100])
                        } else {
                            query.query.clone()
                        };
                        log::info!("   - [{}]: {}", id, query_preview);
                    }
                }
            }

            sleep(Duration::from_millis(QUERY_SUBSCRIPTION_CHECK_INTERVAL)).await;
        }
    }

    async fn fallback_sync_loop(&self) {
        let uuid = self.uuid.clone();
        log::debug!("Starting fallback sync loop for perspective {}", uuid);

        while !self.is_teardown.load(Ordering::Acquire) {
            // Check if we should run the fallback sync (avoid holding multiple locks)
            let should_run = {
                // Check perspective state first
                let is_synced_neighbourhood = {
                    let handle = self.persisted.lock().await;
                    let result =
                        handle.state == PerspectiveState::Synced && handle.neighbourhood.is_some();
                    drop(handle); // Release lock immediately
                    result
                };

                if !is_synced_neighbourhood {
                    false
                } else {
                    // Check link language availability
                    let link_lang_available = {
                        let link_lang = self.link_language.read().await;
                        let result = link_lang.is_some();
                        drop(link_lang); // Release lock immediately
                        result
                    };

                    if !link_lang_available {
                        false
                    } else {
                        // Check timing conditions
                        let last_success = *self.last_successful_fallback_sync.lock().await;
                        let current_interval = *self.fallback_sync_interval.lock().await;

                        // Only run if we haven't had a successful sync recently or it's been a while
                        last_success.is_none() || last_success.unwrap().elapsed() > current_interval
                    }
                }
            };

            if should_run {
                log::debug!("Running fallback sync for perspective {}", uuid);
                let success = self.ensure_public_links_are_shared().await;

                if success {
                    // Update last successful sync time and increase interval
                    {
                        *self.last_successful_fallback_sync.lock().await =
                            Some(tokio::time::Instant::now());
                        *self.fallback_sync_interval.lock().await = Duration::from_secs(300);
                    }
                    log::debug!("Fallback sync successful for perspective {}, increasing interval to 5 minutes", uuid);
                } else {
                    // Exponential backoff capped at 5 minutes. Previously
                    // this path reset the interval to 30s, meaning an
                    // unreachable link language would be called every 30
                    // seconds forever, even after repeated timeouts.
                    let mut guard = self.fallback_sync_interval.lock().await;
                    let doubled = guard.saturating_mul(2);
                    let capped = std::cmp::min(doubled, Duration::from_secs(300));
                    *guard = std::cmp::max(capped, Duration::from_secs(30));
                    log::warn!(
                        "Fallback sync failed for perspective {}, backing off to {:?}",
                        uuid,
                        *guard
                    );
                }
            }

            // Sleep in short slices so teardown is observed promptly even
            // when the backoff interval is several minutes.
            let sleep_interval = *self.fallback_sync_interval.lock().await;
            let mut remaining = sleep_interval;
            let slice = Duration::from_secs(1);
            while remaining > Duration::from_millis(0) {
                if self.is_teardown.load(Ordering::Acquire) {
                    log::debug!("Fallback sync loop ended for perspective {}", uuid);
                    return;
                }
                let step = std::cmp::min(slice, remaining);
                sleep(step).await;
                remaining = remaining.saturating_sub(step);
            }
        }

        log::debug!("Fallback sync loop ended for perspective {}", uuid);
    }

    /// Reset the fallback sync interval to 30 seconds when new links are added
    /// This ensures that new links get synced quickly
    async fn reset_fallback_sync_interval(&self) {
        *self.fallback_sync_interval.lock().await = Duration::from_secs(30);
        let uuid = self.uuid.clone();
        log::debug!(
            "Reset fallback sync interval to 30 seconds for perspective {}",
            uuid
        );
    }

    pub async fn create_batch(&self) -> String {
        let batch_uuid = Uuid::new_v4().to_string();
        self.batch_store.write().await.insert(
            batch_uuid.clone(),
            PerspectiveDiff {
                additions: Vec::new(),
                removals: Vec::new(),
            },
        );
        batch_uuid
    }

    pub async fn commit_batch(
        &mut self,
        batch_uuid: String,
        context: &AgentContext,
    ) -> Result<DecoratedPerspectiveDiff, AnyError> {
        //let commit_start = std::time::Instant::now();
        //log::info!("🔄 BATCH COMMIT: Starting batch commit for batch_uuid: {}", batch_uuid);
        //let batch_retrieval_start = std::time::Instant::now();

        // Get the diff without holding lock during the entire operation
        let diff = {
            let mut batch_store = self.batch_store.write().await;

            match batch_store.remove(&batch_uuid) {
                Some(diff) => diff,
                None => return Err(anyhow!("No batch found with given UUID")),
            }
        };

        //log::info!("🔄 BATCH COMMIT: Retrieved batch diff in {:?} - {} additions, {} removals",
        //    batch_retrieval_start.elapsed(), diff.additions.len(), diff.removals.len());

        //let processing_start = std::time::Instant::now();
        let mut shared_diff = DecoratedPerspectiveDiff {
            additions: Vec::new(),
            removals: Vec::new(),
        };
        let mut local_diff = DecoratedPerspectiveDiff {
            additions: Vec::new(),
            removals: Vec::new(),
        };

        // Process additions
        for link in diff.additions {
            let status = link.status.unwrap_or(LinkStatus::Shared);
            let signed_expr = create_signed_expression(link.data.normalize(), context)?;
            let decorated =
                DecoratedLinkExpression::from((LinkExpression::from(signed_expr), status.clone()));

            match status {
                LinkStatus::Shared => shared_diff.additions.push(decorated),
                LinkStatus::Local => local_diff.additions.push(decorated),
            }
        }

        // Process removals
        for link in diff.removals {
            let status = link.status.clone().unwrap_or(LinkStatus::Shared);
            let decorated = DecoratedLinkExpression::from((link, status.clone()));
            match status {
                LinkStatus::Shared => shared_diff.removals.push(decorated),
                LinkStatus::Local => local_diff.removals.push(decorated),
            }
        }

        //log::info!("🔄 BATCH COMMIT: Link processing took {:?} - shared: {} add/{} rem, local: {} add/{} rem",
        //    processing_start.elapsed(),
        //    shared_diff.additions.len(), shared_diff.removals.len(),
        //    local_diff.additions.len(), local_diff.removals.len());

        // Apply shared changes
        if !shared_diff.additions.is_empty() || !shared_diff.removals.is_empty() {
            //let db_start = std::time::Instant::now();
            //log::info!("🔄 BATCH COMMIT: Starting DB operations for shared changes");

            // Commit to link language (SPARQL store will be updated later via persist_link_diff)
            if self.has_link_language().await {
                //let link_lang_start = std::time::Instant::now();
                //log::info!("🔄 BATCH COMMIT: Starting link language commit");

                let perspective_diff = PerspectiveDiff {
                    additions: shared_diff
                        .additions
                        .iter()
                        .map(|l| l.clone().into())
                        .collect(),
                    removals: shared_diff
                        .removals
                        .iter()
                        .map(|l| l.clone().into())
                        .collect(),
                };
                self.spawn_commit_and_handle_error(&perspective_diff);

                //log::info!("🔄 BATCH COMMIT: Link language commit spawned in {:?}", link_lang_start.elapsed());
            }
        }

        // Create combined diff for prolog update, SPARQL store update, and return value
        let combined_diff = DecoratedPerspectiveDiff {
            additions: [shared_diff.additions.clone(), local_diff.additions.clone()].concat(),
            removals: [shared_diff.removals.clone(), local_diff.removals.clone()].concat(),
        };

        // Only update storage / subscription engines when there are changes.
        // The SPARQL store must be updated first so any subscription re-checks
        // triggered by update_prolog_engines() see the committed batch state.
        if !combined_diff.additions.is_empty() || !combined_diff.removals.is_empty() {
            //let prolog_start = std::time::Instant::now();
            //log::info!("🔄 BATCH COMMIT: Starting DB + prolog updates - {} add, {} rem",
            //    combined_diff.additions.len(), combined_diff.removals.len());

            self.persist_link_diff(&combined_diff).await?;

            // Update Prolog: subscription engine (immediate) + query engine (lazy)
            self.update_prolog_engines(combined_diff.clone()).await;

            //log::info!("🔄 BATCH COMMIT: Prolog facts update completed in {:?}", prolog_start.elapsed());
        }

        //log::info!("🔄 BATCH COMMIT: Total batch commit took {:?}", commit_start.elapsed());

        // Return combined diff
        Ok(combined_diff)
    }
}

pub fn prolog_result(result: String) -> Value {
    let v: Value = serde_json::from_str(&result).unwrap();
    match v {
        Value::String(string) => {
            if string == "true" {
                Value::Bool(true)
            } else if string == "false" {
                Value::Bool(false)
            } else {
                Value::String(string)
            }
        }
        _ => v,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentService;
    use crate::db::Ad4mDb;
    use crate::perspectives::perspective_instance::PerspectiveHandle;
    use crate::prolog_service::init_prolog_service;
    use crate::test_utils::setup_wallet;
    use crate::types::PerspectiveState;
    use fake::{Fake, Faker};
    use uuid::Uuid;

    async fn setup() -> PerspectiveInstance {
        setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();

        // Initialize agent and prolog services for tests
        AgentService::init_global_test_instance();
        init_prolog_service().await;

        let uuid = Uuid::new_v4().to_string();

        let instance = PerspectiveInstance::new(
            PerspectiveHandle {
                uuid,
                name: Some("Test Perspective".to_string()),
                shared_url: None,
                neighbourhood: None,
                state: PerspectiveState::Private,
                owners: None,
            },
            None,
        );

        // Ensure prolog engine pool is initialized
        instance
            .ensure_prolog_engine_pool()
            .await
            .expect("Failed to initialize prolog engine pool");

        instance
    }

    pub fn create_link() -> Link {
        Link {
            source: format!("https://{}.com", Faker.fake::<String>()),
            target: format!("https://{}.org", Faker.fake::<String>()),
            predicate: Some(format!("https://{}.net", Faker.fake::<String>())),
        }
    }

    #[tokio::test]
    async fn test_context_aware_prolog_pools() {
        let perspective = setup().await;

        // Test main agent context
        let main_context = crate::agent::AgentContext::main_agent();
        let main_pool_id = perspective.get_pool_id_for_context("test-uuid", &main_context);
        assert_eq!(main_pool_id, "test-uuid");

        // Test user context
        let user_context =
            crate::agent::AgentContext::for_user_email("test@example.com".to_string());
        let user_pool_id = perspective.get_pool_id_for_context("test-uuid", &user_context);
        assert_eq!(user_pool_id, "test-uuid_test@example.com");

        // Test different users get different pools
        let user2_context =
            crate::agent::AgentContext::for_user_email("test2@example.com".to_string());
        let user2_pool_id = perspective.get_pool_id_for_context("test-uuid", &user2_context);
        assert_eq!(user2_pool_id, "test-uuid_test2@example.com");

        // Verify they're all different
        assert_ne!(main_pool_id, user_pool_id);
        assert_ne!(user_pool_id, user2_pool_id);
        assert_ne!(main_pool_id, user2_pool_id);

        println!("✅ Context-aware prolog pool selection tests passed");
    }

    #[tokio::test]
    async fn test_get_all_links_after_adding_five() {
        let mut perspective = setup().await;
        let mut all_links = Vec::new();

        for _ in 0..5 {
            let link = create_link();
            let expression = perspective
                .add_link(
                    link.clone(),
                    LinkStatus::Local,
                    None,
                    &AgentContext::main_agent(),
                )
                .await
                .unwrap();
            all_links.push(expression);
        }

        let query = LinkQuery::default();
        let mut links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 5);
        let mut all_links_sorted = all_links.clone();
        let cmp = |a: &DecoratedLinkExpression, b: &DecoratedLinkExpression| {
            let at = chrono::DateTime::parse_from_rfc3339(&a.timestamp).unwrap();
            let bt = chrono::DateTime::parse_from_rfc3339(&b.timestamp).unwrap();
            at.cmp(&bt)
                .then(a.data.source.cmp(&b.data.source))
                .then(a.data.predicate.cmp(&b.data.predicate))
                .then(a.data.target.cmp(&b.data.target))
                .then(a.author.cmp(&b.author))
        };
        links.sort_by(cmp);
        all_links_sorted.sort_by(cmp);
        assert_eq!(links, all_links_sorted);
    }

    #[tokio::test]
    async fn test_get_links_by_source() {
        let mut perspective = setup().await;
        let mut all_links = Vec::new();
        let source = "ad4m://self";

        for i in 0..5 {
            let mut link = create_link();
            if i % 2 == 0 {
                link.source = source.to_string();
            }

            let expression = perspective
                .add_link(
                    link.clone(),
                    LinkStatus::Shared,
                    None,
                    &AgentContext::main_agent(),
                )
                .await
                .unwrap();
            all_links.push(expression);
        }

        let query = LinkQuery {
            source: Some(source.to_string()),
            ..Default::default()
        };
        let mut links = perspective.get_links(&query).await.unwrap();
        let mut expected_links: Vec<_> = all_links
            .into_iter()
            .filter(|expr| expr.data.source == source)
            .collect();
        assert_eq!(links.len(), expected_links.len());
        let cmp = |a: &DecoratedLinkExpression, b: &DecoratedLinkExpression| {
            let at = chrono::DateTime::parse_from_rfc3339(&a.timestamp).unwrap();
            let bt = chrono::DateTime::parse_from_rfc3339(&b.timestamp).unwrap();
            at.cmp(&bt)
                .then(a.data.predicate.cmp(&b.data.predicate))
                .then(a.data.target.cmp(&b.data.target))
                .then(a.author.cmp(&b.author))
        };
        links.sort_by(cmp);
        expected_links.sort_by(cmp);
        assert_eq!(links, expected_links);
    }

    #[tokio::test]
    async fn test_remove_link() {
        let mut perspective = setup().await;
        let link = create_link();
        let status = LinkStatus::Local;

        // Add a link to the perspective
        let expression = perspective
            .add_link(link.clone(), status, None, &AgentContext::main_agent())
            .await
            .unwrap();

        // Ensure the link is present
        let query = LinkQuery::default();
        let links_before_removal = perspective.get_links(&query).await.unwrap();
        assert!(links_before_removal.contains(&expression));

        // Remove the link from the perspective
        perspective
            .remove_link(expression.clone().into(), None)
            .await
            .unwrap();

        // Ensure the link is no longer present
        let links_after_removal = perspective.get_links(&query).await.unwrap();
        assert!(!links_after_removal.contains(&expression));
    }

    #[tokio::test]
    async fn test_link_query_date_filtering() {
        let mut perspective = setup().await;
        let mut all_links = Vec::new();
        let now = chrono::Utc::now();

        // Add links with timestamps spread out by one minute intervals
        for i in 0..5 {
            let mut link = create_link();
            link.target = format!("lang://test-target {}", i);
            let mut link = create_signed_expression(link, &AgentContext::main_agent())
                .expect("Failed to create link");
            link.timestamp = (now - chrono::Duration::minutes(5)
                + chrono::Duration::minutes(i as i64))
            .to_rfc3339();
            let expression = perspective
                .add_link_expression(LinkExpression::from(link.clone()), LinkStatus::Shared, None)
                .await
                .unwrap();
            all_links.push(expression);
            println!("Added link with timestamp: {}, {:?}", link.timestamp, link);
        }

        // Query for links with a from_date set to 3 minutes in
        let from_date = (now - chrono::Duration::minutes(5) + chrono::Duration::minutes(3)).into();
        let query_with_from_date = LinkQuery {
            from_date: Some(from_date),
            ..Default::default()
        };
        //println!("Query with from_date: {:?}", query_with_from_date);
        let links_from_date = perspective.get_links(&query_with_from_date).await.unwrap();
        //println!("Links from date: {:?}", links_from_date);
        assert_eq!(links_from_date.len(), 2);

        // Query for links with an until_date set to 3 minutes in
        let until_date = (now - chrono::Duration::minutes(5) + chrono::Duration::minutes(3)).into();
        let query_with_until_date = LinkQuery {
            until_date: Some(until_date),
            ..Default::default()
        };
        let links_until_date = perspective.get_links(&query_with_until_date).await.unwrap();
        assert_eq!(links_until_date.len(), 4);

        // Query for links with both from_date and until_date set to filter a range
        let from_date = (now - chrono::Duration::minutes(4)).into();
        let until_date =
            (now - chrono::Duration::minutes(2) - chrono::Duration::seconds(30)).into();
        let query_with_date_range = LinkQuery {
            from_date: Some(from_date),
            until_date: Some(until_date),
            ..Default::default()
        };
        let links_date_range = perspective.get_links(&query_with_date_range).await.unwrap();
        assert_eq!(links_date_range.len(), 2);

        // reverse for descending order
        let from_date = (now).into();
        let until_date = (now - chrono::Duration::minutes(10)).into();

        let query_with_date_range = LinkQuery {
            from_date: Some(from_date),
            until_date: Some(until_date),
            ..Default::default()
        };

        let links_date_desc = perspective.get_links(&query_with_date_range).await.unwrap();
        assert_eq!(links_date_desc.len(), 5);
        assert_eq!(links_date_desc[0].data.target, all_links[4].data.target);
        assert_eq!(links_date_desc[1].data.target, all_links[3].data.target);
        assert_eq!(links_date_desc[2].data.target, all_links[2].data.target);
        assert_eq!(links_date_desc[3].data.target, all_links[1].data.target);
        assert_eq!(links_date_desc[4].data.target, all_links[0].data.target);

        // reverse for descending order with limit
        let from_date = (now).into();
        let until_date = (now - chrono::Duration::minutes(10)).into();

        let query_with_date_range = LinkQuery {
            from_date: Some(from_date),
            until_date: Some(until_date),
            limit: Some(3),
            ..Default::default()
        };

        let links_date_desc = perspective.get_links(&query_with_date_range).await.unwrap();
        assert_eq!(links_date_desc.len(), 3);
        links_date_desc
            .iter()
            .for_each(|l| println!("Link: {:?}", l.data.target));
        assert_eq!(links_date_desc[0].data.target, all_links[4].data.target);
        assert_eq!(links_date_desc[1].data.target, all_links[3].data.target);
        assert_eq!(links_date_desc[2].data.target, all_links[2].data.target);

        // ascending order with limit
        let from_date = (now - chrono::Duration::minutes(10)).into();
        let until_date = (now).into();

        let query_with_date_range = LinkQuery {
            from_date: Some(from_date),
            until_date: Some(until_date),
            limit: Some(3),
            ..Default::default()
        };

        let links_date_desc = perspective.get_links(&query_with_date_range).await.unwrap();
        assert_eq!(links_date_desc.len(), 3);
        links_date_desc
            .iter()
            .for_each(|l| println!("Link: {:?}", l.data.target));
        assert_eq!(links_date_desc[0].data.target, all_links[0].data.target);
        assert_eq!(links_date_desc[1].data.target, all_links[1].data.target);
        assert_eq!(links_date_desc[2].data.target, all_links[2].data.target);
    }

    // Additional tests for updateLink, removeLink, syncWithSharingAdapter, etc. would go here
    // following the same pattern as above.

    #[tokio::test]
    async fn test_batch_operations() {
        let mut perspective = setup().await;
        let link = create_link();
        let batch_id = perspective.create_batch().await;

        perspective
            .add_link(
                link.clone(),
                LinkStatus::Shared,
                Some(batch_id.clone()),
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();

        let query = LinkQuery::default();
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 0);

        // Commit the batch
        let diff = perspective
            .commit_batch(batch_id, &AgentContext::main_agent())
            .await
            .unwrap();
        assert_eq!(diff.additions.len(), 1);

        // Verify links are now in DB
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 1);
    }

    #[tokio::test]
    async fn test_batch_update_and_remove() {
        let mut perspective = setup().await;
        let link = create_link();
        let batch_id = perspective.create_batch().await;

        // Add initial link
        perspective
            .add_link(
                link.clone(),
                LinkStatus::Shared,
                None,
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();

        let query = LinkQuery::default();
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 1);
        assert_eq!(links[0].data.target, link.target);

        // Update link in batch
        let mut new_link = link.clone();
        new_link.target = "new_target".to_string();
        perspective
            .update_link(
                links[0].clone().into(),
                new_link.clone(),
                Some(batch_id.clone()),
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();

        // Commit the batch
        perspective
            .commit_batch(batch_id, &AgentContext::main_agent())
            .await
            .unwrap();

        // Verify final state in DB
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 1);
        assert_eq!(links[0].data.target, new_link.target);
    }

    #[tokio::test]
    async fn test_batch_multiple_operations() {
        let mut perspective = setup().await;

        // one link outside the batch, for removal
        let link0 = create_link();
        let link0_expression = perspective
            .add_link(
                link0.clone(),
                LinkStatus::Shared,
                None,
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();

        // two links in the batch
        let link1 = create_link();
        let mut link2 = link1.clone();
        link2.target = "test://target2".to_string();

        let batch_id = perspective.create_batch().await;

        // Add two links in batch
        perspective
            .add_link(
                link1.clone(),
                LinkStatus::Shared,
                Some(batch_id.clone()),
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();
        perspective
            .add_link(
                link2.clone(),
                LinkStatus::Shared,
                Some(batch_id.clone()),
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();
        perspective
            .remove_link(link0_expression.clone().into(), Some(batch_id.clone()))
            .await
            .unwrap();

        let query = LinkQuery::default();

        let links_before = perspective.get_links(&query).await.unwrap();
        assert_eq!(links_before.len(), 1);

        // Commit the batch
        let diff = perspective
            .commit_batch(batch_id, &AgentContext::main_agent())
            .await
            .unwrap();
        assert_eq!(diff.additions.len(), 2); // link1 and link2
        assert_eq!(diff.removals.len(), 1); // link1

        let links_after = perspective.get_links(&query).await.unwrap();
        assert_eq!(links_after.len(), 2);
    }

    #[tokio::test]
    async fn test_batch_error_handling() {
        let mut perspective = setup().await;

        // Try to commit non-existent batch
        let result = perspective
            .commit_batch("non-existent".to_string(), &AgentContext::main_agent())
            .await;
        assert!(result.is_err());

        // Create a batch
        let batch_id = perspective.create_batch().await;

        // Try to remove non-existent link in batch
        let non_existent_link = LinkExpression {
            author: "test".to_string(),
            timestamp: "0".to_string(),
            data: Link {
                source: "test://non-existent".to_string(),
                predicate: Some("test://predicate".to_string()),
                target: "test://target".to_string(),
            },
            proof: Default::default(),
            status: None,
        };
        let result = perspective
            .remove_link(non_existent_link.clone(), Some(batch_id.clone()))
            .await;
        assert!(result.is_err());

        // Try to use invalid batch ID
        let result = perspective
            .add_link(
                create_link(),
                LinkStatus::Shared,
                Some("invalid".to_string()),
                &AgentContext::main_agent(),
            )
            .await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_batch_with_execute_commands() {
        let mut perspective = setup().await;
        let batch_id = perspective.create_batch().await;

        // Create commands to add links
        let commands = vec![
            Command {
                source: Some("test://source1".to_string()),
                predicate: Some("test://predicate1".to_string()),
                target: Some("test://target1".to_string()),
                local: None,
                action: Action::AddLink,
            },
            Command {
                source: Some("test://source2".to_string()),
                predicate: Some("test://predicate2".to_string()),
                target: Some("test://target2".to_string()),
                local: None,
                action: Action::AddLink,
            },
        ];

        // Execute commands in batch
        perspective
            .execute_commands(
                commands,
                "test://expression".to_string(),
                vec![],
                Some(batch_id.clone()),
                &AgentContext::main_agent(),
            )
            .await
            .unwrap();

        // Verify links are not visible before commit
        let query = LinkQuery {
            source: None,
            predicate: None,
            target: None,
            from_date: None,
            until_date: None,
            limit: None,
        };
        let links = perspective.get_links(&query).await.unwrap();
        assert_eq!(links.len(), 0);

        // Commit batch and verify links are now visible
        let diff = perspective
            .commit_batch(batch_id, &AgentContext::main_agent())
            .await
            .unwrap();
        assert_eq!(diff.additions.len(), 2);
        assert_eq!(diff.removals.len(), 0);

        let links_after = perspective.get_links(&query).await.unwrap();
        assert_eq!(links_after.len(), 2);
    }

    // #[tokio::test]
    // async fn test_batch_with_create_subject() {
    //     let mut perspective = setup().await;
    //     let batch_id = perspective.create_batch().await;

    //     // Create a subject class option
    //     let subject_class = SubjectClassOption {
    //         class_name: Some("TestSubject".to_string()),
    //         query: None,
    //     };

    //     // Create subject in batch
    //     perspective.create_subject(
    //         subject_class,
    //         "test://expression1".to_string(),
    //         None,
    //         Some(batch_id.clone())
    //     ).await.unwrap();

    //     // Verify subject links are not visible before commit
    //     let query = LinkQuery {
    //         source: Some("test://expression1".to_string()),
    //         predicate: None,
    //         target: None,
    //         from_date: None,
    //         until_date: None,
    //         limit: None,
    //     };
    //     let links = perspective.get_links(&query).await.unwrap();
    //     assert_eq!(links.len(), 0);

    //     // Commit batch and verify subject links are now visible
    //     let diff = perspective.commit_batch(batch_id).await.unwrap();
    //     assert!(diff.additions.len() > 0);
    //     assert_eq!(diff.removals.len(), 0);

    //     let links_after = perspective.get_links(&query).await.unwrap();
    //     assert!(links_after.len() > 0);
    // }

    // ============================================================================
    // DOCUMENTATION EXAMPLES TESTS
    // These tests verify query examples against the SPARQL backend
    // ============================================================================

    // ============================================================================
    // PREDICATE EXTRACTION TESTS
    // ============================================================================

    #[test]
    fn test_extract_predicates_from_sparql() {
        let predicates = extract_predicates_from_sparql(
            "SELECT ?s ?t WHERE { GRAPH ?g { ?s <flux://has_message> ?t } }",
        );
        assert!(predicates.contains("flux://has_message"));
        assert_eq!(predicates.len(), 1);
    }

    #[test]
    fn test_extract_predicates_variable_predicate() {
        // ?s ?p ?o — no fixed predicate, should return empty (always match)
        let predicates = extract_predicates_from_sparql("SELECT ?s ?p ?o WHERE { ?s ?p ?o }");
        assert!(predicates.is_empty());
    }

    #[test]
    fn test_extract_predicates_multiple() {
        let predicates = extract_predicates_from_sparql(
            "SELECT ?s ?o1 ?o2 WHERE { ?s <flux://has_message> ?o1 . ?s <flux://has_reaction> ?o2 }"
        );
        assert!(predicates.contains("flux://has_message"));
        assert!(predicates.contains("flux://has_reaction"));
        assert_eq!(predicates.len(), 2);
    }

    #[test]
    fn test_extract_predicates_uri_subject() {
        let predicates = extract_predicates_from_sparql(
            "SELECT ?o WHERE { <did:key:abc> <flux://has_name> ?o }",
        );
        assert!(predicates.contains("flux://has_name"));
        assert_eq!(predicates.len(), 1);
    }

    #[test]
    fn test_predicate_filtering_skips_unrelated() {
        // Subscription with specific predicates should be skipped when
        // changed predicates don't overlap
        let sub_predicates: HashSet<String> = ["flux://has_message".to_string()].into();
        let changed: HashSet<String> = ["flux://has_reaction".to_string()].into();
        assert!(sub_predicates.is_disjoint(&changed));
    }

    #[test]
    fn test_predicate_filtering_matches_related() {
        let sub_predicates: HashSet<String> = ["flux://has_message".to_string()].into();
        let changed: HashSet<String> = [
            "flux://has_message".to_string(),
            "flux://has_reaction".to_string(),
        ]
        .into();
        assert!(!sub_predicates.is_disjoint(&changed));
    }

    #[test]
    fn test_predicate_filtering_empty_sub_always_matches() {
        // Empty sub predicates (variable predicate query) should always match
        let sub_predicates: HashSet<String> = HashSet::new();
        let _changed: HashSet<String> = ["flux://has_reaction".to_string()].into();
        // Empty set is disjoint with everything, but our code checks !sub_predicates.is_empty() first
        assert!(sub_predicates.is_empty()); // so this subscription would NOT be skipped
    }

    #[test]
    fn test_changed_predicates_enum_check_all_is_sticky() {
        let mut state = ChangedPredicates::NoneRecorded;

        // NoneRecorded + specific → Specific
        state = match state {
            ChangedPredicates::NoneRecorded => {
                ChangedPredicates::Specific(["p1".to_string()].into())
            }
            other => other,
        };
        assert!(matches!(state, ChangedPredicates::Specific(_)));

        // Specific + more specific → Specific (union)
        if let ChangedPredicates::Specific(ref mut set) = state {
            set.insert("p2".to_string());
        }
        if let ChangedPredicates::Specific(ref set) = state {
            assert!(set.contains("p1"));
            assert!(set.contains("p2"));
        }

        // Specific + predicate-less → CheckAll
        state = ChangedPredicates::CheckAll;
        assert!(matches!(state, ChangedPredicates::CheckAll));

        // CheckAll + anything → CheckAll (sticky)
        state = match state {
            ChangedPredicates::CheckAll => ChangedPredicates::CheckAll,
            other => other,
        };
        assert!(matches!(state, ChangedPredicates::CheckAll));
    }

    #[test]
    fn test_changed_predicates_none_recorded_to_check_all() {
        // NoneRecorded + no predicates → CheckAll
        let state = ChangedPredicates::NoneRecorded;
        let has_predicate_less = true;
        let result = if has_predicate_less {
            ChangedPredicates::CheckAll
        } else {
            state
        };
        assert!(matches!(result, ChangedPredicates::CheckAll));
    }

    #[test]
    fn test_extract_predicates_mixed_fixed_and_variable() {
        // A real model SPARQL query has fixed predicates in join patterns
        // (conformance checks) AND a variable predicate in the main triple
        // pattern. Because the variable predicate matches ANY predicate,
        // the subscription must always be re-checked → empty set.
        let query = r#"
            SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
                ?source <test://post_type> <test://post> .
                ?source <test://has_title> ?cfTarget_title .
                GRAPH ?linkGraph { ?source ?predicate ?target . }
                FILTER(isIRI(?source) && isIRI(?predicate))
                ?linkGraph <ad4m://ontology/author> ?author .
                ?linkGraph <ad4m://ontology/timestamp> ?timestamp .
            }
        "#;
        let predicates = extract_predicates_from_sparql(query);
        assert!(
            predicates.is_empty(),
            "Query with variable ?predicate should return empty set, got: {:?}",
            predicates
        );
    }

    #[test]
    fn test_extract_predicates_reifier_pattern_variable_predicate() {
        // RDF 1.2 reifier-based queries use `?source ?predicate ?target .`
        // outside of GRAPH patterns. The variable predicate must still be
        // detected so subscriptions always re-check.
        let query = r#"
            SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
                ?source <test://post_type> <test://post> .
                ?source ?predicate ?target .
                ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
                FILTER(isIRI(?source) && isIRI(?predicate))
                ?_reifier <ad4m://ontology/author> ?author .
                ?_reifier <ad4m://ontology/timestamp> ?timestamp .
            }
        "#;
        let predicates = extract_predicates_from_sparql(query);
        assert!(
            predicates.is_empty(),
            "Reifier query with variable ?predicate should return empty set, got: {:?}",
            predicates
        );
    }

    // -----------------------------------------------------------------------
    // Shape cache: lazy load from SHACL, invalidation on re-write,
    // and explicit error for unknown classes.
    // -----------------------------------------------------------------------

    /// Minimal SHACL JSON for a single-property class.  Builds the same
    /// graph shape `shacl-gen.ts` emits in production.
    fn cache_test_shacl(class: &str, namespace: &str) -> String {
        format!(
            r#"{{
                "target_class": "{ns}{class}",
                "properties": [
                    {{ "path": "{ns}name", "name": "name", "datatype": "xsd://string", "min_count": 1, "max_count": 1, "resolve_language": "literal" }}
                ]
            }}"#,
            ns = namespace,
            class = class
        )
    }

    #[tokio::test]
    async fn test_get_shape_returns_error_when_no_shacl_stored() {
        let perspective = setup().await;
        let result = perspective.get_shape("Unknown");
        assert!(result.is_err(), "missing class should error");
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("No SHACL shape stored for class 'Unknown'"),
            "error should name the class, got: {msg}"
        );
    }

    #[tokio::test]
    async fn test_shape_cache_returns_same_arc_on_second_call() {
        let mut perspective = setup().await;
        let shacl = cache_test_shacl("Recipe", "ns://");
        perspective
            .add_sdna(
                "Recipe".to_string(),
                String::new(),
                SdnaType::SubjectClass,
                Some(shacl),
                &AgentContext::main_agent(),
            )
            .await
            .expect("add_sdna");
        let first = perspective.get_shape("Recipe").expect("first lookup");
        let second = perspective.get_shape("Recipe").expect("second lookup");
        assert!(
            Arc::ptr_eq(&first, &second),
            "cache should return the same Arc on a hit"
        );
        assert_eq!(first.target_class, "ns://Recipe");
    }

    #[tokio::test]
    async fn test_invalidate_shape_forces_reparse() {
        let mut perspective = setup().await;
        let shacl = cache_test_shacl("Recipe", "ns://");
        perspective
            .add_sdna(
                "Recipe".to_string(),
                String::new(),
                SdnaType::SubjectClass,
                Some(shacl),
                &AgentContext::main_agent(),
            )
            .await
            .expect("add_sdna");
        let first = perspective.get_shape("Recipe").expect("first lookup");
        perspective.invalidate_shape("Recipe");
        let second = perspective.get_shape("Recipe").expect("re-parse");
        assert!(
            !Arc::ptr_eq(&first, &second),
            "invalidation should cause a fresh Arc"
        );
        assert_eq!(second.target_class, first.target_class);
    }

    #[tokio::test]
    async fn test_add_sdna_invalidates_cache_for_class() {
        let mut perspective = setup().await;
        let shacl_initial = cache_test_shacl("Recipe", "ns://");
        perspective
            .add_sdna(
                "Recipe".to_string(),
                String::new(),
                SdnaType::SubjectClass,
                Some(shacl_initial),
                &AgentContext::main_agent(),
            )
            .await
            .expect("add_sdna initial");
        let first = perspective.get_shape("Recipe").expect("first lookup");

        // add_sdna refuses to register a class twice with the same name;
        // simulate a SHACL re-write by invalidating directly (matches the
        // path register_shape would take if re-registration were exposed).
        perspective.invalidate_shape("Recipe");

        let second = perspective
            .get_shape("Recipe")
            .expect("re-parse after invalidation");
        assert!(!Arc::ptr_eq(&first, &second));
    }

    #[tokio::test]
    async fn test_where_compiled_getter_on_relation() {
        let mut perspective = setup().await;
        // Mirrors `Ad4mModel — Where-Clause Relation Filtering` integration test:
        // TaskBoard.activeTasks uses a where-compiled getter to filter by status.
        let task_shacl = r#"{
            "target_class": "task://Task",
            "properties": [
                {"path": "task://type", "name": "type", "has_value": "task://task", "min_count": 1, "max_count": 1},
                {"path": "task://title", "name": "title", "min_count": 1, "max_count": 1, "resolve_language": "literal"},
                {"path": "task://status", "name": "status", "min_count": 1, "max_count": 1, "resolve_language": "literal"}
            ]
        }"#;
        let board_shacl = r#"{
            "target_class": "board://TaskBoard",
            "properties": [
                {"path": "board://name", "name": "name", "max_count": 1, "resolve_language": "literal"},
                {
                    "path": "board://has_task",
                    "name": "activeTasks",
                    "node_kind": "IRI",
                    "relation_kind": "hasMany",
                    "target_class_name": "Task",
                    "getter": "SELECT ?target WHERE { <Base> <board://has_task> ?target . ?target <task://status> ?_wc0 . FILTER(STR(?_wc0) = \"active\" || STR(?_wc0) = \"literal:string:active\") }",
                    "where_filter": {"status": "active"},
                    "where_predicates": {"status": "task://status"}
                },
                {
                    "path": "board://has_task",
                    "name": "allTasks",
                    "node_kind": "IRI",
                    "relation_kind": "hasMany",
                    "target_class_name": "Task",
                    "getter": "SELECT ?target WHERE { <Base> <board://has_task> ?target . ?target <task://type> <task://task> . ?target <task://title> ?_v0 . ?target <task://status> ?_v1 . }"
                }
            ]
        }"#;
        perspective.add_sdna("Task".into(), "".into(), SdnaType::SubjectClass, Some(task_shacl.into()), &AgentContext::main_agent()).await.expect("add Task");
        perspective.add_sdna("TaskBoard".into(), "".into(), SdnaType::SubjectClass, Some(board_shacl.into()), &AgentContext::main_agent()).await.expect("add TaskBoard");

        let board = "literal:string:test_board";
        let active1 = "literal:string:active1";
        let active2 = "literal:string:active2";
        let done1 = "literal:string:done1";
        let signed_active = "literal:string:active";
        let signed_done = "literal:string:done";
        let signed_title = "literal:string:t";
        let triples: Vec<(&str, &str, &str)> = vec![
            // Board name
            (board, "board://name", "literal:string:Sprint1"),
            // Tasks: type flag + title + status
            (active1, "task://type", "task://task"),
            (active1, "task://title", signed_title),
            (active1, "task://status", signed_active),
            (active2, "task://type", "task://task"),
            (active2, "task://title", signed_title),
            (active2, "task://status", signed_active),
            (done1, "task://type", "task://task"),
            (done1, "task://title", signed_title),
            (done1, "task://status", signed_done),
            // Board → tasks
            (board, "board://has_task", active1),
            (board, "board://has_task", active2),
            (board, "board://has_task", done1),
        ];
        for (i, (s, p, t)) in triples.iter().enumerate() {
            let link = DecoratedLinkExpression {
                author: "did:key:test".into(),
                timestamp: format!("17000000000{:02}", i),
                data: Link {
                    source: s.to_string(),
                    predicate: Some(p.to_string()),
                    target: t.to_string(),
                },
                proof: DecoratedExpressionProof {
                    key: "k".into(),
                    signature: "s".into(),
                    valid: Some(true),
                    invalid: Some(false),
                },
                status: None,
            };
            perspective.sparql_store.add_link(&link).expect("add link");
        }

        let query_json = format!(r#"{{"where":{{"id":"{}"}},"limit":1,"deepQuery":true}}"#, board);
        let result_json = perspective
            .model_query("TaskBoard", &query_json)
            .expect("model_query");
        let result: serde_json::Value =
            serde_json::from_str(&result_json).expect("parse result");
        let instances = result["instances"].as_array().expect("instances array");
        assert_eq!(instances.len(), 1, "should find the board");
        let active = instances[0]["activeTasks"].as_array().expect("activeTasks array");
        assert_eq!(active.len(), 2, "where-getter should narrow to 2 active tasks");
        let all = instances[0]["allTasks"].as_array().expect("allTasks array");
        assert_eq!(all.len(), 3, "conformance getter should keep all 3 tasks");
    }

    #[tokio::test]
    async fn test_model_query_fires_property_getter() {
        let mut perspective = setup().await;
        // Exact JSON produced by SHACLShape.toJSON() for the BlogPost
        // fixture in tests/js/tests/prolog-and-literals.test.ts.
        let shacl = r#"{
            "node_shape_uri": "blog://BlogPostShape",
            "target_class": "blog://BlogPost",
            "properties": [
                {
                    "path": "blog://title",
                    "name": "title",
                    "datatype": "xsd://string",
                    "max_count": 1,
                    "writable": true,
                    "resolve_language": "literal",
                    "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "blog://title", "target": "value"}]
                },
                {
                    "path": "blog://parent",
                    "name": "parentPost",
                    "max_count": 1,
                    "writable": true,
                    "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "blog://parent", "target": "value"}],
                    "getter": "SELECT ?target WHERE { <Base> <blog://reply_to> ?target . } LIMIT 1"
                },
                {
                    "path": "ad4m://getter/BlogPost/tags",
                    "name": "tags",
                    "node_kind": "IRI",
                    "getter": "SELECT ?target WHERE { <Base> <blog://tagged_with> ?target . }",
                    "relation_kind": "hasMany"
                }
            ],
            "constructor_actions": [],
            "destructor_actions": []
        }"#;
        perspective
            .add_sdna(
                "BlogPost".to_string(),
                String::new(),
                SdnaType::SubjectClass,
                Some(shacl.to_string()),
                &AgentContext::main_agent(),
            )
            .await
            .expect("add_sdna");

        let post_root = "literal:string:test_post_root";
        let parent_root = "literal:string:test_parent_root";

        // Title link makes the post a BlogPost instance (structural conformance)
        for (src, pred, tgt) in &[
            (post_root, "blog://title", "literal:string:my_post"),
            (parent_root, "blog://title", "literal:string:my_parent"),
            (post_root, "blog://reply_to", parent_root),
        ] {
            let link = DecoratedLinkExpression {
                author: "did:key:test".into(),
                timestamp: "1700000000000".into(),
                data: Link {
                    source: src.to_string(),
                    predicate: Some(pred.to_string()),
                    target: tgt.to_string(),
                },
                proof: DecoratedExpressionProof {
                    key: "k".into(),
                    signature: "s".into(),
                    valid: Some(true),
                    invalid: Some(false),
                },
                status: None,
            };
            perspective.sparql_store.add_link(&link).expect("add_link");
        }

        let query_json = format!(
            r#"{{"where":{{"id":"{}"}},"limit":1,"deepQuery":true}}"#,
            post_root
        );
        let result_json = perspective
            .model_query("BlogPost", &query_json)
            .expect("model_query");
        let result: serde_json::Value =
            serde_json::from_str(&result_json).expect("parse result");
        let instances = result["instances"].as_array().expect("instances array");
        assert_eq!(instances.len(), 1, "should find the post");
        let parent_value = instances[0]["parentPost"].as_str();
        assert_eq!(
            parent_value,
            Some(parent_root),
            "getter should populate parentPost"
        );
    }

    #[tokio::test]
    async fn test_property_getter_survives_add_sdna_pipeline() {
        let mut perspective = setup().await;
        let shacl = r#"{
            "target_class": "blog://BlogPost",
            "properties": [
                {
                    "path": "blog://title",
                    "name": "title",
                    "datatype": "xsd://string",
                    "max_count": 1,
                    "resolve_language": "literal"
                },
                {
                    "path": "blog://parent",
                    "name": "parentPost",
                    "max_count": 1,
                    "getter": "SELECT ?target WHERE { <Base> <blog://reply_to> ?target . } LIMIT 1"
                }
            ]
        }"#;
        perspective
            .add_sdna(
                "BlogPost".to_string(),
                String::new(),
                SdnaType::SubjectClass,
                Some(shacl.to_string()),
                &AgentContext::main_agent(),
            )
            .await
            .expect("add_sdna");

        let shape = perspective.get_shape("BlogPost").expect("shape");
        let parent = shape
            .properties
            .iter()
            .find(|p| p.name == "parentPost")
            .expect("parentPost property");
        assert_eq!(
            parent.getter.as_deref(),
            Some("SELECT ?target WHERE { <Base> <blog://reply_to> ?target . } LIMIT 1"),
            "getter must survive addSdna → SHACL store → load_shape round trip"
        );
    }

    #[tokio::test]
    async fn test_model_query_resolves_shape_through_cache() {
        let mut perspective = setup().await;
        let shacl = cache_test_shacl("Recipe", "ns://");
        perspective
            .add_sdna(
                "Recipe".to_string(),
                String::new(),
                SdnaType::SubjectClass,
                Some(shacl),
                &AgentContext::main_agent(),
            )
            .await
            .expect("add_sdna");

        // First query warms the cache (load_shape fires once).
        let result_json = perspective
            .model_query("Recipe", "{}")
            .expect("first query");
        assert!(result_json.contains("\"instances\""));

        // Subsequent queries hit the cache and resolve to the same Arc as
        // get_shape would have returned.
        let warm = perspective.get_shape("Recipe").expect("warm shape");
        let warm_again = perspective.get_shape("Recipe").expect("warm shape again");
        assert!(Arc::ptr_eq(&warm, &warm_again));
    }
}
