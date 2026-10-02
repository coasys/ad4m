// Language feature registry. (Named `LanguageFeature`, not `Capability`:
// "capability" means an auth grant elsewhere in the executor.)
//
// The executor walks a language's exported surface once at load time
// (`LanguageRuntime::register_callbacks`) and stores the detected set of
// method-level features here. Per-call `typeof x === "function"` guards
// in the JS dispatcher scripts are replaced with cheap Rust-side lookups;
// callers can also consult `Language::has(LanguageFeature::…)` before spawning
// background loops that would otherwise run forever against a language that
// cannot satisfy them.
//
// The feature split mirrors the authoritative list in
// `ad4m-ldk/rust/src/traits.rs` — one variant per method, so detection
// stays at the same granularity as the underlying `typeof` checks.

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum LanguageFeature {
    ExpressionCreate,
    ExpressionGet,
    PerspectiveCommit,
    PerspectiveSync,
    PerspectiveRender,
    PerspectiveCurrentRevision,
    PerspectiveQuery,
    PeersLocal,
    PeersRemote,
    TelepresenceSetStatus,
    TelepresenceGetAgents,
    TelepresenceSendSignal,
    TelepresenceSendBroadcast,
    LanguageGetSource,
    HolochainSignal,
}

impl LanguageFeature {
    /// The kebab-case wire name used by the JS detection script.
    pub fn from_wire(name: &str) -> Option<LanguageFeature> {
        match name {
            "expression-create" => Some(LanguageFeature::ExpressionCreate),
            "expression-get" => Some(LanguageFeature::ExpressionGet),
            "perspective-commit" => Some(LanguageFeature::PerspectiveCommit),
            "perspective-sync" => Some(LanguageFeature::PerspectiveSync),
            "perspective-render" => Some(LanguageFeature::PerspectiveRender),
            "perspective-current-revision" => Some(LanguageFeature::PerspectiveCurrentRevision),
            "perspective-query" => Some(LanguageFeature::PerspectiveQuery),
            "peers-local" => Some(LanguageFeature::PeersLocal),
            "peers-remote" => Some(LanguageFeature::PeersRemote),
            "telepresence-set-status" => Some(LanguageFeature::TelepresenceSetStatus),
            "telepresence-get-agents" => Some(LanguageFeature::TelepresenceGetAgents),
            "telepresence-send-signal" => Some(LanguageFeature::TelepresenceSendSignal),
            "telepresence-send-broadcast" => Some(LanguageFeature::TelepresenceSendBroadcast),
            "language-get-source" => Some(LanguageFeature::LanguageGetSource),
            "holochain-signal" => Some(LanguageFeature::HolochainSignal),
            _ => None,
        }
    }
}

lazy_static! {
    static ref LANGUAGE_FEATURES: RwLock<HashMap<String, Arc<HashSet<LanguageFeature>>>> =
        RwLock::new(HashMap::new());
}

pub fn register_features(address: &str, caps: HashSet<LanguageFeature>) {
    let mut map = LANGUAGE_FEATURES.write().unwrap();
    map.insert(address.to_string(), Arc::new(caps));
}

pub fn get_features(address: &str) -> Arc<HashSet<LanguageFeature>> {
    let map = LANGUAGE_FEATURES.read().unwrap();
    match map.get(address) {
        Some(caps) => caps.clone(),
        None => Arc::new(HashSet::new()),
    }
}

pub fn remove_features(address: &str) {
    let mut map = LANGUAGE_FEATURES.write().unwrap();
    map.remove(address);
}

pub fn parse_feature_list(json: &str) -> HashSet<LanguageFeature> {
    let mut out = HashSet::new();
    if let Ok(names) = serde_json::from_str::<Vec<String>>(json) {
        for name in names {
            if let Some(cap) = LanguageFeature::from_wire(&name) {
                out.insert(cap);
            }
        }
    }
    out
}
