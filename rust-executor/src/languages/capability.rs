// Language capability registry.
//
// The executor walks a language's exported surface once at load time
// (`LanguageRuntime::register_callbacks`) and stores the detected set of
// method-level capabilities here. Per-call `typeof x === "function"` guards
// in the JS dispatcher scripts are replaced with cheap Rust-side lookups;
// callers can also consult `Language::has(Capability::…)` before spawning
// background loops that would otherwise run forever against a language that
// cannot satisfy them.
//
// The capability split mirrors the authoritative list in
// `ad4m-ldk/rust/src/traits.rs` — one variant per method, so detection
// stays at the same granularity as the underlying `typeof` checks.

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Capability {
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

impl Capability {
    /// The kebab-case wire name used by the JS detection script.
    pub fn from_wire(name: &str) -> Option<Capability> {
        match name {
            "expression-create" => Some(Capability::ExpressionCreate),
            "expression-get" => Some(Capability::ExpressionGet),
            "perspective-commit" => Some(Capability::PerspectiveCommit),
            "perspective-sync" => Some(Capability::PerspectiveSync),
            "perspective-render" => Some(Capability::PerspectiveRender),
            "perspective-current-revision" => Some(Capability::PerspectiveCurrentRevision),
            "perspective-query" => Some(Capability::PerspectiveQuery),
            "peers-local" => Some(Capability::PeersLocal),
            "peers-remote" => Some(Capability::PeersRemote),
            "telepresence-set-status" => Some(Capability::TelepresenceSetStatus),
            "telepresence-get-agents" => Some(Capability::TelepresenceGetAgents),
            "telepresence-send-signal" => Some(Capability::TelepresenceSendSignal),
            "telepresence-send-broadcast" => Some(Capability::TelepresenceSendBroadcast),
            "language-get-source" => Some(Capability::LanguageGetSource),
            "holochain-signal" => Some(Capability::HolochainSignal),
            _ => None,
        }
    }
}

lazy_static! {
    static ref LANGUAGE_CAPABILITIES: RwLock<HashMap<String, Arc<HashSet<Capability>>>> =
        RwLock::new(HashMap::new());
}

pub fn register_capabilities(address: &str, caps: HashSet<Capability>) {
    let mut map = LANGUAGE_CAPABILITIES.write().unwrap();
    map.insert(address.to_string(), Arc::new(caps));
}

pub fn get_capabilities(address: &str) -> Arc<HashSet<Capability>> {
    let map = LANGUAGE_CAPABILITIES.read().unwrap();
    match map.get(address) {
        Some(caps) => caps.clone(),
        None => Arc::new(HashSet::new()),
    }
}

pub fn remove_capabilities(address: &str) {
    let mut map = LANGUAGE_CAPABILITIES.write().unwrap();
    map.remove(address);
}

pub fn parse_capability_list(json: &str) -> HashSet<Capability> {
    let mut out = HashSet::new();
    if let Ok(names) = serde_json::from_str::<Vec<String>>(json) {
        for name in names {
            if let Some(cap) = Capability::from_wire(&name) {
                out.insert(cap);
            }
        }
    }
    out
}
