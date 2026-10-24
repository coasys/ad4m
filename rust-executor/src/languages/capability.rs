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

use super::error::LanguageError;
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
    // Identity (AR-01 4.9; offer and claim calls of AR-02 4.16). Agent language only.
    IdentityLog,
    IdentityHead,
    IdentityAppend,
    IdentityKeyOwner,
    IdentityPowParams,
    IdentityAttachNote,
    IdentityPutAgent,
    IdentityOpenOffer,
    IdentityListOffers,
    IdentityCancelOffer,
    IdentityClaimOffer,
    IdentityOfferState,
    // Token (AR-01 4.9.1, interfaces only). Agent language only.
    TokenAssets,
    TokenBalance,
    TokenSend,
    TokenReceived,
}

impl Capability {
    /// Every capability, in declaration order.
    pub const ALL: [Capability; 31] = [
        Capability::ExpressionCreate,
        Capability::ExpressionGet,
        Capability::PerspectiveCommit,
        Capability::PerspectiveSync,
        Capability::PerspectiveRender,
        Capability::PerspectiveCurrentRevision,
        Capability::PerspectiveQuery,
        Capability::PeersLocal,
        Capability::PeersRemote,
        Capability::TelepresenceSetStatus,
        Capability::TelepresenceGetAgents,
        Capability::TelepresenceSendSignal,
        Capability::TelepresenceSendBroadcast,
        Capability::LanguageGetSource,
        Capability::HolochainSignal,
        Capability::IdentityLog,
        Capability::IdentityHead,
        Capability::IdentityAppend,
        Capability::IdentityKeyOwner,
        Capability::IdentityPowParams,
        Capability::IdentityAttachNote,
        Capability::IdentityPutAgent,
        Capability::IdentityOpenOffer,
        Capability::IdentityListOffers,
        Capability::IdentityCancelOffer,
        Capability::IdentityClaimOffer,
        Capability::IdentityOfferState,
        Capability::TokenAssets,
        Capability::TokenBalance,
        Capability::TokenSend,
        Capability::TokenReceived,
    ];

    /// The kebab-case wire name used by the JS detection script.
    pub fn wire_name(self) -> &'static str {
        match self {
            Capability::ExpressionCreate => "expression-create",
            Capability::ExpressionGet => "expression-get",
            Capability::PerspectiveCommit => "perspective-commit",
            Capability::PerspectiveSync => "perspective-sync",
            Capability::PerspectiveRender => "perspective-render",
            Capability::PerspectiveCurrentRevision => "perspective-current-revision",
            Capability::PerspectiveQuery => "perspective-query",
            Capability::PeersLocal => "peers-local",
            Capability::PeersRemote => "peers-remote",
            Capability::TelepresenceSetStatus => "telepresence-set-status",
            Capability::TelepresenceGetAgents => "telepresence-get-agents",
            Capability::TelepresenceSendSignal => "telepresence-send-signal",
            Capability::TelepresenceSendBroadcast => "telepresence-send-broadcast",
            Capability::LanguageGetSource => "language-get-source",
            Capability::HolochainSignal => "holochain-signal",
            Capability::IdentityLog => "identity-log",
            Capability::IdentityHead => "identity-head",
            Capability::IdentityAppend => "identity-append",
            Capability::IdentityKeyOwner => "identity-key-owner",
            Capability::IdentityPowParams => "identity-pow-params",
            Capability::IdentityAttachNote => "identity-attach-note",
            Capability::IdentityPutAgent => "identity-put-agent",
            Capability::IdentityOpenOffer => "identity-open-offer",
            Capability::IdentityListOffers => "identity-list-offers",
            Capability::IdentityCancelOffer => "identity-cancel-offer",
            Capability::IdentityClaimOffer => "identity-claim-offer",
            Capability::IdentityOfferState => "identity-offer-state",
            Capability::TokenAssets => "token-assets",
            Capability::TokenBalance => "token-balance",
            Capability::TokenSend => "token-send",
            Capability::TokenReceived => "token-received",
        }
    }

    /// The flat export a language provides for this capability. Detection
    /// reports the capability when this export is a function (for
    /// `ExpressionCreate`, also `expressionAddressOf` / `addressOf`).
    pub fn export_name(self) -> &'static str {
        match self {
            Capability::ExpressionCreate => "expressionCreate",
            Capability::ExpressionGet => "expressionGet",
            Capability::PerspectiveCommit => "perspectiveCommit",
            Capability::PerspectiveSync => "perspectiveSyncSync",
            Capability::PerspectiveRender => "perspectiveSyncRender",
            Capability::PerspectiveCurrentRevision => "perspectiveSyncCurrentRevision",
            Capability::PerspectiveQuery => "perspectiveQueryRun",
            Capability::PeersLocal => "peersSetLocal",
            Capability::PeersRemote => "peersRemote",
            Capability::TelepresenceSetStatus => "telepresenceSetOnlineStatus",
            Capability::TelepresenceGetAgents => "telepresenceGetOnlineAgents",
            Capability::TelepresenceSendSignal => "telepresenceSendSignal",
            Capability::TelepresenceSendBroadcast => "telepresenceSendBroadcast",
            Capability::LanguageGetSource => "languageGetSource",
            Capability::HolochainSignal => "handleHolochainSignal",
            Capability::IdentityLog => "identityLog",
            Capability::IdentityHead => "identityHead",
            Capability::IdentityAppend => "identityAppend",
            Capability::IdentityKeyOwner => "identityKeyOwner",
            Capability::IdentityPowParams => "identityPowParams",
            Capability::IdentityAttachNote => "identityAttachNote",
            Capability::IdentityPutAgent => "identityPutAgent",
            Capability::IdentityOpenOffer => "identityOpenOffer",
            Capability::IdentityListOffers => "identityListOffers",
            Capability::IdentityCancelOffer => "identityCancelOffer",
            Capability::IdentityClaimOffer => "identityClaimOffer",
            Capability::IdentityOfferState => "identityOfferState",
            Capability::TokenAssets => "tokenAssets",
            Capability::TokenBalance => "tokenBalance",
            Capability::TokenSend => "tokenSend",
            Capability::TokenReceived => "tokenReceived",
        }
    }

    /// Identity and token capabilities run only on the language at
    /// `system_addresses.agent_language` (AR-01 4.9, 4.9.1).
    pub fn agent_language_only(self) -> bool {
        matches!(
            self,
            Capability::IdentityLog
                | Capability::IdentityHead
                | Capability::IdentityAppend
                | Capability::IdentityKeyOwner
                | Capability::IdentityPowParams
                | Capability::IdentityAttachNote
                | Capability::IdentityPutAgent
                | Capability::IdentityOpenOffer
                | Capability::IdentityListOffers
                | Capability::IdentityCancelOffer
                | Capability::IdentityClaimOffer
                | Capability::IdentityOfferState
                | Capability::TokenAssets
                | Capability::TokenBalance
                | Capability::TokenSend
                | Capability::TokenReceived
        )
    }

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
            "identity-log" => Some(Capability::IdentityLog),
            "identity-head" => Some(Capability::IdentityHead),
            "identity-append" => Some(Capability::IdentityAppend),
            "identity-key-owner" => Some(Capability::IdentityKeyOwner),
            "identity-pow-params" => Some(Capability::IdentityPowParams),
            "identity-attach-note" => Some(Capability::IdentityAttachNote),
            "identity-put-agent" => Some(Capability::IdentityPutAgent),
            "identity-open-offer" => Some(Capability::IdentityOpenOffer),
            "identity-list-offers" => Some(Capability::IdentityListOffers),
            "identity-cancel-offer" => Some(Capability::IdentityCancelOffer),
            "identity-claim-offer" => Some(Capability::IdentityClaimOffer),
            "identity-offer-state" => Some(Capability::IdentityOfferState),
            "token-assets" => Some(Capability::TokenAssets),
            "token-balance" => Some(Capability::TokenBalance),
            "token-send" => Some(Capability::TokenSend),
            "token-received" => Some(Capability::TokenReceived),
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

/// The JS that reports which capabilities the loaded language exports: a
/// JSON array of wire names, one per flat export that is a function.
/// `ExpressionCreate` also counts `expressionAddressOf` and the legacy
/// `addressOf`.
pub fn detection_script() -> String {
    let checks: Vec<String> = Capability::ALL
        .iter()
        .map(|cap| {
            let test = if *cap == Capability::ExpressionCreate {
                r#"typeof language.expressionCreate === "function" || typeof language.expressionAddressOf === "function" || typeof language.addressOf === "function""#
                    .to_string()
            } else {
                format!(r#"typeof language.{} === "function""#, cap.export_name())
            };
            format!(r#"    if ({test}) caps.push("{}");"#, cap.wire_name())
        })
        .collect();
    format!(
        "(function() {{\n    const language = globalThis.__ad4m_language_instance__;\n    const caps = [];\n    if (!language) return JSON.stringify(caps);\n{}\n    return JSON.stringify(caps);\n}})()",
        checks.join("\n")
    )
}

/// The guard for identity and token capabilities: they run only on the
/// language at `system_addresses.agent_language` (AR-01 4.9, 4.9.1). Any
/// other capability passes for any language.
pub fn check_capability_target(
    cap: Capability,
    address: &str,
    agent_language: Option<&str>,
) -> Result<(), LanguageError> {
    if !cap.agent_language_only() || agent_language == Some(address) {
        return Ok(());
    }
    Err(LanguageError::NotAgentLanguage {
        address: address.to_string(),
        capability: cap.wire_name(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    const AGENT: &str = "QmAgentLanguage";

    #[test]
    fn every_capability_round_trips_through_its_wire_name() {
        let mut names = HashSet::new();
        for cap in Capability::ALL {
            assert_eq!(Capability::from_wire(cap.wire_name()), Some(cap));
            assert!(
                names.insert(cap.wire_name()),
                "duplicate {}",
                cap.wire_name()
            );
        }
        assert_eq!(Capability::from_wire("identity"), None);
        assert_eq!(Capability::from_wire("identityLog"), None);
    }

    #[test]
    fn identity_and_token_wire_and_export_names() {
        let expected = [
            (Capability::IdentityLog, "identity-log", "identityLog"),
            (Capability::IdentityHead, "identity-head", "identityHead"),
            (
                Capability::IdentityAppend,
                "identity-append",
                "identityAppend",
            ),
            (
                Capability::IdentityKeyOwner,
                "identity-key-owner",
                "identityKeyOwner",
            ),
            (
                Capability::IdentityPowParams,
                "identity-pow-params",
                "identityPowParams",
            ),
            (
                Capability::IdentityAttachNote,
                "identity-attach-note",
                "identityAttachNote",
            ),
            (
                Capability::IdentityPutAgent,
                "identity-put-agent",
                "identityPutAgent",
            ),
            (
                Capability::IdentityOpenOffer,
                "identity-open-offer",
                "identityOpenOffer",
            ),
            (
                Capability::IdentityListOffers,
                "identity-list-offers",
                "identityListOffers",
            ),
            (
                Capability::IdentityCancelOffer,
                "identity-cancel-offer",
                "identityCancelOffer",
            ),
            (
                Capability::IdentityClaimOffer,
                "identity-claim-offer",
                "identityClaimOffer",
            ),
            (
                Capability::IdentityOfferState,
                "identity-offer-state",
                "identityOfferState",
            ),
            (Capability::TokenAssets, "token-assets", "tokenAssets"),
            (Capability::TokenBalance, "token-balance", "tokenBalance"),
            (Capability::TokenSend, "token-send", "tokenSend"),
            (Capability::TokenReceived, "token-received", "tokenReceived"),
        ];
        for (cap, wire, export) in expected {
            assert_eq!(cap.wire_name(), wire);
            assert_eq!(cap.export_name(), export);
            assert!(cap.agent_language_only(), "{wire}");
        }
        let agent_only = Capability::ALL
            .iter()
            .filter(|c| c.agent_language_only())
            .count();
        assert_eq!(agent_only, expected.len());
    }

    #[test]
    fn parse_reads_identity_and_token_names() {
        let caps = parse_capability_list(
            r#"["expression-get","identity-log","identity-claim-offer","token-send","unknown"]"#,
        );
        assert_eq!(
            caps,
            HashSet::from([
                Capability::ExpressionGet,
                Capability::IdentityLog,
                Capability::IdentityClaimOffer,
                Capability::TokenSend,
            ])
        );
        assert!(parse_capability_list("not json").is_empty());
    }

    #[test]
    fn detection_script_checks_every_export() {
        let script = detection_script();
        for cap in Capability::ALL {
            assert!(
                script.contains(&format!(
                    "typeof language.{} === \"function\"",
                    cap.export_name()
                )),
                "{}",
                cap.export_name()
            );
            assert!(script.contains(&format!("caps.push(\"{}\")", cap.wire_name())));
        }
        assert!(script.contains("typeof language.expressionAddressOf === \"function\""));
        assert!(script.contains("typeof language.addressOf === \"function\""));
        assert!(script.is_ascii(), "runs in the Deno runtime");
    }

    #[test]
    fn identity_and_token_capabilities_only_target_the_agent_language() {
        for cap in Capability::ALL
            .into_iter()
            .filter(|c| c.agent_language_only())
        {
            assert!(check_capability_target(cap, AGENT, Some(AGENT)).is_ok());
            match check_capability_target(cap, "QmOtherLanguage", Some(AGENT)) {
                Err(LanguageError::NotAgentLanguage {
                    address,
                    capability,
                }) => {
                    assert_eq!(address, "QmOtherLanguage");
                    assert_eq!(capability, cap.wire_name());
                }
                other => panic!("{} on another language: {:?}", cap.wire_name(), other.err()),
            }
            // No agent language yet: nothing qualifies.
            assert!(check_capability_target(cap, AGENT, None).is_err());
        }
    }

    #[test]
    fn other_capabilities_target_any_language() {
        for cap in Capability::ALL
            .into_iter()
            .filter(|c| !c.agent_language_only())
        {
            assert!(check_capability_target(cap, "QmOtherLanguage", Some(AGENT)).is_ok());
            assert!(check_capability_target(cap, "QmOtherLanguage", None).is_ok());
        }
    }
}
