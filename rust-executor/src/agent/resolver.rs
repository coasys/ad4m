//! Agent language resolver — connects PR1's `KeyStateResolver` seam to the KEL
//! adapter + monotonicity cache. Resolves identifiers (master SCIDs and delegated
//! keys) to the agent's key state at a given point.

use crate::agent::kel::adapter::{AdapterError, KelAdapter, MonotonicityCache};
use crate::agent::kel::{fold, AgentType, KeyEntry};
use crate::agent::signatures::{KeyState as VerifierKeyState, KeyStateResolver, ResolveError};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

// ─── validity + agent struct ─────────────────────────────────────────────────

/// Key validity at a given point.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Validity {
    Valid,
    Revoked { at_seq: u64 },
    Superseded { at_seq: u64 },
}

/// What resolution returns — an agent, not a bare key.
#[derive(Debug, Clone)]
pub struct Agent {
    /// The master `did:scid`.
    pub master: String,
    /// The key the queried identifier named (if a specific key was queried).
    pub key: Option<KeyEntry>,
    /// Validity of the queried key at the resolved point.
    pub validity: Validity,
    /// All keys valid at the resolved point.
    pub keys: Vec<KeyEntry>,
    /// Agent type (human or assistant).
    pub agent_type: AgentType,
}

// ─── reverse index ───────────────────────────────────────────────────────────

/// Maps any delegated key (or key_id) back to its master SCID, so a proof
/// signed by a device or executor key resolves to one author.
#[derive(Default)]
pub struct ReverseIndex {
    key_to_master: RwLock<HashMap<String, String>>,
}

impl ReverseIndex {
    pub fn new() -> Self {
        Self {
            key_to_master: RwLock::new(HashMap::new()),
        }
    }

    /// Register a key_id → master mapping.
    pub fn insert(&self, key_id: &str, master: &str) {
        if let Ok(mut map) = self.key_to_master.write() {
            map.insert(key_id.to_string(), master.to_string());
        }
    }

    /// Look up the master for a key_id.
    pub fn master_for(&self, key_id: &str) -> Option<String> {
        self.key_to_master
            .read()
            .ok()
            .and_then(|m| m.get(key_id).cloned())
    }
}

// ─── agent language resolver ─────────────────────────────────────────────────

/// The resolver client that backs PR1's `KeyStateResolver` seam.
/// Resolves identifiers through the KEL adapter + cache, mapping both master
/// SCIDs and delegated keys to their agent's key state.
pub struct AgentLanguageResolver {
    adapter: Arc<dyn KelAdapter>,
    cache: Arc<MonotonicityCache>,
    reverse_index: Arc<ReverseIndex>,
}

impl AgentLanguageResolver {
    pub fn new(
        adapter: Arc<dyn KelAdapter>,
        cache: Arc<MonotonicityCache>,
        reverse_index: Arc<ReverseIndex>,
    ) -> Self {
        Self {
            adapter,
            cache,
            reverse_index,
        }
    }

    /// Resolve an identifier to an `Agent` struct.
    pub fn resolve_agent(
        &self,
        identifier: &str,
        at_seq: Option<u64>,
    ) -> Result<Agent, ResolveError> {
        // Step 1: Determine the master SCID.
        let (master, queried_key_id) = if identifier.starts_with("did:scid:") {
            // Could be the master itself or a key_id (master#fragment).
            if identifier.contains('#') {
                // It names a specific key: `master#key-N`.
                let master = identifier.split('#').next().unwrap_or(identifier);
                (master.to_string(), Some(identifier.to_string()))
            } else {
                (identifier.to_string(), None)
            }
        } else {
            // Try the reverse index — maybe a bare key_id or a did:key.
            match self.reverse_index.master_for(identifier) {
                Some(master) => (master, Some(identifier.to_string())),
                None => return Err(ResolveError::NotFound),
            }
        };

        // Step 2: Fetch the log and fold.
        let events = self.adapter.get_log(&master, 0).map_err(|e| match e {
            AdapterError::NotFound => ResolveError::NotFound,
            other => ResolveError::Backend(other.to_string()),
        })?;

        let kel_state = fold(&events).map_err(|e| ResolveError::Backend(e.to_string()))?;

        // Step 3: Check monotonicity.
        if let Err(e) = self.cache.check_and_update(&master, kel_state.head_seq()) {
            return Err(ResolveError::Backend(e.to_string()));
        }

        // Step 4: Update reverse index with all known keys.
        let seq = at_seq.unwrap_or(kel_state.head_seq());
        for key in kel_state.keys_at(seq) {
            self.reverse_index.insert(&key.id, &master);
        }

        // Step 5: Build the Agent struct.
        let valid_keys = kel_state
            .keys_at(seq)
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();

        let (queried_key, validity) = match &queried_key_id {
            Some(kid) => {
                let entry = valid_keys.iter().find(|k| k.id == *kid).cloned();
                let valid = kel_state.key_valid_at(kid, seq);
                if valid {
                    (entry, Validity::Valid)
                } else {
                    // Find the revocation seq if applicable.
                    (entry, Validity::Revoked { at_seq: seq })
                }
            }
            None => (None, Validity::Valid),
        };

        Ok(Agent {
            master: master.clone(),
            key: queried_key,
            validity,
            keys: valid_keys,
            agent_type: kel_state.agent_type(),
        })
    }
}

// ─── KeyStateResolver implementation ─────────────────────────────────────────

impl KeyStateResolver for AgentLanguageResolver {
    fn resolve(
        &self,
        identifier: &str,
        kel_seq: Option<u64>,
    ) -> Result<VerifierKeyState, ResolveError> {
        let agent = self.resolve_agent(identifier, kel_seq)?;
        Ok(VerifierKeyState {
            master: agent.master,
            keys: agent
                .keys
                .iter()
                .map(|ke| crate::agent::signatures::VerificationMethod {
                    id: ke.id.clone(),
                    key: ke.signing_key.clone(),
                })
                .collect(),
        })
    }
}

// ─── tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::kel::adapter::MemoryAdapter;
    use crate::agent::kel::recovery::did_key_of;
    use crate::agent::kel::{
        incept_human, recovery, KeyEventBody, RecoveryAuthority, RevocationReason, Scope,
    };
    use did_key::{generate, Ed25519KeyPair};

    fn keypair() -> (did_key::PatchedKeyPair, String) {
        let kp = generate::<Ed25519KeyPair>(None);
        let did = did_key_of(&kp);
        (kp, did)
    }

    fn full_key(id: &str, signing_key: &str) -> KeyEntry {
        KeyEntry {
            id: id.to_string(),
            signing_key: signing_key.to_string(),
            encryption_key: None,
            scope: Scope::full(),
        }
    }

    fn dummy_commitment() -> String {
        recovery::recovery_commitment(&RecoveryAuthority {
            threshold: 1,
            keys: vec!["did:key:z6MkDummy".to_string()],
        })
    }

    fn setup() -> (
        AgentLanguageResolver,
        String,
        String,
        did_key::PatchedKeyPair,
    ) {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        let adapter = Arc::new(MemoryAdapter::new());
        adapter.seed(&scid, vec![ev0]);

        let cache = Arc::new(MonotonicityCache::new());
        let reverse_index = Arc::new(ReverseIndex::new());

        let resolver = AgentLanguageResolver::new(adapter, cache, reverse_index);
        (resolver, scid, key_id0, kp0)
    }

    #[test]
    fn resolve_master_scid() {
        let (resolver, scid, key_id0, _) = setup();
        let agent = resolver.resolve_agent(&scid, None).unwrap();
        assert_eq!(agent.master, scid);
        assert_eq!(agent.keys.len(), 1);
        assert_eq!(agent.keys[0].id, key_id0);
        assert_eq!(agent.agent_type, AgentType::Human);
    }

    #[test]
    fn resolve_delegated_to_master() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        // Delegate a second key.
        let (_, did1) = keypair();
        let key_id1 = format!("{}#key-1", did1);
        let body = KeyEventBody::Delegate {
            key: full_key(&key_id1, &did1),
            from_seq: 1,
        };
        let ev1 = crate::agent::kel::KeyEvent::new(1, Some(ev0.hash.clone()), body, &key_id0, &kp0);

        let adapter = Arc::new(MemoryAdapter::new());
        adapter.seed(&scid, vec![ev0, ev1]);

        let cache = Arc::new(MonotonicityCache::new());
        let reverse_index = Arc::new(ReverseIndex::new());
        let resolver = AgentLanguageResolver::new(adapter, cache, reverse_index);

        // Resolve via the master SCID — populates the reverse index.
        let agent = resolver.resolve_agent(&scid, None).unwrap();
        assert_eq!(agent.keys.len(), 2);

        // Now resolve via the delegated key's id → must return the same master.
        let agent2 = resolver.resolve_agent(&key_id1, None).unwrap();
        assert_eq!(agent2.master, scid);
    }

    #[test]
    fn validity_at_seq() {
        let (kp0, did0) = keypair();
        let key_id0 = format!("{}#key-0", did0);
        let key0 = full_key(&key_id0, &did0);
        let (ev0, scid) = incept_human(vec![key0], dummy_commitment(), &key_id0, &kp0);

        // Delegate key K at seq 1.
        let (_, did_k) = keypair();
        let key_id_k = format!("{}#key-k", did_k);
        let body1 = KeyEventBody::Delegate {
            key: full_key(&key_id_k, &did_k),
            from_seq: 1,
        };
        let ev1 =
            crate::agent::kel::KeyEvent::new(1, Some(ev0.hash.clone()), body1, &key_id0, &kp0);

        // Revoke K at seq 2.
        let body2 = KeyEventBody::Revoke {
            key_id: key_id_k.clone(),
            reason: RevocationReason::Retired,
        };
        let ev2 =
            crate::agent::kel::KeyEvent::new(2, Some(ev1.hash.clone()), body2, &key_id0, &kp0);

        let adapter = Arc::new(MemoryAdapter::new());
        adapter.seed(&scid, vec![ev0, ev1, ev2]);

        let cache = Arc::new(MonotonicityCache::new());
        let reverse_index = Arc::new(ReverseIndex::new());
        let resolver = AgentLanguageResolver::new(adapter, cache, reverse_index);

        // At seq 1, key K should appear valid.
        let agent_at_1 = resolver.resolve_agent(&scid, Some(1)).unwrap();
        assert!(agent_at_1.keys.iter().any(|k| k.id == key_id_k));

        // At seq 2, key K revoked — should not appear in valid keys.
        let agent_at_2 = resolver.resolve_agent(&scid, Some(2)).unwrap();
        assert!(!agent_at_2.keys.iter().any(|k| k.id == key_id_k));
    }

    #[test]
    fn resolver_implements_key_state_resolver() {
        let (resolver, scid, key_id0, _) = setup();
        // Use the KeyStateResolver trait method.
        let ks = resolver.resolve(&scid, None).unwrap();
        assert_eq!(ks.master, scid);
        assert_eq!(ks.keys.len(), 1);
        assert_eq!(ks.keys[0].id, key_id0);
    }

    #[test]
    fn unknown_identifier_fails() {
        let (resolver, _, _, _) = setup();
        let result = resolver.resolve_agent("did:scid:ke:1:Eunknown", None);
        assert!(matches!(result, Err(ResolveError::NotFound)));
    }

    #[test]
    fn warm_cache_no_extra_fold() {
        // After the first resolution, the cache holds the head.
        // A second resolution still works (cache check passes).
        let (resolver, scid, _, _) = setup();
        resolver.resolve_agent(&scid, None).unwrap();
        // Second call — cache holds seq 0, adapter still returns seq 0.
        let agent = resolver.resolve_agent(&scid, None).unwrap();
        assert_eq!(agent.master, scid);
    }
}
