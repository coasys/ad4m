//! Service grants: `service:<moduleId>@<compat>` × action.

use crate::agent::capabilities::types::Resource;
use crate::agent::capabilities::{check_capability, Capability, WILD_CARD};

/// The capability domain of an interface line.
pub fn service_domain(module_id: &str, compat: &str) -> String {
    format!("service:{}@{}", module_id, compat)
}

/// The capability one action needs.
pub fn service_capability(module_id: &str, compat: &str, action: &str) -> Capability {
    Capability {
        with: Resource {
            domain: service_domain(module_id, compat),
            pointers: vec![WILD_CARD.to_string()],
        },
        can: vec![action.to_string()],
    }
}

/// `true` when every grant layer allows `expected`.
pub fn allowed(layers: &[Vec<Capability>], expected: &Capability) -> bool {
    !layers.is_empty()
        && layers
            .iter()
            .all(|caps| check_capability(&Ok(caps.clone()), expected).is_ok())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::capabilities::defs::ALL_CAPABILITY;

    fn grant(domain: &str, can: &[&str]) -> Capability {
        Capability {
            with: Resource {
                domain: domain.into(),
                pointers: vec!["*".into()],
            },
            can: can.iter().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn grants_match_module_line_and_action() {
        let need = service_capability("did:key:a/QmX", "1", "SAY");
        assert!(allowed(
            &[vec![grant("service:did:key:a/QmX@1", &["SAY"])]],
            &need
        ));
        assert!(!allowed(
            &[vec![grant("service:did:key:a/QmX@2", &["SAY"])]],
            &need
        ));
        assert!(!allowed(
            &[vec![grant("service:did:key:a/QmX@1", &["OTHER"])]],
            &need
        ));
        assert!(allowed(&[vec![ALL_CAPABILITY.clone()]], &need));
        assert!(!allowed(&[], &need));
    }

    #[test]
    fn every_layer_must_allow() {
        let need = service_capability("did:key:a/QmX", "1", "SAY");
        let app = vec![ALL_CAPABILITY.clone()];
        let service = vec![grant("service:did:key:a/QmX@1", &["READ"])];
        assert!(!allowed(&[app.clone(), service], &need));
        let service = vec![grant("service:did:key:a/QmX@1", &["SAY"])];
        assert!(allowed(&[app, service], &need));
    }
}
