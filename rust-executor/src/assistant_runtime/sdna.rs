//! SDNA (Social DNA) subject-class definitions for the WE entity model.
//!
//! Emits Prolog SDNA facts matching the `Ad4mModel`/`buildSDNA` convention so
//! the `we://` classes are first-class subject classes for WE, the MCP
//! `query_subjects` tool, and Prolog `instance/2` queries. The runtime loop
//! itself operates at the link level (see [`super::store`]) and does not depend
//! on these being registered — this is a bootstrap so the classes exist when
//! WE has not yet published its own SHACL/SDNA.
//!
//! Registration is idempotent: classes already present (e.g. published by WE
//! with SHACL) are skipped, and the per-class `uid` is derived deterministically
//! from the class name so a re-run emits byte-identical facts.

use std::collections::HashSet;

use crate::agent::AgentContext;
use crate::perspectives::perspective_instance::{PerspectiveInstance, SdnaType};

use super::entities::{
    CLASS_ASSISTANT, CLASS_MCP_SERVER, CLASS_MESSAGE, CLASS_PERSONALITY, CLASS_RUN_STATE,
    CLASS_SKILL, CLASS_THREAD, FLAG, P_ASSISTANT_ID, P_AUTH, P_BODY, P_COMMAND, P_CONTENT,
    P_CREATED_AT, P_CURSOR, P_DESCRIPTION, P_MCP_SERVER_IDS, P_MODEL_ID, P_NAME,
    P_PENDING_TOOL_CALL, P_PERSONALITY_IDS, P_ROLE, P_SKILL_IDS, P_STATUS, P_SYSTEM_PROMPT,
    P_THREAD_ID, P_TITLE, P_TOOL_CALLS, P_TS, P_UPDATED_AT, REL_MESSAGE,
};

struct ClassDef {
    /// Registered class name (also the flag target `we://<class>`).
    name: &'static str,
    /// Scalar property predicates.
    properties: &'static [&'static str],
    /// HasMany collection relations as `(name, predicate)`.
    collections: &'static [(&'static str, &'static str)],
}

fn class_defs() -> Vec<ClassDef> {
    vec![
        ClassDef {
            name: CLASS_ASSISTANT,
            properties: &[
                P_NAME,
                P_MODEL_ID,
                P_SYSTEM_PROMPT,
                P_PERSONALITY_IDS,
                P_SKILL_IDS,
                P_MCP_SERVER_IDS,
            ],
            collections: &[],
        },
        ClassDef {
            name: CLASS_PERSONALITY,
            properties: &[P_NAME, P_BODY],
            collections: &[],
        },
        ClassDef {
            name: CLASS_SKILL,
            properties: &[P_NAME, P_DESCRIPTION, P_BODY],
            collections: &[],
        },
        ClassDef {
            name: CLASS_MCP_SERVER,
            properties: &[P_NAME, "we://transport", P_AUTH, "we://url", P_COMMAND],
            collections: &[],
        },
        ClassDef {
            name: CLASS_THREAD,
            properties: &[P_TITLE, P_ASSISTANT_ID, P_MODEL_ID, P_CREATED_AT, P_UPDATED_AT],
            collections: &[("messages", REL_MESSAGE)],
        },
        ClassDef {
            name: CLASS_MESSAGE,
            properties: &[P_THREAD_ID, P_ROLE, P_CONTENT, P_TOOL_CALLS, P_TS, P_STATUS],
            collections: &[],
        },
        ClassDef {
            name: CLASS_RUN_STATE,
            properties: &[P_THREAD_ID, P_STATUS, P_CURSOR, P_PENDING_TOOL_CALL],
            collections: &[],
        },
    ]
}

/// Deterministic positive integer id for a class name (the `uid` slot in the
/// SDNA facts). Stable across runs so re-registration is a no-op.
fn class_uid(name: &str) -> u64 {
    // FNV-1a, bounded to 8 digits to stay a compact Prolog integer.
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for b in name.as_bytes() {
        hash ^= *b as u64;
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash % 100_000_000
}

/// Short property name used in `property/2` facts — the tail after `we://`.
fn short_name(predicate: &str) -> &str {
    predicate.strip_prefix("we://").unwrap_or(predicate)
}

/// Generate the Prolog SDNA for one class.
fn generate_sdna(def: &ClassDef) -> String {
    let uid = class_uid(def.name);
    let mut s = String::new();

    s.push_str(&format!("subject_class(\"{}\", {}).\n", def.name, uid));

    // An instance is any base carrying the class flag link.
    s.push_str(&format!(
        "instance({}, Base) :- triple(Base, \"{}\", \"{}\").\n",
        uid, FLAG, def.name
    ));

    // Constructor writes the flag; destructor removes it.
    s.push_str(&format!(
        "constructor({}, '[{{\"action\":\"addLink\",\"source\":\"this\",\"predicate\":\"{}\",\"target\":\"{}\"}}]').\n",
        uid, FLAG, def.name
    ));
    s.push_str(&format!(
        "destructor({}, '[{{\"action\":\"removeLink\",\"source\":\"this\",\"predicate\":\"{}\",\"target\":\"{}\"}}]').\n",
        uid, FLAG, def.name
    ));

    for pred in def.properties {
        let pname = short_name(pred);
        s.push_str(&format!("property({}, \"{}\").\n", uid, pname));
        s.push_str(&format!(
            "property_getter({}, Base, \"{}\", Value) :- triple(Base, \"{}\", Value).\n",
            uid, pname, pred
        ));
        s.push_str(&format!(
            "property_setter({}, \"{}\", '[{{\"action\":\"setSingleTarget\",\"source\":\"this\",\"predicate\":\"{}\",\"target\":\"value\"}}]').\n",
            uid, pname, pred
        ));
    }

    for (cname, pred) in def.collections {
        s.push_str(&format!("collection({}, \"{}\").\n", uid, cname));
        s.push_str(&format!(
            "collection_getter({}, Base, \"{}\", List) :- findall(C, triple(Base, \"{}\", C), List).\n",
            uid, cname, pred
        ));
        s.push_str(&format!(
            "collection_adder({}, \"{}\", '[{{\"action\":\"addLink\",\"source\":\"this\",\"predicate\":\"{}\",\"target\":\"value\"}}]').\n",
            uid, cname, pred
        ));
        s.push_str(&format!(
            "collection_remover({}, \"{}\", '[{{\"action\":\"removeLink\",\"source\":\"this\",\"predicate\":\"{}\",\"target\":\"value\"}}]').\n",
            uid, cname, pred
        ));
    }

    s
}

/// Register any WE subject classes not already present in the perspective.
/// Best effort — failures are logged and skipped so a bad class never blocks
/// the run loop (which is link-level and independent of SDNA).
pub async fn ensure_subject_classes(p: &mut PerspectiveInstance, ctx: &AgentContext) {
    let existing: HashSet<String> = p
        .get_subject_classes_from_shacl()
        .await
        .unwrap_or_default()
        .into_iter()
        .collect();

    for def in class_defs() {
        if existing.contains(def.name) {
            continue;
        }
        let sdna = generate_sdna(&def);
        if let Err(e) = p
            .add_sdna(def.name.to_string(), sdna, SdnaType::SubjectClass, None, ctx)
            .await
        {
            log::warn!(
                "assistant_runtime: failed to register SDNA class {} in perspective {}: {}",
                def.name,
                p.uuid,
                e
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn class_uid_is_deterministic_and_bounded() {
        assert_eq!(class_uid(CLASS_MESSAGE), class_uid(CLASS_MESSAGE));
        assert!(class_uid(CLASS_MESSAGE) < 100_000_000);
        assert_ne!(class_uid(CLASS_MESSAGE), class_uid(CLASS_THREAD));
    }

    #[test]
    fn message_sdna_contains_expected_facts() {
        let def = class_defs()
            .into_iter()
            .find(|d| d.name == CLASS_MESSAGE)
            .unwrap();
        let sdna = generate_sdna(&def);
        assert!(sdna.contains(&format!("subject_class(\"{}\"", CLASS_MESSAGE)));
        // instance condition keys off the flag link
        assert!(sdna.contains(&format!(
            "instance({}, Base) :- triple(Base, \"{}\", \"{}\").",
            class_uid(CLASS_MESSAGE),
            FLAG,
            CLASS_MESSAGE
        )));
        // property getter for content maps to the we://content predicate
        assert!(sdna.contains("property_getter"));
        assert!(sdna.contains(&format!("triple(Base, \"{}\", Value)", P_CONTENT)));
    }

    #[test]
    fn thread_sdna_declares_messages_collection() {
        let def = class_defs()
            .into_iter()
            .find(|d| d.name == CLASS_THREAD)
            .unwrap();
        let sdna = generate_sdna(&def);
        assert!(sdna.contains("collection("));
        assert!(sdna.contains("\"messages\""));
        assert!(sdna.contains(&format!("triple(Base, \"{}\", C)", REL_MESSAGE)));
    }
}
