//! Parent links for the instances an interpretation pass mints.
//!
//! A caller (a one-shot `runInterpretation`, or an AutoProcessor's config) may
//! name one or more `Raw` scopes. Every base the pass **creates** is linked as
//! a child of each: `scope.id --scope.predicate--> base`. One scope is the
//! common case ("new items live under this node"); a second records another
//! parent of the same kind, e.g. the conversation an item was drawn from as
//! well as the collection it belongs to.
//!
//! Upserts of existing instances are never linked: they already live
//! somewhere, and linking them would multi-parent unrelated graph state into
//! the caller's scope.
//!
//! The links are written in the same batch as the records themselves (see
//! [`apply_with_overlay`](super::overlay::apply_with_overlay)), so no reader
//! ever sees a minted instance without its parents.

use super::InterpretationOp;
use crate::perspectives::model_query::types::Scope;
use crate::types::Link;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// The scopes every freshly minted base is linked under. Empty means the
/// mints are not linked to anything.
///
/// On the wire and in a stored AutoProcessor config this is either a single
/// scope object or a list of them. A single scope is written back as an
/// object, so a config with one scope reads the same as it did before lists
/// were accepted.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MintScopes(pub Vec<Scope>);

impl MintScopes {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Every scope must be `Raw`: only that variant names one parent and a
    /// predicate. Checked before a pass runs, so a misconfigured caller is
    /// refused up front instead of after the model has been paid for.
    pub fn validate(&self) -> anyhow::Result<()> {
        for scope in &self.0 {
            raw_parent(scope)?;
        }
        Ok(())
    }

    /// One `AddLinks` op per scope, linking every base in `created` under that
    /// scope's parent. No ops when there is nothing to link.
    pub fn link_ops(&self, created: &[String]) -> anyhow::Result<Vec<InterpretationOp>> {
        if created.is_empty() {
            return Ok(Vec::new());
        }
        self.0
            .iter()
            .map(|scope| {
                let (parent, predicate) = raw_parent(scope)?;
                Ok(InterpretationOp::AddLinks {
                    source: parent.to_string(),
                    links: created
                        .iter()
                        .map(|base| Link {
                            source: parent.to_string(),
                            predicate: Some(predicate.to_string()),
                            target: base.clone(),
                        })
                        .collect(),
                })
            })
            .collect()
    }
}

fn raw_parent(scope: &Scope) -> anyhow::Result<(&str, &str)> {
    match scope {
        Scope::Raw { id, predicate } => Ok((id, predicate)),
        Scope::Model { .. } => anyhow::bail!(
            "mintScope must be a `Raw` scope (id + predicate); `Model` scopes carry no \
             linking predicate"
        ),
        // A traversal describes how to *read* outward from anchors. Minting
        // against it would have to pick one of them to hang the new link
        // under, and nothing in the scope says which.
        Scope::Traverse { .. } => anyhow::bail!(
            "mintScope must be a `Raw` scope (id + predicate); `Traverse` scopes describe a \
             read and name no single parent"
        ),
    }
}

impl From<Scope> for MintScopes {
    fn from(scope: Scope) -> Self {
        MintScopes(vec![scope])
    }
}

impl<'de> Deserialize<'de> for MintScopes {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum OneOrMany {
            One(Scope),
            Many(Vec<Scope>),
        }
        Ok(match OneOrMany::deserialize(deserializer)? {
            OneOrMany::One(scope) => MintScopes(vec![scope]),
            OneOrMany::Many(scopes) => MintScopes(scopes),
        })
    }
}

impl Serialize for MintScopes {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self.0.as_slice() {
            [one] => one.serialize(serializer),
            many => many.serialize(serializer),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn raw(id: &str, predicate: &str) -> Scope {
        Scope::Raw {
            id: id.into(),
            predicate: predicate.into(),
        }
    }

    #[test]
    fn reads_one_scope_or_a_list() {
        let one: MintScopes =
            serde_json::from_str(r#"{"id":"app://c","predicate":"app://child"}"#).unwrap();
        assert_eq!(one, MintScopes(vec![raw("app://c", "app://child")]));

        let many: MintScopes = serde_json::from_str(
            r#"[{"id":"app://c","predicate":"app://child"},
                {"id":"app://s","predicate":"app://source_of"}]"#,
        )
        .unwrap();
        assert_eq!(
            many,
            MintScopes(vec![
                raw("app://c", "app://child"),
                raw("app://s", "app://source_of")
            ])
        );
    }

    /// One scope is written as an object, so a stored config with a single
    /// scope is unchanged for a reader that predates lists.
    #[test]
    fn writes_one_scope_as_an_object() {
        let one = MintScopes(vec![raw("app://c", "app://child")]);
        assert_eq!(
            serde_json::to_value(&one).unwrap(),
            serde_json::json!({ "id": "app://c", "predicate": "app://child" })
        );
        let two = MintScopes(vec![
            raw("app://c", "app://child"),
            raw("app://s", "app://p"),
        ]);
        assert!(serde_json::to_value(&two).unwrap().is_array());
        let back: MintScopes = serde_json::from_value(serde_json::to_value(&two).unwrap()).unwrap();
        assert_eq!(back, two);
    }

    #[test]
    fn links_every_created_base_under_every_scope() {
        let scopes = MintScopes(vec![
            raw("app://c", "app://child"),
            raw("app://s", "app://source_of"),
        ]);
        let ops = scopes
            .link_ops(&["app://a".to_string(), "app://b".to_string()])
            .unwrap();
        let triples: Vec<(String, String, String)> = ops
            .iter()
            .flat_map(|op| match op {
                InterpretationOp::AddLinks { links, .. } => links.clone(),
                other => panic!("unexpected op {other:?}"),
            })
            .map(|l| (l.source, l.predicate.unwrap(), l.target))
            .collect();
        assert_eq!(
            triples,
            vec![
                ("app://c".into(), "app://child".into(), "app://a".into()),
                ("app://c".into(), "app://child".into(), "app://b".into()),
                ("app://s".into(), "app://source_of".into(), "app://a".into()),
                ("app://s".into(), "app://source_of".into(), "app://b".into()),
            ]
        );
    }

    #[test]
    fn nothing_created_means_no_links() {
        let scopes = MintScopes(vec![raw("app://c", "app://child")]);
        assert!(scopes.link_ops(&[]).unwrap().is_empty());
        assert!(MintScopes::default()
            .link_ops(&["app://a".into()])
            .unwrap()
            .is_empty());
    }

    #[test]
    fn only_raw_scopes_are_accepted() {
        let model = MintScopes(vec![
            raw("app://c", "app://child"),
            Scope::Model {
                model: "Project".into(),
                id: "app://p".into(),
                field: None,
            },
        ]);
        assert!(model.validate().is_err());
        assert!(model.link_ops(&["app://a".into()]).is_err());
        assert!(MintScopes(vec![raw("app://c", "app://child")])
            .validate()
            .is_ok());
    }
}
