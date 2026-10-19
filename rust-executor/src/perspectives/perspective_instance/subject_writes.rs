//! Writes on a subject beyond the setters: a relation or collection member
//! added or removed through the class's adder or remover, the links holding
//! a member, and the delete that also removes what points at the record.
//! The MCP tools write through these, so a write through an abstract class
//! dispatches as it does for the SDK (#1435).

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use std::collections::HashSet;

use super::abstract_classes::Op;
use super::{Command, PerspectiveInstance};
use crate::agent::AgentContext;
use crate::perspectives::model_query::types::ModelShape;
use crate::types::{DecoratedLinkExpression, LinkQuery};
use serde_json::Value;

/// A delete that stopped on a store error, and how far its cascade got.
/// The cascade is not batched: links removed before the error stay removed.
#[derive(Debug)]
pub(crate) struct CascadeFailure {
    /// Links the cascade removed before the error.
    pub removed: usize,
    pub error: String,
}

impl From<AnyError> for CascadeFailure {
    fn from(e: AnyError) -> Self {
        CascadeFailure {
            removed: 0,
            error: format!("{e:#}"),
        }
    }
}

/// The names of `shape` that hold members: its collections and relations.
pub(super) fn membership_names(shape: &ModelShape) -> HashSet<&str> {
    shape
        .properties
        .iter()
        .filter(|p| p.is_collection)
        .map(|p| p.name.as_str())
        .chain(shape.include_relations.iter().map(|r| r.name.as_str()))
        .collect()
}

/// What `update_subject` on a concrete class writes for one value.
pub(super) enum MemberWrite<'v> {
    /// The adder's or remover's commands, once per member.
    Members(Vec<Command>),
    /// A value for the setters.
    Set(&'v Value),
}

impl PerspectiveInstance {
    /// `update_subject` on a concrete class: `{"action": "adder" | "remover",
    /// "value": [...]}` on a relation or collection runs its adder or remover
    /// once per member, as through an abstract class. `{"action": "setter",
    /// "value": [...]}` there hands the setters the inner value, as through
    /// an abstract class. Anything else goes to the setters as it is.
    pub(super) async fn member_op<'v>(
        &self,
        class: &str,
        prop: &str,
        value: &'v Value,
        context: &AgentContext,
    ) -> Result<MemberWrite<'v>, AnyError> {
        let shape = self.get_shape(class)?;
        if !membership_names(&shape).contains(prop) {
            return Ok(MemberWrite::Set(value));
        }
        let (op, value) = Op::of(value);
        let predicate = match op {
            Op::Set => return Ok(MemberWrite::Set(value)),
            Op::Add => "ad4m://adder",
            Op::Remove => "ad4m://remover",
        };
        let actions = self
            .get_property_actions_from_shacl(class, prop, predicate)
            .await?
            .ok_or_else(|| {
                anyhow!(
                    "'{prop}' of '{class}' needs {}, and the class declares none.",
                    op.what()
                )
            })?;
        let mut commands = Vec::new();
        for member in self.members(class, prop, value, context).await? {
            commands.extend(actions.iter().map(|c| Command {
                target: Some(member.clone()),
                ..c.clone()
            }));
        }
        Ok(MemberWrite::Members(commands))
    }

    /// The links of `base` holding the members of `name` of `class`: at its
    /// predicate, or through an abstract class at the predicate of each of
    /// the record's classes it reads.
    pub async fn member_links(
        &self,
        class: &str,
        base: &str,
        name: &str,
    ) -> Result<Vec<DecoratedLinkExpression>, AnyError> {
        let mut predicates = Vec::new();
        if self.is_abstract_class(class)? {
            let index = self.abstract_index()?;
            let (alias_key, _) = self.abstract_shape(class)?;
            for target in self.write_targets(class, base)? {
                let shape = self.get_shape(&target)?;
                if let Some(own) = index.own_name(&target, &shape, name, alias_key) {
                    predicates.extend(predicate_of(&shape, own));
                }
            }
        } else {
            let shape = self.get_shape(class)?;
            predicates.extend(predicate_of(&shape, name));
        }
        let mut links = Vec::new();
        for predicate in predicates.into_iter().collect::<HashSet<_>>() {
            links.extend(
                self.get_links(&LinkQuery {
                    source: Some(base.to_string()),
                    predicate: Some(predicate),
                    ..Default::default()
                })
                .await?,
            );
        }
        Ok(links)
    }

    /// Delete the record of `class` at `base`, and every link that points at
    /// it from other records (collection membership, children). Through a
    /// concrete class every link of `base` goes. Through an abstract class
    /// the destructors of the record's classes it reads run first
    /// ([`Self::remove_subject`]). When the record has other classes it
    /// stays as them: only its links at the deleted classes' predicates go,
    /// minus those a kept class declares too. Its other links (its own
    /// children, links another app wrote) and the links pointing at it stay.
    ///
    /// Returns the links the cascade removed.
    pub(crate) async fn remove_subject_cascading(
        &mut self,
        class: &str,
        base: &str,
        context: &AgentContext,
    ) -> Result<usize, CascadeFailure> {
        // `Some`: the record stays, and only links at these predicates go.
        let mut only: Option<HashSet<String>> = None;
        if self.is_abstract_class(class)? {
            let targets = self.write_targets(class, base)?;
            let own = self
                .subject_classes_of(&[base.to_string()])?
                .remove(base)
                .unwrap_or_default();
            let others: Vec<_> = own.iter().filter(|c| !targets.contains(c)).collect();
            if !others.is_empty() {
                let mut kept = HashSet::new();
                for other in others {
                    kept.extend(self.get_shape(other)?.predicates());
                }
                let mut deleted = HashSet::new();
                for target in &targets {
                    deleted.extend(self.get_shape(target)?.predicates());
                }
                only = Some(deleted.difference(&kept).cloned().collect());
            }
            self.remove_subject(
                super::SubjectClassOption {
                    class_name: Some(class.to_string()),
                    query: None,
                },
                base.to_string(),
                None,
                context,
            )
            .await?;
        }

        let outbound = LinkQuery {
            source: Some(base.to_string()),
            ..Default::default()
        };
        let inbound = LinkQuery {
            target: Some(base.to_string()),
            ..Default::default()
        };
        let queries = match only {
            Some(_) => vec![outbound],
            None => vec![outbound, inbound],
        };
        let mut removed = 0;
        for query in queries {
            let links = self.get_links(&query).await.map_err(|e| CascadeFailure {
                removed,
                error: format!("listing the links of '{base}' failed: {e:#}"),
            })?;
            for link in links {
                let predicate = link.data.predicate.clone().unwrap_or_default();
                if only.as_ref().is_some_and(|o| !o.contains(&predicate)) {
                    continue;
                }
                let described =
                    format!("{} -[{predicate}]-> {}", link.data.source, link.data.target);
                self.remove_link(link.into(), None)
                    .await
                    .map_err(|e| CascadeFailure {
                        removed,
                        error: format!("removing link {described} failed: {e:#}"),
                    })?;
                removed += 1;
            }
        }
        Ok(removed)
    }
}

fn predicate_of(shape: &ModelShape, name: &str) -> Option<String> {
    shape
        .properties
        .iter()
        .find(|p| p.name == name)
        .map(|p| p.predicate.clone())
}

#[cfg(test)]
pub(super) mod faults {
    //! Test hook: fail a `remove_link` of a perspective, as a store error
    //! would.
    use std::collections::HashMap;
    use std::sync::{LazyLock, Mutex};

    static FAIL_AFTER: LazyLock<Mutex<HashMap<String, usize>>> = LazyLock::new(Default::default);

    /// Let `removals` link removals through, then fail the next one.
    pub(crate) fn fail_remove_link_after(perspective_uuid: &str, removals: usize) {
        FAIL_AFTER
            .lock()
            .unwrap()
            .insert(perspective_uuid.to_string(), removals);
    }

    pub(in crate::perspectives::perspective_instance) fn take(perspective_uuid: &str) -> bool {
        let mut armed = FAIL_AFTER.lock().unwrap();
        match armed.get_mut(perspective_uuid) {
            Some(0) => {
                armed.remove(perspective_uuid);
                true
            }
            Some(left) => {
                *left -= 1;
                false
            }
            None => false,
        }
    }
}
