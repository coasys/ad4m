//! The write side of the scope index: which classes are indexed
//! ([`RuleTable`]) and how a transaction's link changes update their scopes
//! ([`RuleTable::apply`]).
//!
//! `apply` does not trust the diff for what is true. The diff only names the
//! link triples a transaction touched; for each member they can affect,
//! `apply` reads the transaction's state (its own writes included) and moves
//! the index to agree with it:
//!
//! - an entry is added only when it is absent and removed only when nothing
//!   supports it any more, so two authors asserting one triple, or a link
//!   removed while another still asserts its triple, change nothing;
//! - a count changes only when an entry does;
//! - a member's key moves only when the time of its earliest verified flag
//!   link moves.
//!
//! Applying the same diff twice therefore changes nothing the second time,
//! and the order links arrive in does not matter.

use deno_core::anyhow::Error;
use oxigraph::model::{
    GraphNameRef, NamedNode, NamedNodeRef, NamedOrBlankNodeRef, Term, TermRef, Triple,
};
use oxigraph::store::{Store, Transaction};
use std::collections::{HashMap, HashSet};

use super::{
    count_key, entry_key, flag_key, member_hash, member_scope_key, noncanonical_scope,
    parse_timestamp, prefix_end, ClassId, MemberKey, Scope, ScopeId, MEMBER, SEQ_KEY,
};
use crate::perspectives::model_query::types::ModelShape;
use crate::perspectives::model_query::utils::emittable_iri;
use crate::perspectives::sparql_store::{
    is_link_metadata, ONT_PROOF_VALID, ONT_TIMESTAMP, RDF_REIFIES,
};

/// How one class is indexed: its members are the subjects of verified links
/// `<member> <flag_predicate> <flag_value>`.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ClassRule {
    /// The class's target class IRI (`ModelShape::target_class`).
    pub class: String,
    pub id: ClassId,
    pub flag_predicate: String,
    pub flag_value: String,
}

impl ClassRule {
    pub fn new(class: &str, flag_predicate: &str, flag_value: &str) -> Self {
        ClassRule {
            class: class.to_string(),
            id: ClassId::of(class),
            flag_predicate: flag_predicate.to_string(),
            flag_value: flag_value.to_string(),
        }
    }

    /// The rule of a shape: its first flag property whose predicate and value
    /// are IRIs. This is the triple the SPARQL path orders instances by, so
    /// both paths see one membership. `None` (not indexed) when the shape has
    /// no such flag, or its value is a `literal:` value, which the store keeps
    /// as an RDF literal and not as the IRI the flag names.
    pub fn from_shape(shape: &ModelShape) -> Option<Self> {
        if shape.target_class.is_empty() {
            return None;
        }
        let flag = shape.properties.iter().find(|p| {
            p.is_flag
                && p.initial_value
                    .as_deref()
                    .is_some_and(|v| emittable_iri(v) && !v.starts_with("literal:"))
                && emittable_iri(&p.predicate)
        })?;
        Some(ClassRule::new(
            &shape.target_class,
            &flag.predicate,
            flag.initial_value.as_deref()?,
        ))
    }

    pub fn class_scope(&self) -> Scope {
        Scope::Class {
            class: self.class.clone(),
        }
    }

    pub fn under(&self, parent: &str, relation: &str) -> Scope {
        Scope::Under {
            class: self.class.clone(),
            parent: parent.to_string(),
            relation: relation.to_string(),
        }
    }
}

/// Every indexed class, and a lookup from a flag triple's predicate and value
/// to its classes.
#[derive(Clone, Default, Debug)]
pub struct RuleTable {
    classes: Vec<ClassRule>,
    /// flag predicate → (flag value, index into `classes`)
    by_flag: HashMap<String, Vec<(String, usize)>>,
}

/// Whether an entry came or went.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ScopeChange {
    Added,
    Removed,
}

/// One entry that [`RuleTable::apply`] added to or removed from a scope.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ScopeDelta {
    pub scope: ScopeId,
    pub member: String,
    pub key: MemberKey,
    pub change: ScopeChange,
}

/// The link triples one transaction touched, as stored: subject, predicate
/// and the object term. The store builds it from the links it wrote or
/// removed; [`RuleTable::apply`] reads which of them are verified.
#[derive(Clone, Default, Debug)]
pub struct VerifiedDiff {
    pub triples: Vec<(NamedNode, NamedNode, Term)>,
}

impl VerifiedDiff {
    pub fn push(&mut self, subject: NamedNode, predicate: NamedNode, object: Term) {
        self.triples.push((subject, predicate, object));
    }

    pub fn is_empty(&self) -> bool {
        self.triples.is_empty()
    }
}

impl RuleTable {
    pub fn new(classes: Vec<ClassRule>) -> Self {
        let mut table = RuleTable::default();
        for rule in classes {
            table = table.with(rule);
        }
        table
    }

    /// This table with `rule` added, or replacing the rule of its class.
    pub fn with(&self, rule: ClassRule) -> Self {
        let mut classes: Vec<ClassRule> = self
            .classes
            .iter()
            .filter(|r| r.id != rule.id)
            .cloned()
            .collect();
        classes.push(rule);
        let mut by_flag: HashMap<String, Vec<(String, usize)>> = HashMap::new();
        for (i, r) in classes.iter().enumerate() {
            by_flag
                .entry(r.flag_predicate.clone())
                .or_default()
                .push((r.flag_value.clone(), i));
        }
        RuleTable { classes, by_flag }
    }

    pub fn get(&self, id: ClassId) -> Option<&ClassRule> {
        self.classes.iter().find(|r| r.id == id)
    }

    pub fn classes(&self) -> &[ClassRule] {
        &self.classes
    }

    pub fn is_empty(&self) -> bool {
        self.classes.is_empty()
    }

    /// Update the index in `tx` for the link triples in `diff`, which `tx`
    /// already wrote or removed. Returns the entries that changed. Call it
    /// once, after the last quad write and before the commit.
    ///
    /// `store` is the store of `tx`: a transaction cannot scan the auxiliary
    /// space, so a member whose key moves or who leaves the class finds the
    /// `Under` scopes it is in by a scan of the committed index. The caller
    /// holds the store's writer lock, so the committed state is the state
    /// `tx` started from.
    pub fn apply(
        &self,
        store: &Store,
        tx: &mut Transaction<'_>,
        diff: &VerifiedDiff,
    ) -> Result<Vec<ScopeDelta>, Error> {
        if self.classes.is_empty() || diff.is_empty() {
            return Ok(Vec::new());
        }
        let mut ctx = Apply::new(tx, store);
        // 1. Members whose flag triple changed: their membership and key.
        let mut seen = HashSet::new();
        for (s, p, o) in &diff.triples {
            let Term::NamedNode(o) = o else { continue };
            let Some(candidates) = self.by_flag.get(p.as_str()) else {
                continue;
            };
            for (value, idx) in candidates {
                if value == o.as_str() && seen.insert((*idx, s.as_str().to_string())) {
                    ctx.reconcile_member(&self.classes[*idx], s.as_str())?;
                }
            }
        }
        // 2. Every triple between two nodes can put its object under its
        //    subject: `Under(class, subject, predicate)`.
        let mut pairs = HashSet::new();
        for (s, p, o) in &diff.triples {
            let Term::NamedNode(o) = o else { continue };
            if is_link_metadata(p.as_str()) || !pairs.insert((s.clone(), p.clone(), o.clone())) {
                continue;
            }
            for rule in &self.classes {
                ctx.reconcile_pair(rule, s.as_str(), p.as_str(), o.as_str())?;
            }
        }
        Ok(ctx.deltas)
    }
}

/// What a member's flag links say for one class.
struct FlagState {
    key: MemberKey,
    noncanonical: bool,
}

fn decode_flag(value: &[u8]) -> Option<FlagState> {
    Some(FlagState {
        key: MemberKey::from_bytes(value.get(..8)?)?,
        noncanonical: value.get(8).copied() == Some(1),
    })
}

fn encode_flag(key: MemberKey, noncanonical: bool) -> [u8; 9] {
    let mut v = [0u8; 9];
    v[..8].copy_from_slice(&key.to_bytes());
    v[8] = u8::from(noncanonical);
    v
}

fn encode_member(key: MemberKey, class: ClassId) -> [u8; 24] {
    let mut v = [0u8; 24];
    v[..8].copy_from_slice(&key.to_bytes());
    v[8..].copy_from_slice(&class.0);
    v
}

/// One `apply` (or one build batch) on one transaction.
pub(crate) struct Apply<'a, 'tx> {
    tx: &'a mut Transaction<'tx>,
    store: &'a Store,
    /// `M` keys this apply wrote, for the scopes of a member the committed
    /// index does not list yet.
    written: HashMap<[u8; 16], HashSet<ScopeId>>,
    pub(crate) deltas: Vec<ScopeDelta>,
}

impl<'a, 'tx> Apply<'a, 'tx> {
    pub(crate) fn new(tx: &'a mut Transaction<'tx>, store: &'a Store) -> Self {
        Apply {
            tx,
            store,
            written: HashMap::new(),
            deltas: Vec::new(),
        }
    }

    /// The verified links asserting `<s> <p> <o>`: for each, its timestamps.
    /// `None` when no link with a verified signature asserts it.
    fn verified_times(
        &self,
        s: &str,
        p: &str,
        o: TermRef<'_>,
    ) -> Result<Option<Vec<String>>, Error> {
        let triple = Triple::new(
            NamedNode::new_unchecked(s),
            NamedNode::new_unchecked(p),
            o.into_owned(),
        );
        let mut times: Option<Vec<String>> = None;
        for quad in self.tx.quads_for_pattern(
            None,
            Some(NamedNodeRef::new_unchecked(RDF_REIFIES)),
            Some(TermRef::Triple(&triple)),
            Some(GraphNameRef::DefaultGraph),
        ) {
            let quad = quad?;
            let reifier = quad.subject.as_ref();
            if !literal_values(self.tx, reifier, ONT_PROOF_VALID)?
                .iter()
                .any(|v| v == "true")
            {
                continue;
            }
            times.get_or_insert_with(Vec::new).extend(literal_values(
                self.tx,
                reifier,
                ONT_TIMESTAMP,
            )?);
        }
        Ok(times)
    }

    fn is_verified(&self, s: &str, p: &str, o: &str) -> Result<bool, Error> {
        Ok(self
            .verified_times(s, p, TermRef::NamedNode(NamedNodeRef::new_unchecked(o)))?
            .is_some())
    }

    /// Bring `member`'s entries for `rule` in line with the transaction:
    /// its flag time, its class scope and every `Under` scope.
    pub(crate) fn reconcile_member(&mut self, rule: &ClassRule, member: &str) -> Result<(), Error> {
        let mh = member_hash(member);
        let fkey = flag_key(&mh, rule.id);
        let old = self.tx.aux_get(&fkey)?.as_deref().and_then(decode_flag);
        let times = self.verified_times(
            member,
            &rule.flag_predicate,
            TermRef::NamedNode(NamedNodeRef::new_unchecked(&rule.flag_value)),
        )?;
        // The earliest verified flag link. The SPARQL path takes the least
        // timestamp string; for canonical timestamps that is the least time.
        let new = times.and_then(|times| {
            let parsed: Vec<(u64, bool)> = times.iter().map(|t| parse_timestamp(t)).collect();
            let t_ms = parsed.iter().map(|(t, _)| *t).min()?;
            Some((t_ms, parsed.iter().any(|(_, canonical)| !canonical)))
        });
        let nc_key = count_key(noncanonical_scope(&rule.class));

        let key = match (&old, new) {
            (None, None) => return Ok(()),
            (Some(old), None) => {
                self.tx.aux_single_delete(&fkey);
                if old.noncanonical {
                    self.tx.aux_merge_add(&nc_key, -1);
                }
                None
            }
            (old, Some((t_ms, noncanonical))) => {
                let key = match old {
                    Some(o) if o.key.t_ms() == t_ms.min(super::MAX_T_MS) => o.key,
                    _ => self.alloc_key(rule, t_ms)?,
                };
                let changed = old
                    .as_ref()
                    .is_none_or(|o| o.key != key || o.noncanonical != noncanonical);
                if changed {
                    if old.is_some() {
                        self.tx.aux_single_delete(&fkey);
                    }
                    self.tx.aux_put(&fkey, &encode_flag(key, noncanonical));
                    let was = old.as_ref().is_some_and(|o| o.noncanonical);
                    if was != noncanonical {
                        self.tx
                            .aux_merge_add(&nc_key, if noncanonical { 1 } else { -1 });
                    }
                }
                Some(key)
            }
        };

        // The scopes the member should be in, and the scopes it is in.
        let mut desired: HashMap<ScopeId, ()> = HashMap::new();
        let mut targets: Vec<ScopeId> = Vec::new();
        if key.is_some() {
            let class_scope = rule.class_scope().id();
            desired.insert(class_scope, ());
            targets.push(class_scope);
            for (parent, relation) in self.incoming(member)? {
                if self.is_verified(&parent, &relation, member)? {
                    let id = rule.under(&parent, &relation).id();
                    if desired.insert(id, ()).is_none() {
                        targets.push(id);
                    }
                }
            }
        }
        for scope in self.current_scopes(&mh, rule.id)? {
            if !desired.contains_key(&scope) {
                targets.push(scope);
            }
        }
        for scope in targets {
            let want = if desired.contains_key(&scope) {
                key
            } else {
                None
            };
            self.sync_entry(scope, &mh, member, rule.id, want)?;
        }
        Ok(())
    }

    /// `Under(rule's class, parent, relation)` for `member`, after the
    /// triple `<parent> <relation> <member>` changed.
    pub(crate) fn reconcile_pair(
        &mut self,
        rule: &ClassRule,
        parent: &str,
        relation: &str,
        member: &str,
    ) -> Result<(), Error> {
        let mh = member_hash(member);
        let Some(flag) = self
            .tx
            .aux_get(&flag_key(&mh, rule.id))?
            .as_deref()
            .and_then(decode_flag)
        else {
            // Not an instance: it is in none of the class's scopes.
            return Ok(());
        };
        let want = self
            .is_verified(parent, relation, member)?
            .then_some(flag.key);
        self.sync_entry(
            rule.under(parent, relation).id(),
            &mh,
            member,
            rule.id,
            want,
        )
    }

    /// The `(subject, predicate)` of every link triple whose object is `member`.
    fn incoming(&self, member: &str) -> Result<Vec<(String, String)>, Error> {
        let mut out = Vec::new();
        for quad in self.tx.quads_for_pattern(
            None,
            None,
            Some(TermRef::NamedNode(NamedNodeRef::new_unchecked(member))),
            Some(GraphNameRef::DefaultGraph),
        ) {
            let quad = quad?;
            if is_link_metadata(quad.predicate.as_str()) {
                continue;
            }
            if let oxigraph::model::NamedOrBlankNode::NamedNode(s) = quad.subject {
                out.push((s.into_string(), quad.predicate.into_string()));
            }
        }
        out.sort();
        out.dedup();
        Ok(out)
    }

    /// The scopes of `class` that hold `member` now, in the transaction.
    fn current_scopes(&self, mh: &[u8; 16], class: ClassId) -> Result<Vec<ScopeId>, Error> {
        let mut found: HashSet<ScopeId> = HashSet::new();
        let mut prefix = vec![MEMBER];
        prefix.extend_from_slice(mh);
        for item in self.store.aux_scan(&prefix, &prefix_end(&prefix), false) {
            let (k, _) = item?;
            if let Some(id) = k.get(17..33).and_then(|b| b.try_into().ok()) {
                found.insert(ScopeId(id));
            }
        }
        if let Some(written) = self.written.get(mh) {
            found.extend(written.iter().copied());
        }
        let mut out = Vec::new();
        for scope in found {
            if let Some(v) = self.tx.aux_get(&member_scope_key(mh, scope))? {
                if v.get(8..24) == Some(&class.0[..]) {
                    out.push(scope);
                }
            }
        }
        Ok(out)
    }

    /// A new key for a member of `rule`'s class at `t_ms`: the next `seq`, and
    /// the one after while the class scope already has that key (only after
    /// 2^20 members in one millisecond).
    fn alloc_key(&mut self, rule: &ClassRule, t_ms: u64) -> Result<MemberKey, Error> {
        let class_scope = rule.class_scope().id();
        loop {
            self.tx.aux_merge_add(&SEQ_KEY, 1);
            let seq = self.tx.aux_count(&SEQ_KEY)? as u64;
            let key = MemberKey::new(t_ms, seq);
            if self.tx.aux_get(&entry_key(class_scope, key))?.is_none() {
                return Ok(key);
            }
        }
    }

    /// Make `member`'s entry in `scope` be `want`: remove it, add it, or move
    /// it to another key.
    fn sync_entry(
        &mut self,
        scope: ScopeId,
        mh: &[u8; 16],
        member: &str,
        class: ClassId,
        want: Option<MemberKey>,
    ) -> Result<(), Error> {
        let mkey = member_scope_key(mh, scope);
        let have = self
            .tx
            .aux_get(&mkey)?
            .and_then(|v| MemberKey::from_bytes(v.get(..8)?));
        if have == want {
            return Ok(());
        }
        let ckey = count_key(scope);
        if let Some(old) = have {
            self.tx.aux_single_delete(&entry_key(scope, old));
            self.tx.aux_single_delete(&mkey);
            self.tx.aux_merge_add(&ckey, -1);
            self.deltas.push(ScopeDelta {
                scope,
                member: member.to_string(),
                key: old,
                change: ScopeChange::Removed,
            });
        }
        if let Some(new) = want {
            self.tx.aux_put(&entry_key(scope, new), member.as_bytes());
            self.tx.aux_put(&mkey, &encode_member(new, class));
            self.tx.aux_merge_add(&ckey, 1);
            self.written.entry(*mh).or_default().insert(scope);
            self.deltas.push(ScopeDelta {
                scope,
                member: member.to_string(),
                key: new,
                change: ScopeChange::Added,
            });
        }
        Ok(())
    }
}

/// The literal values stored under `predicate` on `reifier`.
fn literal_values(
    tx: &Transaction<'_>,
    reifier: NamedOrBlankNodeRef<'_>,
    predicate: &str,
) -> Result<Vec<String>, Error> {
    let mut out = Vec::new();
    for quad in tx.quads_for_pattern(
        Some(reifier),
        Some(NamedNodeRef::new_unchecked(predicate)),
        None,
        Some(GraphNameRef::DefaultGraph),
    ) {
        if let Term::Literal(l) = quad?.object {
            out.push(l.value().to_string());
        }
    }
    Ok(out)
}

/// The members of `rule`'s class scope with keys at or after `after`
/// (exclusive), oldest first, at most `limit`: the members a build checks
/// again in its first phase.
pub(crate) fn class_members_after(
    store: &Store,
    rule: &ClassRule,
    after: Option<MemberKey>,
    limit: usize,
) -> Result<Vec<(String, MemberKey)>, Error> {
    let lower = match after {
        Some(k) => match k.0.checked_add(1) {
            Some(next) => Some(MemberKey(next)),
            None => return Ok(Vec::new()),
        },
        None => None,
    };
    super::ScopeReader::new(store)
        .entries(rule.class_scope().id(), lower, None, false)
        .take(limit)
        .collect()
}
