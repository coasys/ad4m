//! The index of a file import. The bulk loader writes quads and no index
//! entry, so the import writes them itself once the loader has committed,
//! and the classes that were ready go back to ready without a build.
//!
//! What the import derives its work from is the file's links: the workers
//! that parse and verify them name each *candidate* ([`link_candidates`]): a
//! subject of a link with a class's flag triple (a new member, or an old one
//! with one more flag link), and an object of a link between two nodes when
//! the object is a member already (a new parent for it). A member the
//! file only names as the object of a link is not a candidate when it is not
//! a member: it becomes one only through a flag link, which makes it a
//! candidate on its own.
//!
//! After the commit, per class, in bounded transactions:
//!
//! - a candidate that is not a member yet gets its entries from the
//!   committed store, read in bulk (no transaction reads, no scan of the
//!   index): its flag time, its keys, the class scope and every `Under`
//!   scope. These are the rules of [`super::writer::Apply::reconcile_member`]
//!   ([`earliest_flag_time`], [`pick_key`], [`member_scopes`]), over a
//!   member that has no entry yet, so there is nothing to remove or move;
//! - a candidate that is a member already (its earliest flag time may move,
//!   or it gets a new parent) goes through `reconcile_member` itself.
//!
//! The class marks stay `Building` (see `ScopeIndex::begin_rebuild`) until
//! every class is written, then all are marked `Ready` in one transaction. A
//! stop in between leaves them `Building`: the background build checks every
//! member again, and a member written already is a no-op for it.
//!
//! Only a class that was ready when the import began is written here. A class
//! that was building keeps its mark at the start of its build: the build
//! covers the file's links with the rest.

use deno_core::anyhow::Error;
use oxigraph::model::{NamedNode, NamedNodeRef, Term, TermRef};
use oxigraph::store::Store;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::build::{write_mark, BuildMark, BuildState};
use super::writer::{
    earliest_flag_time, member_scopes, pick_key, put_flag, put_membership, verified_times, Apply,
    ClassRule, ScopeChange,
};
use super::{
    count_key, entry_key, flag_key, member_hash, noncanonical_scope, ClassId, ScopeId, ScopeIndex,
    SEQ_KEY,
};

/// Members per transaction.
const CHUNK: usize = 2048;

/// The members the file's links may change, per class (see the module doc).
pub(crate) type Candidates = HashMap<ClassId, HashSet<String>>;

#[cfg(test)]
thread_local! {
    /// Members per transaction, and the number of transactions after which
    /// [`index_import`] stops without marking anything ready, as a process
    /// that dies there does.
    pub(crate) static TEST_LIMITS: std::cell::Cell<(Option<usize>, Option<usize>)> =
        const { std::cell::Cell::new((None, None)) };
}

fn chunk_size() -> usize {
    #[cfg(test)]
    if let Some(size) = TEST_LIMITS.with(|l| l.get().0) {
        return size;
    }
    CHUNK
}

/// Whether a test asked to stop after `done` transactions.
fn stop_now(done: usize) -> bool {
    #[cfg(test)]
    return TEST_LIMITS.with(|l| l.get().1.is_some_and(|stop| done >= stop));
    #[cfg(not(test))]
    {
        let _ = done;
        false
    }
}

/// The candidates one imported link `<s> <p> <o>` adds, in the classes of
/// `ready` (the classes that were ready when the import began). `store` is the
/// store before the load: its flag records say which objects are members.
pub(crate) fn link_candidates(
    store: &Store,
    ready: &[ClassRule],
    s: &NamedNode,
    p: &NamedNode,
    o: &Term,
    out: &mut Vec<(ClassId, String)>,
) -> Result<(), Error> {
    let Term::NamedNode(o) = o else {
        return Ok(());
    };
    for rule in ready {
        if rule.flag_predicate == p.as_str() && rule.flag_value == o.as_str() {
            out.push((rule.id, s.as_str().to_string()));
        }
        if store
            .aux_get(&flag_key(&member_hash(o.as_str()), rule.id))?
            .is_some()
        {
            out.push((rule.id, o.as_str().to_string()));
        }
    }
    Ok(())
}

/// Write the index entries of the file's links, then mark `ready` ready. Call
/// it once the loader has committed, under the writer lock. A failure leaves
/// the marks `Building`.
pub(crate) fn index_import(
    store: &Arc<Store>,
    index: &ScopeIndex,
    ready: &[ClassRule],
    candidates: &Candidates,
) -> Result<(), Error> {
    let chunk = chunk_size();
    let mut transactions = 0usize;
    for rule in ready {
        let Some(members) = candidates.get(&rule.id) else {
            continue;
        };
        let mut members: Vec<&str> = members.iter().map(String::as_str).collect();
        members.sort_unstable();
        // Split before the first write: a member is new when the committed
        // index has no flag record of it, whatever this import writes later.
        let mut new = Vec::new();
        let mut old = Vec::new();
        for member in members {
            if store
                .aux_get(&flag_key(&member_hash(member), rule.id))?
                .is_some()
            {
                old.push(member);
            } else {
                new.push(member);
            }
        }
        for part in new.chunks(chunk) {
            if stop_now(transactions) {
                return Ok(());
            }
            index_new_members(store, rule, part)?;
            transactions += 1;
        }
        for part in old.chunks(chunk) {
            if stop_now(transactions) {
                return Ok(());
            }
            reconcile_members(store, index, rule, part)?;
            transactions += 1;
        }
    }
    let mut tx = store.start_transaction()?;
    for rule in ready {
        write_mark(
            &mut tx,
            &BuildMark {
                rule: rule.clone(),
                state: BuildState::Ready,
            },
        );
    }
    tx.commit()?;
    for rule in ready {
        index.set_ready(rule);
    }
    Ok(())
}

/// One transaction: the entries of members that have none, from the
/// committed store.
fn index_new_members(store: &Store, rule: &ClassRule, members: &[&str]) -> Result<(), Error> {
    let class_scope = rule.class_scope().id();
    let noncanonical = noncanonical_scope(&rule.class);
    let last = store.aux_count(&SEQ_KEY)? as u64;
    let mut used = 0u64;
    let mut counts: HashMap<ScopeId, i64> = HashMap::new();
    let mut tx = store.start_transaction()?;
    for member in members {
        let Some((t_ms, mut is_noncanonical)) = earliest_flag_time(verified_times(
            store,
            member,
            &rule.flag_predicate,
            TermRef::NamedNode(NamedNodeRef::new_unchecked(&rule.flag_value)),
        )?) else {
            // No verified flag link: not an instance.
            continue;
        };
        let (key, displaced) = pick_key(&rule.class, t_ms, last, &mut used, |key| {
            Ok(store.aux_get(&entry_key(class_scope, key))?.is_some())
        })?;
        is_noncanonical |= displaced;
        let mh = member_hash(member);
        put_flag(&mut tx, &mh, rule.id, key, is_noncanonical);
        if is_noncanonical {
            *counts.entry(noncanonical).or_default() += 1;
        }
        for scope in member_scopes(store, rule, member)? {
            put_membership(&mut tx, scope, &mh, member, rule.id, key);
            *counts.entry(scope).or_default() += 1;
        }
    }
    for (scope, n) in counts {
        tx.aux_merge_add(&count_key(scope), n);
    }
    if used > 0 {
        tx.aux_merge_add(&SEQ_KEY, used as i64);
    }
    tx.commit()?;
    Ok(())
}

/// One transaction: members that have entries already, checked as every
/// write checks them.
fn reconcile_members(
    store: &Arc<Store>,
    index: &ScopeIndex,
    rule: &ClassRule,
    members: &[&str],
) -> Result<(), Error> {
    let mut tx = store.start_transaction()?;
    let deltas = {
        let mut ctx = Apply::new(&mut tx, store);
        for member in members {
            ctx.reconcile_member(rule, member)?;
        }
        std::mem::take(&mut ctx.deltas)
    };
    tx.commit()?;
    if deltas.iter().any(|d| d.change == ScopeChange::Removed) {
        index.compactor.note(store, &deltas);
    }
    Ok(())
}
