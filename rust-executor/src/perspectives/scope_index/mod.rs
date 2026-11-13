//! The scope index: ordered member lists of model classes, kept in the
//! store's auxiliary key-value space (oxigraph feature `aux-kv`).
//!
//! A **scope** is a set of instances of one class:
//!
//! - [`Scope::Class`]: every instance of class C.
//! - [`Scope::Under`]: every instance of C that a node X links to with
//!   predicate R (`<X> <R> <member>`), for example the messages of a channel.
//!
//! **Membership is the flag link.** A node is an instance of C when a link
//! with a verified signature asserts C's flag triple, for example
//! `<s> flux://entry_type flux://post`. A class without a flag (see
//! [`writer::ClassRule::from_shape`]) is not indexed. A node is in
//! `Under(C, X, R)` when it is an instance of C and a link with a verified
//! signature asserts `<X> <R> <node>`.
//!
//! **The member key** is `(t_ms << 20) | seq` ([`MemberKey`]): `t_ms` is the
//! time of the member's earliest verified flag link and `seq` is a store-wide
//! counter, so two members never share a key. Ascending keys are oldest
//! first; newest first is a reverse scan. When a class already has a member
//! at every `seq` of one millisecond, a new member of that millisecond gets a
//! key in a near millisecond and counts as noncanonical, so the class is read
//! from SPARQL while it is a member.
//!
//! **One commit.** Every write path of [`super::sparql_store::SparqlStore`]
//! calls [`writer::RuleTable::apply`] on the transaction that writes the
//! quads, so the quads and the index commit together or not at all.
//!
//! **Readiness.** The first build of a class over the links already in the
//! store runs in the background in batches ([`build`]). Each batch records
//! where it stopped in the class's build mark, so a build that a restart
//! interrupted goes on from there. A class's index is read only when its mark
//! is ready for the class's current flag.
//!
//! # Key layout in the auxiliary space
//!
//! | Key | Value | Removed with |
//! |---|---|---|
//! | `E` scope:16 key:8 (big-endian) | member IRI | single delete |
//! | `M` member:16 scope:16 | key:8 class:16 | single delete |
//! | `C` scope:16 | i64 counter (merge) | delete |
//! | `F` member:16 class:16 | key:8 noncanonical:1 | single delete |
//! | `B` class:16 | build mark ([`build::BuildMark`]) | delete |
//! | `V` 0 | layout version | delete |
//! | `V` 1 | i64 counter: the last `seq` | delete |
//!
//! RocksDB does not allow a `SingleDelete` and a `Delete` on one key, so each
//! kind of key uses one of them only. `E`, `M` and `F` keys have one put per
//! membership, so they go with a single delete. Counters, marks and the
//! version change in place, so they go with a delete.
//!
//! `scope`, `member` and `class` are 128-bit BLAKE3 hashes of their canonical
//! descriptions ([`Scope::id`], [`member_hash`], [`ClassId::of`]).

pub mod build;
pub mod compact;
pub mod writer;

#[cfg(test)]
mod build_tests;
#[cfg(test)]
mod keys_tests;
#[cfg(test)]
pub(crate) mod test_support;
#[cfg(test)]
mod writer_tests;

use deno_core::anyhow::{anyhow, Error};
use oxigraph::store::Store;
use std::collections::HashSet;
use std::sync::{Arc, RwLock};

use build::{BuildMark, BuildState};
use compact::Compactor;
use writer::{ClassRule, RuleTable};

/// Key prefix of a scope entry: `E | scope | key` → member IRI.
pub(crate) const ENTRY: u8 = b'E';
/// Key prefix of a member's scope: `M | member | scope` → key and class.
pub(crate) const MEMBER: u8 = b'M';
/// Key prefix of a scope's member count: `C | scope` → i64 counter.
pub(crate) const COUNT: u8 = b'C';
/// Key prefix of a member's flag time: `F | member | class` → key and the
/// noncanonical marker.
pub(crate) const FLAG: u8 = b'F';
/// Key prefix of a class's build mark: `B | class` → [`BuildMark`].
pub(crate) const BUILD: u8 = b'B';
/// Key prefix of the layout version and the `seq` counter.
pub(crate) const VERSION: u8 = b'V';

/// The layout this code reads and writes. A store with another version is
/// not read or written, and its build marks are removed (see
/// [`ScopeIndex::open`]).
pub(crate) const LAYOUT_VERSION: u8 = 1;
pub(crate) const VERSION_KEY: [u8; 2] = [VERSION, 0];
pub(crate) const SEQ_KEY: [u8; 2] = [VERSION, 1];

/// Bits of `seq` in a [`MemberKey`].
pub const SEQ_BITS: u32 = 20;
const SEQ_MASK: u64 = (1 << SEQ_BITS) - 1;
/// The largest `t_ms` a key holds: `t_ms << 20` must fit in 64 bits. That is
/// the year 2527.
pub const MAX_T_MS: u64 = (1 << (64 - SEQ_BITS)) - 1;

/// 128-bit BLAKE3 hash of `parts`, each followed by a NUL, so that
/// `["ab", "c"]` and `["a", "bc"]` hash differently.
fn hash16(parts: &[&str]) -> [u8; 16] {
    let mut hasher = blake3::Hasher::new();
    for part in parts {
        hasher.update(part.as_bytes());
        hasher.update(&[0]);
    }
    let mut out = [0u8; 16];
    out.copy_from_slice(&hasher.finalize().as_bytes()[..16]);
    out
}

/// The identity of a scope: a 128-bit hash of its canonical description.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct ScopeId(pub [u8; 16]);

/// The identity of a class: a 128-bit hash of its target class IRI.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct ClassId(pub [u8; 16]);

impl ClassId {
    pub fn of(class: &str) -> Self {
        ClassId(hash16(&["class-id", class]))
    }
}

/// A member's position in every scope of one class: `(t_ms << 20) | seq`.
/// Ascending order is oldest first.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct MemberKey(pub u64);

impl MemberKey {
    /// The key for `t_ms` (clamped to [`MAX_T_MS`]) and the low 20 bits of `seq`.
    pub fn new(t_ms: u64, seq: u64) -> Self {
        MemberKey((t_ms.min(MAX_T_MS) << SEQ_BITS) | (seq & SEQ_MASK))
    }

    pub fn t_ms(self) -> u64 {
        self.0 >> SEQ_BITS
    }

    pub fn seq(self) -> u64 {
        self.0 & SEQ_MASK
    }

    pub fn to_bytes(self) -> [u8; 8] {
        self.0.to_be_bytes()
    }

    pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
        Some(MemberKey(u64::from_be_bytes(bytes.try_into().ok()?)))
    }
}

/// A set of instances of one class. `class` is the class's target class IRI
/// (`ModelShape::target_class`).
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Scope {
    /// Every instance of `class`.
    Class { class: String },
    /// Every instance of `class` that `parent` links to with `relation`.
    Under {
        class: String,
        parent: String,
        relation: String,
    },
    // Follow-ons: Field, Author, Word, Range, Some, LinkOrder.
}

impl Scope {
    /// The hash of the scope's canonical description.
    pub fn id(&self) -> ScopeId {
        match self {
            Scope::Class { class } => ScopeId(hash16(&["class", class])),
            Scope::Under {
                class,
                parent,
                relation,
            } => ScopeId(hash16(&["under", class, parent, relation])),
        }
    }

    pub fn class(&self) -> &str {
        match self {
            Scope::Class { class } | Scope::Under { class, .. } => class,
        }
    }
}

/// The counter of a class's members whose flag time is not a canonical
/// timestamp (see [`parse_timestamp`]). It has no entries, only a count.
pub(crate) fn noncanonical_scope(class: &str) -> ScopeId {
    ScopeId(hash16(&["noncanonical", class]))
}

/// The 128-bit hash of a member IRI in `M` and `F` keys.
pub(crate) fn member_hash(member: &str) -> [u8; 16] {
    hash16(&["member", member])
}

pub(crate) fn entry_prefix(scope: ScopeId) -> Vec<u8> {
    let mut k = Vec::with_capacity(25);
    k.push(ENTRY);
    k.extend_from_slice(&scope.0);
    k
}

pub(crate) fn entry_key(scope: ScopeId, key: MemberKey) -> Vec<u8> {
    let mut k = entry_prefix(scope);
    k.extend_from_slice(&key.to_bytes());
    k
}

pub(crate) fn member_scope_key(member: &[u8; 16], scope: ScopeId) -> Vec<u8> {
    let mut k = Vec::with_capacity(33);
    k.push(MEMBER);
    k.extend_from_slice(member);
    k.extend_from_slice(&scope.0);
    k
}

pub(crate) fn count_key(scope: ScopeId) -> Vec<u8> {
    let mut k = Vec::with_capacity(17);
    k.push(COUNT);
    k.extend_from_slice(&scope.0);
    k
}

pub(crate) fn flag_key(member: &[u8; 16], class: ClassId) -> Vec<u8> {
    let mut k = Vec::with_capacity(33);
    k.push(FLAG);
    k.extend_from_slice(member);
    k.extend_from_slice(&class.0);
    k
}

pub(crate) fn build_key(class: ClassId) -> Vec<u8> {
    let mut k = Vec::with_capacity(17);
    k.push(BUILD);
    k.extend_from_slice(&class.0);
    k
}

/// The first key after every key that starts with `prefix`, as the exclusive
/// upper bound of a scan. Empty (no upper bound) when `prefix` is all `0xff`.
pub(crate) fn prefix_end(prefix: &[u8]) -> Vec<u8> {
    let mut end = prefix.to_vec();
    while let Some(last) = end.pop() {
        if last < 0xff {
            end.push(last + 1);
            return end;
        }
    }
    Vec::new()
}

/// A member's time from a stored timestamp, and whether the timestamp is
/// canonical.
///
/// Canonical is the form the executor writes:
/// `YYYY-MM-DDTHH:MM:SS.mmmZ` (`to_rfc3339_opts(Millis, true)`), in
/// `[1970, 2527]`. The SPARQL path orders instances by the string of their
/// earliest timestamp. Canonical strings sort as their times do, so the index
/// order equals it for them. Any other string reads as its RFC 3339 time, or 0
/// when it is not RFC 3339, and is reported as not canonical: a class with
/// such a member is not read from the index (see [`ScopeReader::noncanonical`]).
pub fn parse_timestamp(ts: &str) -> (u64, bool) {
    use chrono::{DateTime, SecondsFormat, Utc};
    let Ok(parsed) = DateTime::parse_from_rfc3339(ts) else {
        return (0, false);
    };
    let utc = parsed.with_timezone(&Utc);
    let ms = utc.timestamp_millis();
    if ms < 0 {
        return (0, false);
    }
    let ms = ms as u64;
    let canonical = ms <= MAX_T_MS && utc.to_rfc3339_opts(SecondsFormat::Millis, true) == ts;
    (ms.min(MAX_T_MS), canonical)
}

/// An opaque position in a scope: the key of the last member a page returned.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Cursor(pub MemberKey);

impl Cursor {
    /// The cursor as text: 16 hex digits.
    pub fn to_token(self) -> String {
        format!("{:016x}", self.0 .0)
    }

    pub fn parse(token: &str) -> Result<Self, Error> {
        if token.len() != 16 {
            return Err(anyhow!("scope cursor must be 16 hex digits: '{token}'"));
        }
        u64::from_str_radix(token, 16)
            .map(|k| Cursor(MemberKey(k)))
            .map_err(|_| anyhow!("scope cursor must be 16 hex digits: '{token}'"))
    }
}

/// One page of a scope.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ScopePage {
    /// Members with their keys, in the order of the read.
    pub members: Vec<(String, MemberKey)>,
    /// The cursor for the next page in the same direction, when this page is
    /// full: there can be more.
    pub after: Option<Cursor>,
    /// The greatest key on the page: the cursor for a later
    /// [`ScopeReader::newer_than`].
    pub newest: Option<Cursor>,
}

impl ScopePage {
    fn from_entries(members: Vec<(String, MemberKey)>, limit: usize) -> Self {
        let after = if limit > 0 && members.len() == limit {
            members.last().map(|(_, k)| Cursor(*k))
        } else {
            None
        };
        let newest = members.iter().map(|(_, k)| *k).max().map(Cursor);
        ScopePage {
            members,
            after,
            newest,
        }
    }
}

/// Reads scopes from a store's auxiliary space. Every read is a seek and a
/// scan of the entries it returns; none depends on the size of the scope.
pub struct ScopeReader<'a> {
    store: &'a Store,
}

impl<'a> ScopeReader<'a> {
    pub fn new(store: &'a Store) -> Self {
        ScopeReader { store }
    }

    /// The entries of `scope` with keys in `[lower, upper)`, newest first
    /// when `reverse`. Each item is the member and its key.
    pub(crate) fn entries(
        &self,
        scope: ScopeId,
        lower: Option<MemberKey>,
        upper: Option<MemberKey>,
        reverse: bool,
    ) -> impl Iterator<Item = Result<(String, MemberKey), Error>> + 'a {
        let prefix = entry_prefix(scope);
        let lo = lower.map(|k| entry_key(scope, k)).unwrap_or(prefix.clone());
        let hi = upper
            .map(|k| entry_key(scope, k))
            .unwrap_or_else(|| prefix_end(&prefix));
        self.store.aux_scan(&lo, &hi, reverse).map(|item| {
            let (key, value) = item?;
            let member_key = key
                .get(17..25)
                .and_then(MemberKey::from_bytes)
                .ok_or_else(|| anyhow!("scope index: malformed entry key"))?;
            let member = String::from_utf8(value)
                .map_err(|_| anyhow!("scope index: entry value is not UTF-8"))?;
            Ok((member, member_key))
        })
    }

    fn collect(
        iter: impl Iterator<Item = Result<(String, MemberKey), Error>>,
        limit: usize,
    ) -> Result<ScopePage, Error> {
        let members = iter.take(limit).collect::<Result<Vec<_>, _>>()?;
        Ok(ScopePage::from_entries(members, limit))
    }

    /// The newest `limit` members, or the newest `limit` older than `after`:
    /// a reverse scan from the end of the scope or from the cursor.
    pub fn page(
        &self,
        scope: &Scope,
        after: Option<Cursor>,
        limit: usize,
    ) -> Result<ScopePage, Error> {
        Self::collect(
            self.entries(scope.id(), None, after.map(|c| c.0), true),
            limit,
        )
    }

    /// Like [`Self::page`], restricted to members whose time is in
    /// `[since_ms, until_ms)`.
    pub fn window(
        &self,
        scope: &Scope,
        since_ms: u64,
        until_ms: u64,
        after: Option<Cursor>,
        limit: usize,
    ) -> Result<ScopePage, Error> {
        if since_ms >= until_ms {
            return Ok(ScopePage::default());
        }
        let lower = MemberKey::new(since_ms, 0);
        let mut upper = (until_ms <= MAX_T_MS).then(|| MemberKey::new(until_ms, 0));
        if let Some(Cursor(c)) = after {
            upper = Some(upper.map_or(c, |u| u.min(c)));
        }
        Self::collect(self.entries(scope.id(), Some(lower), upper, true), limit)
    }

    /// The oldest `limit` members newer than `key`, oldest first: catching up
    /// from the newest key a client has seen. Call again with `after` while
    /// it is set.
    pub fn newer_than(&self, scope: &Scope, key: Cursor, limit: usize) -> Result<ScopePage, Error> {
        let Some(next) = key.0 .0.checked_add(1) else {
            return Ok(ScopePage::default());
        };
        Self::collect(
            self.entries(scope.id(), Some(MemberKey(next)), None, false),
            limit,
        )
    }

    /// The number of members of `scope`.
    pub fn count(&self, scope: &Scope) -> Result<i64, Error> {
        Ok(self.store.aux_count(&count_key(scope.id()))?)
    }

    /// Whether `member` is in `scope`.
    pub fn contains(&self, scope: &Scope, member: &str) -> Result<bool, Error> {
        Ok(self
            .store
            .aux_get(&member_scope_key(&member_hash(member), scope.id()))?
            .is_some())
    }

    /// The newest `per_parent` members of each scope, in the order given: one
    /// seek each. The string names the parent for the caller.
    pub fn children(
        &self,
        parents: &[(String, Scope)],
        per_parent: usize,
    ) -> Result<Vec<ScopePage>, Error> {
        parents
            .iter()
            .map(|(_, scope)| self.page(scope, None, per_parent))
            .collect()
    }

    /// How many members of `class` have a flag time that is not canonical.
    pub fn noncanonical(&self, class: &str) -> Result<i64, Error> {
        Ok(self
            .store
            .aux_count(&count_key(noncanonical_scope(class)))?)
    }
}

/// Whether a class's index can be read.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Readiness {
    /// The class has no flag, or the index is off: it is not indexed.
    NotIndexed,
    /// The first build (or a rebuild for a changed flag) has not finished.
    Building,
    Ready,
}

/// What an executor keeps in memory about its store's scope index: the rule
/// table, which classes are ready, and the compaction tracker. Shared by every
/// clone of a [`super::sparql_store::SparqlStore`].
pub struct ScopeIndex {
    state: RwLock<IndexState>,
    /// The builds waiting and running: one at a time per store.
    pub(crate) builds: std::sync::Mutex<build::BuildQueue>,
    /// Classes whose members with a non-canonical flag time were logged.
    noncanonical_logged: std::sync::Mutex<HashSet<ClassId>>,
    pub(crate) compactor: Arc<Compactor>,
    /// False when the store has another layout version: then nothing reads or
    /// writes the auxiliary space.
    enabled: bool,
}

#[derive(Default)]
struct IndexState {
    rules: Arc<RuleTable>,
    ready: HashSet<ClassId>,
}

impl ScopeIndex {
    /// Open the index of `store`: write the layout version into a new store,
    /// and load the rule table and readiness from the build marks. A store
    /// with another layout version gets the index off and loses its build
    /// marks.
    pub fn open(store: &Store) -> Result<Self, Error> {
        match store.aux_get(&VERSION_KEY)? {
            None => {
                let mut tx = store.start_transaction()?;
                tx.aux_put(&VERSION_KEY, &[LAYOUT_VERSION]);
                tx.commit()?;
            }
            Some(v) if v == [LAYOUT_VERSION] => {}
            Some(v) => {
                log::error!(
                    "Scope index: layout version {v:?} is not {LAYOUT_VERSION}; the index is off \
                     for this store and every query uses the SPARQL path"
                );
                // This executor writes links with no index upkeep. Remove
                // every build mark, so an executor of the stored layout does
                // not read its index as ready: with no mark, the class is
                // built again when a shape registers it.
                let prefix = [BUILD];
                let marks = store
                    .aux_scan(&prefix, &prefix_end(&prefix), false)
                    .map(|item| item.map(|(key, _)| key))
                    .collect::<Result<Vec<_>, _>>()?;
                if !marks.is_empty() {
                    let mut tx = store.start_transaction()?;
                    for key in &marks {
                        tx.aux_delete(key);
                    }
                    tx.commit()?;
                }
                return Ok(ScopeIndex {
                    state: RwLock::new(IndexState::default()),
                    builds: Default::default(),
                    noncanonical_logged: Default::default(),
                    compactor: Arc::new(Compactor::default()),
                    enabled: false,
                });
            }
        }
        let marks = build::load_marks(store)?;
        let mut ready = HashSet::new();
        let mut rules = Vec::new();
        for mark in marks {
            if matches!(mark.state, BuildState::Ready) {
                ready.insert(mark.rule.id);
            }
            rules.push(mark.rule);
        }
        Ok(ScopeIndex {
            state: RwLock::new(IndexState {
                rules: Arc::new(RuleTable::new(rules)),
                ready,
            }),
            builds: Default::default(),
            noncanonical_logged: Default::default(),
            compactor: Arc::new(Compactor::default()),
            enabled: true,
        })
    }

    pub fn enabled(&self) -> bool {
        self.enabled
    }

    /// The current rule table. Writers read it under the store's writer lock.
    pub fn rules(&self) -> Arc<RuleTable> {
        self.state
            .read()
            .map(|s| s.rules.clone())
            .unwrap_or_default()
    }

    /// Whether `rule`'s class is indexed, under this same rule, and ready.
    pub fn readiness(&self, rule: &ClassRule) -> Readiness {
        if !self.enabled {
            return Readiness::NotIndexed;
        }
        let Ok(state) = self.state.read() else {
            return Readiness::NotIndexed;
        };
        match state.rules.get(rule.id) {
            Some(r) if r == rule && state.ready.contains(&rule.id) => Readiness::Ready,
            Some(r) if r == rule => Readiness::Building,
            _ => Readiness::NotIndexed,
        }
    }

    /// Put `rule` into the table, not ready. The caller holds the writer lock
    /// and has written the class's new build mark in the same step.
    pub(crate) fn install(&self, rule: ClassRule) {
        if let Ok(mut state) = self.state.write() {
            state.ready.remove(&rule.id);
            state.rules = Arc::new(state.rules.with(rule));
        }
    }

    /// Mark `class` ready when its rule is still `rule`.
    pub(crate) fn set_ready(&self, rule: &ClassRule) {
        if let Ok(mut state) = self.state.write() {
            if state.rules.get(rule.id) == Some(rule) {
                state.ready.insert(rule.id);
            }
        }
    }

    /// Whether this is the first time `rule`'s class is seen with members
    /// whose flag time is not canonical, so the caller logs it once.
    pub(crate) fn first_noncanonical(&self, rule: &ClassRule) -> bool {
        self.noncanonical_logged
            .lock()
            .map(|mut logged| logged.insert(rule.id))
            .unwrap_or(false)
    }

    /// The classes whose build has not finished, as their marks say.
    pub fn unfinished(&self, store: &Store) -> Result<Vec<ClassRule>, Error> {
        if !self.enabled {
            return Ok(Vec::new());
        }
        Ok(build::load_marks(store)?
            .into_iter()
            .filter(|m| !matches!(m.state, BuildState::Ready))
            .map(|m| m.rule)
            .collect())
    }

    /// Mark every class not ready, in the store and here, before
    /// [`Store::clear`] runs: if the process stops before [`Self::clear`]
    /// ends, the builds at the next open remove what the clear left. The
    /// caller holds the writer lock.
    pub(crate) fn begin_clear(&self, store: &Store) -> Result<(), Error> {
        if !self.enabled {
            return Ok(());
        }
        let marks = build::load_marks(store)?;
        let mut tx = store.start_transaction()?;
        for mark in &marks {
            build::write_mark(
                &mut tx,
                &BuildMark {
                    rule: mark.rule.clone(),
                    state: BuildState::Building {
                        phase: 0,
                        cursor: Vec::new(),
                    },
                },
            );
        }
        tx.commit()?;
        if let Ok(mut state) = self.state.write() {
            state.ready.clear();
        }
        Ok(())
    }

    /// Empty the index after [`Store::clear`]: remove every entry, member,
    /// flag time and count, and mark every class ready (an empty store is
    /// fully indexed). The caller holds the writer lock.
    pub(crate) fn clear(&self, store: &Store) -> Result<(), Error> {
        if !self.enabled {
            return Ok(());
        }
        let all = store
            .aux_scan(&[], &[], false)
            .collect::<Result<Vec<_>, _>>()?;
        let mut tx = store.start_transaction()?;
        let mut marks = Vec::new();
        for (key, value) in &all {
            match key.first() {
                Some(&ENTRY) | Some(&MEMBER) | Some(&FLAG) => tx.aux_single_delete(key),
                Some(&COUNT) => tx.aux_delete(key),
                Some(&BUILD) => {
                    if let Ok(mark) = BuildMark::decode(value) {
                        marks.push(mark.rule);
                    }
                }
                _ => {}
            }
        }
        for rule in &marks {
            build::write_mark(
                &mut tx,
                &BuildMark {
                    rule: rule.clone(),
                    state: BuildState::Ready,
                },
            );
        }
        tx.commit()?;
        for rule in &marks {
            self.set_ready(rule);
        }
        Ok(())
    }
}
