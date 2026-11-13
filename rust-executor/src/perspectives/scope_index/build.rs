//! The first build of a class's index over the links already in the store,
//! and its rebuild when the class's flag changes.
//!
//! A build runs in batches. Each batch holds the store's writer lock, checks
//! at most [`BATCH`] members with the same code as every write
//! ([`Apply::reconcile_member`]), and writes the class's build mark, all in
//! one transaction. A batch also stops after [`BATCH_TIME`], and the build
//! waits [`BATCH_PAUSE`] without the lock before the next one, so a link
//! write that waits for the lock gets it between two batches. The mark says
//! where the next batch starts, so a build that a restart stops goes on from
//! there when the store opens again.
//!
//! A store runs one build at a time ([`BuildQueue`]); the others wait.
//!
//! The rule is in the table from the start of the build, so every write
//! during the build updates the class like any other: the build only has to
//! visit the members that were there before it.
//!
//! Phases:
//! 0. Members the class scope already holds, in key order: under a changed
//!    flag some of them are no longer members.
//! 1. Subjects of the flag triple, in IRI order, from a snapshot taken when
//!    the phase starts.
//!
//! The batch that ends phase 1 writes the mark `Ready`.

use deno_core::anyhow::{anyhow, Error};
use oxigraph::model::{NamedNode, NamedNodeRef, NamedOrBlankNode, TermRef};
use oxigraph::store::{Store, Transaction};
use std::collections::VecDeque;
use std::sync::Mutex;
use std::time::{Duration, Instant};

use super::writer::{class_members_after, Apply, ClassRule};
use super::{build_key, prefix_end, MemberKey, ScopeIndex, BUILD};

/// Members per batch: one transaction, one hold of the writer lock.
pub const BATCH: usize = 512;
/// The longest one batch holds the writer lock. It stops after the member
/// that passes this time, with fewer than [`BATCH`] members when they are
/// slow.
pub const BATCH_TIME: Duration = Duration::from_millis(50);
/// The wait without the lock after a batch. `std::sync::Mutex` is not fair:
/// without a wait the build takes the lock again before a waiting writer.
pub const BATCH_PAUSE: Duration = Duration::from_millis(5);

/// How long a batch may hold the writer lock, and the wait after it.
#[derive(Clone, Copy, Debug)]
pub struct BatchLimits {
    pub time: Duration,
    pub pause: Duration,
}

impl Default for BatchLimits {
    fn default() -> Self {
        BatchLimits {
            time: BATCH_TIME,
            pause: BATCH_PAUSE,
        }
    }
}

/// The builds of one store: one runs, the others wait in order.
#[derive(Default, Debug)]
pub struct BuildQueue {
    waiting: VecDeque<ClassRule>,
    running: Option<ClassRule>,
    /// A worker thread takes builds from the queue.
    worker: bool,
}

impl BuildQueue {
    /// Queue `rule`'s build, unless it waits or runs already. A waiting build
    /// of the same class under another rule is replaced. Returns whether the
    /// caller must start a worker.
    pub fn push(&mut self, rule: ClassRule) -> bool {
        if self.contains(&rule) {
            return false;
        }
        match self.waiting.iter_mut().find(|r| r.id == rule.id) {
            Some(waiting) => *waiting = rule,
            None => self.waiting.push_back(rule),
        }
        !std::mem::replace(&mut self.worker, true)
    }

    /// The worker's next build. `None` when the queue is empty: then the
    /// worker stops, and the next push starts a new one.
    pub fn take_next(&mut self) -> Option<ClassRule> {
        self.running = self.waiting.pop_front();
        if self.running.is_none() {
            self.worker = false;
        }
        self.running.clone()
    }

    /// The worker could not start: drop the waiting builds.
    pub fn abandon(&mut self) {
        self.waiting.clear();
        self.running = None;
        self.worker = false;
    }

    /// Whether `rule`'s build waits or runs.
    pub fn contains(&self, rule: &ClassRule) -> bool {
        self.running.as_ref() == Some(rule) || self.waiting.contains(rule)
    }
}

const MARK_VERSION: u8 = 1;

/// Where a class's build is.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BuildState {
    /// Not finished: the next batch starts in `phase` after `cursor` (a
    /// member key in phase 0, a member IRI in phase 1; empty at the start).
    Building {
        phase: u8,
        cursor: Vec<u8>,
    },
    Ready,
}

/// A class's build mark: the rule the class is indexed under, and the state
/// of its build under that rule.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BuildMark {
    pub rule: ClassRule,
    pub state: BuildState,
}

fn put_bytes(out: &mut Vec<u8>, bytes: &[u8]) {
    out.extend_from_slice(&(bytes.len() as u32).to_be_bytes());
    out.extend_from_slice(bytes);
}

fn take_bytes<'b>(input: &mut &'b [u8]) -> Result<&'b [u8], Error> {
    let len_bytes: [u8; 4] = input
        .get(..4)
        .and_then(|b| b.try_into().ok())
        .ok_or_else(|| anyhow!("build mark: truncated"))?;
    let len = u32::from_be_bytes(len_bytes) as usize;
    let bytes = input
        .get(4..4 + len)
        .ok_or_else(|| anyhow!("build mark: truncated"))?;
    *input = &input[4 + len..];
    Ok(bytes)
}

fn take_string(input: &mut &[u8]) -> Result<String, Error> {
    String::from_utf8(take_bytes(input)?.to_vec()).map_err(|_| anyhow!("build mark: not UTF-8"))
}

impl BuildMark {
    pub fn encode(&self) -> Vec<u8> {
        let mut out = vec![MARK_VERSION];
        match &self.state {
            BuildState::Ready => {
                out.push(1);
                out.push(0);
                put_bytes(&mut out, &[]);
            }
            BuildState::Building { phase, cursor } => {
                out.push(0);
                out.push(*phase);
                put_bytes(&mut out, cursor);
            }
        }
        put_bytes(&mut out, self.rule.class.as_bytes());
        put_bytes(&mut out, self.rule.flag_predicate.as_bytes());
        put_bytes(&mut out, self.rule.flag_value.as_bytes());
        out
    }

    pub fn decode(bytes: &[u8]) -> Result<Self, Error> {
        let [version, state, phase, rest @ ..] = bytes else {
            return Err(anyhow!("build mark: truncated"));
        };
        if *version != MARK_VERSION {
            return Err(anyhow!(
                "build mark: version {version} is not {MARK_VERSION}"
            ));
        }
        let mut rest = rest;
        let cursor = take_bytes(&mut rest)?.to_vec();
        let class = take_string(&mut rest)?;
        let predicate = take_string(&mut rest)?;
        let value = take_string(&mut rest)?;
        let state = match state {
            1 => BuildState::Ready,
            0 => BuildState::Building {
                phase: *phase,
                cursor,
            },
            other => return Err(anyhow!("build mark: unknown state {other}")),
        };
        Ok(BuildMark {
            rule: ClassRule::new(&class, &predicate, &value),
            state,
        })
    }
}

/// Write `mark` in `tx`. `B` keys are only ever removed with a delete (not a
/// single delete), so a put over an earlier mark is allowed.
pub(crate) fn write_mark(tx: &mut Transaction<'_>, mark: &BuildMark) {
    tx.aux_put(&build_key(mark.rule.id), &mark.encode());
}

/// Every build mark in the store. A mark that does not decode is logged and
/// left out: its class is not indexed until a shape registers it again.
pub fn load_marks(store: &Store) -> Result<Vec<BuildMark>, Error> {
    let prefix = [BUILD];
    let mut marks = Vec::new();
    for item in store.aux_scan(&prefix, &prefix_end(&prefix), false) {
        let (key, value) = item?;
        match BuildMark::decode(&value) {
            Ok(mark) => marks.push(mark),
            Err(e) => log::warn!(
                "Scope index: skipping build mark {}: {e}",
                hex::encode(&key)
            ),
        }
    }
    Ok(marks)
}

/// What [`build_class`] did.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BuildProgress {
    /// The class is ready.
    Done,
    /// `max_batches` batches ran and the build is not finished.
    Paused,
    /// The class's mark names another rule now: a newer build owns it.
    Superseded,
    /// The store went away.
    Stopped,
}

/// The subjects of the flag triple of `rule`, as text, sorted.
fn flag_subjects(store: &Store, rule: &ClassRule) -> Result<Vec<String>, Error> {
    let mut out = Vec::new();
    for quad in store.quads_for_pattern(
        None,
        Some(NamedNodeRef::new_unchecked(&rule.flag_predicate)),
        Some(TermRef::NamedNode(NamedNodeRef::new_unchecked(
            &rule.flag_value,
        ))),
        Some(oxigraph::model::GraphNameRef::DefaultGraph),
    ) {
        if let NamedOrBlankNode::NamedNode(s) = quad?.subject {
            out.push(NamedNode::into_string(s));
        }
    }
    out.sort();
    out.dedup();
    Ok(out)
}

/// Run `rule`'s build from its mark: at most `max_batches` batches, or to the
/// end with `None`. Marks the class ready in `index` when it ends.
pub fn build_class(
    store: &Store,
    writer: &Mutex<()>,
    index: &ScopeIndex,
    rule: &ClassRule,
    max_batches: Option<usize>,
) -> Result<BuildProgress, Error> {
    build_class_with(
        store,
        writer,
        index,
        rule,
        max_batches,
        BatchLimits::default(),
    )
}

/// [`build_class`] with other batch limits.
pub fn build_class_with(
    store: &Store,
    writer: &Mutex<()>,
    index: &ScopeIndex,
    rule: &ClassRule,
    max_batches: Option<usize>,
    limits: BatchLimits,
) -> Result<BuildProgress, Error> {
    let mut batches = 0usize;
    // Phase 1's snapshot of flag subjects, taken once per call.
    let mut subjects: Option<Vec<String>> = None;
    loop {
        if max_batches.is_some_and(|max| batches >= max) {
            return Ok(BuildProgress::Paused);
        }
        if batches > 0 && !limits.pause.is_zero() {
            // Without the lock: a waiting writer takes it now.
            std::thread::sleep(limits.pause);
        }
        let in_phase_one = matches!(
            store
                .aux_get(&build_key(rule.id))?
                .map(|v| BuildMark::decode(&v)),
            Some(Ok(BuildMark {
                state: BuildState::Building { phase: 1, .. },
                ..
            }))
        );
        if in_phase_one && subjects.is_none() {
            // Outside the lock: the scan reads a snapshot, and writes after it
            // update the class themselves.
            subjects = Some(flag_subjects(store, rule)?);
        }
        let _writer = writer
            .lock()
            .map_err(|_| anyhow!("SPARQL store writer lock poisoned"))?;
        let held = Instant::now();
        let mark = match store.aux_get(&build_key(rule.id))? {
            Some(v) => BuildMark::decode(&v)?,
            None => return Ok(BuildProgress::Superseded),
        };
        if mark.rule != *rule {
            return Ok(BuildProgress::Superseded);
        }
        let (phase, cursor) = match mark.state {
            BuildState::Ready => {
                index.set_ready(rule);
                return Ok(BuildProgress::Done);
            }
            BuildState::Building { phase, cursor } => (phase, cursor),
        };
        if phase == 1 && subjects.is_none() {
            // Phase 0 ended under another hold of the lock: take the snapshot
            // outside it.
            continue;
        }
        let mut tx = store.start_transaction()?;
        let next_state = {
            let mut ctx = Apply::new(&mut tx, store);
            match phase {
                0 => {
                    let after = MemberKey::from_bytes(&cursor);
                    let members = class_members_after(store, rule, after, BATCH)?;
                    let mut done = 0;
                    for (member, _) in &members {
                        ctx.reconcile_member(rule, member)?;
                        done += 1;
                        if held.elapsed() >= limits.time {
                            break;
                        }
                    }
                    match members[..done].last() {
                        Some((_, key)) if done < members.len() || done == BATCH => {
                            BuildState::Building {
                                phase: 0,
                                cursor: key.to_bytes().to_vec(),
                            }
                        }
                        _ => BuildState::Building {
                            phase: 1,
                            cursor: Vec::new(),
                        },
                    }
                }
                _ => {
                    let all = subjects.as_deref().unwrap_or_default();
                    let start = if cursor.is_empty() {
                        0
                    } else {
                        all.partition_point(|s| s.as_bytes() <= cursor.as_slice())
                    };
                    let batch = &all[start.min(all.len())..(start + BATCH).min(all.len())];
                    let mut done = 0;
                    for member in batch {
                        ctx.reconcile_member(rule, member)?;
                        done += 1;
                        if held.elapsed() >= limits.time {
                            break;
                        }
                    }
                    if start + done >= all.len() {
                        BuildState::Ready
                    } else {
                        BuildState::Building {
                            phase: 1,
                            cursor: batch[..done]
                                .last()
                                .map(|s| s.as_bytes().to_vec())
                                .unwrap_or_default(),
                        }
                    }
                }
            }
        };
        let ready = next_state == BuildState::Ready;
        write_mark(
            &mut tx,
            &BuildMark {
                rule: rule.clone(),
                state: next_state,
            },
        );
        tx.commit()?;
        batches += 1;
        if ready {
            index.set_ready(rule);
            return Ok(BuildProgress::Done);
        }
    }
}
