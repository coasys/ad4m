//! What the last writes to a [`SparqlStore`](super::sparql_store::SparqlStore)
//! added, so a reader that kept a query result can read only what changed
//! (the auto-processor's transcript, `auto_processor::delta_read`).
//!
//! The store records each write under the write generation it ends, in
//! [`WriteLog`]. A write that only added links records them as
//! [`AddedLink`]s; any other write (a removal, a link written again over
//! its own reifier, a clear or reload, a repair) records [`WriteDelta::Other`].
//! [`WriteLog::additions_since`] answers the links added after a generation,
//! or `None` when a write in between was not additions only, or when the log
//! no longer reaches back that far: the reader must then read everything
//! again.

use oxigraph::model::{NamedNode, Term};
use std::collections::VecDeque;

/// Most links the log keeps over all its generations. Older writes drop out;
/// a reader whose result predates them reads everything again. A write that
/// adds more than this many links is logged as [`WriteDelta::Other`].
pub(crate) const WRITE_LOG_MAX_LINKS: usize = 1024;

/// The terms of one added link as the store holds them: its direct triple
/// and its reifier. The reifier's own facts (author, timestamp, proof,
/// status) are all on `reifier`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct AddedLink {
    pub source: NamedNode,
    pub predicate: NamedNode,
    pub target: Term,
    pub reifier: NamedNode,
}

/// What one write did.
#[derive(Debug, Clone)]
pub(crate) enum WriteDelta {
    /// Only new links: no quad was removed, and each link's reifier was new.
    Added(Vec<AddedLink>),
    /// Anything else.
    Other,
}

#[derive(Debug, Default)]
pub(crate) struct WriteLog {
    /// `(generation, delta)`, oldest first, for every generation after `floor`.
    entries: VecDeque<(u64, WriteDelta)>,
    /// The generation the oldest kept entry follows.
    floor: u64,
    links: usize,
}

impl WriteLog {
    /// Record the write that ends `generation` (one past the last recorded).
    pub(crate) fn record(&mut self, generation: u64, delta: WriteDelta) {
        let delta = match delta {
            WriteDelta::Added(links) if links.len() > WRITE_LOG_MAX_LINKS => WriteDelta::Other,
            d => d,
        };
        if let WriteDelta::Added(links) = &delta {
            self.links += links.len();
        }
        self.entries.push_back((generation, delta));
        while self.links > WRITE_LOG_MAX_LINKS {
            let Some((dropped, delta)) = self.entries.pop_front() else {
                break;
            };
            if let WriteDelta::Added(links) = delta {
                self.links -= links.len();
            }
            self.floor = dropped;
        }
    }

    /// The links added by the writes after generation `from` up to and
    /// including `to`, oldest first. `None` when one of those writes was not
    /// additions only, or when the log no longer holds all of them.
    pub(crate) fn additions_since(&self, from: u64, to: u64) -> Option<Vec<AddedLink>> {
        if from < self.floor {
            return None;
        }
        let mut added = Vec::new();
        for (generation, delta) in &self.entries {
            if *generation <= from || *generation > to {
                continue;
            }
            match delta {
                WriteDelta::Added(links) => added.extend(links.iter().cloned()),
                WriteDelta::Other => return None,
            }
        }
        Some(added)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn link(i: usize) -> AddedLink {
        AddedLink {
            source: NamedNode::new_unchecked(format!("msg://{i}")),
            predicate: NamedNode::new_unchecked("ns://body"),
            target: Term::NamedNode(NamedNode::new_unchecked("ns://x")),
            reifier: NamedNode::new_unchecked(format!("link://{i}")),
        }
    }

    #[test]
    fn additions_since_answers_the_links_after_a_generation() {
        let mut log = WriteLog::default();
        log.record(1, WriteDelta::Added(vec![link(1)]));
        log.record(2, WriteDelta::Added(vec![link(2), link(3)]));
        assert_eq!(
            log.additions_since(0, 2),
            Some(vec![link(1), link(2), link(3)])
        );
        assert_eq!(log.additions_since(1, 2), Some(vec![link(2), link(3)]));
        assert_eq!(log.additions_since(2, 2), Some(vec![]));
        assert_eq!(
            log.additions_since(0, 1),
            Some(vec![link(1)]),
            "a write after `to` is not counted"
        );
    }

    #[test]
    fn any_other_write_in_between_means_read_everything() {
        let mut log = WriteLog::default();
        log.record(1, WriteDelta::Added(vec![link(1)]));
        log.record(2, WriteDelta::Other);
        log.record(3, WriteDelta::Added(vec![link(3)]));
        assert_eq!(log.additions_since(0, 3), None);
        assert_eq!(log.additions_since(1, 3), None);
        assert_eq!(log.additions_since(2, 3), Some(vec![link(3)]));
    }

    #[test]
    fn a_generation_the_log_dropped_means_read_everything() {
        let mut log = WriteLog::default();
        for g in 1..=(WRITE_LOG_MAX_LINKS as u64 + 10) {
            log.record(g, WriteDelta::Added(vec![link(g as usize)]));
        }
        let last = WRITE_LOG_MAX_LINKS as u64 + 10;
        assert_eq!(log.additions_since(0, last), None);
        assert_eq!(
            log.additions_since(last - 2, last).map(|l| l.len()),
            Some(2)
        );
        assert_eq!(
            log.additions_since(10, last).map(|l| l.len()),
            Some(WRITE_LOG_MAX_LINKS),
            "the oldest generation still covered"
        );
        assert_eq!(log.additions_since(9, last), None);

        let mut big = WriteLog::default();
        big.record(
            1,
            WriteDelta::Added((0..=WRITE_LOG_MAX_LINKS).map(link).collect()),
        );
        assert_eq!(big.additions_since(0, 1), None, "too large to log");
    }

    /// Each store write logs what it did: adding new links logs them; a
    /// removal, a link written again, a diff with removals and a clear (or a
    /// reload, which starts with one) log `Other`.
    #[test]
    fn each_store_write_logs_what_it_did() {
        use crate::agent::signatures::TestSigner;
        use crate::perspectives::sparql_store::SparqlStore;
        use crate::types::{Link, LinkExpression, LinkStatus};

        let signer = TestSigner::generate();
        let make = |i: usize| {
            let signed = signer.sign(
                Link {
                    source: format!("msg://{i}"),
                    predicate: Some("ns://body".into()),
                    target: format!("literal:string:turn {i}"),
                }
                .normalize(),
            );
            LinkExpression {
                author: signed.author,
                timestamp: signed.timestamp,
                data: signed.data,
                proof: signed.proof,
                status: Some(LinkStatus::Shared),
            }
        };
        let links: Vec<LinkExpression> = (0..7).map(make).collect();
        let make = |i: usize| links[i].clone();
        let store = SparqlStore::new(None).unwrap();
        let g = |s: &SparqlStore| s.write_generation();
        let since = |s: &SparqlStore, from: u64| s.additions_since(from, s.write_generation());

        let before = g(&store);
        store.add_link(&make(1)).unwrap();
        let added = since(&store, before).expect("an addition");
        assert_eq!(added.len(), 1);
        assert_eq!(added[0].source.as_str(), "msg://1");
        assert_eq!(added[0].predicate.as_str(), "ns://body");

        let before = g(&store);
        store.apply_diff(&[], &[make(2), make(3)]).unwrap();
        assert_eq!(since(&store, before).map(|l| l.len()), Some(2));

        let before = g(&store);
        store.add_link(&make(1)).unwrap();
        assert_eq!(since(&store, before), None, "a link written again");

        let before = g(&store);
        store.remove_link(&make(2)).unwrap();
        assert_eq!(since(&store, before), None, "a removal");

        let before = g(&store);
        store.apply_diff(&[make(3)], &[make(4)]).unwrap();
        assert_eq!(since(&store, before), None, "a diff with a removal");

        let before = g(&store);
        store.reload(vec![make(5)]).unwrap();
        assert_eq!(since(&store, before), None, "a reload");
        let after_reload = g(&store);
        store.add_link(&make(6)).unwrap();
        assert_eq!(
            since(&store, after_reload).map(|l| l.len()),
            Some(1),
            "additions after it are logged again"
        );
    }
}
