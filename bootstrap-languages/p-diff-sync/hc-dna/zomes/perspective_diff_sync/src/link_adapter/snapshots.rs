//! HDK-side shim onto the algorithm-crate `snapshots` module.
//!
//! Step 13b-D (wake-16): the snapshot-generation algorithm now lives
//! in `perspective_diff_algorithm::snapshots`, generic over the
//! `SnapshotRetriever` trait. This module exists purely so legacy
//! callers (`commit.rs`) keep their existing
//! `link_adapter::snapshots::generate_snapshot(...)` import.
//!
//! The HDK adapter:
//! 1. Converts the HoloHash `latest` argument to the algo `Hash` mirror.
//! 2. Reads `*CHUNK_SIZE` from the lazy_static config (which the
//!    algorithm crate can't see, by design).
//! 3. Calls `algo::generate_snapshot::<HolochainRetreiver>` — all real
//!    work happens substrate-agnostically there.
//! 4. Converts the returned mirror `Snapshot` back to the integrity-zome
//!    `Snapshot` so the caller can write it via `EntryTypes::Snapshot`.

use hdk::prelude::*;
use perspective_diff_algorithm as algo;
use perspective_diff_sync_integrity::Snapshot;

use crate::errors::SocialContextResult;
use crate::link_adapter::conversions::{hash_to_algo, snapshot_from_algo};
use crate::retriever::HolochainRetreiver;
use crate::CHUNK_SIZE;

pub fn generate_snapshot(
    latest: HoloHash<holo_hash::hash_type::Action>,
) -> SocialContextResult<Snapshot> {
    let algo_snapshot =
        algo::generate_snapshot::<HolochainRetreiver>(hash_to_algo(&latest), *CHUNK_SIZE)?;
    Ok(snapshot_from_algo(algo_snapshot))
}
