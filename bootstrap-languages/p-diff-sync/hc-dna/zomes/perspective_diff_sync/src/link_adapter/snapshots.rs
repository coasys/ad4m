//! HDK-side shim onto the algorithm-crate `snapshots` module.

use hdk::prelude::*;
use perspective_diff_algorithm as algo;
use perspective_diff_sync_integrity::Snapshot;

use crate::errors::SocialContextResult;
use crate::retriever::HolochainRetreiver;
use crate::CHUNK_SIZE;

pub fn generate_snapshot(
    latest: HoloHash<holo_hash::hash_type::Action>,
) -> SocialContextResult<Snapshot> {
    Ok(algo::generate_snapshot::<HolochainRetreiver>(latest, *CHUNK_SIZE)?)
}
