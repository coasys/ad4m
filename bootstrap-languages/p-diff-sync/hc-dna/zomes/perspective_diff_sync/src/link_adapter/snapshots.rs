use hdk::prelude::*;
use perspective_diff_sync_integrity::{
    LinkExpression, LinkTypes, PerspectiveDiff, PerspectiveDiffEntryReference, Snapshot,
};

use crate::errors::{SocialContextError, SocialContextResult};
use crate::link_adapter::chunked_diffs::ChunkedDiffs;
use crate::retriever::HolochainRetreiver;
use crate::utils::get_now;
use crate::{Hash, CHUNK_SIZE};

struct SearchPosition {
    hash: Hash,
    is_unseen: bool,
}

pub fn generate_snapshot(
    latest: HoloHash<holo_hash::hash_type::Action>,
) -> SocialContextResult<Snapshot> {
    debug!("===PerspectiveDiffSync.generate_snapshot(): Function start");
    let fn_start = get_now()?.time();
    let mut search_position = SearchPosition {
        hash: latest.clone(),
        is_unseen: false,
    };
    let mut seen: HashSet<Hash> = HashSet::new();
    let mut unseen_parents = vec![];

    let mut all_additions = BTreeSet::new();
    let mut all_removals = BTreeSet::new();

    loop {
        let diff = get(search_position.hash.clone(), GetOptions::latest())?
            .ok_or(SocialContextError::InternalError(
                "generate_snapshot(): Could not find entry while populating search",
            ))?
            .entry()
            .to_app_option::<PerspectiveDiffEntryReference>()?
            .ok_or(SocialContextError::InternalError(
                "Expected element to contain app entry data",
            ))?;
        if diff.diffs_since_snapshot == 0 && search_position.hash != latest {
            let now = get_now()?.time();
            let input = GetLinksInputBuilder::try_new(
                hash_entry(&diff)?,
                LinkTypes::Snapshot
            )
            .unwrap()
            .tag_prefix(LinkTag::new("snapshot"))
            .build();
            let mut snapshot_links = get_links(input)?;
            let after = get_now()?.time();
            debug!("===PerspectiveDiffSync.generate_snapshot() - Profiling: Took {} to get the snapshot links", (after - now).num_milliseconds());
            if snapshot_links.len() == 0 {
                debug!("===PerspectiveDiffSync.generate_snapshot() - ERROR: Did not find snapshot link where we expected to!");
                let should_break = handle_parents(
                    diff,
                    &mut search_position,
                    &mut seen,
                    &mut unseen_parents,
                    &mut all_additions,
                    &mut all_removals,
                )?;
                if should_break {
                    break;
                }
            } else {
                let now = get_now()?.time();
                //get snapshot and add elements to out
                let snapshot = get(
                    snapshot_links
                        .remove(0)
                        .target
                        .into_entry_hash()
                        .expect("Could not get entry_hash"),
                    GetOptions::latest(),
                )?
                .ok_or(SocialContextError::InternalError(
                    "Could not find diff entry for given diff entry reference",
                ))?
                .entry()
                .to_app_option::<Snapshot>()?
                .ok_or(SocialContextError::InternalError(
                    "Expected element to contain app entry data",
                ))?;
                let after = get_now()?.time();
                debug!("===PerspectiveDiffSync.generate_snapshot() - Profiling: Took {} to get the snapshot entry", (after - now).num_milliseconds());

                let diff = ChunkedDiffs::from_entries::<HolochainRetreiver>(snapshot.diff_chunks)?
                    .into_aggregated_diff();
                for addition in diff.additions.iter() {
                    all_additions.insert(addition.clone());
                }
                for removal in diff.removals.iter() {
                    all_removals.insert(removal.clone());
                }
                for hash in snapshot.included_diffs.iter() {
                    seen.insert(hash.clone());
                }
                //Be careful with break here where there are still unseen parents
                if unseen_parents.len() == 0 {
                    // debug!("No more unseen parents within snapshot block");
                    break;
                } else {
                    search_position = unseen_parents.remove(0);
                }
            };
        } else {
            let should_break = handle_parents(
                diff,
                &mut search_position,
                &mut seen,
                &mut unseen_parents,
                &mut all_additions,
                &mut all_removals,
            )?;
            if should_break {
                break;
            }
        }
    }

    let mut chunked_diffs = ChunkedDiffs::new(*CHUNK_SIZE);

    chunked_diffs.add_additions(all_additions.into_iter().collect());
    chunked_diffs.add_removals(all_removals.into_iter().collect());

    let snapshot = Snapshot {
        diff_chunks: chunked_diffs.into_entries::<HolochainRetreiver>()?,
        included_diffs: seen.into_iter().collect(),
    };

    let fn_end = get_now()?.time();
    debug!("===PerspectiveDiffSync.generate_snapshot() - Profiling: Took: {} to complete generate_snapshot function", (fn_end - fn_start).num_milliseconds());
    Ok(snapshot)
}

fn handle_parents(
    diff: PerspectiveDiffEntryReference,
    search_position: &mut SearchPosition,
    seen: &mut HashSet<Hash>,
    unseen_parents: &mut Vec<SearchPosition>,
    all_additions: &mut BTreeSet<LinkExpression>,
    all_removals: &mut BTreeSet<LinkExpression>,
) -> SocialContextResult<bool> {
    //Check if entry is already in graph
    if !seen.contains(&search_position.hash) {
        seen.insert(search_position.hash.clone());
        let diff_entry = get(diff.diff.clone(), GetOptions::latest())?
            .ok_or(SocialContextError::InternalError(
                "Could not find diff entry for given diff entry reference",
            ))?
            .entry()
            .to_app_option::<PerspectiveDiff>()?
            .ok_or(SocialContextError::InternalError(
                "Expected element to contain app entry data",
            ))?;

        for addition in diff_entry.additions.iter() {
            all_additions.insert(addition.clone());
        }
        for removal in diff_entry.removals.iter() {
            all_removals.insert(removal.clone());
        }

        if diff.parents.is_none() {
            //No parents, we have reached the end of the chain
            //Now move onto traversing unseen parents, or break if we dont have any other paths to search
            if unseen_parents.len() == 0 {
                // debug!("No more unseen items within parent block");
                Ok(true)
            } else {
                // debug!("Moving onto unseen fork items within parent block");
                *search_position = unseen_parents.remove(0);
                Ok(false)
            }
        } else {
            //Do the fork traversals
            let mut parents = diff.parents.unwrap();
            //Check if all parents have already been seen, if so then break or move onto next unseen parents
            //TODO; we should use a seen set here versus array iter
            if parents.iter().all(|val| seen.contains(val)) {
                if unseen_parents.len() == 0 {
                    // debug!("Parents of item seen and unseen 0");
                    return Ok(true);
                } else {
                    // debug!("last moving onto unseen");
                    *search_position = unseen_parents.remove(0);
                    Ok(false)
                }
            } else {
                *search_position = SearchPosition {
                    hash: parents.remove(0),
                    is_unseen: false,
                };
                // debug!("Appending parents to look up");
                unseen_parents.append(
                    &mut parents
                        .into_iter()
                        .map(|val| SearchPosition {
                            hash: val,
                            is_unseen: true,
                        })
                        .collect(),
                );
                Ok(false)
            }
        }
    } else if search_position.is_unseen {
        //The parent for this branch is already seen so likely already explored and we are part of the main branch
        if unseen_parents.len() == 0 {
            // debug!("No more unseen items within parent block");
            Ok(true)
        } else {
            // debug!("Moving onto unseen fork items within parent block");
            *search_position = unseen_parents.remove(0);
            Ok(false)
        }
    } else {
        if diff.parents.is_none() {
            //No parents, we have reached the end of the chain
            //Now move onto traversing unseen parents, or break if we dont have any other paths to search
            if unseen_parents.len() == 0 {
                // debug!("No more unseen items within parent block");
                Ok(true)
            } else {
                // debug!("Moving onto unseen fork items within parent block");
                *search_position = unseen_parents.remove(0);
                Ok(false)
            }
        } else {
            //Do the fork traversals
            let mut parents = diff.parents.unwrap();
            //Check if all parents have already been seen, if so then break or move onto next unseen parents
            //TODO; we should use a seen set here versus array iter
            if parents.iter().all(|val| seen.contains(val)) {
                if unseen_parents.len() == 0 {
                    // debug!("Parents of item seen and unseen 0");
                    Ok(true)
                } else {
                    // debug!("last moving onto unseen");
                    *search_position = unseen_parents.remove(0);
                    Ok(false)
                }
            } else {
                *search_position = SearchPosition {
                    hash: parents.remove(0),
                    is_unseen: false,
                };
                // debug!("Appending parents to look up");
                unseen_parents.append(
                    &mut parents
                        .into_iter()
                        .map(|val| SearchPosition {
                            hash: val,
                            is_unseen: true,
                        })
                        .collect(),
                );
                Ok(false)
            }
        }
    }
}
