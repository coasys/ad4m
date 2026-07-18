use hdk::prelude::*;
use perspective_diff_sync_integrity::{
    Anchor, EntryTypes, GraphHeadEntry, LinkTypes, SetGraphHeadInput,
};

use crate::errors::SocialContextResult;

const GRAPH_HEADS_ANCHOR: &str = "graph_heads";

fn graph_heads_anchor() -> Anchor {
    Anchor(GRAPH_HEADS_ANCHOR.to_string())
}

pub fn set_graph_head(input: SetGraphHeadInput) -> SocialContextResult<()> {
    let anchor = graph_heads_anchor();
    let anchor_hash = hash_entry(&anchor)?;
    create_entry(EntryTypes::Anchor(anchor))?;

    let entry = GraphHeadEntry {
        graph_id: input.graph_id.clone(),
        commit_iri: input.commit_iri,
        parent_iris: input.parent_iris,
    };
    let entry_hash = hash_entry(&EntryTypes::GraphHeadEntry(entry.clone()))?;
    create_entry(EntryTypes::GraphHeadEntry(entry))?;

    create_link(
        anchor_hash,
        entry_hash,
        LinkTypes::GraphHead,
        LinkTag::new(input.graph_id.as_bytes()),
    )?;

    Ok(())
}

pub fn get_graph_head(graph_id: String) -> SocialContextResult<Option<GraphHeadEntry>> {
    let anchor_hash = hash_entry(&graph_heads_anchor())?;
    let query = LinkQuery::try_new(anchor_hash, LinkTypes::GraphHead)?
        .tag_prefix(LinkTag::new(graph_id.as_bytes()));

    let mut links = get_links(query, GetStrategy::Network)?;
    links.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));

    if let Some(link) = links.first() {
        if let Some(target) = link.target.clone().into_entry_hash() {
            if let Some(record) = get(target, GetOptions::default())? {
                let entry: GraphHeadEntry = record
                    .entry()
                    .to_app_option()
                    .map_err(|e| wasm_error!(WasmErrorInner::Host(format!("{}", e))))?
                    .ok_or_else(|| {
                        wasm_error!(WasmErrorInner::Host(
                            "GraphHeadEntry not found".to_string()
                        ))
                    })?;
                return Ok(Some(entry));
            }
        }
    }

    Ok(None)
}

pub fn get_all_graph_heads() -> SocialContextResult<Vec<GraphHeadEntry>> {
    let anchor_hash = hash_entry(&graph_heads_anchor())?;
    let query = LinkQuery::try_new(anchor_hash, LinkTypes::GraphHead)?;

    let mut links = get_links(query, GetStrategy::Network)?;
    links.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));

    let mut heads: Vec<GraphHeadEntry> = Vec::new();
    let mut seen_graphs: std::collections::HashSet<String> = std::collections::HashSet::new();

    for link in links {
        if let Some(target) = link.target.into_entry_hash() {
            if let Some(record) = get(target, GetOptions::default())? {
                if let Ok(Some(entry)) = record.entry().to_app_option::<GraphHeadEntry>() {
                    if seen_graphs.insert(entry.graph_id.clone()) {
                        heads.push(entry);
                    }
                }
            }
        }
    }

    Ok(heads)
}
