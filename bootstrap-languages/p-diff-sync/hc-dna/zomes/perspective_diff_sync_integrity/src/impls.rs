use std::cmp::Ordering;

use hdk::prelude::*;

use crate::{
    Anchor, HashBroadcast, OnlineAgent, PerspectiveDiff, PerspectiveDiffEntryReference,
    PerspectiveExpression,
};

impl PerspectiveDiff {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

impl Anchor {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

impl PerspectiveExpression {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

impl OnlineAgent {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

impl HashBroadcast {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

impl PerspectiveDiffEntryReference {
    pub fn new(
        diff: HoloHash<holo_hash::hash_type::Action>,
        parents: Option<Vec<HoloHash<holo_hash::hash_type::Action>>>,
    ) -> Self {
        Self {
            diff: diff,
            parents: parents,
            diffs_since_snapshot: 0,
        }
    }
}

impl PartialOrd for PerspectiveDiffEntryReference {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.diff.partial_cmp(&other.diff)
    }
}

impl Ord for PerspectiveDiffEntryReference {
    fn cmp(&self, other: &Self) -> Ordering {
        self.diff.cmp(&other.diff)
    }
}
