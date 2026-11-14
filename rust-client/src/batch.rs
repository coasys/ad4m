//! An open write batch on a perspective — the analogue of the TypeScript
//! SDK's `Batch` (`core/src/perspectives/PerspectiveClient.ts`:
//! `createBatch` / `commitBatch` / `discardBatch`, plus the `batchId` of the
//! instance writes).
//!
//! `perspective.createBatch` opens one: `commit` applies the batch's writes
//! and answers the links it changed (a `PerspectiveLinkDiff`), `discard`
//! drops them. Between open and commit the writes are invisible to reads —
//! an atomic unit.

use anyhow::Result;
use serde::Deserialize;
use serde_json::{json, Value};
use std::sync::Arc;

use crate::ws_rpc::WsRpcClient;

/// The links one commit added and removed — TS `PerspectiveLinkDiff`.
#[derive(Debug, Clone, Default, PartialEq, Deserialize)]
pub struct LinkDiff {
    #[serde(default)]
    pub additions: Vec<Value>,
    #[serde(default)]
    pub removals: Vec<Value>,
}

/// An open batch. Cheap to clone (writes name it by id — a clone commits or
/// discards the same batch).
#[derive(Debug, Clone)]
pub struct Batch {
    ws: Arc<WsRpcClient>,
    uuid: String,
    id: String,
}

impl Batch {
    /// `perspective.createBatch` — open a new batch on `uuid`.
    pub async fn open(ws: &Arc<WsRpcClient>, uuid: &str) -> Result<Self> {
        let id: String = ws
            .call("perspective.createBatch", json!({ "uuid": uuid }))
            .await?;
        Ok(Self {
            ws: ws.clone(),
            uuid: uuid.to_string(),
            id,
        })
    }

    /// Wrap a batch id the caller opened themselves (e.g. one handed down
    /// from `Model::transaction` — committing it is theirs to do).
    pub fn wrap(ws: &Arc<WsRpcClient>, uuid: &str, id: String) -> Self {
        Self {
            ws: ws.clone(),
            uuid: uuid.to_string(),
            id,
        }
    }

    /// The batch's id — pass it as `batch_id` on the instance writes so the
    /// writes land inside the batch.
    pub fn id(&self) -> &str {
        &self.id
    }

    /// `perspective.commitBatch` — apply the writes and answer the link diff.
    /// TS returns `true` on "no changes made"; here the diff is simply empty.
    pub async fn commit(&self) -> Result<LinkDiff> {
        self.ws
            .call(
                "perspective.commitBatch",
                json!({ "uuid": self.uuid, "batchId": self.id }),
            )
            .await
    }

    /// `perspective.discardBatch` — drop the writes. The executor answers
    /// `false` when it could not discard; that is surfaced as an error, not a
    /// silent `Ok(false)`.
    pub async fn discard(&self) -> Result<()> {
        self.ws
            .call_void(
                "perspective.discardBatch",
                json!({ "uuid": self.uuid, "batchId": self.id }),
            )
            .await
    }
}
