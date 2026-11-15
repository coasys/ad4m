//! An open write batch on a perspective — the analogue of the TypeScript
//! SDK's `Batch` (`core/src/perspectives/PerspectiveClient.ts`:
//! `createBatch` / `commitBatch` / `discardBatch`, plus the `batchId` of the
//! instance writes).
//!
//! `perspective.createBatch` opens one: `commit` applies the batch's writes
//! and answers the links it changed (a `PerspectiveLinkDiff`), `discard`
//! drops them. Between open and commit the writes are invisible to reads,
//! and a commit makes them visible together.
//!
//! That is commit visibility, not per-call rollback: a write that fails
//! inside a batch may already have staged part of its links. A caller that
//! owns a batch and sees a write fail must `discard` the whole batch; catching
//! the error and committing commits whatever prefix the failed write staged.

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
///
/// **Minimum executor.** A write inside a batch must see the writes staged
/// before it in the same batch (the batch's effective graph). An executor
/// built from `87ba32b90` or a later commit does this; an older one reads
/// only the committed graph, so two changes to one collection in one batch
/// commit the wrong value without an error. The version string
/// (`0.13.1-staging.11`) is the same on both. See the crate README.
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
        let discarded: bool = self
            .ws
            .call(
                "perspective.discardBatch",
                json!({ "uuid": self.uuid, "batchId": self.id }),
            )
            .await?;
        if !discarded {
            anyhow::bail!(
                "perspective.discardBatch: the executor holds no open batch {} on {}",
                self.id,
                self.uuid
            );
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testkit::FakeExecutor;

    /// The executor answers `perspective.discardBatch` with `false` when the
    /// batch is unknown (already committed, discarded, or never opened): the
    /// writes the caller meant to drop are not known to be dropped, so the
    /// caller must hear it.
    #[tokio::test]
    async fn a_refused_discard_is_an_error() {
        let fake = FakeExecutor::start().await;
        let ws = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let batch = Batch::wrap(&ws, "p1", "b1".to_string());

        let b = batch.clone();
        let refused = tokio::spawn(async move { b.discard().await });
        let req = fake.next_request().await;
        assert_eq!(req["type"], "perspective.discardBatch");
        assert_eq!(req["params"], json!({"uuid": "p1", "batchId": "b1"}));
        fake.reply(&req, json!(false)).await;
        let err = refused
            .await
            .unwrap()
            .expect_err("a `false` discard must not be Ok");
        assert!(err.to_string().contains("b1"), "{err}");

        let b = batch.clone();
        let accepted = tokio::spawn(async move { b.discard().await });
        let req = fake.next_request().await;
        fake.reply(&req, json!(true)).await;
        accepted.await.unwrap().unwrap();
    }
}
