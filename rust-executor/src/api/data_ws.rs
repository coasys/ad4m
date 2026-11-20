//! The `data.*` methods (AR-06 4.10): a person's export and import, in chunks.
//!
//! The file is gzip JSON, kept in memory for the client to read in 1 MiB chunks (it never
//! rests on the host's disk). Each call acts for the caller's own agent: a managed agent's
//! perspectives, or the main agent's. A `Held` tenant may export (the dispatcher lets
//! `data.export*` through) but not import. See `hosting/transfer.rs` for the limits.

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::hosting::export::{
    export_owned, import_into, ExportError, ImportReport, LivePerspectives,
};
use crate::hosting::tenants;
use crate::hosting::transfer::{self, TransferError};
use crate::types::RequestContext;

use super::guards::Guard;
use super::ws_handler::{HandlerMap, WsRpcError};

/// `data.exportPrepare`'s params.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DataExportPrepareParams {
    /// Only these perspectives (uuids the caller owns); none: all of them.
    pub perspectives: Option<Vec<String>>,
}

/// `data.exportPrepare`'s answer: read the file with `data.exportChunk`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DataExportPrepared {
    pub export_id: String,
    /// The gzip file's size in bytes.
    #[ts(type = "number")]
    pub size: u64,
    /// Hex sha256 of the gzip file.
    pub sha256: String,
    /// How many chunks of 1 MiB (the last is shorter).
    pub chunks: u32,
    /// Unix seconds; every chunk read renews it.
    #[ts(type = "number")]
    pub expires_at: u64,
}

/// `data.exportChunk`'s params.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DataExportChunkParams {
    pub export_id: String,
    pub index: u32,
}

/// One chunk of the file, base64. Reading the last one ends the export.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DataChunk {
    pub data: String,
}

/// `data.importBegin`'s params.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DataImportBeginParams {
    #[ts(type = "number")]
    pub size: u64,
    /// Hex sha256 of the gzip file.
    pub sha256: String,
}

/// `data.importBegin`'s answer.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DataImportBegun {
    pub import_id: String,
}

/// `data.importChunk`'s params: the chunks come in order, from 0.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DataImportChunkParams {
    pub import_id: String,
    pub index: u32,
    /// Base64, at most 1 MiB decoded.
    pub data: String,
}

/// `data.importChunk`'s answer.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DataImportChunked {
    /// Chunks held so far.
    pub chunks: u32,
}

/// `data.importCommit`'s params.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DataImportCommitParams {
    pub import_id: String,
}

/// `data.importCommit`'s answer: what was written, and the spaces the app joins.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct DataImported {
    /// `[file uuid, new uuid]` per private perspective.
    pub perspectives: Vec<(String, String)>,
    #[ts(type = "number")]
    pub links_written: u64,
    /// Written with a failed signature check, and counted.
    #[ts(type = "number")]
    pub links_invalid: u64,
    /// The spaces of the file; the app joins each (`neighbourhood.join`).
    pub neighbourhood_urls: Vec<String>,
}

impl From<ImportReport> for DataImported {
    fn from(r: ImportReport) -> Self {
        DataImported {
            perspectives: r.perspectives,
            links_written: r.links_written,
            links_invalid: r.links_invalid,
            neighbourhood_urls: r.neighbourhood_urls,
        }
    }
}

fn value<T: Serialize>(v: T) -> Result<Value, WsRpcError> {
    serde_json::to_value(v).map_err(|e| WsRpcError::internal(e.to_string()))
}

fn params<T: serde::de::DeserializeOwned>(v: Value) -> Result<T, WsRpcError> {
    serde_json::from_value(v).map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))
}

fn now_secs() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Who the caller is for the limits: the managed agent's DID, else the main agent's.
fn person(ctx: &RequestContext) -> String {
    ctx.user_did()
        .map(str::to_string)
        .unwrap_or_else(crate::agent::did)
}

impl From<TransferError> for WsRpcError {
    fn from(e: TransferError) -> Self {
        let message = e.to_string();
        match e {
            TransferError::NotFound | TransferError::NotYours(_) => WsRpcError::not_found(message),
            TransferError::Busy => WsRpcError::conflict(message),
            TransferError::RateLimited { retry_after_secs } => {
                WsRpcError::too_many_requests(message)
                    .with_data(json!({ "retryAfter": retry_after_secs }))
            }
            TransferError::TooLarge { .. } | TransferError::Invalid(_) => {
                WsRpcError::bad_request(message)
            }
        }
    }
}

async fn export_prepare(raw: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: DataExportPrepareParams = params(raw)?;
    let who = person(&ctx);
    let now = now_secs();
    transfer::may_prepare(&who, now)?;
    let did_scid = match ctx.user_did() {
        Some(did) => tenants::live_tenant_of_device(did)?.map(|t| t.row.did_scid),
        None => None,
    };
    let file = export_owned(
        ctx.user_did(),
        p.perspectives.as_deref(),
        &crate::agent::did(),
        did_scid,
        &who,
        chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
    )
    .await
    .map_err(|e| match e {
        ExportError::NotYours(uuid) => TransferError::NotYours(uuid).into(),
        ExportError::Failed(why) => WsRpcError::internal(why),
    })?;
    let prepared = transfer::prepare(&who, transfer::encode(&file)?, now)?;
    value(DataExportPrepared {
        export_id: prepared.export_id,
        size: prepared.size,
        sha256: prepared.sha256,
        chunks: prepared.chunks,
        expires_at: prepared.expires_at,
    })
}

async fn export_chunk(raw: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: DataExportChunkParams = params(raw)?;
    let data = transfer::chunk(&person(&ctx), &p.export_id, p.index, now_secs())?;
    value(DataChunk { data })
}

async fn import_begin(raw: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: DataImportBeginParams = params(raw)?;
    let import_id = transfer::begin_import(
        &person(&ctx),
        p.size,
        &p.sha256,
        transfer::MAX_IMPORT_BYTES,
        now_secs(),
    )?;
    value(DataImportBegun { import_id })
}

async fn import_chunk(raw: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: DataImportChunkParams = params(raw)?;
    let chunks = transfer::import_chunk(&person(&ctx), &p.import_id, p.index, &p.data, now_secs())?;
    value(DataImportChunked { chunks })
}

async fn import_commit(raw: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: DataImportCommitParams = params(raw)?;
    let bytes = transfer::take_import(&person(&ctx), &p.import_id, now_secs())?;
    let file = transfer::decode(&bytes, transfer::MAX_IMPORT_BYTES)?;
    let mut sink = LivePerspectives {
        owner: ctx.user_did().map(str::to_string),
    };
    let report = import_into(&mut sink, &file)
        .await
        .map_err(WsRpcError::internal)?;
    value(DataImported::from(report))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<DataExportPrepareParams, DataExportPrepared>("data.exportPrepare", export_prepare)
        .long()
        .guard(Guard::CapAnyPointer(perspective_query_capability(vec![
            WILD_CARD.to_string(),
        ])));
    map.method::<DataExportChunkParams, DataChunk>("data.exportChunk", export_chunk)
        .guard(Guard::CapAnyPointer(perspective_query_capability(vec![
            WILD_CARD.to_string(),
        ])));
    map.method::<DataImportBeginParams, DataImportBegun>("data.importBegin", import_begin)
        .guard(Guard::Cap(PERSPECTIVE_CREATE_CAPABILITY.clone()));
    map.method::<DataImportChunkParams, DataImportChunked>("data.importChunk", import_chunk)
        .guard(Guard::Cap(PERSPECTIVE_CREATE_CAPABILITY.clone()));
    map.method::<DataImportCommitParams, DataImported>("data.importCommit", import_commit)
        .long()
        .guard(Guard::Cap(PERSPECTIVE_CREATE_CAPABILITY.clone()));
}
