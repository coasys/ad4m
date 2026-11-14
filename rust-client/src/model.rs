//! A typed model over a perspective's SHACL classes — the Rust analogue of
//! the TypeScript SDK's `Ad4mModel` (`core/src/model/Ad4mModel.ts`).
//!
//! Where TS ties a model to a decorated class, a [`Model`] is constructed
//! from a class **name** and carries plain JSON values: `query`/`get`/
//! `subscribe` run `perspective.modelQuery` / `perspective.modelSubscribe`
//! and return hydrated instances as `serde_json::Value`s; `create`/`update`/
//! `delete`/`save` write through the validated `perspective.instance*` RPCs
//! (never raw links). Batches ([`crate::batch::Batch`]) group writes
//! atomically; `Model::transaction` mirrors `Ad4mModel.transaction`.

use anyhow::{anyhow, Result};
use serde_json::{Map, Value};
use std::sync::Arc;
use tokio::sync::broadcast;

use crate::batch::Batch;
use crate::instances::{
    CollectionItemAdded, CollectionItemRemoved, InstanceCollectionItem, InstanceCreate,
    InstanceCreated, InstanceRemove, InstanceRemoved, InstanceUpdate, InstanceUpdated,
};
use crate::subscription::{mint_subscription_id, LiveQuery, Subscribed};
use crate::ws_rpc::WsRpcClient;

/// One class (`class_name` on the wire) or a union query over several
/// (`class_names`, #1238) — the `classParams` of TS `PerspectiveClient`.
#[derive(Debug, Clone, PartialEq)]
pub enum Classes {
    One(String),
    Many(Vec<String>),
}

impl Classes {
    fn insert_into(&self, params: &mut Map<String, Value>) {
        match self {
            Classes::One(name) => {
                params.insert("class_name".into(), Value::from(name.clone()));
            }
            Classes::Many(names) => {
                params.insert("class_names".into(), Value::from(names.clone()));
            }
        }
    }
}

/// The options of `perspective.modelQuery`/`modelSubscribe` — the `Query`
/// type of the TS SDK (`where`, `order`, `limit`, `offset`, `include`,
/// `count`, plus `properties`, `links`, `parent`, `deepQuery`, `linkStatus`,
/// `includeUnverified`). `query_json` serialises like TS
/// `prepareModelQueryParams`: `include` keys starting with `$` become
/// `projections`, `order` becomes `[[key, direction]]`, and `deepQuery`
/// defaults to `true`.
#[derive(Debug, Clone, Default)]
pub struct ModelQuery {
    pub where_: Option<Value>,
    /// Property → direction pairs (`"ASC"` / `"DESC"`), in order.
    pub order: Option<Vec<(String, String)>>,
    pub limit: Option<u64>,
    pub offset: Option<u64>,
    /// The `IncludeMap` — relations to hydrate. Keys starting with `$` are
    /// projections and are sent under `projections`, as TS does.
    pub include: Option<Map<String, Value>>,
    /// Ask for `totalCount` alongside the page.
    pub count: Option<bool>,
    /// Only these properties are hydrated.
    pub properties: Option<Vec<String>>,
    /// Link-level detail on hydrated instances (`__links`).
    pub links: Option<Value>,
    /// A parent scope: `{ id, predicate }`, `{ id }` (containment), or a
    /// traverse object — passed through as TS sends it.
    pub parent: Option<Value>,
    /// Whether getter properties evaluate (TS default `true`).
    pub deep_query: Option<bool>,
    /// `shared` or `local` — which links may satisfy the query.
    pub link_status: Option<String>,
    /// Include links whose proof could not be verified.
    pub include_unverified: Option<bool>,
}

impl ModelQuery {
    /// Convenience for `ModelQuery { where_: Some(w), ..Default::default() }`.
    pub fn where_(w: Value) -> Self {
        Self {
            where_: Some(w),
            ..Default::default()
        }
    }

    /// The `query_json` string sent to `modelQuery`/`modelSubscribe` —
    /// the `queryInput` of `prepareModelQueryParams`.
    pub fn query_json(&self) -> String {
        let mut q = Map::new();
        if let Some(parent) = &self.parent {
            q.insert("parent".into(), parent.clone());
        }
        if let Some(properties) = &self.properties {
            q.insert("properties".into(), Value::from(properties.clone()));
        }
        if let Some(include) = &self.include {
            // `$`-prefixed keys → `projections`; the rest stays `include`.
            let mut includes = Map::new();
            let mut projections = Map::new();
            for (k, v) in include {
                if k.starts_with('$') {
                    projections.insert(k.clone(), v.clone());
                } else {
                    includes.insert(k.clone(), v.clone());
                }
            }
            if !includes.is_empty() {
                q.insert("include".into(), Value::Object(includes));
            }
            if !projections.is_empty() {
                q.insert("projections".into(), Value::Object(projections));
            }
        }
        if let Some(where_) = &self.where_ {
            q.insert("where".into(), where_.clone());
        }
        if let Some(order) = &self.order {
            q.insert(
                "order".into(),
                Value::Array(
                    order
                        .iter()
                        .map(|(k, d)| {
                            Value::Array(vec![Value::from(k.clone()), Value::from(d.clone())])
                        })
                        .collect(),
                ),
            );
        }
        if let Some(offset) = self.offset {
            q.insert("offset".into(), Value::from(offset));
        }
        if let Some(limit) = self.limit {
            q.insert("limit".into(), Value::from(limit));
        }
        if let Some(count) = self.count {
            q.insert("count".into(), Value::from(count));
        }
        if let Some(links) = &self.links {
            q.insert("links".into(), links.clone());
        }
        q.insert(
            "deepQuery".into(),
            Value::from(self.deep_query.unwrap_or(true)),
        );
        if let Some(link_status) = &self.link_status {
            q.insert("linkStatus".into(), Value::from(link_status.clone()));
        }
        if let Some(include_unverified) = self.include_unverified {
            q.insert("includeUnverified".into(), Value::from(include_unverified));
        }
        Value::Object(q).to_string()
    }
}

/// What `modelQuery` returns: the hydrated instances plus `totalCount`
/// (present when the query asked `count`, and on subscription results).
#[derive(Debug, Clone, PartialEq)]
pub struct ModelResult {
    pub instances: Vec<Value>,
    pub total_count: Option<u64>,
}

/// Options of [`Model::create`] — TS `Model.create`'s
/// `{ parent, batchId }`, plus an explicit `base_uri`.
#[derive(Debug, Clone, Default)]
pub struct CreateOptions {
    /// The new instance's URI; the executor mints one when absent.
    pub base_uri: Option<String>,
    /// A node to put the new instance under.
    pub parent: Option<String>,
    /// Write into this open batch instead of the live graph.
    pub batch_id: Option<String>,
}

/// A model bound to a perspective: all reads and writes against `class_name`
/// (or a union of classes). `Model::new(&proxy.ws(), proxy.uuid, "Recipe")`.
#[derive(Clone)]
pub struct Model {
    ws: Arc<WsRpcClient>,
    uuid: String,
    classes: Classes,
}

impl Model {
    pub fn new(
        ws: &Arc<WsRpcClient>,
        uuid: impl Into<String>,
        class_name: impl Into<String>,
    ) -> Self {
        Self {
            ws: ws.clone(),
            uuid: uuid.into(),
            classes: Classes::One(class_name.into()),
        }
    }

    /// A union query over several classes (`class_names`) — writes still go
    /// to `classes[0]`... actually writes need one class: this constructor is
    /// for query/subscribe only.
    pub fn new_union(
        ws: &Arc<WsRpcClient>,
        uuid: impl Into<String>,
        class_names: Vec<String>,
    ) -> Self {
        Self {
            ws: ws.clone(),
            uuid: uuid.into(),
            classes: Classes::Many(class_names),
        }
    }

    /// The perspective uuid this model is bound to.
    pub fn uuid(&self) -> &str {
        &self.uuid
    }

    /// The class name (`class_name`) or union (`class_names`) queried.
    pub fn classes(&self) -> &Classes {
        &self.classes
    }

    /// `perspective.modelQuery` — returns `{ instances, totalCount }`.
    pub async fn query(&self, query: &ModelQuery) -> Result<ModelResult> {
        let mut params = Map::new();
        params.insert("uuid".into(), Value::from(self.uuid.clone()));
        self.classes.insert_into(&mut params);
        params.insert("query_json".into(), Value::from(query.query_json()));
        let result: Value = self
            .ws
            .call("perspective.modelQuery", Value::Object(params))
            .await?;
        // modelQuery answers a JSON *string*.
        let parsed: Value = match &result {
            Value::String(s) => serde_json::from_str(s)?,
            other => other.clone(),
        };
        let mut instances = parsed
            .get("instances")
            .and_then(Value::as_array)
            .cloned()
            .unwrap_or_default();
        decode_timestamps(&mut instances);
        let total_count = parsed.get("totalCount").and_then(Value::as_u64);
        Ok(ModelResult {
            instances,
            total_count,
        })
    }

    /// `findAll` — the instances of a query.
    pub async fn find_all(&self, query: &ModelQuery) -> Result<Vec<Value>> {
        Ok(self.query(query).await?.instances)
    }

    /// `findOne` — the first match, or `None` (findAll with `limit: 1`).
    pub async fn find_one(&self, query: &ModelQuery) -> Result<Option<Value>> {
        let mut q = query.clone();
        q.limit = Some(1);
        Ok(self.find_all(&q).await?.into_iter().next())
    }

    /// `get` — one instance by URI (`{ where: { id }, limit: 1 }`), as TS
    /// `getData` does, or `None`. `include` hydrates relations.
    pub async fn get(
        &self,
        id: &str,
        include: Option<Map<String, Value>>,
    ) -> Result<Option<Value>> {
        let q = ModelQuery {
            where_: Some(serde_json::json!({ "id": id })),
            limit: Some(1),
            deep_query: Some(true),
            include,
            ..Default::default()
        };
        Ok(self.find_all(&q).await?.into_iter().next())
    }

    /// `count` — total matches (`limit: 0` + totalCount), as TS `count`.
    pub async fn count(&self, query: &ModelQuery) -> Result<u64> {
        let mut q = query.clone();
        q.limit = Some(0);
        Ok(self.query(&q).await?.total_count.unwrap_or(0))
    }

    /// `perspective.modelSubscribe` — a stream yielding the initial result
    /// and every later result (full replacement on resync/re-open), like TS
    /// `modelSubscribe` + `ModelQueryBuilder.subscribe` (which suppresses
    /// identical fingerprints — as does [`ModelSubscription`]).
    pub async fn subscribe(&self, query: &ModelQuery) -> Result<ModelSubscription> {
        let ws = self.ws.clone();
        let uuid = self.uuid.clone();
        let classes = self.classes.clone();
        let query_json = query.query_json();

        let open: crate::subscription::OpenFn = {
            let ws = ws.clone();
            let uuid = uuid.clone();
            let classes = classes.clone();
            let query_json = query_json.clone();
            Arc::new(move || {
                let ws = ws.clone();
                let uuid = uuid.clone();
                let classes = classes.clone();
                let query_json = query_json.clone();
                Box::pin(async move {
                    let subscription_id = mint_subscription_id();
                    let mut params = Map::new();
                    params.insert("uuid".into(), Value::from(uuid.clone()));
                    classes.insert_into(&mut params);
                    params.insert("query_json".into(), Value::from(query_json));
                    params.insert(
                        "subscriptionId".into(),
                        Value::from(subscription_id.clone()),
                    );
                    match ws
                        .call::<Value>("perspective.modelSubscribe", Value::Object(params))
                        .await
                    {
                        Ok(v) => Ok(Subscribed {
                            subscription_id: v
                                .get("subscriptionId")
                                .and_then(Value::as_str)
                                .unwrap_or(&subscription_id)
                                .to_string(),
                            result: v.get("result").cloned().unwrap_or(Value::Null),
                            revision: v.get("revision").and_then(Value::as_u64).unwrap_or(0),
                        }),
                        Err(e) => {
                            // A failed subscribe leaves no live query nobody
                            // holds — dispose the minted id (TS `#released`).
                            let _ = ws
                                .call_void(
                                    "perspective.disposeQuery",
                                    serde_json::json!({
                                        "uuid": uuid,
                                        "subscriptionId": subscription_id,
                                    }),
                                )
                                .await;
                            Err(e)
                        }
                    }
                })
            })
        };

        let live = LiveQuery::new(ws, &uuid, open);
        // Attach the receiver before `start`: updates drained out of the
        // open's buffer emit on `results()` as start resolves, and a
        // broadcast send reaches only receivers that already exist (F4).
        let rx = live.results();
        let initial = match live.start().await {
            Ok(v) => v,
            Err(e) => {
                // A failed open must not leave the installed watch, the
                // listener and the reconnect registration running (F5).
                live.dispose().await;
                return Err(e);
            }
        };
        Ok(ModelSubscription {
            live,
            rx,
            initial: Some(initial.clone()),
            last_fingerprint: Some(fingerprint_result(&initial)),
        })
    }

    /// `perspective.instanceCreate` — a validated write.
    pub async fn create(
        &self,
        properties: Map<String, Value>,
        options: CreateOptions,
    ) -> Result<InstanceCreated> {
        let mut params = serde_json::to_value(InstanceCreate {
            class_name: self.class_name()?.to_string(),
            properties: Some(properties),
            base_uri: options.base_uri,
            parent: options.parent,
            batch_id: options.batch_id,
        })?;
        params["uuid"] = Value::from(self.uuid.clone());
        self.ws.call("perspective.instanceCreate", params).await
    }

    /// `perspective.instanceUpdate` — set the given single-valued properties.
    pub async fn update(
        &self,
        id: &str,
        properties: Map<String, Value>,
        batch_id: Option<String>,
    ) -> Result<InstanceUpdated> {
        let mut params = serde_json::to_value(InstanceUpdate {
            class_name: self.class_name()?.to_string(),
            base_uri: id.to_string(),
            properties,
            batch_id,
        })?;
        params["uuid"] = Value::from(self.uuid.clone());
        self.ws.call("perspective.instanceUpdate", params).await
    }

    /// `perspective.instanceRemove` — delete the instance (and its incoming
    /// links executor-side). TS names this `delete`; `remove` is its alias.
    pub async fn delete(&self, id: &str, batch_id: Option<String>) -> Result<InstanceRemoved> {
        let mut params = serde_json::to_value(InstanceRemove {
            class_name: self.class_name()?.to_string(),
            base_uri: id.to_string(),
            batch_id,
        })?;
        params["uuid"] = Value::from(self.uuid.clone());
        self.ws.call("perspective.instanceRemove", params).await
    }

    /// TS `remove` — the deprecated alias of `delete`.
    pub async fn remove(&self, id: &str, batch_id: Option<String>) -> Result<InstanceRemoved> {
        self.delete(id, batch_id).await
    }

    /// `perspective.instanceAddToCollection` — add `item_uri` to `collection`.
    pub async fn add_to_collection(
        &self,
        id: &str,
        collection: &str,
        item_uri: &str,
        batch_id: Option<String>,
    ) -> Result<CollectionItemAdded> {
        let mut params = serde_json::to_value(InstanceCollectionItem {
            class_name: self.class_name()?.to_string(),
            base_uri: id.to_string(),
            collection: collection.to_string(),
            item_uri: item_uri.to_string(),
            batch_id,
        })?;
        params["uuid"] = Value::from(self.uuid.clone());
        self.ws
            .call("perspective.instanceAddToCollection", params)
            .await
    }

    /// `perspective.instanceRemoveFromCollection`.
    pub async fn remove_from_collection(
        &self,
        id: &str,
        collection: &str,
        item_uri: &str,
        batch_id: Option<String>,
    ) -> Result<CollectionItemRemoved> {
        let mut params = serde_json::to_value(InstanceCollectionItem {
            class_name: self.class_name()?.to_string(),
            base_uri: id.to_string(),
            collection: collection.to_string(),
            item_uri: item_uri.to_string(),
            batch_id,
        })?;
        params["uuid"] = Value::from(self.uuid.clone());
        self.ws
            .call("perspective.instanceRemoveFromCollection", params)
            .await
    }

    /// `save` — create-or-update by whether `base_uri` is given.
    ///
    /// TS `save` on a new instance wraps the write in a fresh batch when no
    /// `batch_id` is supplied (so creation is atomic) and commits it — this
    /// does the same over `instanceCreate`/`createBatch`/`commitBatch`. It
    /// returns the instance URI; TS rehydrates the object in place, the Rust
    /// model holds no local state — call `get` for the hydrated instance.
    /// On an existing instance (`base_uri` set) it updates through
    /// `instanceUpdate`.
    pub async fn save(
        &self,
        base_uri: Option<String>,
        properties: Map<String, Value>,
        batch_id: Option<String>,
    ) -> Result<String> {
        if let Some(uri) = base_uri {
            self.update(&uri, properties, batch_id).await?;
            return Ok(uri);
        }
        match batch_id {
            Some(batch_id) => {
                let created = self
                    .create(
                        properties,
                        CreateOptions {
                            batch_id: Some(batch_id),
                            ..Default::default()
                        },
                    )
                    .await?;
                Ok(created.base_uri)
            }
            None => {
                // No batch handed in — open one ourselves so the create is
                // atomic (TS `save` does the same), then commit.
                let batch = Batch::open(&self.ws, &self.uuid).await?;
                let created = self
                    .create(
                        properties,
                        CreateOptions {
                            batch_id: Some(batch.id().to_string()),
                            ..Default::default()
                        },
                    )
                    .await;
                match created {
                    Ok(created) => {
                        batch.commit().await?;
                        Ok(created.base_uri)
                    }
                    Err(e) => {
                        let _ = batch.discard().await;
                        Err(e)
                    }
                }
            }
        }
    }

    /// `Ad4mModel.transaction` — open a batch, run `f` against it, commit.
    /// A failing `f` never commits: the batch is left unwritten (and
    /// `discard`ed best-effort, an executor-side no-op either way).
    pub async fn transaction<R, Fut>(&self, f: impl FnOnce(&Batch) -> Fut) -> Result<R>
    where
        Fut: std::future::Future<Output = Result<R>>,
    {
        let batch = Batch::open(&self.ws, &self.uuid).await?;
        match f(&batch).await {
            Ok(r) => {
                batch.commit().await?;
                Ok(r)
            }
            Err(e) => {
                let _ = batch.discard().await;
                Err(e)
            }
        }
    }

    /// A new open batch on this model's perspective — see [`Batch`].
    pub async fn batch(&self) -> Result<Batch> {
        Batch::open(&self.ws, &self.uuid).await
    }

    /// The single class name writes go to — `new_union` models cannot write.
    fn class_name(&self) -> Result<&str> {
        match &self.classes {
            Classes::One(name) => Ok(name),
            Classes::Many(_) => Err(anyhow!(
                "a union model (`class_names`) is read-only — construct `Model::new` with one class to write"
            )),
        }
    }
}

/// The stable-stringify a TS model subscription applies before emitting:
/// the parsed instance rows — `totalCount` alone changing does not emit
/// (`subscribe` fingerprints `results` only, `ModelQueryBuilder.ts` L488).
fn fingerprint_result(raw: &Value) -> String {
    Value::Array(parse_model_result(raw).instances).to_string()
}

/// A live model subscription: the initial result, then every pushed result —
/// `ids`/`upsert` deltas applied to the last result, resyncs and re-opens
/// arriving as their full result. Ends when disposed.
pub struct ModelSubscription {
    live: LiveQuery,
    rx: broadcast::Receiver<Value>,
    initial: Option<Value>,
    last_fingerprint: Option<String>,
}

impl ModelSubscription {
    /// The next distinct result: the initial one first, then every update.
    /// `None` when the subscription ended (disposed or the socket closed for
    /// good).
    pub async fn next(&mut self) -> Option<ModelResult> {
        if let Some(initial) = self.initial.take() {
            return Some(parse_model_result(&initial));
        }
        loop {
            match self.rx.recv().await {
                Ok(raw) => {
                    let fp = fingerprint_result(&raw);
                    if Some(&fp) == self.last_fingerprint.as_ref() {
                        continue;
                    }
                    self.last_fingerprint = Some(fp);
                    return Some(parse_model_result(&raw));
                }
                Err(broadcast::error::RecvError::Lagged(_)) => continue,
                Err(broadcast::error::RecvError::Closed) => return None,
            }
        }
    }

    /// The current subscription id on the executor, if opened.
    pub async fn id(&self) -> Option<String> {
        self.live.id().await
    }

    /// Whether a re-open failed and the query is waiting to retry it.
    pub async fn reopening(&self) -> bool {
        self.live.reopening().await
    }

    /// End the subscription on the executor and stop the stream.
    pub async fn dispose(&self) {
        self.live.dispose().await;
    }
}

/// A hydrated model instance and the snapshot it was fetched at — the
/// dirty-tracking half of TS `Ad4mModel`: [`Instance::save`] writes only
/// the properties that differ.
///
/// Build one from a `get`/`find_all` row via [`Model::instance`].
#[derive(Clone)]
pub struct Instance {
    model: Model,
    base_uri: String,
    /// The hydrated values — the baseline `save` diffs against (TS
    /// `_snapshot`; `takeSnapshot` after each save).
    snapshot: Map<String, Value>,
    /// The hydrated values plus every `set` since.
    current: Map<String, Value>,
}

impl Model {
    /// Wrap a hydrated instance row (`get`/`find_all`) for dirty-tracked
    /// mutation. `base_uri` is the instance's own URI (its `id`).
    pub fn instance(&self, base_uri: impl Into<String>, hydrated: Map<String, Value>) -> Instance {
        Instance {
            model: self.clone(),
            base_uri: base_uri.into(),
            snapshot: hydrated.clone(),
            current: hydrated,
        }
    }
}

impl Instance {
    /// The instance's URI.
    pub fn base_uri(&self) -> &str {
        &self.base_uri
    }

    /// The current (possibly edited) value of `key`.
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.current.get(key)
    }

    /// All current properties — the hydrated row plus edits.
    pub fn properties(&self) -> &Map<String, Value> {
        &self.current
    }

    /// Set `key` — `save` writes it only if it differs from the snapshot.
    pub fn set(&mut self, key: impl Into<String>, value: Value) {
        self.current.insert(key.into(), value);
    }

    /// TS `isDirty` — any property differing from the snapshot.
    pub fn is_dirty(&self) -> bool {
        !self.changed_fields().is_empty()
    }

    /// TS `changedFields` — sorted keys whose current value differs from
    /// the snapshot's. A key the caller `set` but the hydration lacked
    /// counts: it is a real write. Values compare order-sensitively —
    /// without the class's `ordering` metadata there is no way to know an
    /// array is an unordered set, and a spurious rewrite is the safe
    /// failure direction.
    pub fn changed_fields(&self) -> Vec<String> {
        let mut changed: Vec<String> = self
            .current
            .iter()
            .filter(|(k, v)| self.snapshot.get(*k) != Some(*v))
            .map(|(k, _)| k.clone())
            .collect();
        changed.sort();
        changed
    }

    /// `save` like TS `Ad4mModel.save` on a hydrated instance: dispatch
    /// only the changed properties — nothing at all when the instance is
    /// clean — then adopt the current values as the snapshot so further
    /// saves diff against them (TS `takeSnapshot`; a save outside a batch
    /// would rehydrate, which is equivalent for the values written here).
    pub async fn save(&mut self, batch_id: Option<String>) -> Result<()> {
        let changed = self.changed_fields();
        if changed.is_empty() {
            return Ok(());
        }
        let props: Map<String, Value> = changed
            .iter()
            .map(|k| (k.clone(), self.current[k].clone()))
            .collect();
        self.model.update(&self.base_uri, props, batch_id).await?;
        self.snapshot = self.current.clone();
        Ok(())
    }
}

impl Drop for ModelSubscription {
    /// Dropping without `dispose()` still tears the subscription down —
    /// executor-side, and this client's watch and listener (F5). Async
    /// teardown needs a runtime; off one, only `dispose().await` cleans up.
    fn drop(&mut self) {
        if let Ok(rt) = tokio::runtime::Handle::try_current() {
            let live = self.live.clone();
            rt.spawn(async move { live.dispose().await });
        }
    }
}

/// `{ instances, totalCount }` out of a raw subscription result — the parse
/// TS `parseModelResult` does.
fn parse_model_result(raw: &Value) -> ModelResult {
    let parsed = match raw {
        Value::String(s) => serde_json::from_str(s).unwrap_or_else(|_| raw.clone()),
        v => v.clone(),
    };
    let mut instances = parsed
        .get("instances")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_else(|| parsed.as_array().cloned().unwrap_or_default());
    decode_timestamps(&mut instances);
    ModelResult {
        instances,
        total_count: parsed.get("totalCount").and_then(Value::as_u64),
    }
}

fn decode_timestamps(instances: &mut [Value]) {
    for instance in instances {
        decode_timestamps_in_instance(instance);
    }
}

/// The decode TS `hydrateFromLinks` applies where the executor's hydration
/// wrote `createdAt`/`updatedAt` as RFC3339 strings: a string containing
/// 'T' becomes `Date.getTime()` — epoch millis — so a fetched row carries
/// typed values, not strings (and `FlowInstance.startedAtMillis` reads the
/// number it expects). An RFC3339-unparseable value stays as it arrived:
/// `getTime()` on it yields NaN, which no honest JSON value can stand in
/// for.
pub(crate) fn decode_timestamps_in_instance(instance: &mut Value) {
    let Some(obj) = instance.as_object_mut() else {
        return;
    };
    for key in ["createdAt", "updatedAt"] {
        let s = match obj.get(key).and_then(Value::as_str) {
            Some(s) if s.contains('T') => s.to_string(),
            _ => continue,
        };
        if let Ok(dt) = chrono::DateTime::parse_from_rfc3339(&s) {
            obj.insert(key.into(), Value::from(dt.timestamp_millis()));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::subscription::{apply_update, QueryUpdate};
    use serde_json::json;
    use std::time::Duration;

    /// Q3: `save` on an instance writes only the properties that changed
    /// since hydration — like TS `Ad4mModel.save` dispatching
    /// `changedValuesForDispatch()`, not the whole row.
    #[tokio::test]
    async fn save_writes_only_the_changed_properties() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let ws = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let model = Model::new(&ws, "p1", "Recipe");

        let hydrated = json!({"id": "recipe://1", "name": "Pasta", "done": false})
            .as_object()
            .unwrap()
            .clone();
        let mut inst = model.instance("recipe://1", hydrated);
        assert!(!inst.is_dirty());
        inst.set("name", json!("Risotto"));
        assert_eq!(inst.changed_fields(), vec!["name".to_string()]);

        let save = tokio::spawn(async move {
            let mut inst = inst;
            let r = inst.save(None).await;
            (r, inst)
        });
        let req = fake.next_request().await;
        assert_eq!(req["type"], "perspective.instanceUpdate");
        assert_eq!(req["params"]["baseUri"], "recipe://1");
        // Only the dirty property — the hydrated `done`/`id` stay home.
        assert_eq!(req["params"]["properties"], json!({"name": "Risotto"}));
        fake.reply(
            &req,
            json!({"className": "Recipe", "baseUri": "recipe://1", "updatedProperties": ["name"]}),
        )
        .await;
        let (res, mut inst) = save.await.unwrap();
        res.unwrap();

        // The snapshot adopted the write: a second clean save sends no RPC.
        assert!(!inst.is_dirty());
        inst.save(None).await.unwrap();
        assert!(fake
            .maybe_request(Duration::from_millis(300))
            .await
            .is_none());
        ws.close().await;
    }

    /// F5: dropping the owned subscription must clean it up executor-side —
    /// an abandoned stream leaves a live subscription (and the interest it
    /// re-registers on every reconnect) behind.
    #[tokio::test]
    async fn dropping_the_subscription_disposes_it_executor_side() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let ws = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let model = Model::new(&ws, "p1", "Recipe");
        let subscribe = tokio::spawn(async move { model.subscribe(&ModelQuery::default()).await });
        for _ in 0..2 {
            let req = fake.next_request().await;
            match req["type"].as_str() {
                Some("events.watch") => fake.reply(&req, json!(true)).await,
                _ => {
                    fake.reply(
                        &req,
                        json!({"subscriptionId": "s1", "result": {"instances": [], "totalCount": 0}, "revision": 0}),
                    )
                    .await
                }
            }
        }
        let sub = subscribe.await.unwrap().unwrap();
        drop(sub);
        let disposed = tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                let req = fake.next_request().await;
                if req["type"] == "perspective.disposeQuery" {
                    return req;
                }
                // The spawned dispose awaits its unwatch reply.
                fake.reply(&req, json!(true)).await;
            }
        })
        .await
        .expect("dropping the subscription must send perspective.disposeQuery");
        assert_eq!(disposed["params"]["subscriptionId"], "s1");
        ws.close().await;
    }

    /// F5: a subscribe that fails must not leave the installed watch,
    /// listener and reconnect registration running — the live query it
    /// built is dropped with the error and would otherwise stay live.
    #[tokio::test]
    async fn a_failed_subscribe_does_not_leave_the_watch_behind() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let ws = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let model = Model::new(&ws, "p1", "Recipe");
        let subscribe = tokio::spawn(async move { model.subscribe(&ModelQuery::default()).await });
        let sub_req = loop {
            let req = fake.next_request().await;
            if req["type"] == "events.watch" {
                fake.reply(&req, json!(true)).await;
            } else {
                break req;
            }
        };
        fake.reply_error(&sub_req, 500, "no such class").await;
        // The failed open releases its minted id (a disposeQuery), then the
        // cleanup unwatch empties the union. The dispose awaits that
        // unwatch's reply, so `subscribe` only resolves once we answer.
        let unwatch = tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                let req = fake.next_request().await;
                if req["type"] == "events.watch" {
                    fake.reply(&req, json!(true)).await;
                    return req;
                }
                fake.reply(&req, json!(true)).await;
            }
        })
        .await
        .expect("a failed subscribe must remove its watch");
        assert_eq!(unwatch["params"], json!({}));
        assert!(subscribe.await.unwrap().is_err());
        ws.close().await;
    }

    /// F4: an update the executor publishes while the subscribe call is
    /// outstanding must reach the subscriber. The change drains inside
    /// `start`'s own snapshot handoff — a `results()` receiver that only
    /// attaches after `start` returns misses it for good (broadcast sends
    /// reach only receivers that already exist).
    #[tokio::test]
    async fn subscribe_delivers_an_update_emitted_during_open() {
        use crate::testkit::FakeExecutor;
        let fake = FakeExecutor::start().await;
        let ws = Arc::new(WsRpcClient::connect(fake.url(), "").await.unwrap());
        let model = Model::new(&ws, "p1", "Recipe");
        let subscribe = tokio::spawn(async move { model.subscribe(&ModelQuery::default()).await });

        // Answer the watch whenever it arrives; hold the subscribe reply.
        let sub_req = loop {
            let req = fake.next_request().await;
            if req["type"] == "perspective.modelSubscribe" {
                break req;
            }
            if req["type"] == "events.watch" {
                fake.reply(&req, json!(true)).await;
            }
        };
        let sid = sub_req["params"]["subscriptionId"].as_str().unwrap();
        // The executor publishes revision 1 while the snapshot is in flight.
        fake.send(json!({
            "type": "query-subscription-update",
            "subscriptionId": sid,
            "revision": 1,
            "ids": ["i1"],
            "upsert": [{"id": "i1", "done": true}],
            "totalCount": 1,
        }))
        .await;
        fake.reply(
            &sub_req,
            json!({
                "subscriptionId": sid,
                "result": {"instances": [], "totalCount": 0},
                "revision": 0,
            }),
        )
        .await;

        let mut sub = subscribe.await.unwrap().unwrap();
        let first = sub.next().await.unwrap();
        assert!(first.instances.is_empty());
        let second = tokio::time::timeout(Duration::from_secs(2), sub.next())
            .await
            .expect("the update emitted during open must reach the subscriber")
            .unwrap();
        assert_eq!(second.instances[0]["id"], "i1");
        ws.close().await;
    }

    #[test]
    fn query_json_serializes_like_prepare_model_query_params() {
        let mut include = Map::new();
        include.insert("ingredients".into(), Value::Bool(true));
        include.insert("$count".into(), json!({"resolveTo": "int"}));
        let q = ModelQuery {
            where_: Some(json!({"name": "Pasta"})),
            order: Some(vec![("rating".into(), "DESC".into())]),
            limit: Some(10),
            offset: Some(5),
            include: Some(include),
            count: Some(true),
            properties: Some(vec!["name".into()]),
            links: Some(json!({"status": "local"})),
            parent: Some(json!({"id": "p1", "predicate": "x://has"})),
            link_status: Some("shared".into()),
            include_unverified: Some(true),
            ..Default::default()
        };
        let v: Value = serde_json::from_str(&q.query_json()).unwrap();
        assert_eq!(v["where"], json!({"name": "Pasta"}));
        assert_eq!(v["order"], json!([["rating", "DESC"]]));
        assert_eq!(v["limit"], json!(10));
        assert_eq!(v["offset"], json!(5));
        assert_eq!(v["count"], json!(true));
        assert_eq!(v["include"], json!({"ingredients": true}));
        assert_eq!(v["projections"], json!({"$count": {"resolveTo": "int"}}));
        assert_eq!(v["properties"], json!(["name"]));
        assert_eq!(v["links"], json!({"status": "local"}));
        assert_eq!(v["parent"], json!({"id": "p1", "predicate": "x://has"}));
        assert_eq!(v["deepQuery"], json!(true));
        assert_eq!(v["linkStatus"], json!("shared"));
        assert_eq!(v["includeUnverified"], json!(true));
    }

    #[test]
    fn query_json_defaults() {
        let v: Value = serde_json::from_str(&ModelQuery::default().query_json()).unwrap();
        assert_eq!(v, json!({"deepQuery": true}));
        // deepQuery: false is honoured, not replaced by the default.
        let v2: Value = serde_json::from_str(
            &ModelQuery {
                deep_query: Some(false),
                ..Default::default()
            }
            .query_json(),
        )
        .unwrap();
        assert_eq!(v2["deepQuery"], json!(false));
    }

    #[test]
    fn apply_update_row_form_parses() {
        let old = json!([{"a": 1}]);
        let update = QueryUpdate {
            added: Some(vec![json!({"a": 2})]),
            ..Default::default()
        };
        assert_eq!(
            apply_update(&old, &update).unwrap(),
            json!([{"a": 1}, {"a": 2}])
        );
    }

    /// The executor hydrates `createdAt`/`updatedAt` as RFC3339 strings; TS
    /// `hydrateFromLinks` converts them to epoch millis before a caller sees
    /// the row (and `FlowInstance.startedAtMillis` reads a number). The Rust
    /// row must carry the same typed value.
    #[test]
    fn hydrated_rows_decode_timestamps_to_epoch_millis() {
        let raw = json!({"instances":[{
            "id": "x://1",
            "createdAt": "2026-01-01T00:00:00.000Z",
            "updatedAt": "2026-01-02T00:00:00.000Z",
            "title": "a"
        }],"totalCount":1});
        let r = parse_model_result(&raw);
        assert_eq!(r.instances[0]["createdAt"], json!(1767225600000_i64));
        assert_eq!(r.instances[0]["updatedAt"], json!(1767312000000_i64));
        // Other strings, including ones containing 'T' under other keys,
        // stay untouched.
        assert_eq!(r.instances[0]["title"], json!("a"));
    }

    /// A `createdAt` that is not an RFC3339 string is left as it is —
    /// `getTime()` on an unparseable string yields NaN in TS; there is no
    /// honest JSON value for that, so the row keeps what the executor sent.
    #[test]
    fn an_unreadable_timestamp_is_left_alone() {
        let raw = json!({"instances":[{"id":"x://1","createdAt":"not a date"}]});
        let r = parse_model_result(&raw);
        assert_eq!(r.instances[0]["createdAt"], json!("not a date"));
    }
}
