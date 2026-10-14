//! Interface (structural) types on `PerspectiveInstance` (#1435): which
//! installed classes implement an interface, cached until the installed
//! shapes change; model queries and subscriptions over an interface; and the
//! SHACL a named interface is registered with.
//!
//! The rule itself lives in `model_query::interfaces`; this module only
//! caches and wires it. A child module of `perspective_instance` so it can
//! reach the shape cache and the subscription registry without widening
//! their visibility.

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use std::collections::HashSet;
use std::sync::Arc;
use std::time::Duration;
use tokio::time::{sleep, Instant};

use super::{PerspectiveInstance, SdnaType, MODEL_QUERY_SHAPE_WAIT};
use crate::perspectives::model_query::{
    ClassSelector, InterfaceRef, ModelQueryInput, QueryClasses,
};
use crate::perspectives::shacl_parser::{
    parse_interface_shacl_to_links, parse_shacl_to_links, INTERFACE_TYPE,
};
use crate::types::{Link, LinkQuery};

impl SdnaType {
    /// The SHACL links for an SDNA entry of this type, validated before the
    /// store is touched (see `add_sdna_inner`).
    pub(super) fn parse_shacl(
        &self,
        shacl_json: Option<&str>,
        name: &str,
    ) -> Result<Option<Vec<Link>>, AnyError> {
        match (self, shacl_json) {
            (SdnaType::Interface, Some(shacl)) => {
                parse_interface_shacl_to_links(shacl, name).map(Some)
            }
            (SdnaType::Interface, None) => Err(anyhow!(
                "Interface '{name}': an interface is defined by its SHACL; pass shaclJson"
            )),
            (_, Some(shacl)) => parse_shacl_to_links(shacl, name).map(Some),
            (_, None) => Ok(None),
        }
    }

    /// The `rdf://type` the SHACL writer gives a target of this type, for the
    /// types whose SHACL is refreshed on re-registration.
    pub(super) fn shape_type(&self) -> Option<&'static str> {
        match self {
            SdnaType::SubjectClass => Some("ad4m://SubjectClass"),
            SdnaType::Interface => Some(INTERFACE_TYPE),
            SdnaType::Flow | SdnaType::Custom => None,
        }
    }
}

impl PerspectiveInstance {
    /// Drop every cached shape, and what was derived from them.
    pub(super) fn clear_shape_cache(&self) {
        self.shape_cache.write().unwrap().clear();
        self.forget_derived_shapes();
    }

    /// The installed shapes changed: a class was added, refreshed or synced
    /// in. Any of these can change which classes implement an interface.
    pub(super) fn forget_derived_shapes(&self) {
        let mut cache = self.interface_cache.write().unwrap();
        cache.generation += 1;
        cache.resolved.clear();
    }

    /// How many times the installed shapes have changed. Read it *before*
    /// resolving an interface: a change that lands mid-resolution moves it,
    /// so the result is never mistaken for current.
    pub(super) fn shapes_generation(&self) -> u64 {
        self.interface_cache.read().unwrap().generation
    }

    /// Every installed class that implements `interface` (#1435). A named
    /// interface's list is cached until the installed shapes change. A named
    /// interface that is not stored is a
    /// `MissingInterface` error; an interface no class implements is an empty
    /// list.
    pub fn classes_implementing(
        &self,
        interface: &InterfaceRef,
    ) -> Result<Arc<Vec<String>>, AnyError> {
        use crate::perspectives::model_query::interfaces;
        let generation = {
            let cache = self.interface_cache.read().unwrap();
            if let Some(hit) = cache.resolved.get(interface) {
                return Ok(hit.clone());
            }
            cache.generation
        };
        let loaded;
        let spec = match interface {
            InterfaceRef::Named(name) => {
                loaded = interfaces::load_interface(&self.sparql_store, name)?;
                &loaded
            }
            InterfaceRef::AdHoc(spec) => spec,
        };
        let classes = Arc::new(interfaces::classes_implementing(
            &self.sparql_store,
            &self.shape_resolver(),
            spec,
        )?);
        // Only named interfaces are kept: they are registered, so few. An
        // ad-hoc spec comes from the caller and would grow the cache by one
        // entry per distinct request; resolving it again costs one class-list
        // query over cached shapes.
        let mut cache = self.interface_cache.write().unwrap();
        // Computed against shapes that changed meanwhile: answer, don't keep.
        if cache.generation == generation && matches!(interface, InterfaceRef::Named(_)) {
            cache.resolved.insert(interface.clone(), classes.clone());
        }
        Ok(classes)
    }

    /// [`Self::classes_implementing`], waiting up to `budget` for a named
    /// interface to sync from a peer, as [`Self::get_shape_or_wait`] waits for
    /// a class.
    pub(super) async fn classes_implementing_or_wait(
        &self,
        interface: &InterfaceRef,
        budget: Duration,
    ) -> Result<Arc<Vec<String>>, AnyError> {
        let missing = |e: &AnyError| {
            e.downcast_ref::<crate::perspectives::model_query::interfaces::MissingInterface>()
                .is_some()
        };
        let deadline = Instant::now() + budget;
        loop {
            match self.classes_implementing(interface) {
                Err(e) if missing(&e) => {
                    let now = Instant::now();
                    if now >= deadline || !self.shapes_may_still_sync().await {
                        return Err(e);
                    }
                    sleep(Duration::from_millis(100).min(deadline - now)).await;
                }
                other => return other,
            }
        }
    }

    /// A class and an interface must not share a target URI: they would share
    /// one node shape, and each refresh would purge the other's triples.
    pub(super) fn refuse_class_interface_clash(
        &self,
        links: &[Link],
        shape_type: &str,
    ) -> Result<(), AnyError> {
        let other = if shape_type == INTERFACE_TYPE {
            "ad4m://SubjectClass"
        } else {
            INTERFACE_TYPE
        };
        for link in links {
            if link.predicate.as_deref() != Some("rdf://type") || link.target != shape_type {
                continue;
            }
            let clash = self.get_links_local(&LinkQuery {
                source: Some(link.source.clone()),
                predicate: Some("rdf://type".to_string()),
                target: Some(other.to_string()),
                ..Default::default()
            })?;
            if !clash.is_empty() {
                return Err(anyhow!(
                    "'{}' is already registered as {}; a class and an interface need different \
                     target URIs",
                    link.source,
                    if other == INTERFACE_TYPE {
                        "an interface"
                    } else {
                        "a subject class"
                    }
                ));
            }
        }
        Ok(())
    }

    /// [`Self::model_query_classes`] over classes named by the caller, or over
    /// every installed class that implements an interface (#1435). An
    /// interface query is a union query over those classes; none of them is
    /// an empty result.
    pub async fn model_query_selector(
        &self,
        selector: &ClassSelector,
        query_json: &str,
    ) -> Result<String, deno_core::anyhow::Error> {
        match selector {
            ClassSelector::Classes(classes) => self.model_query_classes(classes, query_json).await,
            ClassSelector::Interface(interface) => {
                let deadline = Instant::now() + MODEL_QUERY_SHAPE_WAIT;
                let classes = self
                    .interface_union(interface, query_json, MODEL_QUERY_SHAPE_WAIT)
                    .await?;
                let remaining = deadline.saturating_duration_since(Instant::now());
                self.model_query_classes_within(&classes, query_json, remaining)
                    .await
            }
        }
    }

    /// The union an interface query reads. What a union refuses is refused
    /// first, so the answer does not depend on which classes are installed.
    pub(super) async fn interface_union(
        &self,
        interface: &InterfaceRef,
        query_json: &str,
        budget: Duration,
    ) -> Result<QueryClasses, deno_core::anyhow::Error> {
        let query: ModelQueryInput = serde_json::from_str(query_json)
            .map_err(|e| deno_core::anyhow::anyhow!("Failed to parse model query: {}", e))?;
        crate::perspectives::model_query::refuse_union_reads(&query)?;
        let classes = self.classes_implementing_or_wait(interface, budget).await?;
        Ok(QueryClasses::union(classes.to_vec()))
    }

    /// [`Self::model_subscribe_and_query`] over classes named by the caller or
    /// over an interface (#1435). An interface subscription resolves its
    /// classes again whenever the installed shapes change, so a class
    /// installed after subscribe starts producing rows.
    pub async fn model_subscribe_selector(
        &self,
        selector: ClassSelector,
        query_json: String,
        connection_id: String,
    ) -> Result<(String, u64, String), AnyError> {
        match selector {
            ClassSelector::Classes(classes) => {
                self.model_subscribe_and_query(classes, query_json, connection_id)
                    .await
            }
            ClassSelector::Interface(interface) => {
                // Before resolving: a change that lands meanwhile is seen.
                let generation = self.shapes_generation();
                let classes = self
                    .interface_union(&interface, &query_json, MODEL_QUERY_SHAPE_WAIT)
                    .await?;
                self.open_model_subscription(
                    classes,
                    Some((interface, generation)),
                    query_json,
                    connection_id,
                )
                .await
            }
        }
    }

    /// Resolve every interface subscription (#1435) again whose classes were
    /// resolved before the installed shapes last changed. Returns the ids
    /// whose class list changed: those re-run this pass whatever was written,
    /// because their old trigger knows nothing of the new classes.
    pub(super) async fn refresh_interface_subscriptions(&self) -> HashSet<String> {
        let generation = self.shapes_generation();
        let stale: Vec<(String, InterfaceRef, String)> = {
            let queries = self.subscribed_queries.lock().await;
            queries
                .iter()
                .filter_map(|(id, q)| {
                    let params = q.model_query_params.as_ref()?;
                    match &params.interface {
                        Some((interface, at)) if *at != generation => {
                            Some((id.clone(), interface.clone(), params.query_json.clone()))
                        }
                        _ => None,
                    }
                })
                .collect()
        };
        let mut changed = HashSet::new();
        for (id, interface, query_json) in stale {
            let classes = match self.classes_implementing(&interface) {
                Ok(classes) => QueryClasses::union(classes.to_vec()),
                Err(e) => {
                    // Keep the old list; this is retried on the next pass.
                    log::warn!("interface subscription {id}: resolving its classes failed: {e}");
                    continue;
                }
            };
            let (trigger_sparql, predicates) = self.model_trigger_query(&classes, &query_json);
            let mut queries = self.subscribed_queries.lock().await;
            let Some(query) = queries.get_mut(&id) else {
                continue;
            };
            let same = query
                .model_query_params
                .as_ref()
                .is_some_and(|p| p.classes == classes);
            let trigger = (!same).then(|| {
                self.build_model_trigger(
                    classes.names().iter().map(String::as_str),
                    &query_json,
                    &query.last_result,
                )
            });
            let Some(params) = query.model_query_params.as_mut() else {
                continue;
            };
            params.interface = Some((interface, generation));
            if let Some(trigger) = trigger {
                params.trigger = trigger;
                params.classes = classes;
                query.query = trigger_sparql;
                query.predicates = predicates;
                changed.insert(id);
            }
        }
        changed
    }
}
