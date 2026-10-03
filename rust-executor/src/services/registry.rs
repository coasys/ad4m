//! Interfaces, module chains, implementations, preferences and resolution.

use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use serde::{Deserialize, Serialize};
use serde_json::Value;
use ts_rs::TS;

use super::builtin::{ServiceHealth, ServiceImplementation};
use super::capability::service_capability;
use super::interface::{content_hash, is_hash, InterfaceDocument, Selection};
use super::semver::check_successor;
use crate::agent::capabilities::Capability;

/// What a Service Language needs from another interface.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Requirement {
    pub interface: String,
    pub actions: Vec<String>,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub optional: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "kebab-case")]
pub enum Instancing {
    Shared,
    PerUser,
}

/// The manifest of a built-in Service Language (`runtime: builtin`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct BuiltinManifest {
    pub name: String,
    pub author: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub module: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub previous: Option<String>,
    pub version: String,
    #[serde(default)]
    pub description: String,
    pub implements: Vec<String>,
    #[serde(default)]
    pub requires: Vec<Requirement>,
    pub instancing: Instancing,
    /// `{ "kind": "builtin", "id": "<id>" }`
    pub runtime: Value,
}

pub struct Implementation {
    pub hash: String,
    pub module_id: String,
    pub manifest: BuiltinManifest,
    pub version: semver::Version,
    pub service: Arc<dyn ServiceImplementation>,
    pub health: ServiceHealth,
    /// The grants its `requires` give it: one layer of [`super::builtin::CallContext::grants`].
    pub grants: Arc<Vec<Capability>>,
    order: u64,
}

impl Implementation {
    pub fn is_running(&self) -> bool {
        matches!(
            self.health,
            ServiceHealth::Running | ServiceHealth::Degraded(_)
        )
    }
}

/// Why a target did not resolve.
#[derive(Debug, Clone, PartialEq)]
pub enum ResolveError {
    /// Unknown hash or method (→ 404).
    NotFound(String),
    /// Known, but nothing running serves it (→ 503).
    Unavailable(String),
}

#[derive(Default)]
pub struct Registry {
    interfaces: HashMap<String, Arc<InterfaceDocument>>,
    /// module ID → version → hash
    modules: HashMap<String, BTreeMap<semver::Version, String>>,
    implementations: HashMap<String, Implementation>,
    /// (user or None for the admin default, module ID, compat) → implementation module ID
    preferences: HashMap<(Option<String>, String, String), String>,
    next_order: u64,
}

impl Registry {
    /// Register one interface version. `trusted` skips the signature check
    /// for interfaces compiled into the executor.
    pub fn register_interface(
        &mut self,
        raw: Value,
        signature: Option<&str>,
        trusted: bool,
    ) -> Result<Arc<InterfaceDocument>, String> {
        let doc = InterfaceDocument::parse(raw)?;
        if let Some(existing) = self.interfaces.get(&doc.hash) {
            return Ok(existing.clone());
        }
        if !trusted {
            let sig = signature.ok_or("an interface needs its author's signature")?;
            if !doc.verify_signature(sig) {
                return Err(format!(
                    "signature does not match author {}",
                    doc.doc.author
                ));
            }
        }
        let module_id = doc.module_id().to_string();
        if let Some(prev_hash) = &doc.doc.previous {
            let prev = self
                .interfaces
                .get(prev_hash)
                .ok_or_else(|| format!("previous version {} is not registered", prev_hash))?;
            if prev.module_id() != module_id {
                return Err(format!(
                    "previous version {} belongs to another module",
                    prev_hash
                ));
            }
            // The module ID is the genesis hash alone, so only this keeps a
            // module in its author's hands.
            if prev.doc.author != doc.doc.author {
                return Err(format!("previous version {} has another author", prev_hash));
            }
            check_successor(prev, &doc)?;
        }
        if let Some(versions) = self.modules.get(&module_id) {
            if versions.contains_key(&doc.version) {
                return Err(format!(
                    "{} {} already exists with another hash",
                    module_id, doc.version
                ));
            }
            // A chain may branch (two versions with one `previous`). Resolution
            // lets any higher version of a line serve a lower one, so check
            // the registered neighbours too.
            if let Some((_, below)) = versions.range(..doc.version.clone()).next_back() {
                if doc.doc.previous.as_ref() != Some(below) {
                    check_successor(&self.interfaces[below], &doc)?;
                }
            }
            if let Some((_, above)) = versions.range(doc.version.clone()..).next() {
                check_successor(&doc, &self.interfaces[above])?;
            }
        }
        let doc = Arc::new(doc);
        self.modules
            .entry(module_id)
            .or_default()
            .insert(doc.version.clone(), doc.hash.clone());
        self.interfaces.insert(doc.hash.clone(), doc.clone());
        Ok(doc)
    }

    pub fn interface(&self, hash: &str) -> Option<Arc<InterfaceDocument>> {
        self.interfaces.get(hash).cloned()
    }

    pub fn interfaces(&self) -> impl Iterator<Item = &Arc<InterfaceDocument>> {
        self.interfaces.values()
    }

    /// Register a built-in implementation. Returns its hash.
    pub fn register_implementation(
        &mut self,
        manifest: BuiltinManifest,
        service: Arc<dyn ServiceImplementation>,
    ) -> Result<String, String> {
        let raw = serde_json::to_value(&manifest).map_err(|e| e.to_string())?;
        let canonical = serde_json_canonicalizer::to_vec(&raw).map_err(|e| e.to_string())?;
        let hash = content_hash(&canonical);
        if self.implementations.contains_key(&hash) {
            return Err(format!("implementation {} is already registered", hash));
        }
        let version = semver::Version::parse(&manifest.version)
            .map_err(|e| format!("version `{}` is not semver: {}", manifest.version, e))?;
        if !manifest.author.starts_with("did:") {
            return Err("`author` must be a DID".into());
        }
        if manifest.runtime.get("kind").and_then(Value::as_str) != Some("builtin") {
            return Err("only `builtin` runtimes exist in this executor version".into());
        }
        if manifest.module.is_some() != manifest.previous.is_some()
            || manifest.module.as_deref().is_some_and(|m| !is_hash(m))
        {
            return Err("`module` and `previous` must both be hashes, or both absent".into());
        }
        // As for interfaces: a later version continues a registered one of
        // the same module and author.
        if let (Some(module), Some(prev_hash)) = (&manifest.module, &manifest.previous) {
            let prev = self
                .implementations
                .get(prev_hash)
                .ok_or_else(|| format!("previous version {} is not registered", prev_hash))?;
            if &prev.module_id != module {
                return Err(format!(
                    "previous version {} belongs to another module",
                    prev_hash
                ));
            }
            if prev.manifest.author != manifest.author {
                return Err(format!("previous version {} has another author", prev_hash));
            }
        }
        if manifest.implements.is_empty() {
            return Err("an implementation must implement at least one interface".into());
        }
        for h in &manifest.implements {
            if !self.interfaces.contains_key(h) {
                return Err(format!("implemented interface {} is not registered", h));
            }
        }
        let mut grants = Vec::new();
        for r in &manifest.requires {
            let Some(doc) = self.interfaces.get(&r.interface) else {
                if r.optional {
                    continue;
                }
                return Err(format!(
                    "required interface {} is not registered",
                    r.interface
                ));
            };
            for a in &r.actions {
                if !doc.doc.actions.contains_key(a) {
                    return Err(format!("{} has no action `{}`", r.interface, a));
                }
                grants.push(service_capability(doc.module_id(), &doc.compat(), a));
            }
        }
        let module_id = manifest.module.clone().unwrap_or_else(|| hash.clone());
        let order = self.next_order;
        self.next_order += 1;
        self.implementations.insert(
            hash.clone(),
            Implementation {
                hash: hash.clone(),
                module_id,
                manifest,
                version,
                service,
                health: ServiceHealth::Stopped,
                grants: Arc::new(grants),
                order,
            },
        );
        Ok(hash)
    }

    pub fn implementation(&self, hash: &str) -> Option<&Implementation> {
        self.implementations.get(hash)
    }

    pub fn implementations(&self) -> impl Iterator<Item = &Implementation> {
        self.implementations.values()
    }

    pub fn set_health(&mut self, hash: &str, health: ServiceHealth) {
        if let Some(i) = self.implementations.get_mut(hash) {
            i.health = health;
        }
    }

    /// Does `implementation` serve callers of interface version `target`?
    fn serves(&self, implementation: &Implementation, target: &InterfaceDocument) -> bool {
        implementation.manifest.implements.iter().any(|h| {
            self.interfaces.get(h).is_some_and(|d| {
                d.module_id() == target.module_id()
                    && d.compat() == target.compat()
                    && d.version >= target.version
            })
        })
    }

    /// Highest interface version of `target`'s line that `implementation` implements.
    fn served_version(
        &self,
        implementation: &Implementation,
        target: &InterfaceDocument,
    ) -> Option<semver::Version> {
        implementation
            .manifest
            .implements
            .iter()
            .filter_map(|h| self.interfaces.get(h))
            .filter(|d| d.module_id() == target.module_id() && d.compat() == target.compat())
            .map(|d| d.version.clone())
            .max()
    }

    pub fn set_preference(
        &mut self,
        user: Option<String>,
        interface: &str,
        implementation_module: &str,
    ) -> Result<(), String> {
        let doc = self
            .interfaces
            .get(interface)
            .ok_or_else(|| format!("unknown interface {}", interface))?
            .clone();
        if user.is_some() && doc.doc.selection == Selection::Executor {
            return Err("this interface takes only the admin default".into());
        }
        if !self
            .implementations
            .values()
            .any(|i| i.module_id == implementation_module && self.serves(i, &doc))
        {
            return Err(format!(
                "{} does not implement {}",
                implementation_module, interface
            ));
        }
        self.preferences.insert(
            (user, doc.module_id().to_string(), doc.compat()),
            implementation_module.to_string(),
        );
        Ok(())
    }

    /// Resolve `<target>.<method>` for `user`. Returns the
    /// interface version whose method definition applies, and the
    /// implementation hash.
    pub fn resolve(
        &self,
        target: &str,
        method: &str,
        user: Option<&str>,
    ) -> Result<(Arc<InterfaceDocument>, String), ResolveError> {
        if let Some(i) = self.implementations.get(target) {
            // Pinned: the newest implemented version that has the method.
            let doc = i
                .manifest
                .implements
                .iter()
                .filter_map(|h| self.interfaces.get(h))
                .filter(|d| d.doc.methods.contains_key(method))
                .max_by(|a, b| a.version.cmp(&b.version))
                .cloned()
                .ok_or_else(|| {
                    ResolveError::NotFound(format!("{} has no method `{}`", target, method))
                })?;
            if !i.is_running() {
                return Err(ResolveError::Unavailable(format!(
                    "implementation {} is not running",
                    target
                )));
            }
            return Ok((doc, i.hash.clone()));
        }
        let doc = self
            .interfaces
            .get(target)
            .ok_or_else(|| ResolveError::NotFound(format!("unknown service {}", target)))?
            .clone();
        if !doc.doc.methods.contains_key(method) {
            return Err(ResolveError::NotFound(format!(
                "{} has no method `{}`",
                doc.doc.name, method
            )));
        }
        let chosen = self.choose(&doc, user).ok_or_else(|| {
            ResolveError::Unavailable(format!(
                "no running implementation of {} {}",
                doc.doc.name, doc.version
            ))
        })?;
        Ok((doc, chosen))
    }

    fn choose(&self, doc: &InterfaceDocument, user: Option<&str>) -> Option<String> {
        let mut candidates: Vec<&Implementation> = self
            .implementations
            .values()
            .filter(|i| i.is_running() && self.serves(i, doc))
            .collect();
        candidates.sort_by(|a, b| {
            self.served_version(b, doc)
                .cmp(&self.served_version(a, doc))
                .then(a.order.cmp(&b.order))
        });
        let key = |u: Option<String>| (u, doc.module_id().to_string(), doc.compat());
        let mut preferred = Vec::new();
        if doc.doc.selection == Selection::PerUser {
            if let Some(u) = user {
                preferred.extend(self.preferences.get(&key(Some(u.to_string()))));
            }
        }
        preferred.extend(self.preferences.get(&key(None)));
        for module in preferred {
            if let Some(i) = candidates.iter().find(|i| &i.module_id == module) {
                return Some(i.hash.clone());
            }
        }
        candidates.first().map(|i| i.hash.clone())
    }

    /// The event types (`<hash>.<event>`) under which `implementation`'s
    /// `event` goes out: every registered version of each implemented line,
    /// up to the implemented version, that declares the event.
    pub fn event_targets(&self, implementation: &str, event: &str) -> Vec<Arc<InterfaceDocument>> {
        let Some(i) = self.implementations.get(implementation) else {
            return Vec::new();
        };
        let mut out: Vec<Arc<InterfaceDocument>> = Vec::new();
        for h in &i.manifest.implements {
            let Some(top) = self.interfaces.get(h) else {
                continue;
            };
            for d in self.interfaces.values() {
                if d.module_id() == top.module_id()
                    && d.compat() == top.compat()
                    && d.version <= top.version
                    && d.doc.events.contains_key(event)
                    && !out.iter().any(|o| o.hash == d.hash)
                {
                    out.push(d.clone());
                }
            }
        }
        out
    }

    /// The scope field of a service event type, if it is one.
    pub fn event_scope(&self, event_type: &str) -> Option<String> {
        let (hash, event) = event_type.split_once('.')?;
        self.interfaces
            .get(hash)?
            .doc
            .events
            .get(event)?
            .scope
            .clone()
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::services::builtin::{CallContext, ServiceError, StartContext};
    use crate::services::interface::tests::genesis;
    use async_trait::async_trait;
    use serde_json::json;

    pub(crate) struct Noop;

    #[async_trait]
    impl ServiceImplementation for Noop {
        async fn start(&self, _: StartContext) -> Result<(), String> {
            Ok(())
        }
        async fn stop(&self) -> Result<(), String> {
            Ok(())
        }
        async fn health(&self) -> ServiceHealth {
            ServiceHealth::Running
        }
        async fn call(&self, _: &str, _: Value, _: CallContext) -> Result<Value, ServiceError> {
            Ok(json!({}))
        }
    }

    pub(crate) fn manifest(name: &str, implements: Vec<String>) -> BuiltinManifest {
        BuiltinManifest {
            name: name.into(),
            author: "did:key:z6Mkimpl".into(),
            module: None,
            previous: None,
            version: "1.0.0".into(),
            description: String::new(),
            implements,
            requires: vec![],
            instancing: Instancing::Shared,
            runtime: json!({ "kind": "builtin", "id": name }),
        }
    }

    fn successor(
        prev: &InterfaceDocument,
        version: &str,
        mutate: impl FnOnce(&mut Value),
    ) -> Value {
        let mut raw = prev.raw.clone();
        raw["version"] = json!(version);
        raw["module"] = json!(prev.module_id());
        raw["previous"] = json!(prev.hash);
        mutate(&mut raw);
        raw
    }

    fn running(reg: &mut Registry, name: &str, implements: Vec<String>) -> String {
        let h = reg
            .register_implementation(manifest(name, implements), Arc::new(Noop))
            .unwrap();
        reg.set_health(&h, ServiceHealth::Running);
        h
    }

    #[test]
    fn signatures_are_required_unless_trusted() {
        let mut reg = Registry::default();
        let err = reg
            .register_interface(genesis("did:key:z6Mkx"), None, false)
            .unwrap_err();
        assert!(err.contains("signature"));
        let err = reg
            .register_interface(genesis("did:key:z6Mkx"), Some("00"), false)
            .unwrap_err();
        assert!(err.contains("does not match"));
        assert!(reg
            .register_interface(genesis("did:key:z6Mkx"), None, true)
            .is_ok());
    }

    #[test]
    fn signed_interfaces_register() {
        let key = crate::agent::signatures::TestSigner::generate();
        let did = key.did.clone();
        let doc = InterfaceDocument::parse(genesis(&did)).unwrap();
        let sig = key.sign_string_hex(&doc.hash);
        let mut reg = Registry::default();
        assert_eq!(
            reg.register_interface(genesis(&did), Some(&sig), false)
                .unwrap()
                .hash,
            doc.hash
        );
    }

    #[test]
    fn module_chain_rules() {
        let mut reg = Registry::default();
        let g = reg
            .register_interface(genesis("did:key:z6Mkx"), None, true)
            .unwrap();
        // Unknown previous.
        let mut orphan = successor(&g, "1.0.1", |_| {});
        orphan["previous"] = json!("QmUnknownUnknownUnknown");
        assert!(reg
            .register_interface(orphan, None, true)
            .unwrap_err()
            .contains("not registered"));
        // A MINOR that removes a method never registers.
        let bad = successor(&g, "1.1.0", |r| {
            r["methods"].as_object_mut().unwrap().remove("say");
        });
        assert!(reg
            .register_interface(bad, None, true)
            .unwrap_err()
            .contains("removed"));
        // Same version, other content.
        let dup_next = successor(&g, "1.0.1", |r| r["description"] = json!("a"));
        reg.register_interface(dup_next, None, true).unwrap();
        let dup_again = successor(&g, "1.0.1", |r| r["description"] = json!("b"));
        assert!(reg
            .register_interface(dup_again, None, true)
            .unwrap_err()
            .contains("already exists"));
        // Another author cannot continue the module.
        let foreign = successor(&g, "1.0.2", |r| r["author"] = json!("did:key:z6Mky"));
        assert!(reg
            .register_interface(foreign, None, true)
            .unwrap_err()
            .contains("another author"));
    }

    #[test]
    fn implementation_chains_keep_module_and_author() {
        let mut reg = Registry::default();
        let d = reg
            .register_interface(genesis("did:key:z6Mkx"), None, true)
            .unwrap();
        let g = reg
            .register_implementation(manifest("g", vec![d.hash.clone()]), Arc::new(Noop))
            .unwrap();
        assert_eq!(reg.implementation(&g).unwrap().module_id, g);
        let next = |author: &str, previous: &str| {
            let mut m = manifest("g", vec![d.hash.clone()]);
            m.author = author.into();
            m.version = "1.1.0".into();
            m.module = Some(g.clone());
            m.previous = Some(previous.into());
            m
        };
        let err = reg
            .register_implementation(
                next("did:key:z6Mkimpl", "QmUnknownUnknownUnknown"),
                Arc::new(Noop),
            )
            .unwrap_err();
        assert!(err.contains("not registered"), "{}", err);
        let err = reg
            .register_implementation(next("did:key:z6Mkother", &g), Arc::new(Noop))
            .unwrap_err();
        assert!(err.contains("another author"), "{}", err);
        let v2 = reg
            .register_implementation(next("did:key:z6Mkimpl", &g), Arc::new(Noop))
            .unwrap();
        assert_eq!(reg.implementation(&v2).unwrap().module_id, g);
    }

    #[test]
    fn branched_chains_stay_compatible_with_neighbours() {
        let mut reg = Registry::default();
        let g = reg
            .register_interface(genesis("did:key:z6Mkx"), None, true)
            .unwrap();
        reg.register_interface(
            successor(&g, "1.2.0", |r| {
                r["methods"]["whisper"] = r["methods"]["say"].clone();
            }),
            None,
            true,
        )
        .unwrap();
        // 1.1.0 also follows the genesis and adds `shout`; an implementation
        // of 1.2.0 would serve its callers without it.
        let err = reg
            .register_interface(
                successor(&g, "1.1.0", |r| {
                    r["methods"]["shout"] = r["methods"]["say"].clone();
                }),
                None,
                true,
            )
            .unwrap_err();
        assert!(err.contains("`shout` removed"), "{}", err);
    }

    #[test]
    fn resolution_follows_semver_preferences_and_health() {
        let mut reg = Registry::default();
        let v100 = reg
            .register_interface(genesis("did:key:z6Mkx"), None, true)
            .unwrap();
        let v110 = reg
            .register_interface(
                successor(&v100, "1.1.0", |r| {
                    r["methods"]["shout"] = r["methods"]["say"].clone();
                }),
                None,
                true,
            )
            .unwrap();
        let v2 = reg
            .register_interface(successor(&v110, "2.0.0", |_| {}), None, true)
            .unwrap();

        // Nothing running → 503; unknown → 404.
        assert!(matches!(
            reg.resolve(&v100.hash, "say", None),
            Err(ResolveError::Unavailable(_))
        ));
        assert!(matches!(
            reg.resolve("QmNopeNopeNopeNope", "say", None),
            Err(ResolveError::NotFound(_))
        ));
        assert!(matches!(
            reg.resolve(&v100.hash, "nope", None),
            Err(ResolveError::NotFound(_))
        ));

        let old = running(&mut reg, "old", vec![v100.hash.clone()]);
        let new = running(&mut reg, "new", vec![v110.hash.clone()]);
        let next = running(&mut reg, "next", vec![v2.hash.clone()]);

        // Callers of 1.0.0 get the highest 1.x implementation; 2.x never serves them.
        assert_eq!(reg.resolve(&v100.hash, "say", None).unwrap().1, new);
        // Callers of 1.1.0 cannot use an implementation of 1.0.0 only.
        assert_eq!(reg.resolve(&v110.hash, "shout", None).unwrap().1, new);
        assert_eq!(reg.resolve(&v2.hash, "say", None).unwrap().1, next);

        // A user preference wins for per-user interfaces.
        let old_module = reg.implementation(&old).unwrap().module_id.clone();
        reg.set_preference(Some("alice".into()), &v100.hash, &old_module)
            .unwrap();
        assert_eq!(
            reg.resolve(&v100.hash, "say", Some("alice")).unwrap().1,
            old
        );
        assert_eq!(reg.resolve(&v100.hash, "say", Some("bob")).unwrap().1, new);
        // A preference for an implementation that does not serve the line is refused.
        assert!(reg.set_preference(None, &v110.hash, &old_module).is_err());

        // A stopped implementation drops out; pinning it gives 503.
        reg.set_health(&new, ServiceHealth::Stopped);
        assert_eq!(reg.resolve(&v100.hash, "say", Some("bob")).unwrap().1, old);
        assert!(matches!(
            reg.resolve(&new, "say", None),
            Err(ResolveError::Unavailable(_))
        ));
        assert_eq!(reg.resolve(&old, "say", None).unwrap().0.hash, v100.hash);
    }

    #[test]
    fn executor_selection_ignores_user_preferences() {
        let mut raw = genesis("did:key:z6Mkx");
        raw["selection"] = json!("executor");
        let mut reg = Registry::default();
        let d = reg.register_interface(raw, None, true).unwrap();
        let a = running(&mut reg, "a", vec![d.hash.clone()]);
        let module = reg.implementation(&a).unwrap().module_id.clone();
        assert!(reg
            .set_preference(Some("alice".into()), &d.hash, &module)
            .is_err());
        assert!(reg.set_preference(None, &d.hash, &module).is_ok());
    }

    #[test]
    fn requirements_become_grants() {
        let mut reg = Registry::default();
        let d = reg
            .register_interface(genesis("did:key:z6Mkx"), None, true)
            .unwrap();
        let mut m = manifest("user", vec![d.hash.clone()]);
        m.requires = vec![Requirement {
            interface: d.hash.clone(),
            actions: vec!["SAY".into()],
            optional: false,
        }];
        let h = reg
            .register_implementation(m.clone(), Arc::new(Noop))
            .unwrap();
        let grants = reg.implementation(&h).unwrap().grants.clone();
        assert_eq!(
            grants[0].with.domain,
            format!("service:{}@1", d.module_id())
        );
        m.name = "bad".into();
        m.requires[0].actions = vec!["NOPE".into()];
        assert!(reg
            .register_implementation(m, Arc::new(Noop))
            .unwrap_err()
            .contains("no action"));
    }

    #[test]
    fn events_go_out_under_every_compatible_version() {
        let mut reg = Registry::default();
        let v100 = reg
            .register_interface(genesis("did:key:z6Mkx"), None, true)
            .unwrap();
        let v110 = reg
            .register_interface(
                successor(&v100, "1.1.0", |r| {
                    r["events"]["shouted"] = r["events"]["said"].clone();
                }),
                None,
                true,
            )
            .unwrap();
        let i = running(&mut reg, "i", vec![v110.hash.clone()]);
        let mut said: Vec<String> = reg
            .event_targets(&i, "said")
            .iter()
            .map(|d| d.hash.clone())
            .collect();
        said.sort();
        let mut want = vec![v100.hash.clone(), v110.hash.clone()];
        want.sort();
        assert_eq!(said, want);
        assert_eq!(reg.event_targets(&i, "shouted").len(), 1);
        assert_eq!(
            reg.event_scope(&format!("{}.said", v100.hash)).as_deref(),
            Some("room")
        );
        assert_eq!(reg.event_scope("link-added"), None);
    }
}
