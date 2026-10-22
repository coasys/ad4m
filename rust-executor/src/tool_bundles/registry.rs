//! The tool bundles installed on this node, loaded and ready to call.
//!
//! A bundle exports `toolsets`: named sets of tools, each for records of the classes it lists in
//! `appliesTo`, with its own documentation. Installing one starts its sandbox, reads what it
//! exports, checks it against what is already installed, and keeps the source in the store so it
//! is loaded again at the next boot.
//!
//! **A toolset name is owned by one bundle.** Installing a bundle that exports a name another
//! bundle already does replaces that bundle — which is how an app ships a new release of its
//! tools. A release must export every toolset of the bundle it replaces; one that would drop a
//! toolset is refused, so a release cannot remove tools another toolset's users rely on. A tool
//! name must be unique across every installed toolset, since that is how a call names it.
//!
//! **Who may replace a toolset is whoever may install one** — and installing needs the `language`
//! capabilities and refuses a managed user's session, so that is the node's operator. A toolset
//! name is not tied to the app that first installed it: an operator who installs a bundle
//! exporting another app's toolset name takes that toolset over, writes to its records included.
//! Tying a name to its app needs bundles that say who made them (signed), which this does not do
//! yet.
//!
//! **A record is seen by one call.** Each tool call runs in a fresh isolate of its own, started
//! from the bundle's source and dropped after the call, so nothing a call hands the bundle is
//! there for any other call, whoever makes it. A bundle runs one call at a time, as it did with a
//! single isolate: calls made together wait their turn, so callers cannot multiply the memory a
//! bundle may hold by calling at once. The isolate kept per bundle reads what the bundle
//! exports and serves its documentation; it is never given a record.
//!
//! One registry per process ([`ToolBundleRegistry::global`]), like the executor's other
//! services: the RPC handlers that install bundles take no state but the request, so there is
//! nowhere else for it to live until the services move into one context.

use super::{BundleSandbox, SandboxError, SandboxLimits, ToolBundleStore};
use crate::js_core::utils::content_address;
use log::{error, info, warn};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashSet;
use std::sync::{Arc, OnceLock};
use tokio::sync::{RwLock, Semaphore};
use ts_rs::TS;

/// One tool, as its bundle describes it.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct BundleTool {
    pub name: String,
    pub description: String,
    /// The tool's own arguments, as JSON Schema.
    #[ts(type = "Record<string, unknown>")]
    pub parameters: Value,
    /// Whether a call can answer with properties to write to the record.
    pub writes: bool,
}

/// One documentation topic a toolset serves, without its text.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct BundleDocEntry {
    pub path: String,
    pub title: String,
    pub summary: String,
}

/// A named set of tools for records of some classes.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct Toolset {
    pub name: String,
    /// The subject classes these tools work on.
    pub applies_to: Vec<String>,
    pub tools: Vec<BundleTool>,
    /// Its documentation topics, the one to start from first. Every path is the toolset's name
    /// or starts with it and a `/`.
    pub docs: Vec<BundleDocEntry>,
}

/// An installed bundle: its address and what it exports.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InstalledBundle {
    pub address: String,
    pub toolsets: Vec<Toolset>,
}

/// A tool found by name: what it is, and the bundle to call it in.
#[derive(Clone)]
pub struct FoundTool {
    pub toolset: String,
    pub applies_to: Vec<String>,
    pub tool: BundleTool,
    address: String,
    source: Arc<str>,
    limits: SandboxLimits,
    /// The bundle's one call slot.
    calls: Arc<Semaphore>,
}

impl FoundTool {
    /// Call the tool with `input` (the record, the arguments, the author) and await its answer,
    /// in a fresh isolate that ends with the call, once the bundle's call slot is free.
    pub async fn call(&self, input: Value) -> Result<Value, SandboxError> {
        let found = self.clone();
        // On a task of its own, which holds the slot until the isolate has answered or been
        // stopped: a caller that gives up must not free it while the isolate still runs.
        tokio::spawn(async move {
            let _slot = found
                .calls
                .acquire_owned()
                .await
                .map_err(|_| SandboxError::Closed("the bundle's calls were closed".into()))?;
            let sandbox =
                BundleSandbox::start(found.address.clone(), found.source.clone(), found.limits)
                    .await?;
            sandbox
                .call(
                    &["toolsets", &found.toolset, "call"],
                    vec![json!(found.tool.name), input],
                )
                .await
        })
        .await
        .unwrap_or_else(|e| Err(SandboxError::Closed(format!("the call's task failed: {e}"))))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum RegistryError {
    #[error(transparent)]
    Sandbox(#[from] SandboxError),
    /// The bundle does not export toolsets in the shape a host reads.
    #[error("the bundle's toolsets are not usable: {0}")]
    Invalid(String),
    /// A name it exports is already taken by a bundle it does not replace.
    #[error("{0}")]
    Conflict(String),
    #[error("the bundle could not be stored: {0}")]
    Store(#[from] std::io::Error),
}

struct Loaded {
    bundle: InstalledBundle,
    source: Arc<str>,
    /// Reads the bundle's documentation. Never given a record: see the module doc.
    sandbox: BundleSandbox,
    /// One permit: the bundle runs one record call at a time.
    calls: Arc<Semaphore>,
}

pub struct ToolBundleRegistry {
    store: ToolBundleStore,
    limits: SandboxLimits,
    /// Names no toolset may take: the executor's own documentation topics, which a toolset's
    /// documentation would otherwise shadow or be shadowed by.
    reserved: Vec<String>,
    loaded: RwLock<Vec<Loaded>>,
}

static GLOBAL: OnceLock<ToolBundleRegistry> = OnceLock::new();

impl ToolBundleRegistry {
    pub fn new(store: ToolBundleStore, limits: SandboxLimits, reserved: Vec<String>) -> Self {
        Self {
            store,
            limits,
            reserved,
            loaded: RwLock::new(Vec::new()),
        }
    }

    /// The node's registry, over the default store, with the executor's documentation topics
    /// reserved.
    pub fn global() -> &'static Self {
        GLOBAL.get_or_init(|| {
            // Under test, a directory of the test process's own, never the home directory.
            #[cfg(test)]
            let store = ToolBundleStore::new(
                std::env::temp_dir().join(format!("ad4m-test-tool-bundles-{}", std::process::id())),
            );
            #[cfg(not(test))]
            let store = ToolBundleStore::default_location();
            Self::new(
                store,
                SandboxLimits::default(),
                crate::mcp::tools::docs::executor_topic_names(),
            )
        })
    }

    /// Install a bundle from its source, replacing any bundle that exports a toolset of the same
    /// name. Answers with what it exports. Nothing changes if it does not load, if it would drop
    /// a toolset of a bundle it replaces, if a name it exports belongs to a bundle it would not
    /// replace, or if it cannot be stored.
    pub async fn install(&self, source: &str) -> Result<InstalledBundle, RegistryError> {
        let address = content_address(source);
        if let Some(existing) = self.get(&address).await {
            return Ok(existing);
        }
        let (bundle, sandbox) = self.load(&address, source).await?;

        let mut loaded = self.loaded.write().await;
        let names: HashSet<&str> = bundle.toolsets.iter().map(|t| t.name.as_str()).collect();
        let replaces = |l: &Loaded| {
            l.bundle
                .toolsets
                .iter()
                .any(|t| names.contains(t.name.as_str()))
        };
        // Every check first, and the store, so that nothing installed changes unless all pass.
        if let Some((old, dropped)) = loaded
            .iter()
            .filter(|l| replaces(l))
            .flat_map(|l| l.bundle.toolsets.iter().map(move |t| (l, t)))
            .find(|(_, t)| !names.contains(t.name.as_str()))
        {
            return Err(RegistryError::Conflict(format!(
                "this bundle replaces {}, but does not export its toolset {:?}. A release must \
                 export every toolset of the bundle it replaces; to drop one, remove that bundle \
                 first.",
                old.bundle.address, dropped.name
            )));
        }
        check_tool_names(&bundle, loaded.iter().filter(|l| !replaces(l)))?;
        self.store.put(source)?;

        let (replaced, kept): (Vec<_>, Vec<_>) = loaded.drain(..).partition(|l| replaces(l));
        for old in &replaced {
            info!(
                "[tool-bundles] {} replaces {}",
                bundle.address, old.bundle.address
            );
            if let Err(e) = self.store.remove(&old.bundle.address) {
                warn!(
                    "[tool-bundles] could not remove {} from the store: {}",
                    old.bundle.address, e
                );
            }
        }
        loaded.extend(kept);
        loaded.push(Loaded {
            bundle: bundle.clone(),
            source: Arc::from(source),
            sandbox,
            calls: Arc::new(Semaphore::new(1)),
        });
        info!(
            "[tool-bundles] installed {} ({})",
            bundle.address,
            bundle
                .toolsets
                .iter()
                .map(|t| t.name.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        );
        Ok(bundle)
    }

    /// Load every bundle in the store. One that no longer loads is logged and left out; the
    /// node starts either way.
    pub async fn load_stored(&self) {
        let addresses = match self.store.list() {
            Ok(addresses) => addresses,
            Err(e) => {
                error!("[tool-bundles] could not list the store: {}", e);
                return;
            }
        };
        for address in addresses {
            let source = match self.store.get(&address) {
                Ok(Some(source)) => source,
                Ok(None) => continue,
                Err(e) => {
                    error!("[tool-bundles] {} is unreadable: {}", address, e);
                    continue;
                }
            };
            if let Err(e) = self.install(&source).await {
                error!("[tool-bundles] {} did not load: {}", address, e);
            }
        }
    }

    /// Uninstall a bundle. Answers whether it was installed.
    pub async fn remove(&self, address: &str) -> Result<bool, RegistryError> {
        let mut loaded = self.loaded.write().await;
        let before = loaded.len();
        loaded.retain(|l| l.bundle.address != address);
        let stored = self.store.remove(address)?;
        Ok(stored || loaded.len() != before)
    }

    pub async fn list(&self) -> Vec<InstalledBundle> {
        self.loaded
            .read()
            .await
            .iter()
            .map(|l| l.bundle.clone())
            .collect()
    }

    pub async fn get(&self, address: &str) -> Option<InstalledBundle> {
        self.loaded
            .read()
            .await
            .iter()
            .find(|l| l.bundle.address == address)
            .map(|l| l.bundle.clone())
    }

    /// Every installed toolset.
    pub async fn toolsets(&self) -> Vec<Toolset> {
        self.loaded
            .read()
            .await
            .iter()
            .flat_map(|l| l.bundle.toolsets.clone())
            .collect()
    }

    pub async fn find_tool(&self, name: &str) -> Option<FoundTool> {
        let loaded = self.loaded.read().await;
        loaded.iter().find_map(|l| {
            l.bundle.toolsets.iter().find_map(|toolset| {
                toolset
                    .tools
                    .iter()
                    .find(|t| t.name == name)
                    .map(|tool| FoundTool {
                        toolset: toolset.name.clone(),
                        applies_to: toolset.applies_to.clone(),
                        tool: tool.clone(),
                        address: l.bundle.address.clone(),
                        source: l.source.clone(),
                        limits: self.limits,
                        calls: l.calls.clone(),
                    })
            })
        })
    }

    /// The text of a documentation topic a toolset serves, or None when no toolset serves one at
    /// that path.
    pub async fn read_doc(&self, path: &str) -> Result<Option<String>, SandboxError> {
        let found = {
            let loaded = self.loaded.read().await;
            loaded.iter().find_map(|l| {
                l.bundle.toolsets.iter().find_map(|toolset| {
                    toolset
                        .docs
                        .iter()
                        .any(|d| d.path == path)
                        .then(|| (l.sandbox.clone(), toolset.name.clone()))
                })
            })
        };
        let Some((sandbox, toolset)) = found else {
            return Ok(None);
        };
        let text = sandbox
            .call(&["toolsets", &toolset, "docs", "read"], vec![json!(path)])
            .await?;
        Ok(text.as_str().map(str::to_string))
    }

    /// Start a sandbox for a bundle and read what it exports, without installing it.
    async fn load(
        &self,
        address: &str,
        source: &str,
    ) -> Result<(InstalledBundle, BundleSandbox), RegistryError> {
        let sandbox = BundleSandbox::start(address, source.to_string(), self.limits).await?;
        let exported = sandbox.read(&["toolsets"]).await?;
        let names: Vec<String> = match exported {
            Value::Object(map) if !map.is_empty() => map.keys().cloned().collect(),
            _ => {
                return Err(RegistryError::Invalid(
                    "it exports no `toolsets` object, or an empty one".into(),
                ))
            }
        };

        let mut toolsets = Vec::new();
        for name in names {
            if !is_toolset_name(&name) {
                return Err(RegistryError::Invalid(format!(
                    "{name:?} is not a toolset name: use lowercase letters, digits, `-` and `_`"
                )));
            }
            if self.reserved.contains(&name) {
                return Err(RegistryError::Conflict(format!(
                    "a toolset may not be called {name:?}: that is one of the executor's own \
                     documentation topics"
                )));
            }
            let path = ["toolsets", name.as_str()];
            let applies_to: Vec<String> = parse(
                sandbox.read(&[path[0], path[1], "appliesTo"]).await?,
                &name,
                "appliesTo",
            )?;
            let tools: Vec<BundleTool> = parse(
                sandbox.read(&[path[0], path[1], "tools"]).await?,
                &name,
                "tools",
            )?;
            let docs: Vec<BundleDocEntry> = parse(
                sandbox
                    .call(&[path[0], path[1], "docs", "list"], vec![])
                    .await?,
                &name,
                "docs.list()",
            )?;
            if applies_to.is_empty() || tools.is_empty() {
                return Err(RegistryError::Invalid(format!(
                    "toolset {name} lists no classes or no tools"
                )));
            }
            if let Some(stray) = docs
                .iter()
                .find(|d| d.path != name && !d.path.starts_with(&format!("{name}/")))
            {
                return Err(RegistryError::Invalid(format!(
                    "toolset {name} serves the topic {:?}, outside its own name",
                    stray.path
                )));
            }
            toolsets.push(Toolset {
                name,
                applies_to,
                tools,
                docs,
            });
        }

        let bundle = InstalledBundle {
            address: address.to_string(),
            toolsets,
        };
        check_tool_names(&bundle, [])?;
        Ok((bundle, sandbox))
    }
}

fn parse<T: serde::de::DeserializeOwned>(
    value: Value,
    toolset: &str,
    what: &str,
) -> Result<T, RegistryError> {
    serde_json::from_value(value)
        .map_err(|e| RegistryError::Invalid(format!("toolset {toolset}'s {what}: {e}")))
}

/// A tool's name must be unique within the bundle and across every bundle it is installed beside.
fn check_tool_names<'a>(
    bundle: &InstalledBundle,
    beside: impl IntoIterator<Item = &'a Loaded>,
) -> Result<(), RegistryError> {
    let mut seen: HashSet<&str> = beside
        .into_iter()
        .flat_map(|l| l.bundle.toolsets.iter())
        .flat_map(|t| t.tools.iter().map(|tool| tool.name.as_str()))
        .collect();
    for toolset in &bundle.toolsets {
        for tool in &toolset.tools {
            if !seen.insert(tool.name.as_str()) {
                return Err(RegistryError::Conflict(format!(
                    "a tool called {:?} is already installed",
                    tool.name
                )));
            }
        }
    }
    Ok(())
}

fn is_toolset_name(name: &str) -> bool {
    !name.is_empty()
        && name
            .chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-' || c == '_')
}
