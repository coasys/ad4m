//! `call` actions on a shape (design §3): a constructor, setter or
//! destructor entry `{ "action": "call", "source": <url|"this">,
//! "predicate": <interaction name>, "target": <JSON object text> }` calls
//! into a language instead of writing links. `create_subject`,
//! `update_subject` and `remove_subject` make the calls first — before any
//! link run and outside any batch, since a language call cannot be rolled
//! back — and hand each call's `MountDelta` to `apply_call_delta`, where
//! A4 turns it into links.
//!
//! The action's own JSON `target` is parsed as a JSON object and the
//! caller's values merged over it; a call never goes through
//! `resolve_property_value` or `execute_commands`' parameter substitution
//! (§3.4). An action list that mixes calls and link actions is an
//! authoring error.

use std::collections::BTreeMap;

use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use serde_json::{Map, Value};

use super::{Action, Command, PerspectiveInstance};
use crate::agent::AgentContext;
use crate::languages::mount_types::MountDelta;
use crate::languages::LanguageController;
use crate::types::InteractionCall;

/// One language call a write makes: the interaction `name` on the
/// expression URL `source` with the merged JSON object `params`.
pub(super) struct CallRun {
    pub(super) source: String,
    pub(super) name: String,
    pub(super) params: Map<String, Value>,
}

/// A `(source, interaction)` call group under accumulation: every
/// contributing action's `target` defaults and the caller's values, each
/// merged in order on its own side. `into_run` merges all defaults first
/// and all caller values last, so a later action's default can never
/// overwrite an earlier caller value.
pub(super) struct CallGroup {
    source: String,
    name: String,
    defaults: Map<String, Value>,
    values: Map<String, Value>,
}

impl CallGroup {
    /// The `CallRun` this group accumulated: every default under every
    /// caller value.
    pub(super) fn into_run(self) -> CallRun {
        let mut params = self.defaults;
        params.extend(self.values);
        CallRun {
            source: self.source,
            name: self.name,
            params,
        }
    }
}

impl PerspectiveInstance {
    /// Split `commands` into call actions and link actions. A list that
    /// contains a call must contain only calls — anything else would be a
    /// silently dropped write, so it is an error naming the list
    /// (`where_`, e.g. `MountThing.title`, `MountThing.constructor`).
    pub(super) fn partition_actions(
        commands: Vec<Command>,
        where_: &str,
    ) -> Result<(Vec<Command>, Vec<Command>), AnyError> {
        if !commands.iter().any(|c| c.action == Action::Call) {
            return Ok((Vec::new(), commands));
        }
        if commands.iter().any(|c| c.action != Action::Call) {
            return Err(anyhow!(
                "action list of {where_} mixes call and link actions"
            ));
        }
        Ok((commands, Vec::new()))
    }

    /// The JSON object a call action's `target` contributes (usually
    /// "{}"), under which the caller's values are merged. Empty is `{}`;
    /// anything that is not a JSON object is an authoring error, not a
    /// parameter to substitute into.
    fn call_target_object(
        target: Option<&str>,
        where_: &str,
    ) -> Result<Map<String, Value>, AnyError> {
        match target.map(str::trim) {
            None | Some("") => Ok(Map::new()),
            Some(text) => match serde_json::from_str::<Value>(text) {
                Ok(Value::Object(map)) => Ok(map),
                _ => Err(anyhow!(
                    "call action of {where_} has a target that is not a JSON object: {text:?}"
                )),
            },
        }
    }

    /// The `(source, interaction)` key a call command resolves to:
    /// `source` resolved ("this" → the instance URL `instance`, an error
    /// when there is none yet), the interaction name from `predicate`.
    fn call_key(
        command: &Command,
        instance: Option<&str>,
        where_: &str,
    ) -> Result<(String, String), AnyError> {
        let source = match command.source.as_deref().unwrap_or_default() {
            "this" => instance
                .ok_or_else(|| {
                    anyhow!(
                        "call action of {where_} has source \"this\" but there is no instance yet"
                    )
                })?
                .to_string(),
            "" => {
                return Err(anyhow!(
                    "call action of {where_} has no source expression URL"
                ))
            }
            source => source.to_string(),
        };
        let name = command.predicate.clone().ok_or_else(|| {
            anyhow!("call action of {where_} has no predicate to name the interaction")
        })?;
        Ok((source, name))
    }

    /// The resolved `CallRun` of one call command: `source` resolved
    /// ("this" → the instance URL `instance`, an error when there is none
    /// yet), the interaction name from `predicate`, the fixed `target`
    /// params with `values` merged over them.
    fn call_run(
        command: &Command,
        instance: Option<&str>,
        values: Map<String, Value>,
        where_: &str,
    ) -> Result<CallRun, AnyError> {
        let (source, name) = Self::call_key(command, instance, where_)?;
        let mut params = Self::call_target_object(command.target.as_deref(), where_)?;
        params.extend(values);
        Ok(CallRun {
            source,
            name,
            params,
        })
    }

    /// Add `command`'s call to `groups`, keyed by (resolved source,
    /// interaction name): every call of one setter list lands in its
    /// group and shares one call, so `update_subject` of several
    /// properties makes a single `update` carrying each of them (§3.3).
    /// The defaults and the caller's values accumulate on their own side
    /// of the group: merged straight into the params, a later action's
    /// default for an earlier property would overwrite the caller's
    /// explicit value.
    pub(super) fn group_call(
        groups: &mut BTreeMap<(String, String), CallGroup>,
        command: &Command,
        instance: Option<&str>,
        values: Map<String, Value>,
        where_: &str,
    ) -> Result<(), AnyError> {
        let (source, name) = Self::call_key(command, instance, where_)?;
        let defaults = Self::call_target_object(command.target.as_deref(), where_)?;
        groups
            .entry((source.clone(), name.clone()))
            .and_modify(|g| {
                g.defaults.extend(defaults.clone());
                g.values.extend(values.clone());
            })
            .or_insert(CallGroup {
                source,
                name,
                defaults,
                values,
            });
        Ok(())
    }

    /// Run `calls` in order and hand each returned `MountDelta` to
    /// `apply_call_delta`. A caller batch is refused before the first
    /// call: a language call cannot be rolled back with the batch's
    /// links. Returns the deltas in call order.
    pub(super) async fn run_call_actions(
        &self,
        calls: &[CallRun],
        batch_id: Option<&str>,
        context: &AgentContext,
    ) -> Result<Vec<MountDelta>, AnyError> {
        if calls.is_empty() {
            return Ok(Vec::new());
        }
        if batch_id.is_some() {
            return Err(anyhow!("call actions cannot run inside a batch"));
        }
        let mut deltas = Vec::with_capacity(calls.len());
        for run in calls {
            let (lang, _) = LanguageController::parse_expr_url(&run.source)
                .map_err(|e| anyhow!("call action source {:?}: {e}", run.source))?;
            // The mount the call's delta may land in: the one active
            // before the call went out. An unmount meanwhile wins.
            let generation = self.active_mount_generation(&lang).await;
            let delta = self.execute_call(run).await?;
            // Ordered as of its return: the delta is at least as new as
            // anything the language emitted before the call ended.
            let seq = super::next_mount_seq();
            self.apply_call_delta(&lang, &delta, generation, seq, context)
                .await;
            deltas.push(delta);
        }
        Ok(deltas)
    }

    /// One call: `run.name` on `run.source` with `run.params` as the
    /// single JSON object argument. Returns the `MountDelta` the language
    /// answered, an empty one when the result is no delta object at all —
    /// accepted, the next resync picks the change up (§3.4).
    async fn execute_call(&self, run: &CallRun) -> Result<MountDelta, AnyError> {
        // `expression_interact` takes no agent context, so the call runs
        // as the executor's main agent: whatever the language signs or
        // writes during it (zome calls, its own links) is the main agent's,
        // whoever made the write, until ad4m#1775 passes the caller's
        // context through.
        let result = LanguageController::global_instance()
            .expression_interact(
                &run.source,
                &InteractionCall {
                    name: run.name.clone(),
                    parameters_stringified: Value::Object(run.params.clone()).to_string(),
                },
            )
            .await
            .map_err(|e| anyhow!("{e}"))?;
        let Some(text) = result else {
            return Ok(MountDelta::default());
        };
        let Ok(value) = serde_json::from_str::<Value>(&text) else {
            return Ok(MountDelta::default());
        };
        Ok(serde_json::from_value(value).unwrap_or_default())
    }

    /// Where the delta a `call` returns becomes links: the same per-record
    /// expansion `sync_mount` runs, so a created record is queryable before
    /// the call's RPC returns (A4). Only into the mount `generation` names
    /// (active when the call went out): a language this perspective does
    /// not mount, or no longer mounts, gets no links. A failure is logged,
    /// not fatal — the next `sync_mount` converges the store with the
    /// snapshot anyway.
    async fn apply_call_delta(
        &self,
        lang_address: &str,
        delta: &MountDelta,
        generation: Option<u64>,
        seq: u64,
        _context: &AgentContext,
    ) {
        let Some(generation) = generation else {
            log::debug!(
                "call action on {lang_address}: not mounted in perspective {}; its delta is not applied",
                self.uuid
            );
            return;
        };
        if let Err(e) = self
            .clone()
            .apply_mount_delta(lang_address, delta, generation, seq)
            .await
        {
            log::warn!("call action on {lang_address}: applying its delta failed: {e}");
        }
    }

    /// `create_subject` for a class whose constructor calls into a
    /// language: the whole `initial_values` object is the call's
    /// parameters, the caller's `expression_address` is ignored, and the
    /// call's `address` names the new instance (§3.3).
    pub(super) async fn create_through_call(
        &self,
        class_name: &str,
        commands: Vec<Command>,
        initial_values: Option<Value>,
        batch_id: Option<String>,
        context: &AgentContext,
    ) -> Result<Option<String>, AnyError> {
        let where_ = format!("{class_name}.constructor");
        let (call_commands, _) = Self::partition_actions(commands, &where_)?;
        let values = match initial_values {
            Some(Value::Object(map)) => map,
            _ => Map::new(),
        };
        let mut calls = Vec::with_capacity(call_commands.len());
        for command in &call_commands {
            // A constructor has no instance for `source: "this"` to mean.
            calls.push(Self::call_run(command, None, values.clone(), &where_)?);
        }
        let deltas = self
            .run_call_actions(&calls, batch_id.as_deref(), context)
            .await?;
        let Some(address) = deltas.first().and_then(|d| d.address.clone()) else {
            return Err(anyhow!("create returned no address"));
        };
        let (scheme, _) =
            LanguageController::parse_expr_url(&calls[0].source).map_err(|e| anyhow!("{e}"))?;
        Ok(Some(format!("{scheme}://{address}")))
    }

    /// Whether `class_name`'s constructor makes a language call — MCP's
    /// `instance_create` checks this to keep its batch out of
    /// `create_subject`'s way, since calls cannot run inside one.
    pub(crate) async fn constructor_is_call(&self, class_name: &str) -> Result<bool, AnyError> {
        Ok(self
            .get_constructor_actions(class_name)
            .await?
            .iter()
            .any(|c| c.action == Action::Call))
    }

    /// `class_name`'s `ad4m://setter` for `prop` classified as calls or
    /// link actions, `None` when the shape declares none. A write that
    /// involves a `Call` setter cannot run inside a batch
    /// (`run_call_actions`), so `instances` asks here before it opens its
    /// own.
    pub(crate) async fn property_setter_kind(
        &self,
        class_name: &str,
        prop: &str,
    ) -> Result<Option<SetterKind>, AnyError> {
        Ok(self
            .get_property_setter_actions(class_name, prop)
            .await?
            .map(|commands| {
                if commands.iter().any(|c| c.action == Action::Call) {
                    SetterKind::Call
                } else {
                    SetterKind::Links
                }
            }))
    }

    /// `class_name`'s `ad4m://destructor` classified as calls or link
    /// actions, `None` when the shape declares none. As with the setters,
    /// a `call` destructor cannot run inside a batch.
    pub(crate) async fn destructor_kind(
        &self,
        class_name: &str,
    ) -> Result<Option<SetterKind>, AnyError> {
        Ok(self
            .get_shape_actions_from_shacl(class_name, "ad4m://destructor")
            .await?
            .map(|commands| {
                if commands.iter().any(|c| c.action == Action::Call) {
                    SetterKind::Call
                } else {
                    SetterKind::Links
                }
            }))
    }
}

/// How a property's `ad4m://setter` writes: language calls or links.
/// `call_actions` refuses a mixed list (`partition_actions`), so one
/// `Call` member classifies the whole setter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SetterKind {
    /// `call` actions — the write calls into a language and cannot run
    /// inside a batch.
    Call,
    /// Link actions (`collectionSetter`, `addLink`, `setSingleTarget`, …).
    Links,
}
