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

tokio::task_local! {
    /// Whether the caller of the request in flight holds `expression:update`.
    /// Set per RPC call and MCP tool call (`with_interact_grant`). Unset in
    /// executor code that is not a request (tests, flows): the agent check
    /// in `run_call_actions` still applies there.
    static CALLER_MAY_INTERACT: bool;
}

/// Runs `request` with `granted` as whether its caller holds
/// `expression:update`, the capability `expression.interact` needs. A call
/// action runs a language interaction, so it needs the same.
pub async fn with_interact_grant<F: std::future::Future>(granted: bool, request: F) -> F::Output {
    CALLER_MAY_INTERACT.scope(granted, request).await
}

/// `Ok` if a write by `context` may run language calls. Until ad4m#1775
/// passes the caller's context to the language, a call runs as the main
/// agent, so only the main agent may make one. A request also needs
/// `expression:update`, as `expression.interact` does.
pub(crate) fn check_call_caller(context: &AgentContext) -> Result<(), AnyError> {
    if !context.is_main_agent() {
        return Err(anyhow!(
            "language calls run as the main agent until ad4m#1775, so a user may not make one"
        ));
    }
    if matches!(CALLER_MAY_INTERACT.try_with(|granted| *granted), Ok(false)) {
        return Err(anyhow!(
            "a write that calls a language needs the expression:update capability"
        ));
    }
    Ok(())
}

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

/// What one call answered: its `MountDelta`, and why its result did
/// not parse as one, if it did not.
pub(super) struct CallAnswer {
    pub(super) delta: MountDelta,
    pub(super) malformed: Option<String>,
}

/// The `CallAnswer` of a call that returned `result`. No result, or JSON
/// `null`, is no delta: accepted, the next resync picks the change up
/// (§3.4). A result that is not a `MountDelta` object is malformed: the
/// call still happened, so it is not an error here, but its delta is
/// dropped and its `address`, when that alone is a string, kept.
pub(super) fn parse_call_result(result: Option<&str>) -> CallAnswer {
    let malformed = |delta: MountDelta, why: String| CallAnswer {
        delta,
        malformed: Some(why),
    };
    let Some(text) = result else {
        return CallAnswer {
            delta: MountDelta::default(),
            malformed: None,
        };
    };
    let value = match serde_json::from_str::<Value>(text) {
        Ok(Value::Null) => {
            return CallAnswer {
                delta: MountDelta::default(),
                malformed: None,
            }
        }
        Ok(value @ Value::Object(_)) => value,
        Ok(_) => return malformed(MountDelta::default(), "not a JSON object".into()),
        Err(e) => return malformed(MountDelta::default(), format!("not JSON: {e}")),
    };
    match serde_json::from_value::<MountDelta>(value.clone()) {
        Ok(delta) => CallAnswer {
            delta,
            malformed: None,
        },
        Err(e) => {
            let address = value
                .get("address")
                .and_then(Value::as_str)
                .map(str::to_string);
            malformed(
                MountDelta {
                    address,
                    ..MountDelta::default()
                },
                e.to_string(),
            )
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
    /// links. Returns the answers in call order.
    pub(super) async fn run_call_actions(
        &self,
        calls: &[CallRun],
        batch_id: Option<&str>,
        context: &AgentContext,
    ) -> Result<Vec<CallAnswer>, AnyError> {
        if calls.is_empty() {
            return Ok(Vec::new());
        }
        check_call_caller(context)?;
        if batch_id.is_some() {
            return Err(anyhow!("call actions cannot run inside a batch"));
        }
        let mut answers = Vec::with_capacity(calls.len());
        for run in calls {
            let (lang, _) = LanguageController::parse_expr_url(&run.source)
                .map_err(|e| anyhow!("call action source {:?}: {e}", run.source))?;
            // The mount the call's delta may land in: the one active
            // before the call went out. An unmount meanwhile wins.
            let generation = self.active_mount_generation(&lang).await;
            let answer = self.execute_call(run, context).await?;
            if let Some(why) = &answer.malformed {
                log::warn!(
                    "call {} on {}: the call succeeded but its result is not a delta ({why}); \
                     its changes wait for the next resync",
                    run.name,
                    run.source
                );
            }
            // Ordered as of its return: the delta is at least as new as
            // anything the language emitted before the call ended.
            let seq = super::next_mount_seq();
            self.apply_call_delta(&lang, &answer.delta, generation, seq, context)
                .await;
            answers.push(answer);
        }
        Ok(answers)
    }

    /// One call: `run.name` on `run.source` with `run.params` as the
    /// single JSON object argument. Returns what the language answered
    /// (`parse_call_result`).
    async fn execute_call(
        &self,
        run: &CallRun,
        context: &AgentContext,
    ) -> Result<CallAnswer, AnyError> {
        // The call runs as `context` (#1775): what the language signs or
        // writes during it (zome calls, its own links) is that agent's.
        // `check_call_caller` still lets only the main agent make a call
        // here, until the run-as-caller PR drops that gate.
        let result = LanguageController::global_instance()
            .expression_interact(
                &run.source,
                &InteractionCall {
                    name: run.name.clone(),
                    parameters_stringified: Value::Object(run.params.clone()).to_string(),
                },
                context,
            )
            .await
            .map_err(|e| anyhow!("{e}"))?;
        Ok(parse_call_result(result.as_deref()))
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
        let answers = self
            .run_call_actions(&calls, batch_id.as_deref(), context)
            .await?;
        let first = answers.first();
        let Some(address) = first.and_then(|a| a.delta.address.clone()) else {
            // The record may exist in the language now: say so, so a
            // client does not retry and make a second one.
            return Err(match first.and_then(|a| a.malformed.as_deref()) {
                Some(why) => anyhow!(
                    "{where_}: the call succeeded but its result was malformed ({why}) and \
                     names no address; the record may exist, do not retry"
                ),
                None => anyhow!("create returned no address"),
            });
        };
        let (scheme, _) =
            LanguageController::parse_expr_url(&calls[0].source).map_err(|e| anyhow!("{e}"))?;
        Ok(Some(format!("{scheme}://{address}")))
    }
}
