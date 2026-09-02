//! Flow (state machine) tools
//!
//! Tools for managing flows — finite state machines that track expression state transitions.

use super::Ad4mMcpHandler;
use crate::perspectives::flow_context::{
    load_flow_instances, load_shacl_flows, FlowInstanceRecord,
};
use crate::perspectives::perspective_instance::{PerspectiveInstance, SdnaType};
use crate::perspectives::shacl_parser::SHACLFlow;
use crate::types::LinkQuery;
use rmcp::{handler::server::wrapper::Parameters, tool};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::json;

// ============================================================================
// Parameter Types
// ============================================================================

/// Parameters for adding a flow definition
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct AddFlowParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Flow name
    pub flow_name: String,
    /// SHACL flow definition as JSON string
    pub shacl_json: String,
}

/// Parameters for listing flows
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct GetFlowsParams {
    /// Perspective UUID
    pub perspective_id: String,
}

/// Parameters for flow operations on an expression
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct FlowExprParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Flow name
    pub flow_name: String,
    /// Expression address
    pub expression_address: String,
}

/// Parameters for running a flow action
#[derive(Debug, Serialize, Deserialize, JsonSchema)]
pub struct FlowRunActionParams {
    /// Perspective UUID
    pub perspective_id: String,
    /// Flow name
    pub flow_name: String,
    /// Expression address
    pub expression_address: String,
    /// Action name to execute
    pub action_name: String,
}

// ============================================================================
// Tool Implementations
// ============================================================================

impl Ad4mMcpHandler {
    /// Add a flow (state machine definition) to a perspective
    #[tool(
        description = "Register a flow (finite state machine) in a perspective. Flows define states and transitions for expressions."
    )]
    pub async fn add_flow(&self, params: Parameters<AddFlowParams>) -> String {
        let p = &params.0;

        match self.get_writable_perspective(&p.perspective_id).await {
            Ok((mut perspective, agent_context)) => {
                match perspective
                    .add_sdna(
                        p.flow_name.clone(),
                        String::new(),
                        SdnaType::Flow,
                        Some(p.shacl_json.clone()),
                        &agent_context,
                    )
                    .await
                {
                    Ok(_) => {
                        let result = json!({
                            "success": true,
                            "perspective_id": p.perspective_id,
                            "flow_name": p.flow_name,
                        });
                        serde_json::to_string_pretty(&result)
                            .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error adding flow: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// List all flows defined in a perspective
    #[tool(description = "Get all flow (state machine) definitions registered in a perspective.")]
    pub async fn get_flows(&self, params: Parameters<GetFlowsParams>) -> String {
        let p = &params.0;

        match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => {
                match perspective
                    .get_links(&LinkQuery {
                        source: Some("ad4m://self".to_string()),
                        predicate: Some("ad4m://has_flow".to_string()),
                        ..Default::default()
                    })
                    .await
                {
                    Ok(links) => {
                        let flow_names: Vec<String> =
                            links.iter().map(|l| l.data.target.clone()).collect();
                        serde_json::to_string_pretty(&json!({
                            "flows": flow_names,
                            "count": flow_names.len(),
                        }))
                        .unwrap_or_else(|e| format!("Error: {}", e))
                    }
                    Err(e) => format!("Error querying flows: {}", e),
                }
            }
            Err(e) => e,
        }
    }

    /// Get the current state of an expression in a flow
    ///
    /// Reads the live `FlowInstance` row for the expression. The previous
    /// implementation walked `ad4m://stateCheck` raw links, but `stateCheck`
    /// was retired from `FlowState` when the model-level guard replaced it
    /// (design §4.1) — nothing emits that predicate any more, so this tool
    /// could only ever answer "not in any state" regardless of the actual
    /// flow state. State now lives on `FlowInstance.currentState`, which is
    /// what the engine's proposal and consensus passes read and write.
    #[tool(
        description = "Get the current state of an expression within a flow (state machine). Returns the flow instance URI and its current state name."
    )]
    pub async fn flow_state(&self, params: Parameters<FlowExprParams>) -> String {
        let p = &params.0;

        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };

        let (flow_uri, _) = match Self::resolve_flow(&perspective, &p.flow_name).await {
            Ok(resolved) => resolved,
            Err(e) => return format!("Error querying flow state: {:#}", e),
        };

        match Self::flow_instance_for(&perspective, &p.expression_address, &flow_uri).await {
            Err(e) => format!("Error querying flow state: {:#}", e),
            Ok(None) => format!(
                "Expression {} is not in any state of flow {}",
                p.expression_address, p.flow_name
            ),
            Ok(Some(instance)) => serde_json::to_string_pretty(&json!({
                "expression": p.expression_address,
                "flow": flow_uri,
                "state": instance.current_state,
                "instance": instance.instance_uri,
                "started_at": instance.created_at,
            }))
            .unwrap_or_else(|e| format!("Error: {}", e)),
        }
    }

    /// Resolve a caller-supplied flow reference to the canonical flow URI.
    ///
    /// `FlowInstance.flowUri` and the `load_shacl_flows` catalogue are both
    /// keyed by `SHACLFlow::flow_uri()` (`{namespace}{name}Flow`), but the MCP
    /// parameter is historically called `flow_name` and agents pass the bare
    /// human-readable name. Accept either: exact URI hit first, then a name
    /// match against the catalogue. When neither matches (definition removed
    /// but instances remain) the reference is passed through unchanged so the
    /// caller still gets a "not in any state" answer instead of an error.
    ///
    /// A bare name matching *several* flows is rejected rather than resolved.
    /// The name isn't the identity precisely because social-DNA modules from
    /// different communities may share one (PR #929 R5) — picking a winner
    /// would read some other community's flow, and since the catalogue is a
    /// `HashMap` the winner wouldn't even be stable between calls. The error
    /// lists the candidates so the caller can retry with a canonical URI.
    async fn resolve_flow(
        perspective: &PerspectiveInstance,
        flow_ref: &str,
    ) -> anyhow::Result<(String, Option<SHACLFlow>)> {
        let mut flows = load_shacl_flows(perspective).await?;
        if let Some(flow) = flows.remove(flow_ref) {
            return Ok((flow_ref.to_string(), Some(flow)));
        }
        let mut by_name: Vec<String> = flows
            .iter()
            .filter(|(_, flow)| flow.name == flow_ref)
            .map(|(uri, _)| uri.clone())
            .collect();
        by_name.sort();
        match by_name.len() {
            0 => Ok((flow_ref.to_string(), None)),
            1 => {
                let uri = by_name.remove(0);
                let flow = flows.remove(&uri);
                Ok((uri, flow))
            }
            _ => Err(anyhow::anyhow!(
                "flow name `{flow_ref}` is ambiguous on this perspective — \
                 it matches {}. Pass one of those canonical URIs instead.",
                by_name.join(", ")
            )),
        }
    }

    /// The transitions an instance in `current_state` may actually take.
    ///
    /// Pure so the filtering contract — the thing the old implementation got
    /// wrong by returning every transition in the flow — is testable without
    /// a live perspective. Matches `reachable_next_states`' predicate
    /// (`from_state == current_state`, state *names*, not URIs); this one
    /// projects the transition rather than the target state because callers
    /// need the action name to invoke it.
    fn available_transitions(flow: &SHACLFlow, current_state: &str) -> Vec<serde_json::Value> {
        flow.transitions
            .iter()
            .filter(|t| t.from_state == current_state)
            .map(|t| {
                json!({
                    "action": t.action_name,
                    "from_state": t.from_state,
                    "to_state": t.to_state,
                })
            })
            .collect()
    }

    /// Find the live `FlowInstance` binding `expression` to `flow_uri`, if any.
    ///
    /// Shared by [`Self::flow_state`] and [`Self::flow_actions`] so both read
    /// state from exactly one place. Scoped to the single expression URI, so
    /// the loader pushes the filter down to `model_query` instead of sweeping
    /// every instance on the perspective (Model C scope discipline).
    async fn flow_instance_for(
        perspective: &PerspectiveInstance,
        expression: &str,
        flow_uri: &str,
    ) -> anyhow::Result<Option<FlowInstanceRecord>> {
        let instances = load_flow_instances(perspective, &[expression.to_string()]).await?;
        Ok(instances
            .into_iter()
            .find(|record| record.flow_uri == flow_uri))
    }

    /// Get available actions for an expression in a flow
    ///
    /// "Available" means *leaving the state the expression is actually in*.
    /// The previous implementation queried `ad4m://flow_transition` links and
    /// returned every transition target in the flow, unfiltered — so the
    /// answer was identical for an instance in the first state and one in the
    /// last, and identical for an expression with no flow instance at all.
    /// It also read a predicate the SHACL flow serializer never emits
    /// (transitions hang off `ad4m://hasTransition` with `ad4m://fromState` /
    /// `ad4m://toState` children), so in practice the list was always empty.
    #[tool(
        description = "Get the available transition actions for an expression in its current flow state. Returns only transitions whose fromState matches the expression's current state."
    )]
    pub async fn flow_actions(&self, params: Parameters<FlowExprParams>) -> String {
        let p = &params.0;

        let perspective = match self.get_readable_perspective(&p.perspective_id).await {
            Ok(perspective) => perspective,
            Err(e) => return e,
        };

        // Same reference resolution as `flow_state`, and it hands back the
        // parsed definition, so the transition list costs no second read of
        // the flow catalogue.
        let (flow_uri, flow) = match Self::resolve_flow(&perspective, &p.flow_name).await {
            Ok(resolved) => resolved,
            Err(e) => return format!("Error querying flow actions: {:#}", e),
        };

        let instance =
            match Self::flow_instance_for(&perspective, &p.expression_address, &flow_uri).await {
                Err(e) => return format!("Error querying flow actions: {:#}", e),
                Ok(None) => {
                    return format!(
                        "Expression {} is not in any state of flow {}",
                        p.expression_address, p.flow_name
                    )
                }
                Ok(Some(instance)) => instance,
            };

        // An instance can outlive its definition (flow removed from the
        // perspective's SDNA while instances remain). Report that as "no
        // actions from the current state" rather than an error — the same
        // silent-degradation policy the loaders use.
        let Some(flow) = flow else {
            return serde_json::to_string_pretty(&json!({
                "expression": p.expression_address,
                "flow": flow_uri,
                "current_state": instance.current_state,
                "available_actions": Vec::<serde_json::Value>::new(),
                "note": "flow definition not found on this perspective",
            }))
            .unwrap_or_else(|e| format!("Error: {}", e));
        };

        let actions = Self::available_transitions(&flow, &instance.current_state);

        serde_json::to_string_pretty(&json!({
            "expression": p.expression_address,
            "flow": flow_uri,
            "instance": instance.instance_uri,
            "current_state": instance.current_state,
            "available_actions": actions,
        }))
        .unwrap_or_else(|e| format!("Error: {}", e))
    }

    /// Start a flow on an expression
    ///
    /// Still a stub, but no longer for the stated reason: SHACL flow parsing
    /// in Rust exists (`parse_flow_from_links` / `load_shacl_flows`) and the
    /// writer exists (`mint_flow_instance`). What's missing is the write-side
    /// plumbing on this handler — a mutable `PerspectiveInstance` plus an
    /// `AgentContext` to author the mint under, and a duplicate-instance
    /// guard. Keep the message accurate so agents aren't told a capability is
    /// blocked on work that already landed.
    #[tool(
        description = "[NOT YET IMPLEMENTED] Start a flow (state machine) on an expression, putting it into the initial state. Read-only MCP handler cannot author the FlowInstance mint yet."
    )]
    pub async fn flow_start(&self, params: Parameters<FlowExprParams>) -> String {
        let p = &params.0;
        json!({"error": "flow_start is not yet implemented — the MCP flow handler has no authoring path (needs a writable perspective + AgentContext to call mint_flow_instance)", "expression": p.expression_address, "flow": p.flow_name}).to_string()
    }

    /// Execute a transition action on an expression in a flow
    ///
    /// Same stub status and same real blocker as [`Self::flow_start`]: the
    /// engine-side transition writer (`advance_flow_instance_state`) and the
    /// consensus path exist; this handler has no authoring path to them.
    #[tool(
        description = "[NOT YET IMPLEMENTED] Execute a transition action on an expression within a flow (state machine). Read-only MCP handler cannot author the state transition yet."
    )]
    pub async fn flow_run_action(&self, params: Parameters<FlowRunActionParams>) -> String {
        let p = &params.0;
        json!({"error": "flow_run_action is not yet implemented — the MCP flow handler has no authoring path (needs a writable perspective + AgentContext to advance the FlowInstance state)", "expression": p.expression_address, "flow": p.flow_name, "action": p.action_name}).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::flow_classes::mint_flow_instance;
    use crate::perspectives::interpretation_test_support::setup_perspective_no_llm;
    use crate::perspectives::shacl_parser::parse_flow_to_links;
    use crate::types::LinkStatus;

    /// Two states, one transition — so "every transition in the flow"
    /// and "transitions leaving the current state" give different answers
    /// for at least one state. That difference is exactly what the old
    /// unfiltered implementation erased.
    fn delivery_flow_json() -> String {
        delivery_flow_json_in("delivery://")
    }

    /// Same flow shape under a caller-chosen namespace, so a test can seed
    /// two flows that share the name `Delivery` but not their identity.
    fn delivery_flow_json_in(namespace: &str) -> String {
        serde_json::json!({
            "name": "Delivery",
            "namespace": namespace,
            "start_action": [],
            "states": [
                { "name": "identified", "value": 0.0 },
                { "name": "scoped", "value": 0.5 },
            ],
            "transitions": [
                {
                    "action_name": "Scope",
                    "from_state": "identified",
                    "to_state": "scoped",
                    "actions": []
                }
            ],
            "inputTypes": ["ad4m://Task"],
            "outputTypes": [],
        })
        .to_string()
    }

    fn delivery_flow() -> SHACLFlow {
        serde_json::from_str(&delivery_flow_json()).expect("SHACLFlow deserializes from its JSON")
    }

    #[test]
    fn available_transitions_only_leave_the_current_state() {
        let flow = delivery_flow();

        let from_identified = Ad4mMcpHandler::available_transitions(&flow, "identified");
        assert_eq!(
            from_identified.len(),
            1,
            "identified has exactly one outgoing transition, got {from_identified:?}"
        );
        assert_eq!(from_identified[0]["action"], "Scope");
        assert_eq!(from_identified[0]["to_state"], "scoped");

        // The regression lock: `scoped` is terminal in this fixture, so an
        // instance sitting there has nothing available. The pre-fix code
        // returned the whole transition list here — identical output for
        // every state of every instance.
        assert!(
            Ad4mMcpHandler::available_transitions(&flow, "scoped").is_empty(),
            "terminal state must offer no actions"
        );
        assert!(
            Ad4mMcpHandler::available_transitions(&flow, "no-such-state").is_empty(),
            "unknown state must offer no actions"
        );
    }

    /// A bare name that two namespaces both claim must be rejected, not
    /// silently resolved to whichever one the catalogue's `HashMap` happens to
    /// yield first — reading a different community's flow is the failure the
    /// URI-keyed identity (PR #929 R5) exists to prevent.
    #[tokio::test(flavor = "multi_thread")]
    async fn ambiguous_bare_flow_name_is_rejected_with_its_candidates() {
        let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[]).await;

        for namespace in ["delivery://", "othercommunity://"] {
            for link in parse_flow_to_links(&delivery_flow_json_in(namespace), "Delivery")
                .expect("parse_flow_to_links")
            {
                perspective
                    .add_link(link, LinkStatus::Local, None, &ctx)
                    .await
                    .expect("add_link(flow definition)");
            }
        }

        let err = Ad4mMcpHandler::resolve_flow(&perspective, "Delivery")
            .await
            .expect_err("two flows named Delivery ⇒ the bare name must not resolve");
        let msg = format!("{err:#}");
        assert!(
            msg.contains("delivery://DeliveryFlow")
                && msg.contains("othercommunity://DeliveryFlow"),
            "the error must name both candidates so the caller can retry with a URI, got: {msg}"
        );

        // Each canonical URI still resolves unambiguously.
        for uri in ["delivery://DeliveryFlow", "othercommunity://DeliveryFlow"] {
            let (resolved, flow) = Ad4mMcpHandler::resolve_flow(&perspective, uri)
                .await
                .unwrap_or_else(|e| panic!("resolve_flow({uri}) must succeed: {e:#}"));
            assert_eq!(resolved, uri);
            assert!(flow.is_some(), "{uri} must carry its parsed definition");
        }
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn resolve_flow_and_instance_lookup_against_a_live_perspective() {
        let (mut perspective, _shapes, ctx) = setup_perspective_no_llm(&[]).await;

        for link in
            parse_flow_to_links(&delivery_flow_json(), "Delivery").expect("parse_flow_to_links")
        {
            perspective
                .add_link(link, LinkStatus::Local, None, &ctx)
                .await
                .expect("add_link(flow definition)");
        }

        let base_uri = "ad4m://task/onboarding";
        let inst_uri = mint_flow_instance(
            &mut perspective,
            "delivery://DeliveryFlow",
            base_uri,
            "identified",
            "mcp-inst-1",
            None,
            &ctx,
        )
        .await
        .expect("mint_flow_instance");

        // Bare name — what agents actually pass to `flow_name` — resolves to
        // the canonical URI the FlowInstance rows are keyed by.
        let (uri, flow) = Ad4mMcpHandler::resolve_flow(&perspective, "Delivery")
            .await
            .expect("resolve_flow(name)");
        assert_eq!(uri, "delivery://DeliveryFlow");
        assert!(
            flow.is_some(),
            "name lookup must carry the parsed definition"
        );

        // Canonical URI resolves to itself.
        let (uri, flow) = Ad4mMcpHandler::resolve_flow(&perspective, "delivery://DeliveryFlow")
            .await
            .expect("resolve_flow(uri)");
        assert_eq!(uri, "delivery://DeliveryFlow");
        assert!(flow.is_some());

        // Unknown reference passes through undecorated, so the callers can
        // answer "not in any state" instead of erroring.
        let (uri, flow) = Ad4mMcpHandler::resolve_flow(&perspective, "Nonexistent")
            .await
            .expect("resolve_flow(unknown)");
        assert_eq!(uri, "Nonexistent");
        assert!(flow.is_none());

        // Instance lookup is keyed on (expression, flow URI).
        let found =
            Ad4mMcpHandler::flow_instance_for(&perspective, base_uri, "delivery://DeliveryFlow")
                .await
                .expect("flow_instance_for");
        let found = found.expect("the minted instance must be found");
        assert_eq!(found.instance_uri, inst_uri);
        assert_eq!(found.current_state, "identified");
        assert_eq!(found.subject, base_uri);

        assert!(
            Ad4mMcpHandler::flow_instance_for(&perspective, base_uri, "other://OtherFlow")
                .await
                .expect("flow_instance_for(other flow)")
                .is_none(),
            "an instance of a different flow must not match"
        );
        assert!(
            Ad4mMcpHandler::flow_instance_for(
                &perspective,
                "ad4m://task/unrelated",
                "delivery://DeliveryFlow"
            )
            .await
            .expect("flow_instance_for(other subject)")
            .is_none(),
            "an unrelated expression must not match"
        );
    }
}
