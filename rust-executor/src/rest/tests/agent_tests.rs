//! Regression tests for GET /agent disk-recovery behaviour.
//!
//! Covers the scenario where the in-memory `agent` field is `None` but the
//! agent has already been persisted to disk (e.g. after generate()).
//! The REST handler must reload from disk instead of returning 404.

use crate::agent::AgentService;
use crate::wallet::Wallet;

/// Simulate the regression: generate + save to disk, then clear in-memory
/// state and verify that `ensure_main_agent_loaded` recovers from disk.
///
/// Before the fix, the GET /agent handler would return 404 in this situation
/// because it only checked the in-memory `agent` field.
#[test]
fn get_agent_recovers_from_disk_after_memory_cleared() {
    let tmp = tempfile::tempdir().expect("create temp dir");
    let app_path = tmp.path().to_str().unwrap().to_string();
    std::fs::create_dir_all(format!("{}/ad4m", app_path)).expect("create ad4m dir");

    // Bootstrap wallet (global singleton, idempotent)
    {
        let wallet_instance = Wallet::instance();
        let mut wallet = wallet_instance.lock().expect("wallet lock");
        let wallet_ref = wallet.as_mut().expect("wallet instance");
        wallet_ref.generate_keypair("main".to_string());
    }

    // Set up a fresh AgentService pointing at our temp dir and simulate generate
    let expected_did = {
        let global = AgentService::global_instance();
        let mut lock = global.lock().unwrap();
        *lock = Some(AgentService::new(app_path.clone()));
        let svc = lock.as_mut().unwrap();

        // Simulate POST /agent/generate: create keys + save to disk
        svc.create_new_keys();
        let did = svc
            .agent
            .as_ref()
            .expect("agent must exist after create_new_keys")
            .did
            .clone();

        svc.save("test-passphrase".to_string());

        // Verify agent.json was written to disk
        assert!(
            std::path::Path::new(&format!("{}/ad4m/agent.json", app_path)).exists(),
            "agent.json must exist on disk after save()"
        );

        // Simulate the regression: in-memory state lost
        svc.agent = None;
        did
    };

    // Verify the regression condition: in-memory agent is None but disk has data
    AgentService::with_global_instance(|svc| {
        assert!(svc.agent.is_none(), "in-memory agent should be None");
        assert!(svc.is_initialized(), "agent.json must still be on disk");
    });

    // This is exactly what the fixed REST handlers now do before reading the agent
    AgentService::with_mutable_global_instance(|svc| {
        svc.ensure_main_agent_loaded();
    });

    // Verify the agent was recovered from disk
    let recovered = AgentService::with_global_instance(|svc| svc.agent.clone());
    assert!(
        recovered.is_some(),
        "agent must be recovered from disk instead of remaining None (would cause 404)"
    );

    let agent = recovered.unwrap();
    assert_eq!(
        agent.did, expected_did,
        "recovered agent must have the same DID as the generated one"
    );
    assert!(
        agent.perspective.is_some(),
        "recovered agent must have a perspective"
    );
}
