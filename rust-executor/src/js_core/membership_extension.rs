//! `globalThis.MEMBERSHIP`: a link language's way into its space's membership (slice 3.B
//! part A7). `ad4m:host` exports it as `membershipLinks`, `membershipAdmits` and
//! `roomKeyEvent`.
//!
//! The server link language syncs a space's membership links apart from its diffs: a
//! membership link in a diff is dropped (R13, `perspectives::ingest`). `membershipLinks` hands
//! them to the space's [`crate::membership::SpaceMembership`] through the owner gate, one at a
//! time, as a peer's links; the language holds no membership state of its own.
//!
//! Each op acts for the perspective of this isolate's own link language (its
//! `language_address`), never one the language names, as the `LANGUAGE_CONTROLLER` ops do.

use deno_core::anyhow::anyhow;
use deno_core::op2;

use crate::js_core::error::AnyhowWrapperError;
use crate::js_core::languages_extension::try_with_isolate_state;
use crate::membership::{MembershipAdmission, MembershipLinkResult, RoomKeyEvent};
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::types::LinkExpression;

/// The address of the language this isolate runs. Read before any await: the isolate state
/// is a thread local.
fn own_address() -> Result<String, AnyhowWrapperError> {
    try_with_isolate_state(|state| state.language_address.clone())
        .flatten()
        .ok_or_else(|| anyhow!("No language is loaded in this runtime").into())
}

/// The perspective whose link language is `address`.
async fn perspective_of(address: String) -> Result<PerspectiveInstance, AnyhowWrapperError> {
    crate::perspectives::perspective_by_link_language(address.clone())
        .await
        .ok_or_else(|| anyhow!("No perspective uses link language {address}").into())
}

/// Writes the membership links the language synced into its space's membership: a result per
/// link, in order.
#[op2(async(lazy))]
#[serde]
async fn membership_links(
    #[serde] links: Vec<LinkExpression>,
) -> Result<Vec<MembershipLinkResult>, AnyhowWrapperError> {
    let perspective = perspective_of(own_address()?).await?;
    Ok(perspective.membership_links_from_link_language(links).await)
}

/// Whether the space admits the key `did_key` now.
#[op2(async(lazy), fast)]
#[serde]
async fn membership_admits(
    #[string] did_key: String,
) -> Result<MembershipAdmission, AnyhowWrapperError> {
    let perspective = perspective_of(own_address()?).await?;
    Ok(crate::membership::membership_admits(&perspective, &did_key).await)
}

/// A room key change: published to the perspective's owners as
/// `neighbourhood-room-key-changed`.
#[op2(async(lazy))]
async fn room_key_event(#[serde] event: RoomKeyEvent) -> Result<(), AnyhowWrapperError> {
    let perspective = perspective_of(own_address()?).await?;
    perspective.publish_room_key_event(event).await;
    Ok(())
}

deno_core::extension!(
    membership_service,
    ops = [membership_links, membership_admits, room_key_event],
    esm_entry_point = "ext:membership_service/membership_extension.js",
    esm = [dir "src/js_core", "membership_extension.js"]
);

#[cfg(test)]
mod tests {
    use crate::agent::signatures::TestSigner;
    use crate::events::Topic;
    use crate::js_core::languages_extension::init_isolate_state;
    use crate::js_core::IsolateState;
    use crate::perspectives::perspective_instance::PerspectiveInstance;
    use crate::pubsub::get_global_pubsub;
    use crate::types::{Link, LinkExpression, PerspectiveHandle};

    /// A7: `MEMBERSHIP.links` routes the language's membership links to its own perspective's
    /// membership (a result per link: a link without an identity behind it does not count, an
    /// ordinary link is refused as malformed, and neither is written), `MEMBERSHIP.admits`
    /// answers from that membership, and `MEMBERSHIP.roomKeyEvent` reaches the perspective's
    /// owners as `neighbourhood-room-key-changed`. A language whose address no perspective
    /// uses is refused.
    #[tokio::test]
    async fn membership_ops_act_on_the_languages_own_perspective() {
        // The owner gate reads the agent: without it the op panics on the isolate's thread
        // and its promise never settles.
        crate::test_utils::setup_wallet();
        crate::db::Ad4mDb::init_global_instance(":memory:").unwrap();
        crate::agent::AgentService::init_global_test_instance();
        crate::prolog_service::init_prolog_service().await;

        let run = uuid::Uuid::new_v4().to_string();
        let own = format!("membership-own-{run}");
        let handle = PerspectiveHandle {
            uuid: format!("membership-perspective-{run}"),
            ..Default::default()
        };
        let perspective = PerspectiveInstance::new(handle.clone(), None);
        perspective
            .ensure_prolog_engine_pool()
            .await
            .expect("prolog engine pool");
        crate::perspectives::register_perspective(handle.uuid.clone(), perspective.clone());
        crate::perspectives::register_link_language_perspective(own.clone(), handle.clone());

        let signer = TestSigner::generate();
        let link = |predicate: &str| -> LinkExpression {
            signer
                .sign(Link {
                    source: "did:scid:coasys:1:Ada".into(),
                    predicate: Some(predicate.into()),
                    target: "coasys://membership/space".into(),
                })
                .into()
        };
        let links = vec![link("coasys://membership/member"), link("test://likes")];
        let hashes: Vec<String> = links
            .iter()
            .map(|l| hex::encode(crate::perspectives::sparql_store::link_hash(l)))
            .collect();

        let pubsub = get_global_pubsub().await;
        let mut events = pubsub.subscribe_events();

        let dir = tempfile::tempdir().unwrap();
        let runtime = crate::languages::language_runtime::LanguageRuntime::new(
            own.clone(),
            dir.path().to_path_buf(),
            false,
        );
        runtime.init().await.unwrap();
        let mut state = IsolateState::new();
        state.language_address = Some(own.clone());
        init_isolate_state(state);

        let call = |expr: String| {
            format!(
                r#"(async () => JSON.stringify(await {expr}))().then(
                    r => "ok " + r, e => "refused: " + (e && e.message))"#
            )
        };
        let out = runtime
            .execute(&call(format!(
                "MEMBERSHIP.links({})",
                serde_json::to_string(&links).unwrap()
            )))
            .await
            .unwrap();
        // `execute` answers the script's value as JSON: here a string.
        let text = serde_json::from_str::<String>(&out).unwrap_or(out.clone());
        let results: Vec<serde_json::Value> = text
            .strip_prefix("ok ")
            .and_then(|json| serde_json::from_str(json).ok())
            .unwrap_or_else(|| panic!("{out}"));
        assert_eq!(results.len(), 2, "{out}");
        assert_eq!(results[0]["hash"], hashes[0]);
        assert_eq!(results[0]["counted"], false);
        assert_eq!(results[1]["hash"], hashes[1]);
        assert_eq!(results[1]["counted"], false);
        assert_eq!(results[1]["reason"], "malformed");
        assert!(perspective
            .sparql_store
            .membership_links()
            .unwrap()
            .is_empty());

        let out = runtime
            .execute(&call(format!(
                "MEMBERSHIP.admits({})",
                serde_json::to_string(&signer.did).unwrap()
            )))
            .await
            .unwrap();
        assert!(out.contains("not-a-member"), "{out}");

        let out = runtime
            .execute(&call(
                r#"MEMBERSHIP.roomKeyEvent({ kind: "rotated", removedKeys: ["did:key:zGone"], version: 3 })"#
                    .to_string(),
            ))
            .await
            .unwrap();
        assert!(out.contains("ok"), "{out}");
        let event = tokio::time::timeout(std::time::Duration::from_secs(5), async {
            loop {
                let event = events.recv().await.unwrap();
                if event.topic == Topic::NeighbourhoodRoomKeyChanged {
                    return event;
                }
            }
        })
        .await
        .expect("neighbourhood-room-key-changed");
        let payload = &event.payload;
        assert_eq!(payload["perspectiveUuid"], handle.uuid);
        assert_eq!(payload["kind"], "rotated");
        assert_eq!(payload["removedKeys"][0], "did:key:zGone");
        assert_eq!(payload["version"], 3);

        // A language no perspective uses has no membership to act on.
        let mut state = IsolateState::new();
        state.language_address = Some(format!("membership-orphan-{run}"));
        init_isolate_state(state);
        let out = runtime
            .execute(&call("MEMBERSHIP.admits(\"did:key:z\")".to_string()))
            .await
            .unwrap();
        assert!(
            out.contains("refused: No perspective uses link language"),
            "{out}"
        );

        crate::perspectives::unregister_perspective(&handle.uuid);
    }
}
