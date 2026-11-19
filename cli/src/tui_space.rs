//! `o` on the perspectives screen: the selected space rendered through the
//! Coasys template, read-only, over the CLI's already-authenticated socket.
//!
//! The composition mirrors `we-app`'s `live_space_app` (W
//! `we-app/src/live.rs`): the bundle's app root resolving at
//! `/space/<uuid>`, the connected document's allowlisted members
//! underneath, `LiveMembers` serving the store map's live rows, and
//! `Ad4mQuerySource` answering the template's `$queries` — all on one
//! `Connection` built from the CLI's guarded `WsRpcClient`. No second
//! socket, no writes: the host is the offline [`StaticHost`], so a
//! `$invoke` the connected document does not answer lands in its
//! dropped-call log and is named on the notice row.

use std::sync::Arc;

use we_ad4m::live::{LiveMap, LiveMembers};
use we_ad4m::{Ad4mQuerySource, Connection, SourceStats, SpaceBinding, SpaceRow};
use we_data::{OfflineData, Provenance, StoreMap};
use we_expr::{Namespace, Value};
use we_screen::{ScreenSchema, ScreenState};
use we_session::Session;
use we_template::{AppSpec, Bundle, LocalState, NoParts, Runtime, StaticHost, StoreBag};
use we_tui::palette::{load_theme_set, truecolor_env};
use we_tui::TuiApp;

/// The Coasys bundle this slice ships — W `bundles/coasys.bundle.json`,
/// byte for byte.
const BUNDLE: &str = include_str!("../data/coasys.bundle.json");
/// The connected-mode document derived from W — see its `provenance` key.
const CONNECTED: &str = include_str!("../data/coasys-connected.json");

/// A recorded store with its recording left out, as we-app's `GapNs`: what
/// the connected document still holds answers; a read it cannot answer is
/// noted unavailable rather than silently `Undefined`.
#[derive(Debug)]
struct GapNs {
    store: Arc<str>,
    base: Value,
    provenance: Provenance,
}

impl Namespace for GapNs {
    fn get(&self, key: &str) -> Value {
        let v = match &self.base {
            Value::Namespace(ns) => ns.get(key),
            Value::Obj(map) => map.get(key).cloned().unwrap_or(Value::Undefined),
            _ => Value::Undefined,
        };
        if v.is_undefined() {
            self.provenance
                .note_unavailable(format!("{}.{key}", self.store));
        }
        v
    }

    fn keys(&self) -> Vec<Arc<str>> {
        match &self.base {
            Value::Namespace(ns) => ns.keys(),
            Value::Obj(map) => map.keys().cloned().collect(),
            _ => vec![],
        }
    }
}

/// One open space: the app plus the live-member tasks and the notices the
/// CLI status row draws. Dropping it stops the subscriptions.
pub struct SpaceApp {
    /// The terminal frontend — session, focus, drafts, scroll.
    pub app: TuiApp,
    /// The space this window renders.
    pub space: SpaceRow,
    /// The query source's counters — `open` is the live subscription count.
    pub source: Arc<SourceStats>,
    /// What each member read came from — the test reads it.
    pub provenance: Provenance,
    /// The members a live row answers — the test asserts the Coasys groups.
    pub live_members: Vec<String>,
    /// The bag — the notice-row code writes `spaceStore.firstScreenOpen`.
    bag: Arc<StoreBag>,
    /// Dropped-call lines, newest last.
    notices: Vec<String>,
    /// How many dropped calls `notice` has already folded in.
    dropped_seen: usize,
    /// The first frame painted — `firstScreenOpen` may open now.
    first_frame_open: bool,
    _live: LiveMembers,
}

/// First words of the notice row — the view's read-only contract, on
/// every space frame, before any status that joins it.
const READ_ONLY_NOTICE: &str = "read-only view — writes and calls are dropped, not sent";

/// The `$`-types the catalog draws — the gate every frontend wires.
fn known_dollar(ty: &str) -> bool {
    we_catalog::entry(ty).is_some()
}

/// The route a space's index renders at — `/space/<uuid>`.
fn space_route(space: &SpaceRow) -> String {
    format!("/space/{}", space.uuid)
}

fn screen_schema(bundle: &Bundle) -> ScreenSchema {
    ScreenSchema {
        pages: bundle
            .pages
            .as_deref()
            .unwrap_or_default()
            .iter()
            .map(|p| p.id.clone())
            .collect(),
        ..ScreenSchema::default()
    }
}

/// The recorded theme `themeStore.currentThemeId` names, stripped of the
/// `coasys--` prefix the token tables do not carry — `dark` when nothing
/// matches.
fn theme_id() -> &'static str {
    match serde_json::from_str::<serde_json::Value>(CONNECTED)
        .ok()
        .and_then(|doc| {
            doc["stores"]["themeStore"]["currentThemeId"]
                .as_str()
                .map(str::to_string)
        })
        .as_deref()
    {
        Some("coasys--glow") => "glow",
        _ => "dark",
    }
}

/// Assemble the read-only Coasys view of `space` on `conn` — the bundle's
/// app root over the connected document, live members and `$queries`
/// bound to the space, a `StaticHost` for `$invoke`s (writes drop, named
/// on the notice row). Pure setup: every live read runs on `conn`'s
/// runtime, so this never blocks.
pub fn open(conn: &Connection, space: SpaceRow) -> Result<SpaceApp, String> {
    let bundle = Bundle::parse(BUNDLE).map_err(|e| format!("bundle did not parse: {e}"))?;
    let connected: serde_json::Value =
        serde_json::from_str(CONNECTED).map_err(|e| format!("connected doc parse: {e}"))?;
    let live_map = LiveMap::bundled();
    let mut map = StoreMap::parse(we_tui::load::SEED_MAP).map_err(|e| format!("seed map: {e}"))?;
    map.members.extend(live_map.derived.members.clone());
    let data = OfflineData::from_snapshot(&connected, Some(&map))?;
    // A member the connected doc left out answers Undefined and notes
    // itself unavailable — the same role as we-app's DataMode::Live.
    for store in connected["stores"]
        .as_object()
        .into_iter()
        .flatten()
        .map(|(k, _)| k)
    {
        let base = data.bag.root(store).unwrap_or(Value::Undefined);
        data.bag.set_root(
            store,
            Value::Namespace(Arc::new(GapNs {
                store: store.as_str().into(),
                base,
                provenance: data.provenance.clone(),
            })),
        );
    }
    let mut binding = SpaceBinding::new(&space.uuid);
    if let Some(url) = &space.shared_url {
        binding = binding.also(url);
    }
    let live = LiveMembers::install(
        &data.bag,
        &data.provenance,
        &live_map,
        conn,
        &binding,
        space.shared_url.as_deref(),
    );
    let source = Arc::new(Ad4mQuerySource::new(conn.clone(), binding));
    let stats = source.stats();
    let live_members = live_map.members();

    let spec = AppSpec {
        bundle: bundle.name.clone().into(),
        app: bundle.app.clone(),
        template: bundle.templates.as_deref().and_then(|t| t.first()).cloned(),
        pages: bundle.pages.clone().unwrap_or_default(),
        boot: bundle.boot_screen.clone(),
    };
    let screen = ScreenState::desktop(screen_schema(&bundle), &space_route(&space));
    let runtime = Runtime::app(spec, data.bag.clone(), LocalState::new())
        .with_sources(Some(source), Arc::new(NoParts))
        .with_dollar_types(known_dollar);
    let session = Session::from_parts(runtime, Box::new(StaticHost::new(data.bag.clone())), screen);
    // The sidebar's "latest calls" query waits for the first screen — the
    // draw path opens it after the first painted frame, as we-app does.
    data.bag
        .set_host_member("spaceStore", "firstScreenOpen", Value::Bool(false));

    let set = load_theme_set();
    let (theme, _) = set.theme(theme_id());
    let app = TuiApp::new(session, theme.clone(), truecolor_env());
    Ok(SpaceApp {
        app,
        space,
        source: stats,
        provenance: data.provenance.clone(),
        live_members,
        bag: data.bag.clone(),
        notices: Vec::new(),
        dropped_seen: 0,
        first_frame_open: false,
        _live: live,
    })
}

impl SpaceApp {
    /// The first app frame has been painted — what the template held until
    /// now (the sidebar's latest calls) may start.
    fn open_first_screen(&mut self) {
        if !self.first_frame_open {
            self.first_frame_open = true;
            self.bag
                .set_host_member("spaceStore", "firstScreenOpen", Value::Bool(true));
        }
    }

    /// Calls the read-only host dropped since the last drain, in order.
    fn drain_dropped(&mut self) -> Vec<String> {
        let Some(host) = self.app.session.host_as::<StaticHost>() else {
            return Vec::new();
        };
        let fresh: Vec<String> = host
            .dropped
            .iter()
            .skip(self.dropped_seen)
            .cloned()
            .collect();
        self.dropped_seen = host.dropped.len();
        fresh
    }

    /// The notice row under the app — the read-only fact, always, then
    /// the count of reads nothing answered and the newest dropped call.
    pub fn notice_line(&mut self) -> String {
        for call in self.drain_dropped() {
            self.notices.push(format!("dropped: {call}"));
        }
        let mut line = READ_ONLY_NOTICE.to_string();
        let unavailable = self.provenance.report().unavailable.len();
        if unavailable > 0 {
            line.push_str(&format!("  ·  unavailable reads: {unavailable}"));
        }
        if let Some(last) = self.notices.last() {
            line.push_str(&format!("  ·  {last}"));
        }
        line
    }

    /// One frame: resolve + paint into `buf` at `area`, then open the held
    /// first screen.
    pub fn render_into(&mut self, buf: &mut ratatui::buffer::Buffer, area: ratatui::layout::Rect) {
        self.app.render_into(buf, area);
        self.open_first_screen();
    }
}

/// The `o` key's target: the selected visible row as a `SpaceRow`, or
/// `None` when the (filtered) list is empty.
pub fn space_row(perspective: &ad4m_client::types::PerspectiveHandle) -> SpaceRow {
    SpaceRow {
        uuid: perspective.uuid.clone(),
        name: perspective
            .name
            .clone()
            .filter(|n| !n.is_empty())
            .unwrap_or_else(|| perspective.uuid.clone()),
        shared_url: perspective.shared_url.clone().filter(|u| !u.is_empty()),
    }
}

/// `Connection` over the CLI's already-authenticated socket — the brief's
/// one-line adapter. The token stays inside the shared client.
pub fn connection(client: &ad4m_client::Ad4mClient) -> Connection {
    Connection::new(tokio::runtime::Handle::current(), client.ws().clone())
}

/// Test peer: a WebSocket upgrade that then answers nothing — the client
/// connects, every RPC stays pending.
#[cfg(test)]
pub(crate) async fn silent_ws_server() -> (String, tokio::task::JoinHandle<()>) {
    use futures::StreamExt;
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let port = listener.local_addr().unwrap().port();
    let task = tokio::spawn(async move {
        loop {
            let Ok((stream, _)) = listener.accept().await else {
                break;
            };
            tokio::spawn(async move {
                if let Ok(mut ws) = tokio_tungstenite::accept_async(stream).await {
                    while let Some(Ok(_)) = ws.next().await {}
                }
            });
        }
    });
    (format!("http://127.0.0.1:{port}"), task)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ClapApp, Domain};
    use std::collections::HashSet;
    use we_template::{HandlerToken, ResolvedChild, ResolvedNode};

    fn tui_args(url: &str, admin: Option<&str>) -> ClapApp {
        ClapApp {
            domain: Domain::Tui,
            no_capability: admin.is_none(),
            executor_url: Some(url.to_string()),
            admin_credential: admin.map(str::to_string),
            allow_insecure_remote: false,
        }
    }

    /// The adapter the `o` screen uses must be the CLI client's socket
    /// itself — `Arc::ptr_eq` is the check, through the real
    /// `get_ad4m_client` so the transport guard stays in the path.
    #[tokio::test]
    async fn coasys_reuses_guarded_socket() {
        // This test drives `get_ad4m_client`, so it shares the transport
        // probe with `client_transport_tests` — serialize against them.
        let _lock = crate::login::transport_probe::token_dir_lock().await;
        let (url, _server) = silent_ws_server().await;
        let args = tui_args(&url, None);
        let (client, _) = crate::get_ad4m_client(&args, &args.domain)
            .await
            .expect("guarded client connects to the loopback peer");
        let conn = connection(&client);
        assert!(
            Arc::ptr_eq(client.ws(), conn.ws()),
            "the space adapter must share the guarded socket, not dial again"
        );
        assert!(conn.is_connected());

        // A refused executor never gets as far as a socket for the adapter
        // to reuse — the plaintext guard still answers first.
        let refused = tui_args("http://203.0.113.55:9", Some("not-a-real-secret"));
        let err = crate::get_ad4m_client(&refused, &refused.domain)
            .await
            .err()
            .expect("remote plaintext stays refused");
        assert!(err.to_string().contains("plaintext"), "{err}");
    }

    /// First `$invoke`-carrying handler of each node, pre-order — a caller
    /// picks a target the bag cannot answer.
    fn invoke_nodes(root: &ResolvedNode) -> Vec<(we_template::NodeId, String, String)> {
        let mut out = Vec::new();
        for (name, handler) in &root.handlers {
            for token in &handler.tokens {
                if let HandlerToken::Invoke { target, .. } = token {
                    out.push((root.id.clone(), name.to_string(), target.clone()));
                    break;
                }
            }
        }
        for child in &root.children {
            if let ResolvedChild::Node(node) = child {
                out.extend(invoke_nodes(node));
            }
        }
        out
    }

    /// The connected document carries the bundle's allowlist plus the
    /// connected-mode facts — and nothing recorded.
    #[test]
    fn coasys_reads_connected_document() {
        let doc: serde_json::Value = serde_json::from_str(CONNECTED).unwrap();
        assert_eq!(
            doc["queries"].as_object().unwrap().len(),
            0,
            "no recorded queries"
        );
        assert_eq!(
            doc["invokes"].as_object().unwrap().len(),
            0,
            "no recorded invokes"
        );
        let allowed: HashSet<String> = [
            "themeStore.builtInThemes",
            "themeStore.currentThemeId",
            "templateStore.brand",
            "recordStore.displays",
            "sessionStore.bootState",
            "sessionStore.signInStep",
            "identityStore.needsFirstRun",
            "spaceStore.canAdministerCurrentSpace",
        ]
        .into_iter()
        .map(str::to_string)
        .collect();
        for (store, members) in doc["stores"].as_object().unwrap() {
            for member in members.as_object().unwrap().keys() {
                let path = format!("{store}.{member}");
                assert!(allowed.contains(&path), "unlisted member recorded: {path}");
            }
        }
        assert_eq!(
            doc["stores"]["spaceStore"]["canAdministerCurrentSpace"], false,
            "the view is read-only from the first frame"
        );
        assert_eq!(
            doc["stores"]["sessionStore"]["bootState"],
            serde_json::json!("ready")
        );
    }

    /// `o` navigates the assembled session at `/space/<uuid>` — the same
    /// route `we-app` gives the space screen.
    #[tokio::test]
    async fn coasys_open_uses_uuid_route() {
        let (url, _server) = silent_ws_server().await;
        let conn = Connection::open(tokio::runtime::Handle::current(), &url, "")
            .await
            .unwrap();
        let space = open(
            &conn,
            SpaceRow {
                uuid: "11111111-2222-3333-4444-555555555555".to_string(),
                name: "routed".to_string(),
                shared_url: Some("perspective://routed".to_string()),
            },
        )
        .expect("space assembles");
        assert_eq!(
            space.app.session.screen().nav.href(),
            "/space/11111111-2222-3333-4444-555555555555"
        );
    }

    /// `open` mounts the shipped bundle's app root — `Bundle::parse` and
    /// the first resolve see the same `Coasys`/`Column` the file says.
    #[tokio::test]
    async fn coasys_uses_bundle_template() {
        let bundle = Bundle::parse(BUNDLE).expect("the shipped bundle parses");
        assert_eq!(bundle.name, "Coasys");

        let (url, _server) = silent_ws_server().await;
        let conn = Connection::open(tokio::runtime::Handle::current(), &url, "")
            .await
            .unwrap();
        let mut space = open(
            &conn,
            SpaceRow {
                uuid: "bundled".to_string(),
                name: "bundled".to_string(),
                shared_url: None,
            },
        )
        .expect("space assembles");
        let root = space.app.session.resolve();
        assert_eq!(
            root.ty.as_ref(),
            "Column",
            "the mounted root is the bundle's app, not a hand-built shell"
        );
    }

    /// The connected document holds no recorded rows, and a write-like
    /// activation is dropped with a notice — never sent to the executor.
    #[tokio::test]
    async fn coasys_read_only_has_no_recorded_rows() {
        // Assemble over a peer that would hear anything the view sent.
        let (url, _server) = silent_ws_server().await;
        let conn = Connection::open(tokio::runtime::Handle::current(), &url, "")
            .await
            .expect("test connection");
        let mut space = open(
            &conn,
            SpaceRow {
                uuid: "read-only-space".to_string(),
                name: "read only".to_string(),
                shared_url: None,
            },
        )
        .expect("space assembles");

        // There is no write path at all: the host is the offline StaticHost.
        assert!(space.app.session.host_as::<we_ad4m::WriteHost>().is_none());
        assert!(space.app.session.host_as::<StaticHost>().is_some());

        // A write-like activation: dispatch an $invoke the document does
        // not answer. It is recorded as dropped, and the notice names it.
        let root = space.app.session.resolve();
        let bag_reply = |target: &str| space.bag.invoke_reply(target).is_some();
        let candidates: Vec<_> = invoke_nodes(&root)
            .into_iter()
            .filter(|(_, _, target)| !bag_reply(target))
            .take(20)
            .collect();
        assert!(!candidates.is_empty(), "the tree has an $invoke to try");
        let mut dropped = None;
        for (node, handler, target) in candidates {
            space.app.session.dispatch(&node, &handler, Value::Null);
            let host = space.app.session.host_as::<StaticHost>().unwrap();
            if host.dropped.iter().any(|t| t == &target) {
                dropped = Some(target);
                break;
            }
        }
        let target = dropped.expect("an unanswerable $invoke must be dropped");
        let line = space.notice_line();
        assert!(line.starts_with(READ_ONLY_NOTICE), "{line}");
        assert!(line.contains(&format!("dropped: {target}")), "{line}");
    }

    /// The read-only fact leads the notice row on every frame — a dropped
    /// call and a read nothing answered extend it, never replace it.
    #[tokio::test]
    async fn coasys_notice_stays_read_only() {
        let (url, _server) = silent_ws_server().await;
        let conn = Connection::open(tokio::runtime::Handle::current(), &url, "")
            .await
            .unwrap();
        let mut space = open(
            &conn,
            SpaceRow {
                uuid: "notice".to_string(),
                name: "notice".to_string(),
                shared_url: None,
            },
        )
        .expect("space assembles");

        assert_eq!(space.notice_line(), READ_ONLY_NOTICE);

        // A member read nothing can answer is counted, beside read-only.
        if let Some(Value::Namespace(ns)) = space.bag.root("spaceStore") {
            ns.get("aMemberNobodyRecorded");
        }
        let line = space.notice_line();
        assert!(line.starts_with(READ_ONLY_NOTICE), "{line}");
        assert!(line.contains("unavailable reads: 1"), "{line}");

        // A dropped call joins both — read-only still first.
        space
            .app
            .session
            .host_mut_as::<StaticHost>()
            .unwrap()
            .dropped
            .push("aStore.aCall".to_string());
        let line = space.notice_line();
        assert!(line.starts_with(READ_ONLY_NOTICE), "{line}");
        assert!(line.contains("unavailable reads: 1"), "{line}");
        assert!(line.contains("dropped: aStore.aCall"), "{line}");
    }

    /// Real executor, real socket: the guarded client the `tui` command
    /// builds feeds the production adapter, a seeded call renders, a
    /// second call arrives live, and the frame is dumped for review.
    ///
    /// `EXECUTOR_PATH` names the executor binary; with
    /// `AD4M_REQUIRE_EXECUTOR=1` a missing binary fails rather than skips.
    #[test]
    #[ignore = "needs an ad4m-executor binary (EXECUTOR_PATH)"]
    fn coasys_live_space_capture() {
        use ratatui::backend::TestBackend;
        use ratatui::layout::Rect;
        use ratatui::Terminal;
        use std::time::{Duration, Instant};

        let Some(executor) = we_ad4m::testkit::start_executor() else {
            return;
        };
        let rt = we_ad4m::Runtime::new().expect("adapter runtime");
        // `get_ad4m_client` shares the transport probe with
        // `client_transport_tests` — serialize against them.
        let _lock = rt.block_on(crate::login::transport_probe::token_dir_lock());

        // Seed over the testkit's own socket: agent, space,
        // CollectionBlock, one call.
        let seeder = we_ad4m::testkit::connect(&rt, &executor.url);
        let uuid = we_ad4m::testkit::seed_space(&rt, &seeder, "slice3-live");
        we_ad4m::testkit::create_collection(&rt, &seeder, &uuid, "call", "slice3-live-call");

        // The guarded client `ad4m tui` builds — no token, the executor
        // has no admin credential — then the production adapter on top.
        let args = tui_args(&executor.url, None);
        let (client, _) = rt
            .block_on(crate::get_ad4m_client(&args, &args.domain))
            .expect("guarded client connects to the test executor");
        let conn = {
            // `connection` takes the ambient handle; here that is the
            // adapter runtime's.
            let _enter = rt.handle().enter();
            connection(&client)
        };
        assert!(Arc::ptr_eq(client.ws(), conn.ws()));
        let mut space = open(
            &conn,
            SpaceRow {
                uuid: uuid.clone(),
                name: "slice3-live".to_string(),
                shared_url: None,
            },
        )
        .expect("space assembles");

        let (width, height) = (120u16, 40u16);
        let mut terminal = Terminal::new(TestBackend::new(width, height)).unwrap();
        let mut draw = |space: &mut SpaceApp| -> String {
            space.app.session.poll();
            terminal
                .draw(|f| {
                    let area = f.area();
                    space.render_into(f.buffer_mut(), area);
                })
                .unwrap();
            we_tui::buffer_text(terminal.backend().buffer(), Rect::new(0, 0, width, height))
        };

        let started = Instant::now();
        let mut draw_until = |label: &str, space: &mut SpaceApp| {
            let deadline = Instant::now() + Duration::from_secs(10);
            while Instant::now() < deadline {
                let text = draw(space);
                if text.contains(label) {
                    return text;
                }
                std::thread::sleep(Duration::from_millis(200));
            }
            draw(space)
        };
        let mut text = draw_until("slice3-live-call", &mut space);
        assert!(
            text.contains("slice3-live-call"),
            "the seeded call never rendered:\n{text}"
        );

        // A second call arrives on the live subscription — no reopen.
        we_ad4m::testkit::create_collection(&rt, &seeder, &uuid, "call", "slice3-live-call-2");
        text = draw_until("slice3-live-call-2", &mut space);
        assert!(
            text.contains("slice3-live-call-2"),
            "the second call never arrived live:\n{text}"
        );

        // The route carries the uuid; the sidebar shows the Coasys groups.
        let route = format!("/space/{uuid}");
        assert!(text.contains(&route), "no uuid route on screen:\n{text}");
        assert!(
            text.contains("Calls"),
            "the Coasys group labels are missing:\n{text}"
        );

        let out = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../target/1779-slice3");
        std::fs::create_dir_all(&out).expect("capture dir");
        std::fs::write(out.join("live-space.txt"), &text).expect("frame dump");
        let info = rt.block_on(client.runtime.info()).expect("executor info");
        let head = std::process::Command::new("git")
            .args(["-C", env!("CARGO_MANIFEST_DIR"), "rev-parse", "HEAD"])
            .output()
            .ok()
            .filter(|o| o.status.success())
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .unwrap_or_default();
        let meta = serde_json::json!({
            "ad4m": head,
            "we": "aebab9a2e0532000bac8458585a55fa080c0333f",
            "executor": {
                "url": executor.url,
                "version": info.ad4m_executor_version,
            },
            "space": uuid,
            "backend": { "width": width, "height": height },
            "elapsed_ms": started.elapsed().as_millis() as u64,
        });
        std::fs::write(
            out.join("live-space.meta.json"),
            serde_json::to_string_pretty(&meta).unwrap(),
        )
        .expect("capture metadata");
    }
}
