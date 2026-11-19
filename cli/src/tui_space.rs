//! The Coasys app on one perspective, read-only, inside `ad4m tui` (#1779).
//!
//! The screen is the WE renderer's terminal frontend (`we-tui`) over the
//! Coasys bundle, with the template's queries answered by the executor
//! through `we-ad4m`. It uses the socket `ad4m tui` already holds: the
//! adapter gets a clone of the client's `Arc<WsRpcClient>` and the current
//! tokio handle. It does not dial, log in, or start a runtime.
//!
//! Read-only: the host is a `StaticHost`, so an `$invoke` is not run. It is
//! listed in the notice row instead. Nothing recorded is shown: the store
//! document is `data/coasys-connected.json` (the bundle's themes, brand and
//! record displays, and the facts of connected mode), and every row comes
//! from the executor.

use std::sync::Arc;

use ad4m_client::types::PerspectiveHandle;
use ad4m_client::Ad4mClient;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use we_ad4m::live::{LiveMap, LiveMembers};
use we_ad4m::{Ad4mQuerySource, Connection, SourceStats, SpaceBinding, SpaceRow};
use we_data::{OfflineData, StoreMap};
use we_screen::{ScreenSchema, ScreenState};
use we_session::Session;
use we_template::{AppSpec, Bundle, LocalState, NoParts, Runtime, StaticHost, StoreBag};
use we_tui::{KeyResult, TuiApp};

/// `coasys/we` `bundles/coasys.bundle.json`, unchanged.
pub(crate) const BUNDLE: &str = include_str!("../data/coasys.bundle.json");
/// The store document a read-only space starts from. See its `provenance`.
pub(crate) const CONNECTED: &str = include_str!("../data/coasys-connected.json");
/// The `coasys/we` revision the renderer crates and both data files come from.
pub(crate) const WE_REV: &str = "aebab9a2e0532000bac8458585a55fa080c0333f";

/// The rows the space screen keeps for itself above the app.
const CHROME_ROWS: u16 = 2;

/// The adapter's view of the CLI's socket: the same `Arc`, on the current
/// tokio runtime.
pub(crate) fn connection(client: &Ad4mClient) -> Connection {
    Connection::new(tokio::runtime::Handle::current(), client.ws().clone())
}

/// A perspective as the adapter names it. An empty name or URL counts as none.
pub(crate) fn space_row(perspective: &PerspectiveHandle) -> SpaceRow {
    SpaceRow {
        uuid: perspective.uuid.clone(),
        name: perspective
            .name
            .clone()
            .filter(|name| !name.is_empty())
            .unwrap_or_else(|| perspective.uuid.clone()),
        shared_url: perspective.shared_url.clone().filter(|url| !url.is_empty()),
    }
}

/// The route the app opens a space at. Every `Here` query keys by it.
pub(crate) fn space_route(space: &SpaceRow) -> String {
    format!("/space/{}", space.uuid)
}

/// What a key did to the space screen.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SpaceKey {
    /// The app took the key.
    Stay,
    /// Back to the perspectives list: F10, or the app quit (`q`).
    Back,
    /// Ctrl-C: leave `ad4m tui`.
    Exit,
}

/// One open space. Dropping it stops its live reads and subscriptions.
pub(crate) struct CoasysScreen {
    pub(crate) app: TuiApp,
    pub(crate) space: SpaceRow,
    pub(crate) stats: Arc<SourceStats>,
    /// Set until the first frame is drawn; then `firstScreenOpen` opens.
    first_screen: Option<Arc<StoreBag>>,
    /// Declared last: the session drops first, then the live member tasks.
    _live: LiveMembers,
}

/// Open the Coasys app on `space`, read-only, over `conn`.
pub(crate) fn open(conn: &Connection, space: SpaceRow) -> Result<CoasysScreen, String> {
    open_bundle(BUNDLE, conn, space)
}

fn known_dollar(ty: &str) -> bool {
    we_catalog::entry(ty).is_some()
}

pub(crate) fn open_bundle(
    bundle_text: &str,
    conn: &Connection,
    space: SpaceRow,
) -> Result<CoasysScreen, String> {
    let bundle = Bundle::parse(bundle_text).map_err(|e| format!("Coasys bundle: {e}"))?;
    let doc: serde_json::Value =
        serde_json::from_str(CONNECTED).map_err(|e| format!("connected document: {e}"))?;
    let live_map = LiveMap::bundled();
    let mut map =
        StoreMap::parse(we_tui::load::SEED_MAP).map_err(|e| format!("store map: {e}"))?;
    map.members.extend(live_map.derived.members.clone());
    let data = OfflineData::from_snapshot(&doc, Some(&map))?;

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

    let pages = bundle.pages.clone().unwrap_or_default();
    let schema = ScreenSchema {
        pages: pages.iter().map(|page| page.id.clone()).collect(),
        ..ScreenSchema::default()
    };
    let spec = AppSpec {
        bundle: bundle.name.clone().into(),
        app: bundle.app.clone(),
        template: bundle.templates.as_deref().and_then(|t| t.first()).cloned(),
        pages,
        boot: bundle.boot_screen.clone(),
    };
    let screen = ScreenState::desktop(schema, &space_route(&space));
    // What the template holds for after the first screen (the sidebar's
    // latest calls) starts once the first frame is drawn, as in we-app.
    data.bag
        .set_host_member("spaceStore", "firstScreenOpen", we_expr::Value::Bool(false));
    let runtime = Runtime::app(spec, data.bag.clone(), LocalState::new())
        .with_sources(Some(source), Arc::new(NoParts))
        .with_dollar_types(known_dollar);
    let session = Session::from_parts(
        runtime,
        Box::new(StaticHost::new(data.bag.clone())),
        screen,
    );
    let theme = we_tui::palette::load_theme_set().theme("dark").0.clone();
    let app = TuiApp::new(session, theme, we_tui::palette::truecolor_env());
    Ok(CoasysScreen {
        app,
        space,
        stats,
        first_screen: Some(data.bag.clone()),
        _live: live,
    })
}

impl CoasysScreen {
    /// Take in what arrived since the last frame. Never waits.
    pub(crate) fn tick(&mut self) {
        self.app.session.poll();
    }

    /// The route the app shows.
    pub(crate) fn route(&self) -> String {
        self.app.session.screen().nav.href()
    }

    /// The first chrome row: what is open, and how to leave.
    pub(crate) fn header(&self) -> String {
        format!(
            "Coasys (read-only)  {}  {}  ·  F10 back  ·  Ctrl-C quit",
            self.space.name, self.space.uuid
        )
    }

    /// Calls the app asked for that this read-only screen did not run.
    pub(crate) fn dropped(&self) -> Vec<String> {
        self.app
            .session
            .host_as::<StaticHost>()
            .map(|host| host.dropped.clone())
            .unwrap_or_default()
    }

    /// The second chrome row: what the screen does not do.
    pub(crate) fn notice(&self) -> String {
        let dropped = self.dropped();
        match dropped.last() {
            None => "Read-only: changes are not available in the terminal.".to_string(),
            Some(last) => format!(
                "Read-only: {} action(s) not run, last {last}. Changes are not available in the terminal.",
                dropped.len()
            ),
        }
    }

    pub(crate) fn handle_key(&mut self, key: KeyEvent) -> SpaceKey {
        if key.code == KeyCode::Char('c') && key.modifiers.contains(KeyModifiers::CONTROL) {
            return SpaceKey::Exit;
        }
        if key.code == KeyCode::F(10) {
            return SpaceKey::Back;
        }
        match self.app.handle_key(key) {
            KeyResult::Quit => SpaceKey::Back,
            KeyResult::Continue => SpaceKey::Stay,
        }
    }

    /// Draw the chrome and the app into `area` of `buf`.
    pub(crate) fn draw(&mut self, buf: &mut Buffer, area: Rect) {
        let chrome = CHROME_ROWS.min(area.height);
        let bold = Style::default()
            .fg(Color::Black)
            .bg(Color::Cyan)
            .add_modifier(Modifier::BOLD);
        let rows = [
            (self.header(), bold),
            (self.notice(), Style::default().fg(Color::Yellow)),
        ];
        for (y, (text, style)) in rows.iter().enumerate().take(chrome as usize) {
            let row = Rect::new(area.x, area.y + y as u16, area.width, 1);
            buf.set_style(row, *style);
            buf.set_stringn(row.x, row.y, text, area.width as usize, *style);
        }
        let app_area = Rect::new(
            area.x,
            area.y + chrome,
            area.width,
            area.height.saturating_sub(chrome),
        );
        self.app.render_into(buf, app_area);
        if app_area.height > 1 && app_area.width > 0 {
            if let Some(bag) = self.first_screen.take() {
                bag.set_host_member("spaceStore", "firstScreenOpen", we_expr::Value::Bool(true));
            }
        }
    }
}

/// A loopback WebSocket peer for unit tests. It takes sockets and requests.
/// [`FakeWs::start`] never answers, so every read the app starts stays
/// pending; [`FakeWs::refusing`] answers every request with an RPC error.
#[cfg(test)]
pub(crate) mod fake_ws {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::{Arc, Mutex};

    use futures::{SinkExt, StreamExt};
    use tokio::net::TcpListener;
    use tokio_tungstenite::tungstenite::Message;

    pub(crate) struct FakeWs {
        pub(crate) url: String,
        accepted: Arc<AtomicUsize>,
        methods: Arc<Mutex<Vec<String>>>,
    }

    impl FakeWs {
        pub(crate) async fn start() -> Self {
            Self::serve(false).await
        }

        pub(crate) async fn refusing() -> Self {
            Self::serve(true).await
        }

        async fn serve(refuse: bool) -> Self {
            let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
            let url = format!("http://127.0.0.1:{}", listener.local_addr().unwrap().port());
            let accepted = Arc::new(AtomicUsize::new(0));
            let methods = Arc::new(Mutex::new(Vec::new()));
            let (count, seen) = (accepted.clone(), methods.clone());
            tokio::spawn(async move {
                while let Ok((tcp, _)) = listener.accept().await {
                    count.fetch_add(1, Ordering::SeqCst);
                    let seen = seen.clone();
                    tokio::spawn(async move {
                        let Ok(mut ws) = tokio_tungstenite::accept_async(tcp).await else {
                            return;
                        };
                        while let Some(Ok(message)) = ws.next().await {
                            if let Message::Text(text) = message {
                                let frame: serde_json::Value =
                                    serde_json::from_str(&text).unwrap_or_default();
                                let method = frame["type"].as_str().unwrap_or("?").to_string();
                                seen.lock().unwrap().push(method);
                                if refuse {
                                    let reply = serde_json::json!({
                                        "id": frame["id"],
                                        "error": {"code": -32000, "message": "fake peer refuses"},
                                    });
                                    let _ = ws.send(Message::Text(reply.to_string())).await;
                                }
                            }
                        }
                    });
                }
            });
            Self {
                url,
                accepted,
                methods,
            }
        }

        /// Sockets accepted so far.
        pub(crate) fn accepted(&self) -> usize {
            self.accepted.load(Ordering::SeqCst)
        }

        /// RPC methods received so far, in order.
        pub(crate) fn methods(&self) -> Vec<String> {
            self.methods.lock().unwrap().clone()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Labels of the Coasys app's own navigation, present on a live space.
    const LIVE_GROUPS: &[&str] = &[
        "SPACE",
        "Feed",
        "Calls",
        "Workflows",
        "People",
        "Social DNA",
        "Templates",
        "Themes",
    ];

    fn row(uuid: &str) -> SpaceRow {
        SpaceRow {
            uuid: uuid.to_string(),
            name: "Town".to_string(),
            shared_url: None,
        }
    }

    fn frame(screen: &mut CoasysScreen, w: u16, h: u16) -> String {
        let area = Rect::new(0, 0, w, h);
        let mut buf = Buffer::empty(area);
        screen.draw(&mut buf, area);
        we_tui::buffer_text(&buf, area)
    }

    /// The JSON keys of every object in `value`, recursively.
    fn keys(value: &serde_json::Value, out: &mut Vec<String>) {
        match value {
            serde_json::Value::Object(map) => {
                for (key, inner) in map {
                    out.push(key.clone());
                    keys(inner, out);
                }
            }
            serde_json::Value::Array(items) => items.iter().for_each(|item| keys(item, out)),
            _ => {}
        }
    }

    /// Fail when a write RPC reached the executor.
    fn assert_no_writes(methods: &[String]) {
        for method in methods {
            let lower = method.to_lowercase();
            for word in ["add", "create", "update", "remove", "set", "execute", "publish"] {
                assert!(
                    !lower.contains(word),
                    "a read-only screen sent the write-like call {method}"
                );
            }
        }
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn coasys_read_only_has_no_recorded_rows() {
        // The document: allowlisted bundle members and connected facts only.
        let doc: serde_json::Value = serde_json::from_str(CONNECTED).unwrap();
        assert_eq!(doc["queries"], serde_json::json!({}));
        assert_eq!(doc["invokes"], serde_json::json!({}));
        assert_eq!(doc["provenance"]["rev"], WE_REV);
        let mut allowed: Vec<String> = doc["provenance"]["bundle"]
            .as_array()
            .unwrap()
            .iter()
            .map(|m| m.as_str().unwrap().to_string())
            .collect();
        allowed.extend(
            doc["provenance"]["connected"]
                .as_array()
                .unwrap()
                .iter()
                .map(|m| m.as_str().unwrap().to_string()),
        );
        for (store, members) in doc["stores"].as_object().unwrap() {
            for member in members.as_object().unwrap().keys() {
                let path = format!("{store}.{member}");
                assert!(allowed.contains(&path), "{path} is not allowlisted");
            }
        }
        let mut found = Vec::new();
        keys(&doc["stores"], &mut found);
        for recorded in ["members", "profiles", "currentSpace", "did", "author"] {
            assert!(
                !found.iter().any(|k| k == recorded),
                "recorded key {recorded:?} in the connected document"
            );
        }
        assert!(!CONNECTED.contains("did:key:"), "an identity is recorded");

        // A write-like activation is not run, and the notice says so.
        let fake = fake_ws::FakeWs::start().await;
        let client = Ad4mClient::connect(fake.url.clone(), String::new())
            .await
            .unwrap();
        let mut screen = open(&connection(&client), row("space-ro")).unwrap();
        let before = frame(&mut screen, 120, 40);
        assert!(
            before.contains("Read-only: changes are not available"),
            "{before}"
        );
        // Activate every control the first screen offers, one at a time.
        let mut activations = 0;
        for index in 0..screen.app.focusables.len().min(40) {
            screen.app.focus = screen.app.focusables.get(index).map(|f| f.id.clone());
            screen.handle_key(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
            screen.tick();
            frame(&mut screen, 120, 40);
            activations += 1;
        }
        assert!(activations > 0, "the first screen offers no control");
        let invoked = screen.app.session.host_as::<StaticHost>().unwrap();
        assert!(invoked.parked.is_empty());
        let dropped = screen.dropped();
        if !dropped.is_empty() {
            let text = frame(&mut screen, 120, 40);
            assert!(text.contains("action(s) not run"), "{text}");
        }
        assert_no_writes(&fake.methods());
    }

    /// Draw the space into a 120x40 test terminal until `want` shows, or
    /// panic after `limit` with the last frame.
    fn wait_for_text(
        terminal: &mut ratatui::Terminal<ratatui::backend::TestBackend>,
        screen: &mut CoasysScreen,
        want: &str,
        limit: std::time::Duration,
    ) -> String {
        let started = std::time::Instant::now();
        loop {
            screen.tick();
            terminal
                .draw(|frame| {
                    let area = frame.area();
                    screen.draw(frame.buffer_mut(), area);
                })
                .unwrap();
            let area = terminal.backend().buffer().area;
            let text = we_tui::buffer_text(terminal.backend().buffer(), area);
            if text.contains(want) {
                return text;
            }
            assert!(
                started.elapsed() < limit,
                "{want:?} not on screen after {limit:?}:\n{text}"
            );
            std::thread::sleep(std::time::Duration::from_millis(50));
        }
    }

    /// The app on a real executor's space, through the production adapter
    /// and the CLI's guarded client: a seeded call shows, and a call created
    /// afterwards arrives live. Writes `target/1779-slice3/live-space.txt`.
    ///
    /// `AD4M_REQUIRE_EXECUTOR=1 EXECUTOR_PATH=/path/ad4m-executor
    ///  cargo test -p ad4m --bin ad4m coasys_live_space_capture -- --ignored`
    #[test]
    #[ignore = "starts a real executor: set EXECUTOR_PATH"]
    fn coasys_live_space_capture() {
        use std::time::{Duration, Instant};
        use we_ad4m::testkit as kit;

        let Some(executor) = kit::start_executor() else {
            return;
        };
        let rt = we_ad4m::Runtime::new().unwrap();
        let writer = kit::connect(&rt, &executor.url);
        let uuid = kit::seed_space(&rt, &writer, "slice3-live");
        kit::create_collection(&rt, &writer, &uuid, "call", "slice3-live-call");

        // The CLI's own path: the shared transport check, no stored token.
        let _lock = rt.block_on(crate::login::transport_probe::token_dir_lock());
        let tokens = tempfile::tempdir().unwrap();
        std::env::set_var("AD4M_TOKEN_DIR", tokens.path());
        let args = crate::ClapApp {
            domain: crate::Domain::Tui,
            no_capability: false,
            executor_url: Some(executor.url.clone()),
            admin_credential: None,
            allow_insecure_remote: false,
        };
        let (client, url) = rt
            .block_on(crate::get_ad4m_client(&args, &args.domain))
            .unwrap();
        let _enter = rt.handle().enter();
        let conn = connection(&client);
        assert!(Arc::ptr_eq(conn.ws(), client.ws()));
        let base = Arc::strong_count(client.ws());
        let version = rt
            .block_on(client.runtime.info())
            .map(|info| info.ad4m_executor_version)
            .unwrap_or_else(|e| format!("<unknown: {e}>"));
        let handles = rt.block_on(client.perspectives.all()).unwrap();
        let handle = handles.iter().find(|h| h.uuid == uuid).unwrap();

        let started = Instant::now();
        let mut screen = open(&conn, space_row(handle)).unwrap();
        let mut terminal =
            ratatui::Terminal::new(ratatui::backend::TestBackend::new(120, 40)).unwrap();
        let first = wait_for_text(
            &mut terminal,
            &mut screen,
            "slice3-live-call",
            Duration::from_secs(10),
        );
        let seeded_after = started.elapsed();
        kit::create_collection(&rt, &writer, &uuid, "call", "slice3-live-call-2");
        let created = Instant::now();
        let text = wait_for_text(
            &mut terminal,
            &mut screen,
            "slice3-live-call-2",
            Duration::from_secs(10),
        );
        let live_after = created.elapsed();

        assert_eq!(screen.route(), format!("/space/{uuid}"));
        assert!(text.contains(&format!("/space/{uuid}")), "{text}");
        assert!(
            text.contains(&format!("Coasys (read-only)  slice3-live  {uuid}")),
            "{text}"
        );
        for group in LIVE_GROUPS {
            assert!(text.contains(group), "Coasys group {group:?} missing:\n{text}");
        }
        assert!(screen.stats.deliveries.load(std::sync::atomic::Ordering::Relaxed) > 0);
        assert!(screen.dropped().is_empty(), "{:?}", screen.dropped());

        let out = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../target/1779-slice3");
        std::fs::create_dir_all(&out).unwrap();
        std::fs::write(out.join("live-space.txt"), &text).unwrap();
        std::fs::write(out.join("live-space-first.txt"), &first).unwrap();
        let meta = serde_json::json!({
            "test": "coasys_live_space_capture",
            "ad4m": env!("CARGO_PKG_VERSION"),
            "we_rev": WE_REV,
            "executor_version": version,
            "executor_url": url,
            "space": "slice3-live",
            "route": screen.route(),
            "size": "120x40",
            "seeded_call_ms": seeded_after.as_millis() as u64,
            "live_call_ms": live_after.as_millis() as u64,
        });
        std::fs::write(
            out.join("live-space.json"),
            serde_json::to_string_pretty(&meta).unwrap(),
        )
        .unwrap();
        eprintln!("{meta:#}\n{text}");

        // Closing the app lets go of every clone of the socket it took.
        drop(screen);
        let deadline = Instant::now() + Duration::from_secs(10);
        while Arc::strong_count(client.ws()) > base {
            assert!(
                Instant::now() < deadline,
                "the closed app still holds {} clones of the socket",
                Arc::strong_count(client.ws()) - base
            );
            std::thread::sleep(Duration::from_millis(50));
        }
        assert!(client.ws().is_connected());
        std::env::remove_var("AD4M_TOKEN_DIR");
    }

    #[test]
    fn the_space_row_falls_back_to_the_uuid() {
        let handle = PerspectiveHandle {
            name: Some(String::new()),
            neighbourhood: None,
            shared_url: Some(String::new()),
            uuid: "u-1".to_string(),
            state: None,
        };
        let row = space_row(&handle);
        assert_eq!(row.name, "u-1");
        assert_eq!(row.shared_url, None);
        assert_eq!(space_route(&row), "/space/u-1");
    }
}
