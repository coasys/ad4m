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
use we_ad4m::{Ad4mQuerySource, Connection, SpaceBinding, SpaceRow, SourceStats};
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
    /// Read-only and dropped-call lines, newest last.
    notices: Vec<String>,
    /// How many dropped calls `notice` has already folded in.
    dropped_seen: usize,
    /// The first frame painted — `firstScreenOpen` may open now.
    first_frame_open: bool,
    _live: LiveMembers,
}

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
        template: bundle
            .templates
            .as_deref()
            .and_then(|t| t.first())
            .cloned(),
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
        notices: vec![
            "read-only view — writes and calls are dropped, not sent".to_string(),
        ],
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
        let fresh: Vec<String> = host.dropped.iter().skip(self.dropped_seen).cloned().collect();
        self.dropped_seen = host.dropped.len();
        fresh
    }

    /// The notice row under the app — the read-only fact and the newest
    /// dropped call, honestly named.
    pub fn notice_line(&mut self) -> String {
        for call in self.drain_dropped() {
            self.notices.push(format!("dropped: {call}"));
        }
        self.notices.last().cloned().unwrap_or_default()
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
        shared_url: perspective
            .shared_url
            .clone()
            .filter(|u| !u.is_empty()),
    }
}

/// `Connection` over the CLI's already-authenticated socket — the brief's
/// one-line adapter. The token stays inside the shared client.
pub fn connection(client: &ad4m_client::Ad4mClient) -> Connection {
    Connection::new(
        tokio::runtime::Handle::current(),
        client.ws().clone(),
    )
}
