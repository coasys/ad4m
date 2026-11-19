//! Admin and debug screens for `ad4m tui`.
//!
//! Three screens, in this order: executor status, the list of perspectives
//! and neighbourhoods, and the links of one perspective. Keys `1`, `2`, and
//! `3` open those screens. Tab moves to the next screen. The open screen
//! loads again every two seconds. A slow load does not block `q` or Ctrl-C.
//! The links screen shows at most 200 links.
//!
//! On the perspectives screen, `o` opens the selected perspective in the
//! Coasys app, read-only ([`crate::tui_space`]). While it is open the admin
//! screens do not load, and every key except F10 and Ctrl-C goes to the app.
//! F10, or the app's own quit, comes back to the perspectives list.

use std::future::Future;
use std::io::{self, Stdout};
use std::pin::Pin;
use std::time::{Duration, Instant};

use ad4m_client::connect::normalize_base_url;
use ad4m_client::types::{AgentStatus, LinkExpression, PerspectiveHandle, RuntimeInfo};
use ad4m_client::Ad4mClient;
use anyhow::{Context, Result};
use we_ad4m::{Connection, SpaceRow};

use crate::tui_space::{self, CoasysScreen, SpaceKey};
use crossterm::{
    cursor::{Hide, Show},
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent, KeyEventKind,
        KeyModifiers,
    },
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, ListState, Paragraph},
    Frame, Terminal,
};

const REFRESH: Duration = Duration::from_secs(2);
/// How often the input loop looks for a key while a refresh is in flight.
const INPUT_SLICE: Duration = Duration::from_millis(50);
const LINK_LIMIT: i64 = 200;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Screen {
    Status,
    Perspectives,
    Links,
}

impl Screen {
    fn next(self) -> Self {
        match self {
            Screen::Status => Screen::Perspectives,
            Screen::Perspectives => Screen::Links,
            Screen::Links => Screen::Status,
        }
    }

    fn title(self) -> &'static str {
        match self {
            Screen::Status => "Executor status",
            Screen::Perspectives => "Perspectives and neighbourhoods",
            Screen::Links => "Links",
        }
    }
}

struct Model {
    executor_url: String,
    screen: Screen,
    agent: Option<AgentStatus>,
    runtime: Option<RuntimeInfo>,
    perspectives: Vec<PerspectiveHandle>,
    selected: usize,
    neighbourhoods_only: bool,
    links: Vec<LinkExpression>,
    link_selected: usize,
    open_uuid: Option<String>,
    /// The perspective `o` asked to open in the Coasys app.
    open_request: Option<SpaceRow>,
    /// The open Coasys app. While it is set the admin screens do not load.
    space: Option<CoasysScreen>,
    error: Option<String>,
    loaded_at: Option<Instant>,
    /// Bumped when the user asks for a new load. An in-flight result from an
    /// older generation is dropped, so `r` and a screen change are not overwritten.
    refresh_gen: u64,
}

impl Model {
    fn new(executor_url: String) -> Self {
        Self {
            executor_url: display_executor_url(&executor_url),
            screen: Screen::Status,
            agent: None,
            runtime: None,
            perspectives: Vec::new(),
            selected: 0,
            neighbourhoods_only: false,
            links: Vec::new(),
            link_selected: 0,
            open_uuid: None,
            open_request: None,
            space: None,
            error: None,
            loaded_at: None,
            refresh_gen: 0,
        }
    }

    fn stale(&self) -> bool {
        match self.loaded_at {
            None => true,
            Some(at) => at.elapsed() >= REFRESH,
        }
    }

    fn refresh_request(&self) -> RefreshRequest {
        RefreshRequest {
            screen: self.screen,
            open_uuid: self.open_uuid.clone(),
            gen: self.refresh_gen,
        }
    }

    fn visible(&self) -> Vec<usize> {
        self.perspectives
            .iter()
            .enumerate()
            .filter(|(_, perspective)| {
                !self.neighbourhoods_only || perspective.neighbourhood.is_some()
            })
            .map(|(index, _)| index)
            .collect()
    }

    fn clamp_selection(&mut self) {
        let visible = self.visible();
        if visible.is_empty() {
            self.selected = 0;
        } else if self.selected >= visible.len() {
            self.selected = visible.len() - 1;
        }
        if self.links.is_empty() {
            self.link_selected = 0;
        } else if self.link_selected >= self.links.len() {
            self.link_selected = self.links.len() - 1;
        }
    }
}

fn display_executor_url(url: &str) -> String {
    normalize_base_url(url)
}

/// Text rows for the status screen. The executor URL has no userinfo, query, or fragment.
pub(crate) fn status_lines(
    executor_url: &str,
    agent: Option<&AgentStatus>,
    runtime: Option<&RuntimeInfo>,
) -> Vec<String> {
    let mut lines = vec![format!("Executor: {}", display_executor_url(executor_url))];
    match agent {
        Some(agent) => {
            lines.push(format!("DID: {}", agent.did.as_deref().unwrap_or("<none>")));
            lines.push(format!("Initialized: {}", agent.is_initialized));
            lines.push(format!("Unlocked: {}", agent.is_unlocked));
            if let Some(error) = &agent.error {
                lines.push(format!("Agent error: {error}"));
            }
        }
        None => lines.push("Agent status: <not loaded>".to_string()),
    }
    match runtime {
        Some(runtime) => {
            lines.push(format!("Version: {}", runtime.ad4m_executor_version));
            lines.push(format!(
                "Commit: {}",
                runtime.ad4m_executor_commit.as_deref().unwrap_or("<none>")
            ));
        }
        None => lines.push("Runtime info: <not loaded>".to_string()),
    }
    lines
}

/// Title line shared by every screen. The URL is the normalized executor address.
fn header_text(screen: Screen, executor_url: &str) -> String {
    format!("{}  {}", screen.title(), display_executor_url(executor_url))
}

/// One perspective row. A neighbourhood row includes its link language.
pub(crate) fn perspective_label(perspective: &PerspectiveHandle) -> String {
    let name = perspective
        .name
        .clone()
        .unwrap_or_else(|| "<unnamed>".to_string());
    let mut line = format!("{name}  {}", perspective.uuid);
    if let Some(neighbourhood) = &perspective.neighbourhood {
        line.push_str(&format!(
            "  neighbourhood {}",
            neighbourhood.data.link_language
        ));
    }
    if perspective.shared_url.is_some() {
        line.push_str("  shared");
    }
    line
}

/// One link row: source, predicate, target.
pub(crate) fn link_label(link: &LinkExpression) -> String {
    format!(
        "{}  {}  {}",
        link.data.source,
        link.data.predicate.as_deref().unwrap_or(""),
        link.data.target
    )
}

pub(crate) fn neighbourhood_count(perspectives: &[PerspectiveHandle]) -> usize {
    perspectives
        .iter()
        .filter(|perspective| perspective.neighbourhood.is_some())
        .count()
}

/// The links list is capped. A full page of 200 is not proof that more exist.
pub(crate) fn links_title(uuid: Option<&str>, shown: usize) -> String {
    let Some(uuid) = uuid else {
        return "no perspective open".to_string();
    };
    if shown >= LINK_LIMIT as usize {
        format!("perspective {uuid} — list capped at {LINK_LIMIT}")
    } else {
        format!("perspective {uuid} — at most {LINK_LIMIT} links")
    }
}

/// The admin screens load only while the Coasys app is closed.
fn should_start_refresh(model: &Model, inflight: bool) -> bool {
    model.space.is_none() && model.stale() && !inflight
}

fn invalidate(model: &mut Model) {
    model.refresh_gen = model.refresh_gen.wrapping_add(1);
    model.loaded_at = None;
}

pub(crate) fn key_quits(key: &KeyEvent) -> bool {
    match key.code {
        KeyCode::Char('q') => true,
        KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => true,
        _ => false,
    }
}

struct RefreshRequest {
    screen: Screen,
    open_uuid: Option<String>,
    gen: u64,
}

struct RefreshOutcome {
    gen: u64,
    error: Option<String>,
    agent: Option<AgentStatus>,
    wrote_agent: bool,
    runtime: Option<RuntimeInfo>,
    wrote_runtime: bool,
    perspectives: Option<Vec<PerspectiveHandle>>,
    links: Option<Vec<LinkExpression>>,
}

impl RefreshOutcome {
    fn blank() -> Self {
        Self {
            gen: 0,
            error: None,
            agent: None,
            wrote_agent: false,
            runtime: None,
            wrote_runtime: false,
            perspectives: None,
            links: None,
        }
    }
}

fn apply_outcome(model: &mut Model, outcome: RefreshOutcome) {
    if outcome.gen != model.refresh_gen {
        return;
    }
    if outcome.wrote_agent {
        model.agent = outcome.agent;
    }
    if outcome.wrote_runtime {
        model.runtime = outcome.runtime;
    }
    if let Some(perspectives) = outcome.perspectives {
        model.perspectives = perspectives;
    }
    if let Some(links) = outcome.links {
        model.links = links;
    }
    model.error = outcome.error;
    model.loaded_at = Some(Instant::now());
    model.clamp_selection();
}

async fn fetch(client: &Ad4mClient, request: RefreshRequest) -> RefreshOutcome {
    let RefreshRequest {
        screen,
        open_uuid,
        gen,
    } = request;
    match screen {
        Screen::Status => fetch_status(client, gen).await,
        Screen::Perspectives => match client.perspectives.all().await {
            Ok(perspectives) => RefreshOutcome {
                gen,
                perspectives: Some(perspectives),
                ..RefreshOutcome::blank()
            },
            Err(error) => RefreshOutcome {
                gen,
                error: Some(error.to_string()),
                ..RefreshOutcome::blank()
            },
        },
        Screen::Links => fetch_links(client, open_uuid, gen).await,
    }
}

async fn fetch_status(client: &Ad4mClient, gen: u64) -> RefreshOutcome {
    // Both calls run together. A failed agent status still drops the runtime
    // value, matching the old `agent?` short-circuit: the previous rows stay.
    let (agent, runtime) = tokio::join!(client.agent.status(), client.runtime.info());
    match agent {
        Err(error) => RefreshOutcome {
            gen,
            error: Some(error.to_string()),
            ..RefreshOutcome::blank()
        },
        Ok(agent) => match runtime {
            Err(error) => RefreshOutcome {
                gen,
                error: Some(error.to_string()),
                agent: Some(agent),
                wrote_agent: true,
                ..RefreshOutcome::blank()
            },
            Ok(runtime) => RefreshOutcome {
                gen,
                agent: Some(agent),
                wrote_agent: true,
                runtime: Some(runtime),
                wrote_runtime: true,
                ..RefreshOutcome::blank()
            },
        },
    }
}

async fn fetch_links(client: &Ad4mClient, open_uuid: Option<String>, gen: u64) -> RefreshOutcome {
    let Some(uuid) = open_uuid else {
        return RefreshOutcome {
            gen,
            links: Some(Vec::new()),
            ..RefreshOutcome::blank()
        };
    };
    match client
        .perspectives
        .query_links(uuid, None, None, None, None, None, Some(LINK_LIMIT))
        .await
    {
        Ok(links) => RefreshOutcome {
            gen,
            links: Some(links),
            ..RefreshOutcome::blank()
        },
        Err(error) => RefreshOutcome {
            gen,
            error: Some(error.to_string()),
            ..RefreshOutcome::blank()
        },
    }
}

enum DriveEvent {
    Quit,
    Key(KeyEvent),
    Refresh(Box<RefreshOutcome>),
    Tick,
}

type BoxedRefresh<'a> = Pin<Box<dyn Future<Output = RefreshOutcome> + Send + 'a>>;

/// Wait one input slice, or until `inflight` finishes.
///
/// `biased` polls the sleep first. When the slice has elapsed it does not poll
/// the refresh, so that future can go back into `inflight` unharmed. A quit
/// drops it. The key source runs only after the slice, never behind the RPC.
async fn refresh_or_quit<F, K>(inflight: &mut Option<F>, mut next_key: K) -> Result<DriveEvent>
where
    F: Future<Output = RefreshOutcome> + Unpin,
    K: FnMut() -> io::Result<Option<KeyEvent>>,
{
    let Some(mut pending) = inflight.take() else {
        tokio::time::sleep(INPUT_SLICE).await;
        return drive_key(next_key()?);
    };

    tokio::select! {
        biased;
        _ = tokio::time::sleep(INPUT_SLICE) => {
            match next_key()? {
                Some(key) if key_quits(&key) => Ok(DriveEvent::Quit),
                other => {
                    *inflight = Some(pending);
                    drive_key(other)
                }
            }
        }
        outcome = &mut pending => Ok(DriveEvent::Refresh(Box::new(outcome))),
    }
}

fn drive_key(key: Option<KeyEvent>) -> Result<DriveEvent> {
    Ok(match key {
        Some(key) if key_quits(&key) => DriveEvent::Quit,
        Some(key) => DriveEvent::Key(key),
        None => DriveEvent::Tick,
    })
}

fn poll_key() -> io::Result<Option<KeyEvent>> {
    if !event::poll(Duration::ZERO)? {
        return Ok(None);
    }
    match event::read()? {
        Event::Key(key) if key.kind == KeyEventKind::Press => Ok(Some(key)),
        _ => Ok(None),
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SetupStep {
    Raw,
    Alternate,
    Mouse,
    Cursor,
}

/// Terminal modes this session has turned on. A flag is cleared only after
/// that step's restore succeeds, so `Drop` can retry a failed step.
#[derive(Default)]
struct ModeFlags {
    raw: bool,
    alternate: bool,
    mouse: bool,
    cursor_hidden: bool,
}

impl ModeFlags {
    fn is_idle(&self) -> bool {
        !self.raw && !self.alternate && !self.mouse && !self.cursor_hidden
    }

    fn restore(&mut self, ops: &mut dyn ModeOps) -> io::Result<()> {
        let mut first_error: Option<io::Error> = None;
        let mut note = |result: io::Result<()>| {
            if let Err(error) = result {
                if first_error.is_none() {
                    first_error = Some(error);
                }
            }
        };
        if self.cursor_hidden {
            match ops.show_cursor() {
                Ok(()) => self.cursor_hidden = false,
                Err(error) => note(Err(error)),
            }
        }
        if self.alternate {
            match ops.leave_alternate() {
                Ok(()) => self.alternate = false,
                Err(error) => note(Err(error)),
            }
        }
        if self.mouse {
            match ops.disable_mouse() {
                Ok(()) => self.mouse = false,
                Err(error) => note(Err(error)),
            }
        }
        if self.raw {
            match ops.disable_raw() {
                Ok(()) => self.raw = false,
                Err(error) => note(Err(error)),
            }
        }
        match first_error {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }
}

trait ModeOps {
    fn enable_raw(&mut self) -> io::Result<()>;
    fn disable_raw(&mut self) -> io::Result<()>;
    fn enter_alternate(&mut self) -> io::Result<()>;
    fn leave_alternate(&mut self) -> io::Result<()>;
    fn enable_mouse(&mut self) -> io::Result<()>;
    fn disable_mouse(&mut self) -> io::Result<()>;
    fn hide_cursor(&mut self) -> io::Result<()>;
    fn show_cursor(&mut self) -> io::Result<()>;
}

fn engage(flags: &mut ModeFlags, ops: &mut dyn ModeOps, step: SetupStep) -> io::Result<()> {
    match step {
        SetupStep::Raw => {
            ops.enable_raw()?;
            flags.raw = true;
        }
        SetupStep::Alternate => {
            ops.enter_alternate()?;
            flags.alternate = true;
        }
        SetupStep::Mouse => {
            ops.enable_mouse()?;
            flags.mouse = true;
        }
        SetupStep::Cursor => {
            ops.hide_cursor()?;
            flags.cursor_hidden = true;
        }
    }
    Ok(())
}

fn run_steps(flags: &mut ModeFlags, ops: &mut dyn ModeOps, steps: &[SetupStep]) -> io::Result<()> {
    for step in steps {
        engage(flags, ops, *step)?;
    }
    Ok(())
}

struct LiveOps<'a> {
    terminal: &'a mut Option<Terminal<CrosstermBackend<Stdout>>>,
}

impl LiveOps<'_> {
    fn run(&mut self, command: impl crossterm::Command + Copy) -> io::Result<()> {
        if let Some(terminal) = self.terminal.as_mut() {
            execute!(terminal.backend_mut(), command)
        } else {
            execute!(io::stdout(), command)
        }
    }
}

impl ModeOps for LiveOps<'_> {
    fn enable_raw(&mut self) -> io::Result<()> {
        enable_raw_mode()
    }

    fn disable_raw(&mut self) -> io::Result<()> {
        disable_raw_mode()
    }

    fn enter_alternate(&mut self) -> io::Result<()> {
        self.run(EnterAlternateScreen)
    }

    fn leave_alternate(&mut self) -> io::Result<()> {
        self.run(LeaveAlternateScreen)
    }

    fn enable_mouse(&mut self) -> io::Result<()> {
        self.run(EnableMouseCapture)
    }

    fn disable_mouse(&mut self) -> io::Result<()> {
        self.run(DisableMouseCapture)
    }

    fn hide_cursor(&mut self) -> io::Result<()> {
        if let Some(terminal) = self.terminal.as_mut() {
            return terminal.hide_cursor();
        }
        self.run(Hide)
    }

    fn show_cursor(&mut self) -> io::Result<()> {
        if let Some(terminal) = self.terminal.as_mut() {
            return terminal.show_cursor();
        }
        self.run(Show)
    }
}

struct TerminalSession {
    terminal: Option<Terminal<CrosstermBackend<Stdout>>>,
    flags: ModeFlags,
}

impl TerminalSession {
    /// The session exists before raw mode is enabled, so a later setup error
    /// still has a guard to restore from.
    fn enter() -> Result<Self> {
        let mut session = Self {
            terminal: None,
            flags: ModeFlags::default(),
        };
        if let Err(error) = session.setup() {
            let _ = session.restore();
            return Err(error.into());
        }
        Ok(session)
    }

    fn setup(&mut self) -> io::Result<()> {
        let TerminalSession { terminal, flags } = self;
        let opened = (|| {
            {
                let mut ops = LiveOps { terminal };
                run_steps(
                    flags,
                    &mut ops,
                    &[SetupStep::Raw, SetupStep::Alternate, SetupStep::Mouse],
                )?;
            }
            *terminal = Some(Terminal::new(CrosstermBackend::new(io::stdout()))?);
            let mut ops = LiveOps { terminal };
            run_steps(flags, &mut ops, &[SetupStep::Cursor])?;
            Ok(())
        })();
        if let Err(error) = opened {
            let _ = flags.restore(&mut LiveOps { terminal });
            return Err(error);
        }
        Ok(())
    }

    fn restore(&mut self) -> Result<()> {
        let TerminalSession { terminal, flags } = self;
        flags
            .restore(&mut LiveOps { terminal })
            .map_err(anyhow::Error::from)
    }
}

impl Drop for TerminalSession {
    fn drop(&mut self) {
        let _ = self.restore();
    }
}

pub async fn run(client: Ad4mClient, executor_url: String) -> Result<()> {
    let mut session = TerminalSession::enter()?;
    let mut model = Model::new(executor_url);
    let outcome = drive(&client, &mut session, &mut model).await;
    session.restore()?;
    outcome
}

async fn drive(
    client: &Ad4mClient,
    session: &mut TerminalSession,
    model: &mut Model,
) -> Result<()> {
    let mut inflight: Option<BoxedRefresh<'_>> = None;
    let conn = tui_space::connection(client);
    loop {
        if model.open_request.is_some() {
            open_coasys(model, &conn);
        }
        if model.space.is_some() {
            // The admin refresh is suspended: an in-flight one is dropped.
            inflight = None;
            if drive_space(session, model).await? {
                return Ok(());
            }
            continue;
        }
        if should_start_refresh(model, inflight.is_some()) {
            let request = model.refresh_request();
            inflight = Some(Box::pin(fetch(client, request)));
        }
        session
            .terminal
            .as_mut()
            .context("tui terminal is not open")?
            .draw(|frame| draw(frame, model))?;
        match refresh_or_quit(&mut inflight, poll_key).await? {
            DriveEvent::Quit => return Ok(()),
            DriveEvent::Refresh(outcome) => apply_outcome(model, *outcome),
            DriveEvent::Key(key) => handle_key(model, &key),
            DriveEvent::Tick => {}
        }
    }
}

/// One frame of the open Coasys app, then one input slice. `true`: leave
/// the TUI. The app never waits on the executor here: its reads land
/// between frames, so F10 and Ctrl-C always get through.
async fn drive_space(session: &mut TerminalSession, model: &mut Model) -> Result<bool> {
    let terminal = session
        .terminal
        .as_mut()
        .context("tui terminal is not open")?;
    if let Some(space) = model.space.as_mut() {
        space.tick();
        terminal.draw(|frame| {
            let area = frame.area();
            space.draw(frame.buffer_mut(), area);
        })?;
    }
    tokio::time::sleep(INPUT_SLICE).await;
    let Some(key) = poll_key()? else {
        return Ok(false);
    };
    let exit = space_key(model, key);
    if model.space.is_none() {
        // Back on the admin screens: clear and redraw all of it. Not
        // `clear()`, which asks the terminal for the cursor position and
        // fails when the terminal does not answer.
        let size = terminal.size()?;
        terminal.resize(ratatui::layout::Rect::new(0, 0, size.width, size.height))?;
    }
    Ok(exit)
}

fn select_screen(model: &mut Model, screen: Screen) {
    if model.screen != screen {
        model.screen = screen;
        invalidate(model);
    }
}

fn handle_key(model: &mut Model, key: &KeyEvent) {
    match key.code {
        KeyCode::Char('1') => select_screen(model, Screen::Status),
        KeyCode::Char('2') => select_screen(model, Screen::Perspectives),
        KeyCode::Char('3') => select_screen(model, Screen::Links),
        KeyCode::Tab => {
            let next = model.screen.next();
            select_screen(model, next);
        }
        KeyCode::Char('r') => invalidate(model),
        KeyCode::Char('n') if model.screen == Screen::Perspectives => {
            model.neighbourhoods_only = !model.neighbourhoods_only;
            model.selected = 0;
            model.clamp_selection();
        }
        KeyCode::Up | KeyCode::Char('k') => move_selection(model, -1),
        KeyCode::Down | KeyCode::Char('j') => move_selection(model, 1),
        KeyCode::Enter if model.screen == Screen::Perspectives => open_selected(model),
        KeyCode::Char('o') if model.screen == Screen::Perspectives => request_coasys(model),
        KeyCode::Esc if model.screen == Screen::Links => model.screen = Screen::Perspectives,
        _ => {}
    }
}

fn move_selection(model: &mut Model, delta: isize) {
    match model.screen {
        Screen::Perspectives => {
            let len = model.visible().len();
            model.selected = step(model.selected, len, delta);
        }
        Screen::Links => {
            model.link_selected = step(model.link_selected, model.links.len(), delta);
        }
        Screen::Status => {}
    }
}

fn step(index: usize, len: usize, delta: isize) -> usize {
    if len == 0 {
        return 0;
    }
    let last = len - 1;
    if delta < 0 {
        index.saturating_sub(1)
    } else if index >= last {
        last
    } else {
        index + 1
    }
}

fn open_selected(model: &mut Model) {
    let visible = model.visible();
    let Some(index) = visible.get(model.selected).copied() else {
        return;
    };
    model.open_uuid = Some(model.perspectives[index].uuid.clone());
    model.screen = Screen::Links;
    model.links.clear();
    model.link_selected = 0;
    invalidate(model);
}

/// Open the perspective `o` asked for, over the TUI's own socket.
fn open_coasys(model: &mut Model, conn: &Connection) {
    open_coasys_with(model, |row| tui_space::open(conn, row));
}

fn open_coasys_with(
    model: &mut Model,
    open: impl FnOnce(SpaceRow) -> Result<CoasysScreen, String>,
) {
    let Some(row) = model.open_request.take() else {
        return;
    };
    match open(row) {
        Ok(space) => {
            model.space = Some(space);
            model.error = None;
            invalidate(model);
        }
        Err(error) => model.error = Some(format!("Cannot open Coasys: {error}")),
    }
}

/// A key while the Coasys app is open. `true`: leave the TUI.
fn space_key(model: &mut Model, key: KeyEvent) -> bool {
    let Some(space) = model.space.as_mut() else {
        return false;
    };
    match space.handle_key(key) {
        SpaceKey::Stay => false,
        SpaceKey::Exit => true,
        SpaceKey::Back => {
            // Dropping the screen stops its reads; the socket stays open.
            model.space = None;
            model.screen = Screen::Perspectives;
            invalidate(model);
            false
        }
    }
}

/// `o`: ask to open the selected visible row in the Coasys app. No row, no request.
fn request_coasys(model: &mut Model) {
    let visible = model.visible();
    if let Some(index) = visible.get(model.selected).copied() {
        model.open_request = Some(tui_space::space_row(&model.perspectives[index]));
    }
}

fn draw(frame: &mut Frame, model: &Model) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(1)
        .constraints([
            Constraint::Length(3),
            Constraint::Min(0),
            Constraint::Length(3),
            Constraint::Length(3),
        ])
        .split(frame.area());

    let header = header_text(model.screen, &model.executor_url);
    let title = Paragraph::new(Line::from(vec![Span::styled(
        header,
        Style::default()
            .fg(Color::Cyan)
            .add_modifier(Modifier::BOLD),
    )]))
    .block(Block::default().borders(Borders::ALL).title("ad4m tui"));
    frame.render_widget(title, chunks[0]);

    match model.screen {
        Screen::Status => draw_status(frame, model, chunks[1]),
        Screen::Perspectives => draw_perspectives(frame, model, chunks[1]),
        Screen::Links => draw_links(frame, model, chunks[1]),
    }

    let error = model.error.clone().unwrap_or_default();
    let error_line = Paragraph::new(error).block(
        Block::default()
            .borders(Borders::ALL)
            .title("Error")
            .style(Style::default().fg(Color::Red)),
    );
    frame.render_widget(error_line, chunks[2]);

    let help = Paragraph::new(
        "1 status  2 perspectives  3 links  tab next  j/k move  enter links  o Coasys (read-only)  n neighbourhoods  r refresh  q quit",
    )
    .block(Block::default().borders(Borders::ALL).title("Keys"));
    frame.render_widget(help, chunks[3]);
}

fn draw_status(frame: &mut Frame, model: &Model, area: ratatui::layout::Rect) {
    let lines = status_lines(
        &model.executor_url,
        model.agent.as_ref(),
        model.runtime.as_ref(),
    );
    let text: Vec<Line> = lines.into_iter().map(Line::from).collect();
    let paragraph = Paragraph::new(text).block(Block::default().borders(Borders::ALL));
    frame.render_widget(paragraph, area);
}

fn draw_perspectives(frame: &mut Frame, model: &Model, area: ratatui::layout::Rect) {
    let visible = model.visible();
    let (total, neighbourhoods) = (
        model.perspectives.len(),
        neighbourhood_count(&model.perspectives),
    );
    let filter = if model.neighbourhoods_only {
        "neighbourhoods only"
    } else {
        "all"
    };
    let items: Vec<ListItem> = visible
        .iter()
        .map(|index| ListItem::new(perspective_label(&model.perspectives[*index])))
        .collect();
    let list = List::new(items)
        .block(Block::default().borders(Borders::ALL).title(format!(
            "{total} perspectives, {neighbourhoods} neighbourhoods, {filter}"
        )))
        .highlight_style(
            Style::default()
                .bg(Color::Cyan)
                .fg(Color::Black)
                .add_modifier(Modifier::BOLD),
        )
        .highlight_symbol("> ");
    let mut state = ListState::default();
    if !visible.is_empty() {
        state.select(Some(model.selected));
    }
    frame.render_stateful_widget(list, area, &mut state);
}

fn draw_links(frame: &mut Frame, model: &Model, area: ratatui::layout::Rect) {
    let title = links_title(model.open_uuid.as_deref(), model.links.len());
    let items: Vec<ListItem> = if model.open_uuid.is_none() {
        vec![ListItem::new(
            "Open a perspective from the perspectives screen.",
        )]
    } else if model.links.is_empty() {
        vec![ListItem::new("No links.")]
    } else {
        model
            .links
            .iter()
            .map(|link| ListItem::new(link_label(link)))
            .collect()
    };
    let list = List::new(items)
        .block(Block::default().borders(Borders::ALL).title(title))
        .highlight_style(
            Style::default()
                .bg(Color::Cyan)
                .fg(Color::Black)
                .add_modifier(Modifier::BOLD),
        )
        .highlight_symbol("> ");
    let mut state = ListState::default();
    if model.open_uuid.is_some() && !model.links.is_empty() {
        state.select(Some(model.link_selected));
    }
    frame.render_stateful_widget(list, area, &mut state);
}

#[cfg(test)]
mod tests {
    use super::*;
    use ad4m_client::types::{
        ExpressionProof, Link, Neighbourhood, NeighbourhoodExpression, Perspective,
    };

    fn proof() -> ExpressionProof {
        ExpressionProof {
            invalid: None,
            key: None,
            signature: None,
            valid: None,
        }
    }

    fn perspective(name: Option<&str>, uuid: &str, neighbourhood: bool) -> PerspectiveHandle {
        PerspectiveHandle {
            name: name.map(str::to_string),
            neighbourhood: neighbourhood.then(|| NeighbourhoodExpression {
                author: "did:test".to_string(),
                data: Neighbourhood {
                    link_language: "lang://social".to_string(),
                    meta: Perspective { links: vec![] },
                },
                proof: proof(),
                timestamp: "2026-11-18T00:00:00Z".to_string(),
            }),
            shared_url: None,
            uuid: uuid.to_string(),
            state: None,
        }
    }

    fn agent_status() -> AgentStatus {
        AgentStatus {
            did: Some("did:key:abc".to_string()),
            did_document: None,
            error: None,
            is_initialized: true,
            is_unlocked: false,
        }
    }

    fn runtime_info() -> RuntimeInfo {
        RuntimeInfo {
            ad4m_executor_version: "0.13.1".to_string(),
            ad4m_executor_commit: Some("abcdef".to_string()),
            is_initialized: true,
            is_unlocked: false,
        }
    }

    #[test]
    fn status_lines_include_the_url_the_lock_and_the_version() {
        let lines = status_lines(
            "http://127.0.0.1:12000",
            Some(&agent_status()),
            Some(&runtime_info()),
        );
        let text = lines.join("\n");
        assert!(text.contains("http://127.0.0.1:12000"));
        assert!(text.contains("Unlocked: false"));
        assert!(text.contains("Version: 0.13.1"));
        assert!(text.contains("Commit: abcdef"));
    }

    #[test]
    fn header_and_status_strip_userinfo_query_and_fragment() {
        let raw = "http://alice:password@NODE.example/api?token=secret#frag";
        let model = Model::new(raw.to_string());
        assert_eq!(model.executor_url, "http://node.example/api");
        let text = status_lines(raw, None, None).join("\n");
        let header = header_text(Screen::Status, raw);
        for shown in [&model.executor_url, &text, &header] {
            assert!(shown.contains("http://node.example/api"), "{shown}");
            assert!(!shown.contains("password"), "{shown}");
            assert!(!shown.contains("alice"), "{shown}");
            assert!(!shown.contains("secret"), "{shown}");
            assert!(!shown.contains("token="), "{shown}");
        }
        assert!(header.starts_with("Executor status  "));
    }

    #[test]
    fn links_title_shows_the_cap() {
        assert_eq!(links_title(None, 0), "no perspective open");
        assert_eq!(
            links_title(Some("p1"), 0),
            "perspective p1 — at most 200 links"
        );
        assert_eq!(
            links_title(Some("p1"), 199),
            "perspective p1 — at most 200 links"
        );
        assert_eq!(
            links_title(Some("p1"), 200),
            "perspective p1 — list capped at 200"
        );
    }

    #[test]
    fn perspective_label_names_a_neighbourhood_by_its_link_language() {
        let local = perspective(None, "local-1", false);
        assert_eq!(perspective_label(&local), "<unnamed>  local-1");
        let shared = perspective(Some("Town"), "nh-1", true);
        assert_eq!(
            perspective_label(&shared),
            "Town  nh-1  neighbourhood lang://social"
        );
        assert_eq!(neighbourhood_count(&[local, shared]), 1);
    }

    #[test]
    fn link_label_keeps_source_predicate_and_target() {
        let link = LinkExpression {
            author: "did:test".to_string(),
            data: Link {
                predicate: Some("ad4m://knows".to_string()),
                source: "did:a".to_string(),
                target: "did:b".to_string(),
            },
            proof: proof(),
            timestamp: "2026-11-18T00:00:00Z".to_string(),
            status: None,
        };
        assert_eq!(link_label(&link), "did:a  ad4m://knows  did:b");
    }

    #[test]
    fn visible_rows_can_hide_local_perspectives() {
        let mut model = Model::new("http://127.0.0.1:1".to_string());
        model.perspectives = vec![
            perspective(Some("Local"), "local-1", false),
            perspective(Some("Town"), "nh-1", true),
        ];
        assert_eq!(model.visible(), vec![0, 1]);
        model.neighbourhoods_only = true;
        assert_eq!(model.visible(), vec![1]);
    }

    #[test]
    fn refresh_key_drops_a_stale_outcome_and_asks_again() {
        let mut model = Model::new("http://127.0.0.1:1".to_string());
        model.loaded_at = Some(Instant::now());
        let gen = model.refresh_gen;
        handle_key(
            &mut model,
            &KeyEvent::new(KeyCode::Char('r'), KeyModifiers::NONE),
        );
        assert!(model.loaded_at.is_none());
        assert_ne!(model.refresh_gen, gen);
        apply_outcome(
            &mut model,
            RefreshOutcome {
                gen,
                wrote_agent: true,
                agent: Some(agent_status()),
                ..RefreshOutcome::blank()
            },
        );
        assert!(model.agent.is_none());
        assert!(model.loaded_at.is_none());
        assert!(should_start_refresh(&model, false));
        assert!(!should_start_refresh(&model, true));
    }

    #[test]
    fn a_matching_refresh_is_kept_and_a_failed_agent_leaves_the_old_one() {
        let mut model = Model::new("http://127.0.0.1:1".to_string());
        let gen = model.refresh_gen;
        apply_outcome(
            &mut model,
            RefreshOutcome {
                gen,
                wrote_agent: true,
                agent: Some(agent_status()),
                wrote_runtime: true,
                runtime: Some(runtime_info()),
                ..RefreshOutcome::blank()
            },
        );
        assert!(model.agent.is_some());
        assert!(!should_start_refresh(&model, false));

        let gen = model.refresh_gen;
        apply_outcome(
            &mut model,
            RefreshOutcome {
                gen,
                error: Some("down".to_string()),
                ..RefreshOutcome::blank()
            },
        );
        assert_eq!(
            model.agent.as_ref().unwrap().did.as_deref(),
            Some("did:key:abc")
        );
        assert!(model.runtime.is_some());
        assert_eq!(model.error.as_deref(), Some("down"));
        assert!(!model.stale());
    }

    fn pending_refresh() -> (
        tokio::sync::oneshot::Sender<()>,
        Option<BoxedRefresh<'static>>,
    ) {
        let (tx, rx) = tokio::sync::oneshot::channel();
        let fut = Box::pin(async move {
            rx.await.ok();
            RefreshOutcome::blank()
        });
        (tx, Some(fut))
    }

    #[tokio::test]
    async fn quit_does_not_wait_for_a_pending_refresh() {
        for key in [
            KeyEvent::new(KeyCode::Char('q'), KeyModifiers::NONE),
            KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL),
        ] {
            let (tx, mut inflight) = pending_refresh();
            let event = tokio::time::timeout(
                Duration::from_millis(500),
                refresh_or_quit(&mut inflight, move || Ok(Some(key))),
            )
            .await
            .expect("quit waited on the refresh")
            .unwrap();
            assert!(matches!(event, DriveEvent::Quit));
            assert!(inflight.is_none(), "quit must drop the in-flight refresh");
            assert!(tx.is_closed());
        }
    }

    #[tokio::test]
    async fn a_move_key_leaves_the_refresh_in_flight() {
        for key in [
            KeyEvent::new(KeyCode::Char('j'), KeyModifiers::NONE),
            KeyEvent::new(KeyCode::Down, KeyModifiers::NONE),
        ] {
            let (tx, mut inflight) = pending_refresh();
            let event = tokio::time::timeout(
                Duration::from_millis(500),
                refresh_or_quit(&mut inflight, move || Ok(Some(key))),
            )
            .await
            .expect("input waited on the refresh")
            .unwrap();
            assert!(matches!(event, DriveEvent::Key(_)));
            assert!(inflight.is_some());
            assert!(!tx.is_closed());
        }
    }

    fn press(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn ctrl_c() -> KeyEvent {
        KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL)
    }

    fn perspectives_model() -> Model {
        let mut model = Model::new("http://127.0.0.1:1".to_string());
        model.screen = Screen::Perspectives;
        model.perspectives = vec![
            perspective(Some("Local"), "local-1", false),
            perspective(Some("Town"), "nh-1", true),
            perspective(Some("Garden"), "nh-2", true),
        ];
        model
    }

    async fn fake_client() -> (tui_space::fake_ws::FakeWs, Ad4mClient) {
        client_of(tui_space::fake_ws::FakeWs::start().await).await
    }

    async fn client_of(
        fake: tui_space::fake_ws::FakeWs,
    ) -> (tui_space::fake_ws::FakeWs, Ad4mClient) {
        let client = Ad4mClient::connect(fake.url.clone(), String::new())
            .await
            .unwrap();
        (fake, client)
    }

    /// Paint one frame of the open app; `None` when no app is open.
    fn draw_app(model: &mut Model) -> Option<String> {
        let area = ratatui::layout::Rect::new(0, 0, 120, 40);
        let mut buf = ratatui::buffer::Buffer::empty(area);
        let space = model.space.as_mut()?;
        space.tick();
        space.draw(&mut buf, area);
        Some(we_tui::buffer_text(&buf, area))
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn open_coasys_uses_filtered_selection() {
        let (_fake, client) = fake_client().await;
        let conn = tui_space::connection(&client);

        // Under the neighbourhood filter, the second visible row is nh-2.
        let mut model = perspectives_model();
        handle_key(&mut model, &press(KeyCode::Char('n')));
        handle_key(&mut model, &press(KeyCode::Char('j')));
        handle_key(&mut model, &press(KeyCode::Char('o')));
        open_coasys(&mut model, &conn);
        let space = model.space.as_ref().expect("o opened the app");
        assert_eq!(space.space.uuid, "nh-2");
        assert_eq!(space.space.name, "Garden");
        assert_eq!(space.route(), "/space/nh-2");
        assert!(space.header().contains("nh-2"), "{}", space.header());
        assert!(space.header().contains("read-only"), "{}", space.header());
        let text = draw_app(&mut model).unwrap();
        assert!(text.contains("Coasys (read-only)  Garden  nh-2"), "{text}");
        assert!(text.contains("/space/nh-2"), "{text}");

        // No visible row: `o` does nothing.
        let mut empty = Model::new("http://127.0.0.1:1".to_string());
        empty.screen = Screen::Perspectives;
        handle_key(&mut empty, &press(KeyCode::Char('o')));
        open_coasys(&mut empty, &conn);
        assert!(empty.space.is_none());
        assert!(empty.error.is_none());
        assert!(empty.screen == Screen::Perspectives);
        let mut hidden = Model::new("http://127.0.0.1:1".to_string());
        hidden.screen = Screen::Perspectives;
        hidden.perspectives = vec![perspective(Some("Local"), "local-1", false)];
        handle_key(&mut hidden, &press(KeyCode::Char('n')));
        handle_key(&mut hidden, &press(KeyCode::Char('o')));
        open_coasys(&mut hidden, &conn);
        assert!(hidden.space.is_none(), "o opened a row the filter hides");

        // `o` is a perspectives-screen key only.
        let mut status = perspectives_model();
        status.screen = Screen::Status;
        handle_key(&mut status, &press(KeyCode::Char('o')));
        open_coasys(&mut status, &conn);
        assert!(status.space.is_none());

        // Enter still opens the links of the selected row.
        let mut links = perspectives_model();
        handle_key(&mut links, &press(KeyCode::Char('j')));
        handle_key(&mut links, &press(KeyCode::Enter));
        assert!(links.screen == Screen::Links);
        assert_eq!(links.open_uuid.as_deref(), Some("nh-1"));
        assert!(links.space.is_none());
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn coasys_back_preserves_admin_state() {
        // Every read is refused, so none waits on the peer: a clone of the
        // socket that outlives the app is the app's, not a pending call's.
        let (_fake, client) = client_of(tui_space::fake_ws::FakeWs::refusing().await).await;
        let conn = tui_space::connection(&client);
        let base = std::sync::Arc::strong_count(client.ws()) - 1; // minus `conn`

        let mut model = perspectives_model();
        handle_key(&mut model, &press(KeyCode::Char('j')));
        model.loaded_at = Some(Instant::now());
        let gen = model.refresh_gen;
        handle_key(&mut model, &press(KeyCode::Char('o')));
        open_coasys(&mut model, &conn);
        assert!(model.space.is_some());
        draw_app(&mut model).unwrap();
        assert!(
            std::sync::Arc::strong_count(client.ws()) > base + 1,
            "the app holds no clone of the shared socket"
        );
        // The admin refresh is suspended while the app is open, and an
        // outcome that was in flight is dropped.
        model.loaded_at = None;
        assert!(!should_start_refresh(&model, false));
        assert_ne!(model.refresh_gen, gen);

        // Admin keys go to the app: `1` and Tab do not change screens.
        for key in [press(KeyCode::Char('1')), press(KeyCode::Tab)] {
            assert!(!space_key(&mut model, key));
            assert!(model.space.is_some());
        }
        assert!(model.screen == Screen::Perspectives);

        // F10: back to the list, selection kept, refresh resumed.
        assert!(!space_key(&mut model, press(KeyCode::F(10))));
        assert!(model.space.is_none());
        assert!(model.screen == Screen::Perspectives);
        assert_eq!(model.selected, 1);
        assert!(should_start_refresh(&model, false));

        // The app's reads let go of the socket; the socket stays open.
        let deadline = Instant::now() + Duration::from_secs(5);
        while std::sync::Arc::strong_count(client.ws()) > base + 1 {
            assert!(
                Instant::now() < deadline,
                "the closed app still holds {} clones of the socket",
                std::sync::Arc::strong_count(client.ws()) - base - 1
            );
            tokio::time::sleep(Duration::from_millis(20)).await;
        }
        assert!(client.ws().is_connected());

        // Open again: the same row. The app's own quit (`q`) is back too.
        handle_key(&mut model, &press(KeyCode::Char('o')));
        open_coasys(&mut model, &conn);
        assert_eq!(model.space.as_ref().unwrap().space.uuid, "nh-1");
        draw_app(&mut model).unwrap();
        assert!(!space_key(&mut model, press(KeyCode::Char('q'))));
        assert!(model.space.is_none());
        assert_eq!(model.selected, 1);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn coasys_pending_load_still_exits() {
        // The peer never answers: every read the app starts stays pending.
        let (fake, client) = fake_client().await;
        let conn = tui_space::connection(&client);
        let mut model = perspectives_model();
        handle_key(&mut model, &press(KeyCode::Char('o')));
        open_coasys(&mut model, &conn);
        assert!(model.space.is_some());
        let started = Instant::now();
        for _ in 0..5 {
            draw_app(&mut model).unwrap();
        }
        assert!(
            started.elapsed() < Duration::from_secs(5),
            "drawing waited on the pending reads"
        );
        assert!(!fake.methods().is_empty(), "the app started no read");
        assert!(!space_key(&mut model, press(KeyCode::F(10))));
        assert!(model.space.is_none(), "F10 did not leave a pending app");

        handle_key(&mut model, &press(KeyCode::Char('o')));
        open_coasys(&mut model, &conn);
        draw_app(&mut model).unwrap();
        assert!(space_key(&mut model, ctrl_c()), "Ctrl-C did not leave the TUI");

        // A failed open leaves the admin screens usable.
        let mut failed = perspectives_model();
        handle_key(&mut failed, &press(KeyCode::Char('o')));
        open_coasys_with(&mut failed, |row| {
            tui_space::open_bundle("{ not a bundle", &conn, row)
        });
        assert!(failed.space.is_none());
        assert!(failed.screen == Screen::Perspectives);
        let error = failed.error.clone().expect("the failure is shown");
        assert!(error.contains("Coasys"), "{error}");
        handle_key(&mut failed, &press(KeyCode::Char('j')));
        assert_eq!(failed.selected, 1);
        handle_key(&mut failed, &press(KeyCode::Char('1')));
        assert!(failed.screen == Screen::Status);
        assert_eq!(fake.accepted(), 1, "the app dialed a second socket");
    }

    struct FakeOps {
        fail: Option<&'static str>,
        calls: Vec<&'static str>,
    }

    impl FakeOps {
        fn call(&mut self, name: &'static str) -> io::Result<()> {
            self.calls.push(name);
            if self.fail == Some(name) {
                Err(io::Error::other(name))
            } else {
                Ok(())
            }
        }
    }

    impl ModeOps for FakeOps {
        fn enable_raw(&mut self) -> io::Result<()> {
            self.call("enable_raw")
        }
        fn disable_raw(&mut self) -> io::Result<()> {
            self.call("disable_raw")
        }
        fn enter_alternate(&mut self) -> io::Result<()> {
            self.call("enter_alternate")
        }
        fn leave_alternate(&mut self) -> io::Result<()> {
            self.call("leave_alternate")
        }
        fn enable_mouse(&mut self) -> io::Result<()> {
            self.call("enable_mouse")
        }
        fn disable_mouse(&mut self) -> io::Result<()> {
            self.call("disable_mouse")
        }
        fn hide_cursor(&mut self) -> io::Result<()> {
            self.call("hide_cursor")
        }
        fn show_cursor(&mut self) -> io::Result<()> {
            self.call("show_cursor")
        }
    }

    #[test]
    fn raw_mode_is_recorded_on_the_guard_and_rolled_back() {
        let mut flags = ModeFlags::default();
        assert!(
            flags.is_idle(),
            "the guard starts before any mode is enabled"
        );
        let mut ops = FakeOps {
            fail: Some("enter_alternate"),
            calls: Vec::new(),
        };
        let error = run_steps(
            &mut flags,
            &mut ops,
            &[
                SetupStep::Raw,
                SetupStep::Alternate,
                SetupStep::Mouse,
                SetupStep::Cursor,
            ],
        )
        .unwrap_err();
        assert_eq!(error.to_string(), "enter_alternate");
        assert!(flags.raw, "raw mode is tracked before the later step fails");
        assert!(!flags.alternate);
        flags.restore(&mut ops).unwrap();
        assert!(flags.is_idle());
        assert_eq!(
            ops.calls,
            vec!["enable_raw", "enter_alternate", "disable_raw"]
        );
    }

    #[test]
    fn a_failed_raw_restore_still_restores_the_other_modes() {
        let mut flags = ModeFlags {
            raw: true,
            alternate: true,
            mouse: true,
            cursor_hidden: true,
        };
        let mut ops = FakeOps {
            fail: Some("disable_raw"),
            calls: Vec::new(),
        };
        assert!(flags.restore(&mut ops).is_err());
        assert!(flags.raw, "a failed step stays set so Drop can retry");
        assert!(!flags.alternate);
        assert!(!flags.mouse);
        assert!(!flags.cursor_hidden);
        assert_eq!(
            ops.calls,
            vec![
                "show_cursor",
                "leave_alternate",
                "disable_mouse",
                "disable_raw",
            ]
        );
        ops.fail = None;
        flags.restore(&mut ops).unwrap();
        assert!(flags.is_idle());
    }
}
