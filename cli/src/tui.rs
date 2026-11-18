//! Admin and debug screens for `ad4m tui`.
//!
//! Three screens, in this order: executor status, the list of perspectives
//! and neighbourhoods, and the links of one perspective. Keys `1`, `2`, and
//! `3` open those screens. Tab moves to the next screen. The open screen
//! loads again every two seconds.

use std::io::{self, Stdout};
use std::time::{Duration, Instant};

use ad4m_client::types::{AgentStatus, LinkExpression, PerspectiveHandle, RuntimeInfo};
use ad4m_client::Ad4mClient;
use anyhow::Result;
use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind, KeyModifiers,
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
    error: Option<String>,
    loaded_at: Option<Instant>,
}

impl Model {
    fn new(executor_url: String) -> Self {
        Self {
            executor_url,
            screen: Screen::Status,
            agent: None,
            runtime: None,
            perspectives: Vec::new(),
            selected: 0,
            neighbourhoods_only: false,
            links: Vec::new(),
            link_selected: 0,
            open_uuid: None,
            error: None,
            loaded_at: None,
        }
    }

    fn stale(&self) -> bool {
        match self.loaded_at {
            None => true,
            Some(at) => at.elapsed() >= REFRESH,
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

/// Text rows for the status screen. The executor URL is the one the CLI used.
pub(crate) fn status_lines(
    executor_url: &str,
    agent: Option<&AgentStatus>,
    runtime: Option<&RuntimeInfo>,
) -> Vec<String> {
    let mut lines = vec![format!("Executor: {executor_url}")];
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

struct TerminalSession {
    terminal: Terminal<CrosstermBackend<Stdout>>,
    active: bool,
}

impl TerminalSession {
    fn enter() -> Result<Self> {
        enable_raw_mode()?;
        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
        let terminal = Terminal::new(CrosstermBackend::new(stdout))?;
        Ok(Self {
            terminal,
            active: true,
        })
    }

    fn restore(&mut self) -> Result<()> {
        if !self.active {
            return Ok(());
        }
        self.active = false;
        disable_raw_mode()?;
        execute!(
            self.terminal.backend_mut(),
            LeaveAlternateScreen,
            DisableMouseCapture
        )?;
        self.terminal.show_cursor()?;
        Ok(())
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
    loop {
        if model.stale() {
            refresh(client, model).await;
        }
        session.terminal.draw(|frame| draw(frame, model))?;
        if !event::poll(Duration::from_millis(200))? {
            continue;
        }
        let Event::Key(key) = event::read()? else {
            continue;
        };
        if key.kind != KeyEventKind::Press {
            continue;
        }
        if key.code == KeyCode::Char('c') && key.modifiers.contains(KeyModifiers::CONTROL) {
            return Ok(());
        }
        match key.code {
            KeyCode::Char('q') => return Ok(()),
            KeyCode::Char('1') => select_screen(model, Screen::Status),
            KeyCode::Char('2') => select_screen(model, Screen::Perspectives),
            KeyCode::Char('3') => select_screen(model, Screen::Links),
            KeyCode::Tab => {
                let next = model.screen.next();
                select_screen(model, next);
            }
            KeyCode::Char('r') => model.loaded_at = None,
            KeyCode::Char('n') if model.screen == Screen::Perspectives => {
                model.neighbourhoods_only = !model.neighbourhoods_only;
                model.selected = 0;
                model.clamp_selection();
            }
            KeyCode::Up | KeyCode::Char('k') => move_selection(model, -1),
            KeyCode::Down | KeyCode::Char('j') => move_selection(model, 1),
            KeyCode::Enter if model.screen == Screen::Perspectives => open_selected(model),
            KeyCode::Esc if model.screen == Screen::Links => model.screen = Screen::Perspectives,
            _ => {}
        }
    }
}

fn select_screen(model: &mut Model, screen: Screen) {
    if model.screen != screen {
        model.screen = screen;
        model.loaded_at = None;
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
    model.loaded_at = None;
}

async fn refresh(client: &Ad4mClient, model: &mut Model) {
    let result = match model.screen {
        Screen::Status => load_status(client, model).await,
        Screen::Perspectives => load_perspectives(client, model).await,
        Screen::Links => load_links(client, model).await,
    };
    model.loaded_at = Some(Instant::now());
    model.error = result.err().map(|error| error.to_string());
    model.clamp_selection();
}

async fn load_status(client: &Ad4mClient, model: &mut Model) -> Result<()> {
    let (agent, runtime) = tokio::join!(client.agent.status(), client.runtime.info());
    model.agent = Some(agent?);
    model.runtime = Some(runtime?);
    Ok(())
}

async fn load_perspectives(client: &Ad4mClient, model: &mut Model) -> Result<()> {
    model.perspectives = client.perspectives.all().await?;
    Ok(())
}

async fn load_links(client: &Ad4mClient, model: &mut Model) -> Result<()> {
    let Some(uuid) = model.open_uuid.clone() else {
        model.links.clear();
        return Ok(());
    };
    model.links = client
        .perspectives
        .query_links(uuid, None, None, None, None, None, Some(LINK_LIMIT))
        .await?;
    Ok(())
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
        .split(frame.size());

    let title = Paragraph::new(Line::from(vec![
        Span::styled(
            model.screen.title(),
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        ),
        Span::raw("  "),
        Span::styled(&model.executor_url, Style::default().fg(Color::Gray)),
    ]))
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
        "1 status  2 perspectives  3 links  tab next  j/k move  enter open  n neighbourhoods  r refresh  q quit",
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
    let title = match &model.open_uuid {
        Some(uuid) => format!("perspective {uuid}"),
        None => "no perspective open".to_string(),
    };
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

    #[test]
    fn status_lines_include_the_url_the_lock_and_the_version() {
        let agent = AgentStatus {
            did: Some("did:key:abc".to_string()),
            did_document: None,
            error: None,
            is_initialized: true,
            is_unlocked: false,
        };
        let runtime = RuntimeInfo {
            ad4m_executor_version: "0.13.1".to_string(),
            ad4m_executor_commit: Some("abcdef".to_string()),
            is_initialized: true,
            is_unlocked: false,
        };
        let lines = status_lines("http://127.0.0.1:12000", Some(&agent), Some(&runtime));
        let text = lines.join("\n");
        assert!(text.contains("http://127.0.0.1:12000"));
        assert!(text.contains("Unlocked: false"));
        assert!(text.contains("Version: 0.13.1"));
        assert!(text.contains("Commit: abcdef"));
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
}
