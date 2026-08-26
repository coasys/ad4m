use crate::{formatting::*, repl::repl_loop, util::maybe_parse_datetime};
use ad4m_client::types::LinkInput;
use ad4m_client::Ad4mClient;
use anyhow::{anyhow, Context, Result};
use clap::{Args, Subcommand};
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, Paragraph},
    Terminal,
};
use std::io;

#[derive(Args, Debug)]
pub struct QueryLinksArgs {
    /// Perspective ID
    id: String,

    /// Filter by source
    source: Option<String>,

    /// Filter by target
    target: Option<String>,

    /// Filter by predicate
    predicate: Option<String>,

    /// Get only links after this date (fromat: %Y-%m-%dT%H:%M:%S%.fZ)
    #[arg(short, long)]
    from_date: Option<String>,

    /// Get only links before this date (fromat: %Y-%m-%dT%H:%M:%S%.fZ)
    #[arg(short, long)]
    until_date: Option<String>,

    /// Get only the first n links
    #[arg(short, long)]
    limit: Option<f64>,
}

#[derive(Debug, Subcommand)]
pub enum PerspectiveFunctions {
    /// Add a perspective with given name
    Add { name: String },

    /// Remove perspective with given uuid
    Remove { id: String },

    /// Add link to perspective with given uuid
    AddLink {
        id: String,
        source: String,
        target: String,
        predicate: Option<String>,
        status: Option<String>,
    },

    /// Query links from perspective with given uuid
    QueryLinks(QueryLinksArgs),

    /// Retrieve snapshot of perspective with given uuid
    Snapshot { id: String },

    /// Run Prolog / SDNA query on perspective with given uuid
    Infer { id: String, query: String },

    /// Stay connected and print any changes (links added/removed) to the perspective
    Watch { id: String },

    /// Interactive Perspective shell based on Prolog/SDNA runtime
    Repl { id: String },

    /// Set Social DNA of given perspective with SDNA code from file
    AddDna {
        id: String,
        name: String,
        file: String,
        dna_type: String,
    },

    /// Get all defined Subject classes
    SubjectClasses { id: String },

    /// Construct a new Subject instance of given class over given base
    SubjectConstruct {
        id: String,
        class: String,
        base: String,
    },

    /// Get the value of the subject instance's given property
    SubjectGetProperty {
        id: String,
        base: String,
        property: String,
    },

    /// Set the value of the subject instance's given property
    SubjectSetProperty {
        id: String,
        base: String,
        property: String,
        value: String,
    },

    /// Add the given value to the subject instance's collection
    SubjectAddCollection {
        id: String,
        base: String,
        collection: String,
        value: String,
    },
}

/// The kind of link change carried by a perspective watch event.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LinkChangeKind {
    Added,
    Removed,
    Updated,
}

impl LinkChangeKind {
    /// ANSI-coloured label for terminal output.
    pub fn label(self) -> &'static str {
        match self {
            LinkChangeKind::Added => "\x1b[32m+ ADDED",
            LinkChangeKind::Removed => "\x1b[31m- REMOVED",
            LinkChangeKind::Updated => "\x1b[33m~ UPDATED",
        }
    }
}

/// A classified perspective link-watch event, ready to render.
#[derive(Debug, PartialEq)]
pub struct LinkWatchEvent {
    pub kind: LinkChangeKind,
    pub link: serde_json::Value,
}

/// Classify a raw event from the executor's events websocket for `watch <id>`.
///
/// Returns `Some` only for a link change on the watched perspective. The executor
/// serialises the perspective id as `perspectiveUuid` and an updated link as
/// `newLink`/`oldLink` (camelCase — see `PerspectiveLinkWithOwner` and
/// `PerspectiveLinkUpdatedWithOwner`), so this reads those keys. Reading `uuid`
/// or `link` for updates silently matches nothing. A matched event always yields
/// a payload (falling back to the whole event) so a change is never dropped.
///
/// Pure over the event JSON, so it unit-tests without a live executor.
pub fn classify_link_watch_event(
    event: &serde_json::Value,
    watched_id: &str,
) -> Option<LinkWatchEvent> {
    let event_uuid = event
        .get("perspectiveUuid")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    if event_uuid != watched_id {
        return None;
    }
    let (kind, key) = match event.get("type").and_then(|v| v.as_str()).unwrap_or("") {
        "link-added" => (LinkChangeKind::Added, "link"),
        "link-removed" => (LinkChangeKind::Removed, "link"),
        "link-updated" => (LinkChangeKind::Updated, "newLink"),
        _ => return None,
    };
    let link = event
        .get(key)
        .or_else(|| event.get("data"))
        .cloned()
        .unwrap_or_else(|| event.clone());
    Some(LinkWatchEvent { kind, link })
}

pub async fn run(ad4m_client: Ad4mClient, command: Option<PerspectiveFunctions>) -> Result<()> {
    if command.is_none() {
        // Use interactive perspective selector instead of just printing
        return interactive_perspective_selector(ad4m_client).await;
    }

    match command.unwrap() {
        PerspectiveFunctions::Add { name } => {
            let new_perspective = ad4m_client.perspectives.add(name).await?;
            println!("{}", new_perspective.uuid);
        }
        PerspectiveFunctions::Remove { id } => {
            ad4m_client.perspectives.remove(id).await?;
        }
        PerspectiveFunctions::AddLink {
            id,
            source,
            target,
            predicate,
            status: _status,
        } => {
            ad4m_client
                .perspectives
                .add_link(
                    id,
                    LinkInput {
                        source,
                        target,
                        predicate,
                    },
                )
                .await?;
        }
        PerspectiveFunctions::QueryLinks(args) => {
            let from_date = maybe_parse_datetime(args.from_date)?;
            let until_date = maybe_parse_datetime(args.until_date)?;
            let result = ad4m_client
                .perspectives
                .query_links(
                    args.id,
                    args.source.filter(|s| s != "_"),
                    args.target.filter(|s| s != "_"),
                    args.predicate,
                    from_date.map(|d| d.to_string()),
                    until_date.map(|d| d.to_string()),
                    args.limit.map(|l| l as i64),
                )
                .await?;
            for link in result {
                print_link(link.into());
            }
        }
        PerspectiveFunctions::Infer { id, query } => {
            let results = ad4m_client.perspectives.infer(id, query).await?;
            print_prolog_results(results)?;
        }
        PerspectiveFunctions::Watch { id } => {
            println!("Watching perspective {} for link changes...", id);
            println!("(Press Ctrl+C to stop)\n");
            let mut rx = ad4m_client.subscribe_events();
            loop {
                match rx.recv().await {
                    Ok(event) => {
                        if let Some(change) = classify_link_watch_event(&event, &id) {
                            let label = change.kind.label();
                            if let Ok(link) = serde_json::from_value::<
                                ad4m_client::types::LinkExpression,
                            >(change.link.clone())
                            {
                                print!("{}\x1b[0m ", label);
                                print_link(link.into());
                            } else {
                                println!("{}\x1b[0m {}", label, change.link);
                            }
                        }
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Lagged(n)) => {
                        eprintln!("Warning: dropped {} events (too slow)", n);
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Closed) => {
                        eprintln!("Event stream closed");
                        break;
                    }
                }
            }
        }
        PerspectiveFunctions::Snapshot { id } => {
            let result = ad4m_client.perspectives.snapshot(id).await?;
            println!("{:#?}", result);
        }
        PerspectiveFunctions::Repl { id } => {
            repl_loop(ad4m_client.perspectives.get(id).await?).await?;
        }
        PerspectiveFunctions::AddDna {
            id,
            file,
            name,
            dna_type,
        } => {
            let dna = std::fs::read_to_string(file.clone())
                .with_context(|| anyhow!("Could not read provided SDNA file {}", file))?;
            let perspective = ad4m_client.perspectives.get(id).await?;
            perspective.add_dna(name, dna, dna_type).await?;
            println!("SDNA set successfully");
        }
        PerspectiveFunctions::SubjectClasses { id } => {
            let perspective = ad4m_client.perspectives.get(id).await?;
            let classes = perspective.subject_classes().await?;
            println!("{}", classes.join("\n"));
        }
        PerspectiveFunctions::SubjectConstruct { id, class, base } => {
            let perspective = ad4m_client.perspectives.get(id).await?;
            perspective.create_subject(&class, &base).await?;
        }
        PerspectiveFunctions::SubjectGetProperty { id, base, property } => {
            let perspective = ad4m_client.perspectives.get(id).await?;
            let classes = perspective.get_subject_classes(&base).await?;
            for class in &classes {
                if let Ok(subject) = perspective.get_subject(class, &base).await {
                    let props = subject.get_property_values().await?;
                    if let Some(value) = props.get(&property) {
                        println!("{:#?}", value);
                        return Ok(());
                    }
                }
            }

            println!(
                "\x1b[91mNone of the found classes have a property '{}'",
                property
            );
            println!("The found classes are: {}", classes.join(", "));
        }
        PerspectiveFunctions::SubjectSetProperty {
            id,
            base,
            property,
            value,
        } => {
            let perspective = ad4m_client.perspectives.get(id).await?;
            let classes = perspective.get_subject_classes(&base).await?;
            for class in &classes {
                if let Ok(subject) = perspective.get_subject(class, &base).await {
                    if subject.set_property(&property, &value).await.is_ok() {
                        return Ok(());
                    }
                }
            }

            println!(
                "\x1b[91mNone of the found classes have a property '{}'",
                property
            );
            println!("The found classes are: {}", classes.join(", "));
        }
        PerspectiveFunctions::SubjectAddCollection {
            id,
            base,
            collection,
            value,
        } => {
            let perspective = ad4m_client.perspectives.get(id).await?;
            let classes = perspective.get_subject_classes(&base).await?;
            for class in &classes {
                if let Ok(subject) = perspective.get_subject(class, &base).await {
                    if subject.add_collection(&collection, &value).await.is_ok() {
                        return Ok(());
                    }
                }
            }

            println!(
                "\x1b[91mNone of the found classes have a collection '{}'",
                collection
            );
            println!("The found classes are: {}", classes.join(", "));
        }
    }
    Ok(())
}

async fn interactive_perspective_selector(ad4m_client: Ad4mClient) -> Result<()> {
    // Enable raw mode and alternate screen
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Get all perspectives
    let all_perspectives = ad4m_client.perspectives.all().await?;

    if all_perspectives.is_empty() {
        // Clean up terminal
        disable_raw_mode()?;
        execute!(
            terminal.backend_mut(),
            LeaveAlternateScreen,
            DisableMouseCapture
        )?;
        terminal.show_cursor()?;

        println!("\x1b[91mNo perspectives found!");
        return Ok(());
    }

    let mut selected = 0;
    let mut scroll = 0;
    let list_height = 20; // Maximum visible items
    let mut list_state = ratatui::widgets::ListState::default();
    list_state.select(Some(selected));

    loop {
        terminal.draw(|f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .margin(2)
                .constraints([
                    Constraint::Length(3),
                    Constraint::Min(0),
                    Constraint::Length(2),
                    Constraint::Length(3),
                ])
                .split(f.size());

            // Title
            let title = Paragraph::new(Line::from(vec![Span::styled(
                "AD4M Perspective Selector",
                Style::default()
                    .fg(Color::Cyan)
                    .add_modifier(Modifier::BOLD),
            )]))
            .block(Block::default().borders(Borders::ALL));
            f.render_widget(title, chunks[0]);

            // Perspective list
            let items: Vec<ListItem> = all_perspectives
                .iter()
                .enumerate()
                .map(|(_i, perspective)| {
                    let name = perspective
                        .name
                        .clone()
                        .unwrap_or_else(|| "<unnamed>".to_string());
                    let mut spans = vec![
                        Span::styled(name, Style::default().fg(Color::White)),
                        Span::styled(
                            format!(" ({})", perspective.uuid),
                            Style::default().fg(Color::Gray),
                        ),
                    ];

                    if let Some(nh) = &perspective.neighbourhood {
                        spans.push(Span::styled(
                            format!(" [Neighbourhood: {}]", nh.data.link_language),
                            Style::default().fg(Color::Yellow),
                        ));
                    }

                    if perspective.shared_url.is_some() {
                        spans.push(Span::styled(" [Shared]", Style::default().fg(Color::Green)));
                    }

                    ListItem::new(vec![Line::from(spans)])
                })
                .collect();

            let list = List::new(items)
                .block(
                    Block::default()
                        .borders(Borders::ALL)
                        .title("Select a perspective (↑/↓ arrows, Enter to select, q to quit)"),
                )
                .highlight_style(
                    Style::default()
                        .bg(Color::Cyan)
                        .fg(Color::Black)
                        .add_modifier(Modifier::BOLD),
                )
                .highlight_symbol("> ")
                .highlight_spacing(ratatui::widgets::HighlightSpacing::Always);

            f.render_stateful_widget(list, chunks[1], &mut list_state);

            // Status line showing current selection
            let status = Paragraph::new(Line::from(vec![Span::styled(
                format!("Selection: {} of {}", selected + 1, all_perspectives.len()),
                Style::default().fg(Color::Blue),
            )]))
            .block(Block::default().borders(Borders::ALL));
            f.render_widget(status, chunks[2]);

            // Instructions
            let instructions = Paragraph::new(Line::from(vec![
                Span::styled("Navigation: ", Style::default().fg(Color::Yellow)),
                Span::styled("↑/↓ arrows, ", Style::default().fg(Color::White)),
                Span::styled("Enter: ", Style::default().fg(Color::Green)),
                Span::styled("select perspective, ", Style::default().fg(Color::White)),
                Span::styled("q: ", Style::default().fg(Color::Red)),
                Span::styled("quit", Style::default().fg(Color::White)),
            ]))
            .block(Block::default().borders(Borders::ALL));
            f.render_widget(instructions, chunks[3]);
        })?;

        // Handle input
        if let Event::Key(key) = event::read()? {
            match key.code {
                KeyCode::Up | KeyCode::Char('k') => {
                    if selected > 0 {
                        selected -= 1;
                        if selected < scroll {
                            scroll = selected;
                        }
                        list_state.select(Some(selected));
                    }
                }
                KeyCode::Down | KeyCode::Char('j') => {
                    if selected < all_perspectives.len() - 1 {
                        selected += 1;
                        if selected >= scroll + list_height {
                            scroll = selected - list_height + 1;
                        }
                        list_state.select(Some(selected));
                    }
                }
                KeyCode::Enter => {
                    // Clean up terminal
                    disable_raw_mode()?;
                    execute!(
                        terminal.backend_mut(),
                        LeaveAlternateScreen,
                        DisableMouseCapture
                    )?;
                    terminal.show_cursor()?;

                    // Get the selected perspective and start REPL
                    let selected_perspective = &all_perspectives[selected];
                    let name = selected_perspective
                        .name
                        .clone()
                        .unwrap_or_else(|| "<unnamed>".to_string());
                    println!("\x1b[32mSelected perspective: \x1b[97m{}", name);
                    println!(
                        "\x1b[32mStarting REPL for perspective: \x1b[97m{}",
                        selected_perspective.uuid
                    );

                    // Start REPL for the selected perspective
                    let perspective_proxy = ad4m_client
                        .perspectives
                        .get(selected_perspective.uuid.clone())
                        .await?;
                    crate::repl::repl_loop(perspective_proxy).await?;
                    return Ok(());
                }
                KeyCode::Char('q') | KeyCode::Esc => {
                    // Clean up terminal
                    disable_raw_mode()?;
                    execute!(
                        terminal.backend_mut(),
                        LeaveAlternateScreen,
                        DisableMouseCapture
                    )?;
                    terminal.show_cursor()?;
                    return Ok(());
                }
                _ => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    // Wire shapes match the executor's `PerspectiveLinkWithOwner` (added/removed)
    // and `PerspectiveLinkUpdatedWithOwner` (updated), both `rename_all=camelCase`.
    fn added_event(uuid: &str) -> serde_json::Value {
        json!({
            "type": "link-added",
            "perspectiveUuid": uuid,
            "link": { "author": "did:key:z6Mkxyz", "timestamp": "t", "data": { "source": "s", "predicate": "p", "target": "tgt" } },
            "owner": "did:key:z6Mkxyz"
        })
    }

    #[test]
    fn classifies_link_added_on_the_watched_perspective() {
        let ev = added_event("uuid-1");
        let got = classify_link_watch_event(&ev, "uuid-1").expect("should classify");
        assert_eq!(got.kind, LinkChangeKind::Added);
        assert_eq!(
            got.link.get("author").and_then(|v| v.as_str()),
            Some("did:key:z6Mkxyz")
        );
    }

    #[test]
    fn ignores_events_for_a_different_perspective() {
        let ev = added_event("uuid-OTHER");
        // Regression guard: the id lives under `perspectiveUuid`, not `uuid`.
        assert_eq!(classify_link_watch_event(&ev, "uuid-1"), None);
    }

    #[test]
    fn reads_the_perspective_id_from_perspectiveuuid_not_uuid() {
        // The old code read `uuid`, which the executor never emits — so every
        // event mismatched and `watch` displayed nothing. Pin the correct key.
        let ev = json!({ "type": "link-added", "uuid": "uuid-1", "perspectiveUuid": "uuid-1", "link": {} });
        assert!(classify_link_watch_event(&ev, "uuid-1").is_some());
        let wrong = json!({ "type": "link-added", "uuid": "uuid-1", "link": {} }); // no perspectiveUuid
        assert_eq!(classify_link_watch_event(&wrong, "uuid-1"), None);
    }

    #[test]
    fn link_updated_reads_newlink_not_link() {
        // Updated events carry `newLink`/`oldLink`, never a bare `link`.
        let ev = json!({
            "type": "link-updated",
            "perspectiveUuid": "uuid-1",
            "newLink": { "marker": "NEW" },
            "oldLink": { "marker": "OLD" }
        });
        let got = classify_link_watch_event(&ev, "uuid-1").expect("should classify update");
        assert_eq!(got.kind, LinkChangeKind::Updated);
        assert_eq!(got.link.get("marker").and_then(|v| v.as_str()), Some("NEW"));
    }

    #[test]
    fn classifies_removed() {
        let ev = json!({ "type": "link-removed", "perspectiveUuid": "uuid-1", "link": { "marker": "R" } });
        assert_eq!(
            classify_link_watch_event(&ev, "uuid-1").unwrap().kind,
            LinkChangeKind::Removed
        );
    }

    #[test]
    fn ignores_non_link_event_types() {
        let ev = json!({ "type": "agent-updated", "perspectiveUuid": "uuid-1", "agent": {} });
        assert_eq!(classify_link_watch_event(&ev, "uuid-1"), None);
    }

    #[test]
    fn matched_event_missing_payload_falls_back_to_whole_event() {
        // Never silently drop a matched change: with no `link` key, surface the event.
        let ev = json!({ "type": "link-added", "perspectiveUuid": "uuid-1" });
        let got = classify_link_watch_event(&ev, "uuid-1").expect("should still classify");
        assert_eq!(
            got.link.get("type").and_then(|v| v.as_str()),
            Some("link-added")
        );
    }
}
