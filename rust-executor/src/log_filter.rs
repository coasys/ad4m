//! The log filter, changeable while the executor runs (`runtime.setLogFilter`,
//! `runtime.logFilter`).
//!
//! env_logger fixes its filter when it is built. [`install`] puts a [`log::Log`] in front of
//! it that asks [`state()`] instead: the startup filter (`RUST_LOG`, as env_logger reads it)
//! plus an override an operator sets for a while. The override is a list of directives like
//! `ws_rpc=debug,sfu=debug`; a target it names gets the override's level, every other target
//! keeps the startup filter. It reverts by itself once its time is up (checked on the next
//! log call and on every read of the status, so no timer runs).
//!
//! A directive name matches a target by whole path segments, anywhere in the path: `ws_rpc`
//! matches `rust_executor::api::ws_rpc`, `api::ws_rpc` too, `rust_executor` every module of
//! the crate. The most specific match decides: the one that reaches deepest into the target's
//! path (`ws_rpc` over `rust_executor` for `rust_executor::api::ws_rpc`), then the longer name.
//!
//! An override is sanitised as the startup filter is: rmcp stays at `warn` unless the
//! override names `rmcp` (`logging::without_mcp_session_ids`; rmcp logs each MCP session's
//! id, a bearer, at info), and a name matched in the middle of a path never reaches into rmcp.

use log::{LevelFilter, Log, Metadata, Record};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{LazyLock, RwLock};
use std::time::{Duration, Instant};

/// How long an override lasts when the caller names no time.
pub const DEFAULT_TTL: Duration = Duration::from_secs(1800);

/// The longest an override may last: a debug level left on fills the disk.
pub const MAX_TTL: Duration = Duration::from_secs(4 * 3600);

/// One directive of an override: a target name (or every target) and its level.
#[derive(Debug, Clone, PartialEq)]
struct Directive {
    name: Option<String>,
    level: LevelFilter,
}

#[derive(Debug, Clone)]
struct Override {
    spec: String,
    directives: Vec<Directive>,
    until: Instant,
    until_wall: chrono::DateTime<chrono::Utc>,
}

impl Override {
    /// The level the override gives `target`, if it names it.
    fn level_for(&self, target: &str) -> Option<LevelFilter> {
        self.directives
            .iter()
            .filter_map(|d| match &d.name {
                None => Some(((0, 0), d.level)),
                Some(name) => match_depth(target, name).map(|depth| ((depth, name.len()), d.level)),
            })
            // On a tie the later directive wins, as in env_logger.
            .max_by_key(|(specificity, _)| *specificity)
            .map(|(_, level)| level)
    }

    fn max_level(&self) -> LevelFilter {
        self.directives
            .iter()
            .map(|d| d.level)
            .max()
            .unwrap_or(LevelFilter::Off)
    }
}

/// How deep into `target`'s path directive name `name` matches (the number of the target's
/// segments up to the end of the deepest match), or `None` when it does not: whole path
/// segments, anywhere in the path.
fn match_depth(target: &str, name: &str) -> Option<usize> {
    let target: Vec<&str> = target.split("::").collect();
    let name: Vec<&str> = name.split("::").collect();
    (0..target.len())
        .rev()
        // Into rmcp only by a name that starts with `rmcp`: `session=debug` must not reach
        // `rmcp::...::session::local`, which logs MCP session ids (bearers) at info.
        .find(|&i| target[i..].starts_with(&name) && (i == 0 || target[0] != "rmcp"))
        .map(|i| i + name.len())
}

#[cfg(test)]
fn names(target: &str, name: &str) -> bool {
    match_depth(target, name).is_some()
}

/// Parse an override like `ws_rpc=debug,sfu=debug` or `debug`. Refuses what env_logger would
/// skip with a message on stderr: an unknown level, an empty name, a `/regex` part.
fn parse(spec: &str) -> Result<Vec<Directive>, String> {
    let mut directives = Vec::new();
    for part in spec.split(',').map(str::trim).filter(|p| !p.is_empty()) {
        if part.contains('/') {
            return Err(format!("'{part}': a /regex filter is not supported"));
        }
        if part.starts_with('=') {
            return Err(format!("'{part}': no target name"));
        }
        let mut halves = part.splitn(2, '=');
        let first = halves.next().unwrap_or_default().trim();
        let directive = match halves.next() {
            Some(level) => Directive {
                name: Some(first.to_string()),
                level: level
                    .trim()
                    .parse()
                    .map_err(|_| format!("'{part}': unknown level '{}'", level.trim()))?,
            },
            // A bare level applies to every target; a bare name is that name at trace.
            None => match first.parse::<LevelFilter>() {
                Ok(level) => Directive { name: None, level },
                Err(_) => Directive {
                    name: Some(first.to_string()),
                    level: LevelFilter::Trace,
                },
            },
        };
        directives.push(directive);
    }
    if directives.is_empty() {
        return Err("the filter names no directive".to_string());
    }
    Ok(directives)
}

/// What `runtime.logFilter` reports.
#[derive(Debug, Clone, PartialEq)]
pub struct FilterStatus {
    /// The startup filter (`RUST_LOG` as the executor started with it).
    pub startup: String,
    /// The override in force, if any.
    pub current: Option<String>,
    /// When the override reverts (RFC 3339, UTC).
    pub reverts_at: Option<chrono::DateTime<chrono::Utc>>,
}

/// The startup filter: its `RUST_LOG` text, the env_logger filter it builds, and, when it has
/// a `/regex` part, a filter that applies only that regex (to the records an override lets
/// through: env_logger applies the regex to every record, in `Filter::matches`).
struct Startup {
    spec: String,
    filter: env_logger::filter::Filter,
    regex: Option<env_logger::filter::Filter>,
}

impl Startup {
    fn new(spec: &str) -> Self {
        Startup {
            spec: spec.to_string(),
            filter: build_filter(spec),
            regex: spec
                .split_once('/')
                .map(|(_, regex)| build_filter(&format!("trace/{regex}"))),
        }
    }
}

/// The filter every log call asks: the startup filter plus an override.
pub struct FilterState {
    startup: RwLock<Option<Startup>>,
    has_override: AtomicBool,
    over: RwLock<Option<Override>>,
    /// Whether this state sets `log`'s global max level: the executor's own does; a test's
    /// does not, so tests do not change it under each other.
    drives_max_level: bool,
}

static STATE: LazyLock<FilterState> = LazyLock::new(|| FilterState::new(true));

/// The executor's filter state.
pub fn state() -> &'static FilterState {
    &STATE
}

fn build_filter(spec: &str) -> env_logger::filter::Filter {
    env_logger::filter::Builder::new().parse(spec).build()
}

impl FilterState {
    fn new(drives_max_level: bool) -> Self {
        FilterState {
            startup: RwLock::new(None),
            has_override: AtomicBool::new(false),
            over: RwLock::new(None),
            drives_max_level,
        }
    }

    /// `log::set_max_level`, for the executor's state only. Called with the `over` write
    /// lock held (see `revert_if`).
    fn set_max_level(&self, level: LevelFilter) {
        if self.drives_max_level {
            log::set_max_level(level);
        }
    }

    /// Record the startup filter. Called once by [`install`].
    fn set_startup(&self, spec: &str) {
        *self.startup.write().unwrap() = Some(Startup::new(spec));
    }

    /// `f` of the startup filter; before [`install`] (unit tests), the one the environment's
    /// `RUST_LOG` would give.
    fn with_startup<T>(&self, f: impl FnOnce(&Startup) -> T) -> T {
        match &*self.startup.read().unwrap() {
            Some(startup) => f(startup),
            None => f(&Startup::new(
                &std::env::var("RUST_LOG").unwrap_or_default(),
            )),
        }
    }

    fn startup_spec(&self) -> String {
        self.with_startup(|s| s.spec.clone())
    }

    fn startup_enabled(&self, metadata: &Metadata) -> bool {
        self.with_startup(|s| s.filter.enabled(metadata))
    }

    fn startup_max(&self) -> LevelFilter {
        self.with_startup(|s| s.filter.filter())
    }

    /// The override in force; one whose time is up is removed here.
    fn current(&self) -> Option<Override> {
        if !self.has_override.load(Ordering::Relaxed) {
            return None;
        }
        let over = self.over.read().unwrap().clone();
        match over {
            Some(over) if Instant::now() < over.until => Some(over),
            Some(over) => {
                self.revert_if(&over);
                None
            }
            None => None,
        }
    }

    /// Remove `expired` unless another override replaced it meanwhile.
    ///
    /// `log::set_max_level` is written only under the `over` write lock (here, in
    /// [`Self::set_override`] and in [`install`]), so the last level written always belongs to
    /// the last override written: a revert cannot lower the level after a newer override
    /// raised it.
    fn revert_if(&self, expired: &Override) {
        let reverted = {
            let mut slot = self.over.write().unwrap();
            if slot.as_ref().is_some_and(|o| o.until == expired.until) {
                *slot = None;
                self.has_override.store(false, Ordering::Relaxed);
                self.set_max_level(self.startup_max());
                true
            } else {
                false
            }
        };
        if reverted {
            // After the lock is gone: this line goes through the filter again.
            crate::call_diag!(
                log::Level::Info,
                "log.filter-reverted",
                "filter" => expired.spec,
                "startup" => self.startup_spec(),
            );
        }
    }

    /// Whether a record with `metadata` is logged now.
    pub fn enabled(&self, metadata: &Metadata) -> bool {
        if is_audit(metadata) && self.current().is_some() {
            return true;
        }
        if let Some(over) = self.current() {
            if let Some(level) = over.level_for(metadata.target()) {
                return metadata.level() <= level;
            }
        }
        self.startup_enabled(metadata)
    }

    /// Whether `record` is logged now: [`Self::enabled`] for its target and level, and the
    /// startup filter's `/regex` (if any) for its message.
    pub fn record_enabled(&self, record: &Record) -> bool {
        if is_audit(record.metadata()) && self.current().is_some() {
            return true;
        }
        if let Some(over) = self.current() {
            if let Some(level) = over.level_for(record.target()) {
                return record.level() <= level
                    && self.with_startup(|s| s.regex.as_ref().is_none_or(|r| r.matches(record)));
            }
        }
        self.with_startup(|s| s.filter.matches(record))
    }

    /// Set `spec` as the override for `ttl` (at most [`MAX_TTL`]). An empty `spec` reverts to
    /// the startup filter at once.
    pub fn set_override(&self, spec: &str, ttl: Duration) -> Result<FilterStatus, String> {
        if spec.trim().is_empty() {
            let current = self.over.read().unwrap().clone();
            if let Some(over) = current {
                self.revert_if(&over);
            }
            return Ok(self.status());
        }
        if ttl.is_zero() || ttl > MAX_TTL {
            return Err(format!(
                "ttlSeconds must be between 1 and {}",
                MAX_TTL.as_secs()
            ));
        }
        // The startup filter's rule: rmcp stays at warn unless the filter names rmcp, so a
        // global `debug` does not log MCP session ids (`logging::without_mcp_session_ids`).
        let spec = crate::logging::without_mcp_session_ids(spec.trim());
        let directives = parse(&spec)?;
        let over = Override {
            spec,
            directives,
            until: Instant::now() + ttl,
            until_wall: chrono::Utc::now() + chrono::Duration::from_std(ttl).unwrap_or_default(),
        };
        let max = over.max_level().max(self.startup_max());
        {
            // One step under the lock with the level (see `revert_if`).
            let mut slot = self.over.write().unwrap();
            *slot = Some(over.clone());
            self.has_override.store(true, Ordering::Relaxed);
            self.set_max_level(max);
        }
        crate::call_diag!(
            log::Level::Warn,
            "log.filter-set",
            "filter" => over.spec,
            "ttl_s" => ttl.as_secs(),
            "reverts_at" => over.until_wall.to_rfc3339(),
        );
        Ok(self.status())
    }

    /// The startup filter, the override in force and when it reverts.
    pub fn status(&self) -> FilterStatus {
        let over = self.current();
        FilterStatus {
            startup: self.startup_spec(),
            current: over.as_ref().map(|o| o.spec.clone()),
            reverts_at: over.map(|o| o.until_wall),
        }
    }
}

/// A warning of this module: `log.filter-set`, the audit line of an override. No override
/// hides it, not even the `off` or `error` it reports (it is logged once the override is in
/// force, through it).
fn is_audit(metadata: &Metadata) -> bool {
    metadata.target() == module_path!() && metadata.level() <= log::Level::Warn
}

/// The executor's logger: `inner` (an env_logger that passes every record to its output)
/// behind [`state()`].
pub struct ReloadableLogger {
    inner: Box<dyn Log>,
    state: &'static FilterState,
}

impl ReloadableLogger {
    pub fn new(inner: Box<dyn Log>) -> Self {
        Self {
            inner,
            state: state(),
        }
    }

    /// Test-only: a logger behind a state of the test's own.
    #[cfg(test)]
    fn with_state(inner: Box<dyn Log>, state: &'static FilterState) -> Self {
        Self { inner, state }
    }
}

impl Log for ReloadableLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        self.state.enabled(metadata)
    }

    fn log(&self, record: &Record) {
        if self.state.record_enabled(record) {
            self.inner.log(record);
        }
    }

    fn flush(&self) {
        self.inner.flush();
    }
}

/// Install `inner` as the global logger behind the filter state, with `startup_spec` (the
/// `RUST_LOG` the executor starts with) as the startup filter. `inner` must not filter
/// itself: build it with `filter_level(LevelFilter::Trace)`.
pub fn install(inner: Box<dyn Log>, startup_spec: &str) -> Result<(), log::SetLoggerError> {
    state().set_startup(startup_spec);
    log::set_boxed_logger(Box::new(ReloadableLogger::new(inner)))?;
    let _slot = state().over.write().unwrap();
    log::set_max_level(state().startup_max());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Arc, Mutex};

    const WS_RPC: &str = "rust_executor::api::ws_rpc";

    /// A filter state of this test's own: tests run in parallel, and the executor's global
    /// state is shared (only `runtime_ws_tests` uses that one, through the RPC).
    fn test_state() -> &'static FilterState {
        Box::leak(Box::new(FilterState::new(false)))
    }

    fn meta(target: &str, level: log::Level) -> Metadata<'_> {
        Metadata::builder().target(target).level(level).build()
    }

    /// A `Log` that keeps what reaches it.
    #[derive(Clone, Default)]
    struct Sink(Arc<Mutex<Vec<String>>>);

    impl Log for Sink {
        fn enabled(&self, _: &Metadata) -> bool {
            true
        }
        fn log(&self, record: &Record) {
            self.0.lock().unwrap().push(record.args().to_string());
        }
        fn flush(&self) {}
    }

    fn log_debug(logger: &ReloadableLogger, target: &str, text: &str) {
        logger.log(
            &Record::builder()
                .target(target)
                .level(log::Level::Debug)
                .args(format_args!("{text}"))
                .build(),
        );
    }

    #[test]
    fn a_name_matches_whole_path_segments_anywhere() {
        assert!(names(WS_RPC, "ws_rpc"));
        assert!(names(WS_RPC, "api::ws_rpc"));
        assert!(names(WS_RPC, "rust_executor"));
        assert!(names("rust_executor::sfu::server", "sfu"));
        assert!(!names(WS_RPC, "rpc"));
        assert!(!names(WS_RPC, "ws"));
        assert!(!names("rust_executor::sfu_extra", "sfu"));
    }

    #[test]
    fn a_filter_parses_into_directives_or_is_refused() {
        assert_eq!(
            parse("ws_rpc=debug, sfu=trace").unwrap(),
            vec![
                Directive {
                    name: Some("ws_rpc".into()),
                    level: LevelFilter::Debug
                },
                Directive {
                    name: Some("sfu".into()),
                    level: LevelFilter::Trace
                },
            ]
        );
        assert_eq!(parse("debug").unwrap()[0].name, None);
        for bad in ["", "ws_rpc=loud", "=debug", "ws_rpc=debug/foo", " , "] {
            assert!(parse(bad).is_err(), "{bad:?} was accepted");
        }
    }

    #[test]
    fn the_most_specific_name_decides() {
        let over = Override {
            spec: String::new(),
            directives: parse("rust_executor=warn,ws_rpc=debug").unwrap(),
            until: Instant::now() + Duration::from_secs(60),
            until_wall: chrono::Utc::now(),
        };
        assert_eq!(over.level_for(WS_RPC), Some(LevelFilter::Debug));
        assert_eq!(
            over.level_for("rust_executor::sfu::server"),
            Some(LevelFilter::Warn)
        );
        assert_eq!(over.level_for("holochain"), None);
        // Deeper beats longer: `rust_executor::api::ws_rpc` is the more specific name.
        let over = Override {
            directives: parse("api::ws_rpc=error,ws_rpc=trace,api=info").unwrap(),
            ..over
        };
        assert_eq!(over.level_for(WS_RPC), Some(LevelFilter::Error));
        assert_eq!(
            over.level_for("rust_executor::api::runtime_ws"),
            Some(LevelFilter::Info)
        );
    }

    /// `ws_rpc=debug` for one second: a debug line of the socket module passes, others stay
    /// filtered, and after the second the startup filter is back by itself.
    #[test]
    fn an_override_lets_debug_through_until_its_time_is_up() {
        let state = test_state();
        state.set_startup("info");
        let sink = Sink::default();
        let logger = ReloadableLogger::with_state(Box::new(sink.clone()), state);

        log_debug(&logger, WS_RPC, "before");
        let status = state
            .set_override("ws_rpc=debug", Duration::from_secs(1))
            .unwrap();
        assert_eq!(status.current.as_deref(), Some("ws_rpc=debug,rmcp=warn"));
        assert!(status.reverts_at.is_some());
        assert!(state.enabled(&meta(WS_RPC, log::Level::Debug)));
        assert!(!state.enabled(&meta("rust_executor::db", log::Level::Debug)));
        assert!(state.enabled(&meta("rust_executor::db", log::Level::Info)));
        log_debug(&logger, WS_RPC, "during");
        log_debug(&logger, "rust_executor::db", "elsewhere");

        std::thread::sleep(Duration::from_millis(1100));
        log_debug(&logger, WS_RPC, "after");
        assert_eq!(*sink.0.lock().unwrap(), vec!["during".to_string()]);
        let status = state.status();
        assert_eq!(status.current, None);
        assert_eq!(status.reverts_at, None);
        assert_eq!(status.startup, "info");
    }

    #[test]
    fn an_empty_filter_reverts_at_once_and_a_bad_ttl_is_refused() {
        let state = test_state();
        state.set_startup("info");
        state
            .set_override("sfu=debug", Duration::from_secs(60))
            .unwrap();
        assert!(state.enabled(&meta("rust_executor::sfu::server", log::Level::Debug)));
        assert_eq!(state.set_override("", DEFAULT_TTL).unwrap().current, None);
        assert!(!state.enabled(&meta("rust_executor::sfu::server", log::Level::Debug)));
        assert!(state.set_override("sfu=debug", Duration::ZERO).is_err());
        assert!(state
            .set_override("sfu=debug", MAX_TTL + Duration::from_secs(1))
            .is_err());
        assert_eq!(state.status().current, None);
    }

    const RMCP_SESSIONS: &str = "rmcp::transport::streamable_http_server::session::local";

    fn log_info(logger: &ReloadableLogger, target: &str, text: &str) {
        logger.log(
            &Record::builder()
                .target(target)
                .level(log::Level::Info)
                .args(format_args!("{text}"))
                .build(),
        );
    }

    /// rmcp logs each new MCP session's id at info, and that id is a bearer for the session's
    /// remembered token (an admin credential's never expires). The startup filter holds rmcp at
    /// warn (`logging::without_mcp_session_ids`); no override may lift it unless it names rmcp.
    #[test]
    fn an_override_never_logs_mcp_session_ids() {
        let state = test_state();
        state.set_startup(&crate::logging::without_mcp_session_ids("info"));
        let sink = Sink::default();
        let logger = ReloadableLogger::with_state(Box::new(sink.clone()), state);
        for spec in [
            "debug",
            "trace",
            "info",
            "session=debug",
            "local=trace",
            "streamable_http_server=debug,ws_rpc=debug",
        ] {
            state.set_override(spec, Duration::from_secs(60)).unwrap();
            assert!(
                !state.enabled(&meta(RMCP_SESSIONS, log::Level::Info)),
                "{spec:?} lets rmcp's session ids through"
            );
            log_info(
                &logger,
                RMCP_SESSIONS,
                "create new session secret-session-id",
            );
        }
        assert!(
            sink.0.lock().unwrap().is_empty(),
            "{:?}",
            sink.0.lock().unwrap()
        );
        // An operator who names rmcp gets what they asked for, as at startup.
        state
            .set_override("rmcp=debug", Duration::from_secs(60))
            .unwrap();
        assert!(state.enabled(&meta(RMCP_SESSIONS, log::Level::Info)));
        state.set_override("", DEFAULT_TTL).unwrap();
    }

    /// `off` (or `error`) still logs its own `log.filter-set` line: the audit of who turned
    /// the logs down must not be hidden by that very filter.
    #[test]
    fn an_override_does_not_hide_its_own_audit_line() {
        let state = test_state();
        state.set_startup("info");
        let sink = Sink::default();
        let logger = ReloadableLogger::with_state(Box::new(sink.clone()), state);
        for (spec, ttl) in [("off", 4321), ("error", 4322)] {
            state.set_override(spec, Duration::from_secs(ttl)).unwrap();
            let captured =
                crate::call_diag::capture::lines("log.filter-set", &[&format!("ttl_s={ttl}")]);
            let audit = captured.last().expect("log.filter-set was emitted");
            logger.log(
                &Record::builder()
                    .target(&audit.target)
                    .level(audit.level)
                    .args(format_args!("{}", audit.line))
                    .build(),
            );
            logger.log(
                &Record::builder()
                    .target("rust_executor::db")
                    .level(log::Level::Warn)
                    .args(format_args!("hidden"))
                    .build(),
            );
            let seen = sink.0.lock().unwrap().clone();
            assert_eq!(seen.len(), 1, "{spec:?}: {seen:?}");
            assert!(seen[0].contains("event=log.filter-set"), "{seen:?}");
            sink.0.lock().unwrap().clear();
        }
        state.set_override("", DEFAULT_TTL).unwrap();
    }

    /// A startup `RUST_LOG` with a `/regex` keeps only the messages it matches, with or
    /// without an override (env_logger applies it in `matches`, not in `enabled`).
    #[test]
    fn the_startup_regex_still_filters_messages() {
        let state = test_state();
        state.set_startup("info/keep");
        let sink = Sink::default();
        let logger = ReloadableLogger::with_state(Box::new(sink.clone()), state);
        log_info(&logger, "rust_executor::db", "keep this");
        log_info(&logger, "rust_executor::db", "drop this");
        state
            .set_override("ws_rpc=debug", Duration::from_secs(60))
            .unwrap();
        log_debug(&logger, WS_RPC, "keep debug");
        log_debug(&logger, WS_RPC, "drop debug");
        state.set_override("", DEFAULT_TTL).unwrap();
        assert_eq!(
            *sink.0.lock().unwrap(),
            vec!["keep this".to_string(), "keep debug".to_string()]
        );
    }
}
