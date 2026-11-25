//! Call diagnostics: the log lines that say where a call's time went (#1830 and the calls of
//! 2026-11-19). Every line is one line, starts with [`TAG`], then `event=<name>` and
//! `key=value` fields, so `grep '\[call-diag\]'` and `grep 'event=rpc.slow'` work on a
//! server's log. A value with a space, a quote or an `=` is written in double quotes.
//!
//! | event | level | where | when |
//! |---|---|---|---|
//! | `ws.open` / `ws.close` | debug | `api::ws_rpc` | every RPC socket |
//! | `ws.pong-late` | warn | `api::ws_rpc` | a pong left the writer > 1 s after its ping arrived |
//! | `rpc.slow` | warn | `api::ws_rpc` | an RPC took > 2 s or its reply / subscription update exceeded 1,000,000 bytes |
//! | `broadcast.slow` | warn | `perspectives` | `neighbourhood.sendBroadcast` took > 1 s |
//! | `language.queue` | debug | `languages` | every 30 s per language with calls waiting |
//! | `log.filter-set` / `log.filter-reverted` | warn / info | `log_filter` | `runtime.setLogFilter` and its end |

pub(crate) mod model;

/// The tag every call-diagnostics line starts with.
pub const TAG: &str = "[call-diag]";

/// `value` as a field value: bare, or in double quotes when it is empty or holds a space,
/// a quote, an `=` or a line break.
pub fn field_value(value: &str) -> String {
    if value.is_empty()
        || value
            .chars()
            .any(|c| c.is_whitespace() || c == '"' || c == '=' || c.is_control())
    {
        format!("{value:?}")
    } else {
        value.to_string()
    }
}

/// The line for `event` with `fields`.
pub fn line(event: &str, fields: &[(&str, String)]) -> String {
    let mut line = format!("{TAG} event={}", field_value(event));
    for (key, value) in fields {
        line.push(' ');
        line.push_str(key);
        line.push('=');
        line.push_str(&field_value(value));
    }
    line
}

/// Log `event` with `fields` at `level` under `target` (the caller's module).
pub fn emit(level: log::Level, target: &str, event: &str, fields: &[(&str, String)]) {
    let line = line(event, fields);
    #[cfg(test)]
    capture::push(level, target, &line);
    log::log!(target: target, level, "{}", line);
}

/// `call_diag!(level, "event", "key" => value, ...)`: one call-diagnostics line from the
/// calling module. Values are anything `Display`.
#[macro_export]
macro_rules! call_diag {
    ($level:expr, $event:expr $(, $key:literal => $value:expr)* $(,)?) => {
        $crate::call_diag::emit(
            $level,
            module_path!(),
            $event,
            &[$(($key, ($value).to_string())),*],
        )
    };
}

/// Milliseconds of `d`, for a field.
pub fn ms(d: std::time::Duration) -> u128 {
    d.as_millis()
}

tokio::task_local! {
    static PHASES: std::sync::Mutex<Vec<(&'static str, std::time::Duration)>>;
}

/// Record that phase `name` of the call being timed took `took`; nothing when no call is
/// timed on this task ([`timed`]). Code deep in a path (the language runtime handle) reports
/// its phases this way without its callers passing anything down.
pub fn phase(name: &'static str, took: std::time::Duration) {
    let _ = PHASES.try_with(|phases| {
        phases
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .push((name, took));
    });
}

/// Run `f`, collecting the phases it and the code it calls report with [`phase`], in order.
/// Tasks `f` spawns are not part of it.
pub async fn timed<F: std::future::Future>(
    f: F,
) -> (F::Output, Vec<(&'static str, std::time::Duration)>) {
    let phases = std::sync::Mutex::new(Vec::new());
    PHASES
        .scope(phases, async move {
            let out = f.await;
            let phases =
                PHASES.with(|p| std::mem::take(&mut *p.lock().unwrap_or_else(|e| e.into_inner())));
            (out, phases)
        })
        .await
}

/// The milliseconds of phase `name` in `phases`, summed (0 when it did not run).
pub fn phase_ms(phases: &[(&'static str, std::time::Duration)], name: &str) -> u128 {
    phases
        .iter()
        .filter(|(n, _)| *n == name)
        .map(|(_, d)| d.as_millis())
        .sum()
}

/// Test-only record of every line [`emit`] wrote. Tests cannot rely on owning the global
/// logger (another test's `try_init` may have set it first), so the lines are kept here too.
#[cfg(test)]
pub(crate) mod capture {
    use std::sync::Mutex;

    #[derive(Debug, Clone)]
    pub(crate) struct Captured {
        pub level: log::Level,
        pub target: String,
        pub line: String,
    }

    static LINES: Mutex<Vec<Captured>> = Mutex::new(Vec::new());

    pub(super) fn push(level: log::Level, target: &str, line: &str) {
        let mut lines = LINES.lock().unwrap_or_else(|e| e.into_inner());
        // Bounded: a long test run does not grow it without end.
        if lines.len() > 10_000 {
            lines.drain(..5_000);
        }
        lines.push(Captured {
            level,
            target: target.to_string(),
            line: line.to_string(),
        });
    }

    /// The captured lines of `event` that hold every one of `fields` (`key=value`, as
    /// written).
    pub(crate) fn lines(event: &str, fields: &[&str]) -> Vec<Captured> {
        let event_field = format!(" event={event}");
        LINES
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .iter()
            .filter(|c| {
                c.line.contains(&event_field)
                    && fields
                        .iter()
                        .all(|f| c.line.split(' ').any(|part| part == *f))
            })
            .cloned()
            .collect()
    }

    /// The value of field `key` in `line`, unquoted.
    pub(crate) fn field(line: &str, key: &str) -> Option<String> {
        let prefix = format!("{key}=");
        let start = line.find(&format!(" {prefix}"))? + 1 + prefix.len();
        let rest = &line[start..];
        if let Some(quoted) = rest.strip_prefix('"') {
            let end = quoted.find('"')?;
            Some(quoted[..end].to_string())
        } else {
            Some(rest.split(' ').next().unwrap_or_default().to_string())
        }
    }

    /// Assert `line` is one call-diagnostics line: one line, the tag first, then
    /// `key=value` fields only.
    pub(crate) fn assert_format(line: &str) {
        assert!(!line.contains('\n'), "more than one line: {line:?}");
        assert!(line.starts_with(super::TAG), "no tag first: {line:?}");
        let mut rest = line[super::TAG.len()..].trim_start();
        while !rest.is_empty() {
            let eq = rest
                .find('=')
                .unwrap_or_else(|| panic!("not key=value at {rest:?} in {line:?}"));
            let key = &rest[..eq];
            assert!(
                !key.is_empty() && key.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'),
                "bad key {key:?} in {line:?}"
            );
            rest = &rest[eq + 1..];
            let end = if rest.starts_with('"') {
                // The Debug quoting: skip escaped characters.
                let mut escaped = false;
                let mut end = None;
                for (i, c) in rest.char_indices().skip(1) {
                    match c {
                        _ if escaped => escaped = false,
                        '\\' => escaped = true,
                        '"' => {
                            end = Some(i + 1);
                            break;
                        }
                        _ => {}
                    }
                }
                end.unwrap_or_else(|| panic!("unclosed quote in {line:?}"))
            } else {
                rest.find(' ').unwrap_or(rest.len())
            };
            rest = rest[end..].trim_start();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_line_is_the_tag_then_key_value_fields() {
        let line = line(
            "ws.close",
            &[
                ("did", "did:key:z6Mk".to_string()),
                ("reason", "client went away".to_string()),
                ("code", "1001".to_string()),
                ("empty", String::new()),
                ("nl", "a\nb".to_string()),
            ],
        );
        assert_eq!(
            line,
            "[call-diag] event=ws.close did=did:key:z6Mk reason=\"client went away\" code=1001 empty=\"\" nl=\"a\\nb\""
        );
        capture::assert_format(&line);
        assert_eq!(
            capture::field(&line, "reason").as_deref(),
            Some("client went away")
        );
        assert_eq!(capture::field(&line, "code").as_deref(), Some("1001"));
    }

    #[test]
    fn the_macro_writes_one_line_from_the_calling_module() {
        crate::call_diag!(log::Level::Info, "test.macro", "n" => 42, "who" => "unit test");
        let lines = capture::lines("test.macro", &["n=42"]);
        let last = lines.last().expect("the line was captured");
        assert_eq!(last.target, "rust_executor::call_diag::tests");
        assert_eq!(last.level, log::Level::Info);
        assert_eq!(
            last.line,
            "[call-diag] event=test.macro n=42 who=\"unit test\""
        );
    }
}
