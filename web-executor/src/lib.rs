//! The web executor: `ad4m-executor` built for wasm32, behind three wasm-bindgen exports.
//! The core's side is `rust_executor::web::boot`; this crate only turns it into JS calls and
//! sends the core's log to the console. Empty on native.
#![cfg(target_arch = "wasm32")]

use rust_executor::web::boot;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn console_log(line: &str);
    #[wasm_bindgen(js_namespace = console, js_name = warn)]
    fn console_warn(line: &str);
    #[wasm_bindgen(js_namespace = console, js_name = error)]
    fn console_error(line: &str);
}

/// The core's `log` records on the console: errors and warnings always, the rest up to the
/// level `boot` sets (`AD4M_WEB_LOG` is not read: a Worker has no environment).
struct ConsoleLogger;

impl log::Log for ConsoleLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::max_level()
    }

    fn log(&self, record: &log::Record) {
        if !self.enabled(record.metadata()) {
            return;
        }
        let line = format!(
            "[ad4m {} {}] {}",
            record.level(),
            record.target(),
            record.args()
        );
        match record.level() {
            log::Level::Error => console_error(&line),
            log::Level::Warn => console_warn(&line),
            _ => console_log(&line),
        }
    }

    fn flush(&self) {}
}

static LOGGER: ConsoleLogger = ConsoleLogger;

/// Starts the core in memory. `config_json`: `{}` or `{"languageLanguageOnly": bool}`.
/// `log_level` (`"error"`, `"warn"`, `"info"`, `"debug"`, `"trace"`, `"off"`; default
/// `"warn"`) sets how much of the core's log reaches the console. Throws a string when the
/// core does not start.
#[wasm_bindgen]
pub fn boot(config_json: &str, log_level: Option<String>) -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    let level = match log_level.as_deref() {
        None => log::LevelFilter::Warn,
        Some(name) => name
            .parse()
            .map_err(|_| JsValue::from_str(&format!("unknown log level {name:?}")))?,
    };
    if log::set_logger(&LOGGER).is_ok() {
        log::set_max_level(level);
    }
    boot::boot(config_json).map_err(|e| JsValue::from_str(&e))
}

/// One request frame (`{"id", "type", "params"}`, a JSON string) in; resolves to its reply
/// frame (`{"id", "result"}` or `{"id", "error"}`, a JSON string). Never rejects.
#[wasm_bindgen]
pub fn dispatch(frame_json: String) -> js_sys::Promise {
    wasm_bindgen_futures::future_to_promise(async move {
        Ok(JsValue::from_str(&boot::dispatch(frame_json).await))
    })
}

/// Calls `callback(frame)` with every event the core publishes, as a JSON string
/// (`{"type", ...payload}`).
#[wasm_bindgen]
pub fn on_event(callback: js_sys::Function) {
    boot::on_event(move |frame| {
        if let Err(e) = callback.call1(&JsValue::NULL, &JsValue::from_str(&frame)) {
            log::warn!("on_event callback threw: {e:?}");
        }
    });
}
