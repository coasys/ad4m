//! The web executor: `ad4m-executor` built for wasm32, behind wasm-bindgen exports: `boot`,
//! `connect` (one RPC connection, the Worker's `WorkerSocket`), `dispatch` (one frame, for
//! tests), `on_panic`.
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
    std::panic::set_hook(Box::new(panic_hook));
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

/// One RPC connection: what a `WorkerSocket` holds in the Worker.
#[wasm_bindgen]
pub struct Connection {
    socket: rust_executor::web::session::Socket,
}

#[wasm_bindgen]
impl Connection {
    /// One client frame (a JSON string), as the socket's `send`. `false` once the connection
    /// has ended.
    pub fn send(&self, frame: String) -> bool {
        self.socket.send(frame)
    }

    /// Ends the connection, as a closed socket.
    pub fn close(&self) {
        self.socket.close();
    }

    #[wasm_bindgen(getter, js_name = connectionId)]
    pub fn connection_id(&self) -> String {
        self.socket.connection_id().to_string()
    }
}

/// Opens a connection as the operator (no token). Every frame the core sends on it (replies,
/// `pong`, the events its `events.watch` asks for) goes to `on_frame(text)`; `on_closed()` runs
/// once when the connection has ended. Throws a string before `boot`.
#[wasm_bindgen]
pub fn connect(
    on_frame: js_sys::Function,
    on_closed: js_sys::Function,
) -> Result<Connection, JsValue> {
    let socket = rust_executor::web::session::connect(
        move |frame| {
            if let Err(e) = on_frame.call1(&JsValue::NULL, &JsValue::from_str(&frame)) {
                log::warn!("on_frame threw: {e:?}");
            }
        },
        move || {
            let _ = on_closed.call0(&JsValue::NULL);
        },
    )
    .map_err(|e| JsValue::from_str(&e))?;
    Ok(Connection { socket })
}

thread_local! {
    static PANIC_LISTENER: std::cell::RefCell<Option<js_sys::Function>> =
        const { std::cell::RefCell::new(None) };
}

/// Calls `callback(message)` when the core panics, just before the module aborts (a panic on
/// wasm32 aborts: the JS caller then gets a `RuntimeError: unreachable`, and the instance is
/// done). The Worker uses the message as the reason it closes its sockets with. The panic
/// still goes to the console.
#[wasm_bindgen]
pub fn on_panic(callback: js_sys::Function) {
    PANIC_LISTENER.with(|listener| *listener.borrow_mut() = Some(callback));
}

fn panic_hook(info: &std::panic::PanicHookInfo<'_>) {
    console_error_panic_hook::hook(info);
    let message = match info.payload().downcast_ref::<&str>() {
        Some(text) => text.to_string(),
        None => match info.payload().downcast_ref::<String>() {
            Some(text) => text.clone(),
            None => info.to_string(),
        },
    };
    PANIC_LISTENER.with(|listener| {
        if let Ok(listener) = listener.try_borrow() {
            if let Some(callback) = listener.as_ref() {
                let _ = callback.call1(&JsValue::NULL, &JsValue::from_str(&message));
            }
        }
    });
}

/// Panics with `message` in a task on the event loop, as a fault in the core would. Only for
/// the test that a panic kills the Worker (`tests/web-executor/worker-death.test.mjs`).
#[wasm_bindgen]
pub fn panic_for_test(message: String) {
    wasm_bindgen_futures::spawn_local(async move { panic!("{message}") });
}
