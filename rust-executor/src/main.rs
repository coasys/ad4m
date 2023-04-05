mod js_core;

fn main() {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    let js_core = js_core::JsCore::new();
    js_core.init_engine();
    if let Err(error) = runtime.block_on(js_core.event_loop()) {
        eprintln!("error: {}", error);
    }
}
