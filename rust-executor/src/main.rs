use deno_core::error::AnyError;

mod js_core;

async fn run() -> Result<(), AnyError> {
    let js_core = js_core::JsCore::new();
    js_core.init_engine().await;
    let core_init = async {
        let result = js_core.init_core().expect("core init failed").await;
        println!("core init done!");
        result
    };
    let (event, init) = tokio::join!(core_init, js_core.event_loop());
    event.and(init)
}

fn main() {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    if let Err(error) = runtime.block_on(run()) {
        eprintln!("error: {}", error);
    }
}
