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

#[tokio::main]
async fn main() {
    if let Err(error) = run().await {
        eprintln!("error: {}", error);
    }
}
