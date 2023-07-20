use deno_core::{error::AnyError, include_js_files, op, Extension};

use super::{get_prolog_service, init_prolog_service};

#[op]
async fn init() -> Result<(), AnyError> {
    init_prolog_service().await;
    Ok(())
}

#[op]
async fn spawn_engine(engine_name: String) -> Result<(), AnyError> {
    let mut service = get_prolog_service().await;
    service.spawn_engine(engine_name).await
}

#[op]
async fn run_query(engine_name: String, query: String) -> Result<String, AnyError> {
    let service = get_prolog_service().await;
    let result = service.run_query(engine_name, query).await;
    let string_result = format!("{:?}", result);
    Ok(string_result)
}

#[op]
async fn load_module_string(
    engine_name: String,
    module_name: String,
    program: String,
) -> Result<(), AnyError> {
    let service = get_prolog_service().await;
    service
        .load_module_string(engine_name, module_name, program)
        .await
}

pub fn build() -> Extension {
    Extension::builder("prolog_service")
        .js(include_js_files!(holochain_service "prolog_service_extension.js",))
        .ops(vec![
            spawn_engine::decl(),
            run_query::decl(),
            load_module_string::decl(),
        ])
        .force_op_registration()
        .build()
}
