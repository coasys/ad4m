use deno_core::{error::AnyError, include_js_files, op, Extension};

use super::{get_prolog_service, PrologService};

#[op]
async fn start_prolog_service() -> Result<(), AnyError> {
    PrologService::init().await?;
    Ok(())
}

#[op]
async fn run_query(query: String) -> Result<String, AnyError> {
    let interface = get_prolog_service().await;
    let result = interface.run_query(query).await;
    let string_result = format!("{:?}", result);
    Ok(string_result)
}

#[op]
async fn load_module_string(module_name: String, program: String) -> Result<(), AnyError> {
    let interface = get_prolog_service().await;
    interface.load_module_string(module_name, program).await
}

pub fn build() -> Extension {
    Extension::builder("prolog_service")
        .js(include_js_files!(holochain_service "prolog_service_extension.js",))
        .ops(vec![
            start_prolog_service::decl(),
            run_query::decl(),
            load_module_string::decl(),
        ])
        .force_op_registration()
        .build()
}
