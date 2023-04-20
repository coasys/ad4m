use deno_core::{error::AnyError, include_js_files, op, Extension};

use crate::pubsub::get_global_pubsub;

#[op]
async fn publish(topic: String, data: String) -> Result<(), AnyError> {
    let pub_sub = get_global_pubsub().await;
    pub_sub.publish(&topic, &data).await;
    Ok(())
}

pub fn build() -> Extension {
    Extension::builder("wallet")
        .js(include_js_files!(wallet "pubsub_extension.js",))
        .ops(vec![publish::decl()])
        .force_op_registration()
        .build()
}
