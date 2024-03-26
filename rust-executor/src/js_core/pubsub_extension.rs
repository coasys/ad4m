use std::borrow::Cow;

use deno_core::{error::AnyError, include_js_files, op2, Extension, Op};

use crate::pubsub::get_global_pubsub;

#[op2(async)]
async fn publish(#[string] topic: String, #[string] data: String) -> Result<(), AnyError> {
    let pub_sub = get_global_pubsub().await;
    pub_sub.publish(&topic, &data).await;
    Ok(())
}


pub fn build() -> Extension {
    Extension {
        name: "pubsub",
        js_files: Cow::Owned(include_js_files!(holochain_service "src/js_core/pubsub_extension.js").to_vec()),
        ops: Cow::Borrowed(&[
            publish::DECL
        ]),
        ..Default::default()
    }
}
