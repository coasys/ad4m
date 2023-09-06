use std::borrow::Cow;

use deno_core::{error::AnyError, include_js_files, op, Extension, Op};

use crate::pubsub::get_global_pubsub;

#[op]
async fn publish(topic: String, data: String) -> Result<(), AnyError> {
    let pub_sub = get_global_pubsub().await;
    pub_sub.publish(&topic, &data).await;
    Ok(())
}

pub fn build() -> Extension {
    Extension {
        name: "pubsub",
        js_files: Cow::Borrowed(&include_js_files!(holochain_service "src/js_core/pubsub_extension.js",)),
        ops: Cow::Borrowed(&[
            publish::DECL
        ]),
        ..Default::default()
    }
}
