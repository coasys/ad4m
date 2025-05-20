use deno_core::op2;
use crate::js_core::error::AnyhowWrapperError;
use crate::pubsub::get_global_pubsub;

#[op2(async)]
async fn publish(#[string] topic: String, #[string] data: String) -> Result<(), AnyhowWrapperError> {
    let pub_sub = get_global_pubsub().await;
    pub_sub.publish(&topic, &data).await;
    Ok(())
}

deno_core::extension!(
    pubsub_service,
    ops = [publish],
    esm_entry_point = "ext:pubsub_service/pubsub_extension.js",
    esm = [dir "src/js_core", "pubsub_extension.js"]
);
