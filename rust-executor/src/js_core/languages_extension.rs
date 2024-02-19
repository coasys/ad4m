use std::borrow::Cow;

use cid::Cid;
use deno_core::{error::AnyError, include_js_files, op2, Extension, Op};
use multibase::Base;
use multihash::{Code, MultihashDigest};
use log::{error, info, debug, warn};

use super::JS_CORE_HANDLE;

#[op2]
#[serde]
fn perspective_diff_received(
    diff: PerspectiveDiff, 
    #[string] language_address: String
) -> Result<(), AnyError> {
    crate::perspectives::handle_perspective_diff_from_link_language(diff, language_address);
}

#[op2]
#[serde]
fn sync_state_changed(
    state: PerspectiveState, 
    #[string] language_address: String
) -> Result<(), AnyError> {
    crate::perspectives::handle_sync_state_changed_from_link_language(diff, language_address);
}


#[op2]
#[serde]
fn sync_state_changed(
    signal: PerspectiveExpression,
    #[string] language_address: String
) -> Result<(), AnyError> {
    crate::perspectives::handle_signal_from_neighbourhood(signal, language_address);
}
