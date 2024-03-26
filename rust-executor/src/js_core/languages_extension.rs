use std::borrow::Cow;
use deno_core::{include_js_files, op2, Extension, Op};

use crate::{graphql::graphql_types::{PerspectiveExpression, PerspectiveState}, types::PerspectiveDiff};


#[op2]
#[serde]
fn perspective_diff_received(
    #[serde] diff: PerspectiveDiff, 
    #[string] language_address: String
) {
    crate::perspectives::handle_perspective_diff_from_link_language(diff, language_address);
}

#[op2]
#[serde]
fn sync_state_changed(
    #[serde] state: PerspectiveState, 
    #[string] language_address: String
)  {
    crate::perspectives::handle_sync_state_changed_from_link_language(state, language_address);
}


#[op2]
#[serde]
fn telepresence_signal_received(
    #[serde] signal: PerspectiveExpression,
    #[string] language_address: String
) {
    crate::perspectives::handle_telepresence_signal_from_link_language(signal, language_address);
}


pub fn build() -> Extension {
    Extension {
        name: "agent",
        js_files: Cow::Owned(include_js_files!(rust_executor "src/js_core/languages_extension.js").to_vec()),
        ops: Cow::Borrowed(&[
            perspective_diff_received::DECL,
            sync_state_changed::DECL,
            telepresence_signal_received::DECL,
        ]),
        ..Default::default()
    }
}