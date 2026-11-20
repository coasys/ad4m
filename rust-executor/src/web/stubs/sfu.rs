//! `crate::sfu` on wasm32: no call hosting (str0m) in the web executor. Only the event
//! payload types, the native `sfu/types.rs` by path, so the event list keeps its topics.

#[path = "../../sfu/types.rs"]
mod types;

pub use types::{
    SfuCallRenegotiationOffer, SfuDataMessage, SfuMigrateEvent, SfuRoomRoster,
    SfuRosterParticipant, SfuSessionReplaced,
};
