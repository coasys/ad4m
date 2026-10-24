use chrono::{DateTime, Utc};
use perspective_diff_sync_integrity::{
    EntryTypes, HashReference, LocalHashReference, PerspectiveDiffEntryReference,
};

use crate::errors::SocialContextResult;
use crate::Hash;

pub mod holochain;
pub mod mock;

pub use holochain::HolochainRetreiver;
pub use mock::*;

/// Abstraction over the backing store the perspective-diff algorithm reads
/// from and writes to.
///
/// Step 1 of the holograph spike: removed HDK trait bounds (`SerializedBytes`,
/// `SerializedBytesError`, `Entry`, `EntryVisibility`, `WasmError`,
/// `ScopedEntryDefIndex`) from the trait definition. `get`/`get_with_timestamp`
/// are now concretely typed to `PerspectiveDiffEntryReference` — the only `T`
/// the algorithm ever fetches anyway — which lets us drop the
/// `T: TryFrom<SerializedBytes, …>` machinery. `create_entry` now takes
/// `EntryTypes` directly rather than going through the HDK
/// `Entry: TryFrom<I>` trait bounds; all call sites already pass
/// `EntryTypes::Foo(…)`.
///
/// The `HolochainRetreiver` impl still uses HDK internally (its
/// `PerspectiveDiffEntryReference` decode goes through `SerializedBytes`, its
/// create-entry calls `hdk::prelude::create_entry`). The trait surface no
/// longer carries HDK conversions on its method signatures, so the upcoming
/// `KitsuneRetreiver` (Step 2) and the in-process `MockPerspectiveGraph`
/// can implement it without inheriting HDK type machinery via the trait.
pub trait PerspectiveDiffRetreiver {
    fn get(hash: Hash) -> SocialContextResult<PerspectiveDiffEntryReference>;
    fn get_with_timestamp(
        hash: Hash,
    ) -> SocialContextResult<(PerspectiveDiffEntryReference, DateTime<Utc>)>;
    fn create_entry(entry: EntryTypes) -> SocialContextResult<Hash>;
    fn current_revision() -> SocialContextResult<Option<LocalHashReference>>;
    fn latest_revision() -> SocialContextResult<Option<HashReference>>;
    fn update_current_revision(hash: Hash, timestamp: DateTime<Utc>) -> SocialContextResult<()>;
    fn update_latest_revision(hash: Hash, timestamp: DateTime<Utc>) -> SocialContextResult<()>;
}
