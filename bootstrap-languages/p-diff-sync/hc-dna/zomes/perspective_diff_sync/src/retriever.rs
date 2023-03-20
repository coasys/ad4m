use crate::Hash;
use crate::errors::SocialContextResult;
use hdk::prelude::*;
use chrono::{DateTime, Utc};

pub mod holochain;
pub mod mock;

pub use holochain::HolochainRetreiver;
pub use mock::*;
use perspective_diff_sync_integrity::{LocalHashReference, HashReference};

pub trait PerspectiveDiffRetreiver {
    fn get<T>(hash: Hash) -> SocialContextResult<T> 
        where
        T: TryFrom<SerializedBytes, Error = SerializedBytesError>;

    fn get_with_timestamp<T>(hash: Hash) -> SocialContextResult<(T, DateTime<Utc>)> 
        where
        T: TryFrom<SerializedBytes, Error = SerializedBytesError>;

    fn create_entry<I, E: std::fmt::Debug, E2>(entry: I) -> SocialContextResult<Hash>
        where
        ScopedEntryDefIndex: for<'a> TryFrom<&'a I, Error = E2>,
        EntryVisibility: for<'a> From<&'a I>,
        Entry: TryFrom<I, Error = E>,
        WasmError: From<E>,
        WasmError: From<E2>;
    fn current_revision() -> SocialContextResult<Option<LocalHashReference>>;
    fn latest_revision() -> SocialContextResult<Option<HashReference>>;
    fn update_current_revision(hash: Hash, timestamp: DateTime<Utc>) -> SocialContextResult<()>;
    fn update_latest_revision(hash: Hash, timestamp: DateTime<Utc>) -> SocialContextResult<()>;
}


