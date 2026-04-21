//! # ad4m-ldk — AD4M Language Development Kit (Rust)
//!
//! Typed wrappers around the runtime imports (spec §7) and an
//! `ad4m_language!` macro (spec §9) for authoring flat-export AD4M
//! Languages compiled to WASM via wasm-bindgen.
//!
//! Quick start:
//! ```ignore
//! use ad4m_ldk::prelude::*;
//!
//! pub struct MyLang;
//! impl Language for MyLang {
//!     fn name() -> &'static str { "@example/my-lang" }
//!     fn version() -> &'static str { "0.1.0" }
//!     async fn init() -> LanguageResult<Self> { Ok(Self) }
//! }
//! impl PerspectiveCommitCapability for MyLang {
//!     fn perspective_commit(&mut self, _diff: PerspectiveDiff) -> LanguageResult<()> { Ok(()) }
//! }
//! ad4m_language! { language: MyLang, capabilities: [perspective_commit] }
//! ```

pub mod errors;
pub mod imports;
pub mod macros;
pub mod state;
pub mod traits;
pub mod types;

#[doc(hidden)]
pub mod __serde {
    //! Internal — used by the `ad4m_language!` macro.
    //!
    //! `serde_wasm_bindgen::to_value` defaults to serializing maps as
    //! JS `Map` objects. The runtime dispatcher (and most JS clients)
    //! expect plain objects: `JSON.stringify(new Map([['a',1]]))`
    //! returns `"{}"`, which silently loses all structured data. We
    //! always serialize through this helper so every Rust ALDK shim
    //! emits plain objects.
    use serde::Serialize;
    use wasm_bindgen::JsValue;

    pub fn to_js<T: ?Sized + Serialize>(
        value: &T,
    ) -> Result<JsValue, serde_wasm_bindgen::Error> {
        let serializer = serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
        value.serialize(&serializer)
    }
}

pub mod prelude {
    pub use crate::ad4m_language;
    pub use crate::errors::{ErrorCode, LanguageError, LanguageResult};
    pub use crate::imports as runtime;
    pub use crate::traits::{
        ExpressionCapability, HolochainSignalHandler, Language, PeersCapability,
        PerspectiveCommitCapability, PerspectiveQueryCapability, PerspectiveSyncCapability,
        TelepresenceCapability,
    };
    pub use crate::types::*;
}
