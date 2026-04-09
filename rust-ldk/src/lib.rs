//! # AD4M Language Development Kit (LDK) for Rust/WASM
//!
//! This crate provides the core types, traits, and utilities for building
//! AD4M languages in Rust that compile to WebAssembly.
//!
//! ## Usage
//!
//! ```rust
//! use ad4m_ldk::{Language, LanguageContext, Perspective};
//!
//! pub struct MyLanguage {
//!     context: LanguageContext,
//! }
//!
//! impl Language for MyLanguage {
//!     const NAME: &'static str = "my-language";
//!     const VERSION: &'static str = "0.1.0";
//!
//!     fn init(context: LanguageContext) -> Result<Self, String> {
//!         Ok(Self { context })
//!     }
//!
//!     fn get_state(&self) -> Result<Option<Perspective>, String> {
//!         Ok(None)
//!     }
//!
//!     fn receive(&self, _data: Vec<u8>) -> Result<(), String> {
//!         Ok(())
//!     }
//! }
//! ```

pub mod callbacks;
pub mod capabilities;
pub mod imports;
pub mod language;
pub mod types;

// Re-export commonly used items
pub use callbacks::{dm_trigger_callback, links_trigger_callback};
pub use capabilities::{ExpressionCapability, LinkSyncCapability};
pub use imports::{agent, holochain, signal_emit};
pub use imports::language as lang;
pub use language::{Language, LanguageContext};
pub use types::*;

// Use wee_alloc for smaller WASM binary size when the feature is enabled
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
