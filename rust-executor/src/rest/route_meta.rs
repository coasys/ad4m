//! Route metadata — collected at link-time via `inventory` from `#[rest_handler]` annotations.
//!
//! This replaces the hand-maintained `route_registry.rs` array.

/// Metadata for a single REST route, auto-collected via `#[rest_handler]`.
pub struct RouteMetadata {
    pub method: &'static str,
    pub path: &'static str,
    pub handler_name: &'static str,
    pub request_type: &'static str,
    pub response_type: &'static str,
}

inventory::collect!(RouteMetadata);
