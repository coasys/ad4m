//! The language call counters `GET /metrics` reads (`crate::metrics::LanguageRow`).

use super::LanguageController;

impl LanguageController {
    /// The call counters of every loaded language (`GET /metrics`). An unloaded language is
    /// not in the runtimes map, so it is not here.
    pub async fn call_stats(&self) -> Vec<crate::metrics::LanguageRow> {
        self.runtimes
            .lock()
            .await
            .iter()
            .map(|(address, handle)| {
                crate::metrics::LanguageRow::of(
                    address,
                    handle.language_name.as_deref(),
                    &handle.call_stats,
                )
            })
            .collect()
    }
}
