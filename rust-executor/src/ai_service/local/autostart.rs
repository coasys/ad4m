//! Autostart: the `local_ai_autostart` switch, and the start of the installed runtimes when
//! the executor starts. It starts only what an explicit install put in place
//! (`ai.localInstall`, `ad4m-executor local-ai install`, the Docker image's
//! `INCLUDE_MODELS`); nothing here, nor any request, downloads or installs.

use super::runtimes::{reap_from_pid_file, start_at};
use super::{AIService, LocalPaths, Selection};

/// `local_ai_autostart`: start the installed runtimes when the executor starts. On unless
/// set off. It never installs anything.
pub(crate) fn autostart_enabled() -> bool {
    crate::config::get_global_config()
        .local_ai_autostart
        .unwrap_or(true)
}

/// At executor start: stop what a crash left, then start the installed runtimes unless
/// `local_ai_autostart` is off. Without an install, nothing. In the background: the API
/// serves meanwhile.
pub fn autostart() {
    tokio::spawn(async {
        let paths = match LocalPaths::from_config() {
            Ok(paths) => paths,
            Err(e) => {
                log::warn!("Local AI does not start: {e:#}");
                return;
            }
        };
        if Selection::load(&paths).is_none() {
            return;
        }
        reap_from_pid_file(&paths);
        if !autostart_enabled() {
            log::info!(
                "Local AI is installed; local_ai_autostart is off, so it waits for ai.localStart"
            );
            return;
        }
        // The start's load of the models first: done later, it would put back the clients of
        // the managed endpoints' settings from before this start registers them.
        if let Ok(service) = AIService::global_instance().await {
            service.wait_loaded().await;
        }
        log::info!("Local AI: starting the installed runtimes");
        if let Err(e) = start_at(&paths, false).await {
            log::error!("Local AI did not start: {e:#}");
        }
    });
}
