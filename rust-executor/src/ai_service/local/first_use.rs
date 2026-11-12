//! First use and autostart: the `local_ai_autostart` switch, the setup of a missing
//! service when something first needs a model of its type, and the start of the installed
//! runtimes when the executor starts.

use anyhow::Result;

use super::data::Service;
use super::runtimes::{reap_from_pid_file, start_at, stop_all};
use super::setup::{install_at, run_installing, take_install_flag};
use super::{AIService, InstallParams, LocalAiRefused, LocalPaths, Selection};

pub(crate) fn autostart_enabled() -> bool {
    crate::config::get_global_config()
        .local_ai_autostart
        .unwrap_or(true)
}

/// Whether the executor has a model of `service`'s type.
fn has_model_of(service: Service) -> Result<bool> {
    let model_type = service.model_type();
    Ok(crate::db_backend::db_backend()
        .get_models()?
        .iter()
        .any(|m| m.model_type == model_type))
}

/// Set up local AI's `service` for an executor with no model of its type, on its first use:
/// what dev's default models were (Bert, Whisper), downloaded when something first needed
/// them, not at start. The selection keeps what it had and gains `service`; the runtimes
/// restart with it, and its model becomes the default of a type that has none (an
/// operator's defaults stay). Off with `local_ai_autostart: false`. An install already
/// running (the launcher's) is waited for first.
pub async fn ensure_service(service: Service) -> Result<()> {
    if !autostart_enabled() {
        return Err(LocalAiRefused(format!(
            "No {} model on this executor, and local_ai_autostart is off: set up local AI \
             (ai.localInstall) or register a model on an endpoint",
            service.as_str()
        ))
        .into());
    }
    // Taken, not only waited for: an install that begins meanwhile makes this wait, not
    // fail; and the selection read below is the one that install left.
    let guard = take_install_flag().await;
    // The install waited for (the launcher's) may have registered a model of the type: no
    // second install, which would restart the runtimes under requests that use them.
    if has_model_of(service)? {
        return Ok(());
    }
    let paths = LocalPaths::from_config()?;
    let mut services: Vec<Service> = Selection::load(&paths)
        .map(|s| s.models.iter().map(|m| m.service).collect())
        .unwrap_or_default();
    if !services.contains(&service) {
        services.push(service);
    }
    log::info!(
        "No {} model on this executor: setting up local AI for it ({})",
        service.as_str(),
        services
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    );
    let params = InstallParams {
        services: Some(services),
        start: Some(false),
    };
    run_installing(guard, async {
        install_at(&paths, &params).await?;
        stop_all().await;
        start_at(&paths, false).await
    })
    .await?;
    Ok(())
}

/// At executor start: stop what a crash left, then start the installed runtimes unless
/// `local_ai_autostart` is off. In the background: the API serves meanwhile.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ai_service::local::test_support::TEST_LOCK;
    use crate::types::{Endpoint, EndpointKind, ModelInput};

    // The first use of a missing default sets up local AI only where local AI may start on
    // its own; with local_ai_autostart off it says what to do instead, and downloads nothing.
    #[tokio::test]
    async fn without_autostart_a_first_use_sets_nothing_up_and_says_how() {
        let _serial = TEST_LOCK.lock().await;
        use crate::config::{Ad4mConfig, GLOBAL_AD4M_CONFIG};
        let store = tempfile::tempdir().unwrap();
        let saved_config = GLOBAL_AD4M_CONFIG
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .clone();
        crate::config::set_global_config(Ad4mConfig {
            local_ai_dir: Some(store.path().to_string_lossy().into_owned()),
            app_data_path: Some(store.path().join("app").to_string_lossy().into_owned()),
            local_ai_autostart: Some(false),
            ..Default::default()
        });
        let outcome = ensure_service(Service::Embeddings).await;
        *GLOBAL_AD4M_CONFIG.lock().unwrap_or_else(|e| e.into_inner()) = saved_config;

        let error = outcome.unwrap_err();
        assert!(error.downcast_ref::<LocalAiRefused>().is_some(), "{error}");
        assert!(
            error.to_string().contains("local_ai_autostart is off"),
            "{error}"
        );
        assert_eq!(
            std::fs::read_dir(store.path()).unwrap().count(),
            0,
            "nothing was downloaded"
        );
    }

    /// A first use that waited for an install finds the model that install registered: it
    /// sets up nothing more.
    #[tokio::test]
    async fn a_first_use_after_an_install_finds_its_model() {
        let _serial = TEST_LOCK.lock().await;
        crate::test_utils::ensure_test_db();
        let db = crate::db_backend::db_backend();
        let endpoint = Endpoint::for_test(
            "has-model-test",
            EndpointKind::OpenAiAudio,
            "http://127.0.0.1:2",
            "",
        );
        db.insert_endpoint(&endpoint).unwrap();
        let id = db
            .add_model(&ModelInput {
                name: "has-model-test".into(),
                model_type: crate::types::ModelType::Transcription,
                endpoint_id: endpoint.id.clone(),
                remote_model: "m".into(),
                native_tools: None,
            })
            .unwrap();
        let after = has_model_of(Service::Stt).unwrap();
        db.remove_model(&id).unwrap();
        db.remove_endpoint(&endpoint.id).unwrap();
        assert!(after, "the registered transcription model is found");
    }
}
