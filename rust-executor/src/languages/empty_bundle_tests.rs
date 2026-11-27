//! An empty `languageLanguageBundle` is refused before `bundle.js` is written.

use std::path::{Path, PathBuf};

use crate::languages::LanguageController;
use crate::runtime_service::RuntimeService;

fn write_seed(dir: &Path, bundle: &str) -> PathBuf {
    let path = dir.join("seed.json");
    let seed = serde_json::json!({
        "trustedAgents": [],
        "knownLinkLanguages": [],
        "directMessageLanguage": "",
        "agentLanguage": "",
        "perspectiveLanguage": "",
        "neighbourhoodLanguage": "",
        "languageLanguageBundle": bundle,
    });
    std::fs::write(&path, seed.to_string()).unwrap();
    path
}

struct ClearRuntime;

impl Drop for ClearRuntime {
    fn drop(&mut self) {
        let instance = RuntimeService::global_instance();
        *instance.lock().unwrap_or_else(|p| p.into_inner()) = None;
    }
}

fn bundle_files(dir: &Path) -> Vec<PathBuf> {
    let mut found = Vec::new();
    let Ok(entries) = std::fs::read_dir(dir) else {
        return found;
    };
    for entry in entries.flatten() {
        let bundle = entry.path().join("bundle.js");
        if bundle.is_file() {
            found.push(bundle);
        }
    }
    found.sort();
    found
}

/// Loading core languages with an empty or whitespace-only
/// `languageLanguageBundle` fails, names that field, and writes no bundle.
#[tokio::test]
async fn load_refuses_an_empty_language_language_bundle() {
    let _clear = ClearRuntime;
    let languages = crate::test_utils::test_languages_directory();
    for bundle in ["", " \n\t"] {
        let dir = tempfile::tempdir().unwrap();
        let seed = write_seed(dir.path(), bundle);
        let before = bundle_files(&languages);
        RuntimeService::init_global_instance(seed.to_string_lossy().into_owned());
        let err = LanguageController::global_instance()
            .load_core_system_languages(true)
            .await
            .expect_err("an empty languageLanguageBundle must not load");
        let message = err.to_string();
        assert!(
            message.contains("languageLanguageBundle"),
            "the error must name the seed field, got: {message}"
        );
        assert_eq!(
            bundle_files(&languages),
            before,
            "an empty languageLanguageBundle must not write bundle.js"
        );
    }
}
