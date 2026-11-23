use log::{info, warn};
use semver::{Version, VersionReq};
use std::error::Error;
use std::fs;
use std::path::Path;

use super::utils::ad4m_data_directory;
use crate::globals::{AD4M_VERSION, MAINNET_JSON, OLDEST_VERSION};

/// Sets up the ad4m data directory and config files ready for the executor to consume
pub fn init(
    data_path: Option<String>,
    network_bootstrap_seed: Option<String>,
) -> Result<(), Box<dyn Error>> {
    // Initialize CLI logging with default configuration
    crate::logging::init_cli_logging(None);

    //Get the default data path if none is provided
    let app_data_path = match data_path {
        Some(data_path) => Path::new(&data_path).to_path_buf(),
        None => ad4m_data_directory(),
    };

    // last-seen-version file path
    let last_seen_file = app_data_path.join("last-seen-version");

    // Check the ad4m data directory exists
    if !app_data_path.exists() {
        // Create the data path
        fs::create_dir_all(&app_data_path)?;

        //Create the last seen version file
        fs::write(&last_seen_file, AD4M_VERSION.to_string())?;
    };

    if !last_seen_file.exists() {
        //Create the last seen version file
        fs::write(&last_seen_file, AD4M_VERSION.to_string())?;
    }

    let latest_seen_version = fs::read_to_string(&last_seen_file)?;
    info!("Current last seen version is: {}", latest_seen_version);
    let last_seen_version = Version::parse(&latest_seen_version)?;
    let version_comparison =
        format!(">={}, <={}", &OLDEST_VERSION.version, *AD4M_VERSION).to_string();
    let compare = VersionReq::parse(&version_comparison)?;

    if !compare.matches(&last_seen_version) {
        // Agents old ad4m version is too old, lets clean their state
        warn!("Agents old ad4m version is too old, lets clean their state");
        clean_ad4m_data(&app_data_path, OLDEST_VERSION.clear_state, false)?;
        //Create the last seen version file
        fs::write(last_seen_file, AD4M_VERSION.to_string())?;
    }

    //Write the mainnet seed to the data directory
    write_seed_config(&app_data_path, network_bootstrap_seed)?;

    Ok(())
}

fn write_seed_config(
    app_data_path: &Path,
    network_bootstrap_seed: Option<String>,
) -> Result<(), Box<dyn Error>> {
    let target_seed_path = app_data_path.join("mainnet_seed.seed");
    let supplied = network_bootstrap_seed.is_some();
    let seed_file_data = match network_bootstrap_seed {
        None => {
            info!("No bootstrap seed supplied... using the one found in local files");
            MAINNET_JSON.to_string()
        }
        Some(seed_path) => fs::read_to_string(seed_path)?,
    };
    // Refuse before the write. A file that does not parse as a seed is copied
    // as before; loading it reports the parse error. An existing
    // mainnet_seed.seed is left in place when this returns.
    if let Ok(seed) = serde_json::from_str::<crate::runtime_service::BootstrapSeed>(&seed_file_data)
    {
        crate::runtime_service::require_language_language_bundle(&seed.language_language_bundle)?;
    }
    fs::write(target_seed_path, seed_file_data)?;
    if !supplied {
        info!("wrote seed file");
    }
    Ok(())
}

fn clean_ad4m_data(
    app_data_path: &Path,
    should_clear_state: bool,
    should_clear_everything: bool,
) -> Result<(), Box<dyn Error>> {
    if app_data_path.exists() {
        let binary_path = app_data_path.join("binary");
        let config_path = app_data_path.join("ad4m-host-config.json");
        let bootstrap_seed_path = app_data_path.join("mainnet_seed.json");
        let holochain_data_path = app_data_path.join("ad4m").join("h");
        let languages_path = app_data_path.join("ad4m").join("languages");

        // Delete all the data which may conflict with the new version
        info!("Deleting binary path");
        if fs::metadata(&binary_path).is_ok() {
            fs::remove_dir_all(binary_path)?;
        }
        info!("Deleting config path");
        if fs::metadata(&config_path).is_ok() {
            fs::remove_file(config_path)?;
        }
        info!("Deleting bootstrap seed path");
        if fs::metadata(&bootstrap_seed_path).is_ok() {
            fs::remove_file(bootstrap_seed_path)?;
        }
        info!("Deleting holochain data path");
        if fs::metadata(&holochain_data_path).is_ok() {
            fs::remove_dir_all(holochain_data_path)?;
        }
        info!("Deleting languages path");
        if fs::metadata(&languages_path).is_ok() {
            fs::remove_dir_all(languages_path)?;
        }

        if should_clear_state {
            let db_path = app_data_path.join("ad4m").join("data").join("db.json");
            let languages_path = app_data_path.join("ad4m").join("languages");
            let perspective_path = app_data_path.join("ad4m").join("perspectives.json");

            info!("Deleting db path");
            if fs::metadata(&db_path).is_ok() {
                fs::remove_file(db_path)?;
            }
            info!("Deleting languages path");
            if fs::metadata(&languages_path).is_ok() {
                fs::remove_dir_all(languages_path)?;
            }
            info!("Deleting perspectives path");
            if fs::metadata(&perspective_path).is_ok() {
                fs::remove_file(perspective_path)?;
            }
        };

        if should_clear_everything {
            let swipl_path = app_data_path.join("swipl");
            if fs::metadata(&swipl_path).is_ok() {
                fs::remove_dir_all(swipl_path)?;
            };
            let ipfs_path = app_data_path.join("ipfs");
            if fs::metadata(&ipfs_path).is_ok() {
                fs::remove_dir_all(ipfs_path)?;
            };
            let ad4m_path = app_data_path.join("ad4m");
            if fs::metadata(&ad4m_path).is_ok() {
                fs::remove_dir_all(ad4m_path)?;
            };
            let binary_path = app_data_path.join("binary");
            if fs::metadata(&binary_path).is_ok() {
                fs::remove_dir_all(binary_path)?;
            };
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    /// A bootstrap seed whose `languageLanguageBundle` is `bundle`.
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
        fs::write(&path, seed.to_string()).unwrap();
        path
    }

    /// `init` with an empty or whitespace-only `languageLanguageBundle` fails
    /// and names that seed field. It does not write `mainnet_seed.seed`.
    #[test]
    fn init_refuses_an_empty_language_language_bundle() {
        for bundle in ["", " \n\t"] {
            let dir = tempfile::tempdir().unwrap();
            let data = dir.path().join("data");
            let seed = write_seed(dir.path(), bundle);
            let err = init(
                Some(data.to_string_lossy().into_owned()),
                Some(seed.to_string_lossy().into_owned()),
            )
            .expect_err("an empty languageLanguageBundle must fail init");
            let message = err.to_string();
            assert!(
                message.contains("languageLanguageBundle"),
                "the error must name the seed field, got: {message}"
            );
            assert!(
                !data.join("mainnet_seed.seed").exists(),
                "init must not write mainnet_seed.seed when languageLanguageBundle is empty"
            );
        }

        // A folder that already has a seed keeps it: the refused seed is not written over it.
        let dir = tempfile::tempdir().unwrap();
        let data = dir.path().join("data");
        fs::create_dir_all(&data).unwrap();
        fs::write(
            data.join("last-seen-version"),
            crate::globals::AD4M_VERSION.as_str(),
        )
        .unwrap();
        fs::write(data.join("mainnet_seed.seed"), "kept").unwrap();
        let seed = write_seed(dir.path(), "");
        let err = init(
            Some(data.to_string_lossy().into_owned()),
            Some(seed.to_string_lossy().into_owned()),
        )
        .expect_err("an empty languageLanguageBundle must fail init");
        assert!(
            err.to_string().contains("languageLanguageBundle"),
            "the error must name the seed field, got: {err}"
        );
        assert_eq!(
            fs::read_to_string(data.join("mainnet_seed.seed")).unwrap(),
            "kept"
        );
    }

    /// A seed whose `languageLanguageBundle` has source still initialises, and
    /// so does `init` with no seed path (the built-in seed).
    #[test]
    fn init_writes_a_seed_whose_language_language_bundle_is_present() {
        let dir = tempfile::tempdir().unwrap();
        let data = dir.path().join("data");
        let bundle = "export async function init() { return {}; }\n";
        let seed = write_seed(dir.path(), bundle);
        init(
            Some(data.to_string_lossy().into_owned()),
            Some(seed.to_string_lossy().into_owned()),
        )
        .expect("a languageLanguageBundle with source must init");
        let written = fs::read_to_string(data.join("mainnet_seed.seed")).unwrap();
        let parsed: crate::runtime_service::BootstrapSeed = serde_json::from_str(&written).unwrap();
        assert_eq!(parsed.language_language_bundle, bundle);

        let builtin = tempfile::tempdir().unwrap();
        let builtin_data = builtin.path().join("data");
        init(Some(builtin_data.to_string_lossy().into_owned()), None)
            .expect("the built-in seed must init");
        let written = fs::read_to_string(builtin_data.join("mainnet_seed.seed")).unwrap();
        let parsed: crate::runtime_service::BootstrapSeed = serde_json::from_str(&written).unwrap();
        assert!(
            !parsed.language_language_bundle.trim().is_empty(),
            "the built-in seed's languageLanguageBundle is present"
        );
    }
}
