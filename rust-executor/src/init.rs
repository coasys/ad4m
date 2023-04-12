use log::{info, warn};
use semver::{Version, VersionReq};
use std::error::Error;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

use super::utils::ad4m_data_directory;
use crate::globals::{
    AD4M_VERSION, HC_BIN, HOLOCHAIN_BIN, MAINNET_JSON, OLDEST_VERSION, SWIPL_ZIP,
};
use crate::utils::write_zip;

/// Sets up the ad4m data directory and config files ready for the executor to consume
pub fn init(
    hc_only: bool,
    data_path: Option<String>,
    network_bootstrap_seed: Option<String>,
) -> Result<(), Box<dyn Error>> {
    std::env::set_var("RUST_LOG", "info");
    env_logger::init();

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

    let latest_seen_version = fs::read_to_string(&last_seen_file)?;
    info!("Current last seen version is: {}", latest_seen_version);
    let last_seen_version = Version::parse(&latest_seen_version)?;
    let version_comparison =
        format!(">={}, <={}", &OLDEST_VERSION.version, *AD4M_VERSION).to_string();
    let compare = VersionReq::parse(&version_comparison)?;

    if !compare.matches(&last_seen_version) {
        // Agents old ad4m version is too old, lets clean their state
        warn!("Agents old ad4m version is too old, lets clean their state");
        clean_ad4m_data(&app_data_path, OLDEST_VERSION.clear_state)?;
        //Create the last seen version file
        fs::write(last_seen_file, AD4M_VERSION.to_string())?;
    }

    //Write the mainnet seed to the data directory
    write_seed_config(&app_data_path, network_bootstrap_seed)?;

    //Create the path for binaries if it doesn't already exist
    let binary_path = Path::new(&app_data_path).join("binary");
    if !Path::new(&binary_path).exists() {
        info!("Creating binary path");
        fs::create_dir_all(&binary_path)?;
    }

    let platform = os_info::get().os_type();
    let (holochain, hc) = match platform {
        os_info::Type::Windows => ("holochain.exe", "hc.exe"),
        _ => ("holochain", "hc"),
    };

    if !hc_only {
        let holochain_data = HOLOCHAIN_BIN;
        let holochain_target = binary_path.join(holochain);
        info!("writing holochain target");
        fs::write(&holochain_target, holochain_data)?;
        fs::set_permissions(holochain_target, fs::Permissions::from_mode(0o755))?;
    }

    let hc_data = HC_BIN;
    let hc_target = binary_path.join(hc);
    info!("write hc target");
    fs::write(&hc_target, hc_data)?;
    fs::set_permissions(hc_target, fs::Permissions::from_mode(0o755))?;

    info!("write swipl target");
    write_zip(SWIPL_ZIP.to_vec(), app_data_path);

    Ok(())
}

fn write_seed_config(
    app_data_path: &PathBuf,
    network_bootstrap_seed: Option<String>,
) -> Result<(), Box<dyn Error>> {
    let target_seed_path = app_data_path.join("mainnet_seed.seed");
    if network_bootstrap_seed.is_none() {
        info!("No bootstrap seed supplied... using the one found in local files");
        let seed_file_data = MAINNET_JSON;
        fs::write(target_seed_path, seed_file_data)?;
        info!("wrote seed file");
    } else {
        let seed_path = PathBuf::from(network_bootstrap_seed.unwrap());
        let seed_file_data = fs::read_to_string(seed_path)?;
        fs::write(target_seed_path, seed_file_data)?;
    };
    Ok(())
}

fn clean_ad4m_data(
    app_data_path: &PathBuf,
    should_clear_state: bool,
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
    }
    Ok(())
}