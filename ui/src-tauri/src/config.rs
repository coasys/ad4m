use std::path::PathBuf;

use tauri::api::path::home_dir;

pub fn data_path() -> PathBuf {
    home_dir().expect("Could not get home dir").join(".ad4m")
}

pub fn log_path() -> PathBuf {
    data_path().join("ad4min.log")
}

pub fn binary_path() -> PathBuf {
    data_path().join("binary")
}

pub fn holochain_binary_path() -> PathBuf {
    if cfg!(windows) {
        binary_path().join("holochain.exe")
    } else {
        binary_path().join("holochain")
    }
}

#[cfg(feature = "custom-protocol")]
pub fn app_url() -> String {
    "index.html".to_string()
}

#[cfg(not(feature = "custom-protocol"))]
pub fn app_url() -> String {
    "http://localhost:3000".to_string()
}

pub fn executor_port_path() -> PathBuf {
    data_path().join("executor-port")
}