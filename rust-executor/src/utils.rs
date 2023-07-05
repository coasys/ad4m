use std::env::var_os;
use std::path::{Path, PathBuf};

pub(crate) fn ad4m_data_directory() -> PathBuf {
    let mut ad4m_dir = var_os("HOME")
        .unwrap_or_else(|| panic!("Failed to get home directory"))
        .into_string()
        .unwrap_or_else(|_| panic!("Failed to convert HOME env variable to string"));
    ad4m_dir.push_str("/.ad4m");
    Path::new(&ad4m_dir).to_path_buf()
}
