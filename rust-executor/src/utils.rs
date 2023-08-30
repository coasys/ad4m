use std::env::home_dir;
use std::path::{Path, PathBuf};


pub(crate) fn ad4m_data_directory() -> PathBuf {
    let ad4m_dir = home_dir().unwrap().join(".ad4m");
    ad4m_dir
}
