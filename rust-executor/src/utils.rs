use dirs::home_dir;
use std::path::PathBuf;

pub(crate) fn ad4m_data_directory() -> PathBuf {
    home_dir().unwrap().join(".ad4m")
}
