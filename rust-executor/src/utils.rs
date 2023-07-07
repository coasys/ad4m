use std::env::var_os;
use std::error::Error;
use std::path::{Path, PathBuf};

pub(crate) fn ad4m_data_directory() -> PathBuf {
    let mut ad4m_dir = var_os("HOME")
        .unwrap_or_else(|| panic!("Failed to get home directory"))
        .into_string()
        .unwrap_or_else(|_| panic!("Failed to convert HOME env variable to string"));
    ad4m_dir.push_str("/.ad4m");
    Path::new(&ad4m_dir).to_path_buf()
}

#[cfg(not(target_os = "windows"))]
pub fn set_permissions(path: PathBuf) -> Result<(), Box<dyn Error>> {
    use std::fs::Permissions;
    use std::os::unix::fs::PermissionsExt;

    let mut perms = Permissions::from_mode(0o755);
    perms.set_readonly(true);
    perms.set_mode(0o755);
    std::fs::set_permissions(path, perms)?;
    Ok(())
}

#[cfg(target_os = "windows")]
pub fn set_permissions(_path: PathBuf) -> Result<(), Box<dyn Error>> {
    Ok(())
}
