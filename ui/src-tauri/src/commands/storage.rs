use crate::encryption;

/// Keeps a storage secret the user typed or pasted in the OS keyring, so the next start
/// finds it. The secret must be the executor's 64-hex form.
#[tauri::command]
pub fn storage_secret_set(secret: String) -> Result<(), String> {
    encryption::set_storage_secret(secret.trim()).map_err(|e| e.to_string())
}

/// The storage secret the OS keyring holds: for the user to back up (shown once, or saved
/// to a file they choose).
#[tauri::command]
pub fn storage_secret_get() -> Result<String, String> {
    encryption::get_storage_secret().map_err(|e| e.to_string())
}

/// Reads a storage secret file, keeps it in the OS keyring and answers the secret, so the
/// caller can unlock the executor with it.
#[tauri::command]
pub fn storage_secret_restore_file(path: String) -> Result<String, String> {
    encryption::restore_storage_secret_file(std::path::Path::new(&path)).map_err(|e| e.to_string())
}
