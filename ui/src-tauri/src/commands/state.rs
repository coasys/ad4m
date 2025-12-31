use serde::Serialize;
use tauri::State;

use crate::AppState;

#[derive(Serialize)]
pub struct PortInfo {
    pub port: u16,
    pub tls_enabled: bool,
}

#[tauri::command]
pub fn request_credential(state: State<'_, AppState>) -> String {
    state.req_credential.clone()
}

#[tauri::command]
pub fn get_port(state: State<'_, AppState>) -> PortInfo {
    PortInfo {
        // Return local_port for UI to connect to
        // This is the plain HTTP port even when TLS is enabled
        port: state.local_port,
        tls_enabled: state.tls_enabled,
    }
}
