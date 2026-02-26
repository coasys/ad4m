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
        // Return graphql_port which is always the local HTTP port
        port: state.graphql_port,
        tls_enabled: state.tls_enabled,
    }
}
