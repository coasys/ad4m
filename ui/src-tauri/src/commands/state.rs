use tauri::State;

use crate::AppState;

#[tauri::command]
pub fn request_credential(state: State<'_, AppState>) -> String {
    state.req_credential.clone()
}

#[tauri::command]
pub fn get_port(state: State<'_, AppState>) -> u16 {
    state.graphql_port
}
