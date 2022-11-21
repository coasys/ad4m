use localtunnel_client::open_tunnel;
use tauri::State;
use tokio::sync::broadcast;

use crate::{AppState, ProxyState, ProxyService};

#[tauri::command]
pub async fn setup_proxy(subdomain: String, app_state: State<'_, AppState>, proxy: State<'_, ProxyState>) -> Result<String, String> {
    let local_port = app_state.graphql_port;
    let (notify_shutdown, _) = broadcast::channel(1);
    let subdomain = subdomain.replace(":", "-").to_lowercase();

    let rand = reqwest::get(format!("https://proxy-worker.ad4m.dev/login?did={}", subdomain))
        .await
        .map_err(|err| format!("Error happend when send login request: {:?}", err))?
        .text()
        .await
        .map_err(|err| format!("Error happend when retrieving the content: {:?}", err))?;

    // let {public_key, signature} = ad4m.agent.sign_message(rand)

    let public_key = "demo-key";
    let signature = "demo-signature";
    let credential = reqwest::get(
            format!(
                "https://proxy-worker.ad4m.dev/login/verify?did={}&signature={}&publicKey={}",
                subdomain, signature, public_key
            ))
        .await
        .map_err(|err| format!("Error happend when send login verify request: {:?}", err))?
        .text()
        .await
        .map_err(|err| format!("Error happend when retrieving the login verify content: {:?}", err))?;

    let endpoint = open_tunnel(
        Some("http://proxy.ad4m.dev"),
        Some(&subdomain),
        None,
        local_port,
        notify_shutdown.clone(),
        5,
        Some(credential),
    )
    .await
    .map_err(|err| format!("Error happend when setup proxy: {:?}", err))?;

    *proxy.0.lock().unwrap() = Some(ProxyService{
        endpoint: endpoint.clone(),
        shutdown_signal: notify_shutdown,
    });

    Ok(endpoint)
}

#[tauri::command]
pub fn get_proxy(proxy: State<'_, ProxyState>) -> Option<String> {
    (*proxy.0.lock().unwrap()).as_ref().map(|s| s.endpoint.clone())
}

#[tauri::command]
pub fn stop_proxy(proxy: State<'_, ProxyState>) {
    match &(*proxy.0.lock().unwrap()) {
        Some(s) => {
            let _ = s.shutdown_signal.send(());
        },
        None => log::info!("Proxy is not set up."),
    };
    *proxy.0.lock().unwrap() = None;
}
