use ad4m_client::Ad4mClient;
use localtunnel_client::open_tunnel;
use tauri::State;
use tokio::sync::broadcast;

use crate::{AppState, ProxyService, ProxyState};

const PROXY_SERVER: &str = "http://140.82.10.81:3030";
const AD4M_SERVER: &str = "http://127.0.0.1";

#[tauri::command]
pub async fn login_proxy(
    subdomain: String,
    app_state: State<'_, AppState>,
    proxy: State<'_, ProxyState>,
) -> Result<(), String> {
    log::info!("Login proxy server with did: {}", subdomain);

    let graphql_port = app_state.graphql_port;
    let req_credential = &app_state.req_credential;
    let subdomain = format_subdomain(&subdomain);

    let rand = reqwest::get(format!("{}/login?did={}", PROXY_SERVER, subdomain))
        .await
        .map_err(|err| {
            log::error!("Error happend when send login request: {:?}", err);
            format!("Set proxy error: {:?}", err)
        })?
        .text()
        .await
        .map_err(|err| {
            log::error!("Error happend when retrieving the content: {:?}", err);
            format!("Set proxy error:  {:?}", err)
        })?;

    let ad4m_client = Ad4mClient::new(
        format!("{}:{}/graphql", AD4M_SERVER, graphql_port),
        req_credential.to_string(),
    );
    let signed_message = ad4m_client.agent.sign_message(rand).await.map_err(|err| {
        log::error!("Error happend when agent sign message: {:?}", err);
        format!("Set proxy error:  {:?}", err)
    })?;

    let credential = reqwest::get(format!(
        "{}/login/verify?did={}&signature={}&publicKey={}",
        PROXY_SERVER, subdomain, signed_message.signature, signed_message.public_key
    ))
    .await
    .map_err(|err| {
        log::error!("Error happend when send login verify request: {:?}", err);
        format!("Set proxy error:  {:?}", err)
    })?
    .text()
    .await
    .map_err(|err| {
        log::error!(
            "Error happend when retrieving the login verify content: {:?}",
            err
        );
        format!("Set proxy error:  {:?}", err)
    })?;

    *proxy.0.lock().unwrap() = ProxyService {
        credential: Some(credential),
        endpoint: None,
        shutdown_signal: None,
    };

    Ok(())
}

#[tauri::command]
pub async fn setup_proxy(
    subdomain: String,
    app_state: State<'_, AppState>,
    proxy: State<'_, ProxyState>,
) -> Result<String, String> {
    log::info!("Setup proxy: {}", subdomain);

    let graphql_port = app_state.graphql_port;
    let (notify_shutdown, _) = broadcast::channel(1);
    let subdomain = format_subdomain(&subdomain);

    let credential = proxy.0.lock().unwrap().credential.clone();

    let endpoint = open_tunnel(
        Some(PROXY_SERVER),
        Some(&subdomain),
        None,
        graphql_port,
        notify_shutdown.clone(),
        5,
        credential.clone(),
    )
    .await
    .map_err(|err| {
        log::error!("Error happend when open proxy: {:?}", err);
        format!("Error happend when setup proxy: {:?}", err)
    })?;
    log::info!("Proxy endpoint: {}", endpoint);

    *proxy.0.lock().unwrap() = ProxyService {
        credential,
        endpoint: Some(endpoint.clone()),
        shutdown_signal: Some(notify_shutdown),
    };

    Ok(endpoint)
}

#[tauri::command]
pub fn get_proxy(proxy: State<'_, ProxyState>) -> Option<String> {
    proxy.0.lock().unwrap().endpoint.clone()
}

#[tauri::command]
pub fn stop_proxy(proxy: State<'_, ProxyState>) {
    match &proxy.0.lock().unwrap().shutdown_signal {
        Some(signal) => {
            let _ = signal.send(());
        }
        None => log::info!("Proxy is not set up."),
    };

    let credential = proxy.0.lock().unwrap().credential.clone();
    *proxy.0.lock().unwrap() = ProxyService {
        credential,
        endpoint: None,
        shutdown_signal: None,
    };
}

fn format_subdomain(subdomain: &str) -> String {
    subdomain.replace("did:key:", "")
}
