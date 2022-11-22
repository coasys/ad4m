use localtunnel_client::open_tunnel;
use tauri::State;
use tokio::sync::broadcast;
use graphql_client::{GraphQLQuery, Response};

use crate::{AppState, ProxyState, ProxyService};

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "resources/schema.json",
    query_path = "resources/sign_message.gql",
    response_derives = "Debug"
)]
struct AgentSignMessage;

#[tauri::command]
pub async fn setup_proxy(subdomain: String, app_state: State<'_, AppState>, proxy: State<'_, ProxyState>) -> Result<String, String> {
    let graphql_port = app_state.graphql_port;
    let req_credential = &app_state.req_credential;
    let (notify_shutdown, _) = broadcast::channel(1);
    let subdomain = subdomain.replace(":", "-").to_lowercase();

    let rand = reqwest::get(format!("https://proxy-worker.ad4m.dev/login?did={}", subdomain))
        .await
        .map_err(|err| format!("Error happend when send login request: {:?}", err))?
        .text()
        .await
        .map_err(|err| format!("Error happend when retrieving the content: {:?}", err))?;

    let client = reqwest::Client::new();
    let query = AgentSignMessage::build_query(agent_sign_message::Variables {message: rand});
    let response: Response<agent_sign_message::ResponseData> = client.post(format!("http://localhost:{}/graphql", graphql_port))
        .header("Authorization", req_credential)
        .json(&query)
        .send()
        .await
        .map_err(|err| format!("Error happend when agent sign message: {:?}", err))?
        .json()
        .await
        .map_err(|err| format!("Error happend when the signing content: {:?}", err))?;

    let resp_data = response.data.ok_or("No data provided".to_string())?;
    let signature = resp_data.agent_sign_message.signature;
    let public_key = resp_data.agent_sign_message.public_key;

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
        Some("https://proxy-worker.ad4m.dev"),
        Some(&subdomain),
        None,
        graphql_port,
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
