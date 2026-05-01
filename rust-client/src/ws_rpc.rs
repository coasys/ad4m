use anyhow::{anyhow, Result};
use futures_util::{SinkExt, StreamExt};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::net::TcpStream;
use tokio::sync::{oneshot, Mutex};
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{connect_async, MaybeTlsStream, WebSocketStream};

static ID_COUNTER: AtomicU64 = AtomicU64::new(0);

fn next_id() -> String {
    ID_COUNTER.fetch_add(1, Ordering::Relaxed).to_string()
}

#[derive(Serialize)]
struct RpcMessage {
    id: String,
    #[serde(rename = "type")]
    msg_type: String,
    params: Value,
}

#[derive(Deserialize)]
struct RpcResponse {
    id: Option<String>,
    result: Option<Value>,
    error: Option<RpcError>,
}

#[derive(Deserialize)]
struct RpcError {
    code: Option<i32>,
    message: Option<String>,
}

type PendingMap = Arc<Mutex<HashMap<String, oneshot::Sender<Result<Value>>>>>;
type WsSink =
    futures_util::stream::SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, Message>;

pub struct WsRpcClient {
    sender: Arc<Mutex<WsSink>>,
    pending: PendingMap,
}

fn to_ws_url(executor_url: &str, token: &str) -> String {
    let ws_base = executor_url
        .replace("http://", "ws://")
        .replace("https://", "wss://")
        .trim_end_matches('/')
        .to_string();
    if token.is_empty() {
        format!("{}/api/v1/ws", ws_base)
    } else {
        format!(
            "{}/api/v1/ws?token={}",
            ws_base,
            urlencoding::encode(token)
        )
    }
}

impl WsRpcClient {
    pub async fn connect(executor_url: &str, token: &str) -> Result<Self> {
        let url = to_ws_url(executor_url, token);
        let (ws_stream, _) = connect_async(&url)
            .await
            .map_err(|e| anyhow!("WebSocket connection to {} failed: {}", url, e))?;
        let (write, mut read) = ws_stream.split();

        let pending: PendingMap = Arc::new(Mutex::new(HashMap::new()));
        let pending_reader = pending.clone();

        // Background reader task: dispatches responses to pending callers
        tokio::spawn(async move {
            while let Some(msg) = read.next().await {
                let text = match msg {
                    Ok(Message::Text(t)) => t,
                    Ok(Message::Close(_)) => break,
                    Err(_) => break,
                    _ => continue,
                };

                let resp: RpcResponse = match serde_json::from_str(&text) {
                    Ok(r) => r,
                    Err(_) => continue, // ignore unparseable messages
                };

                if let Some(id) = resp.id {
                    if let Some(tx) = pending_reader.lock().await.remove(&id) {
                        if let Some(error) = resp.error {
                            let _ = tx.send(Err(anyhow!(
                                "RPC error {}: {}",
                                error.code.unwrap_or(500),
                                error.message.unwrap_or_default()
                            )));
                        } else {
                            let _ = tx.send(Ok(resp.result.unwrap_or(Value::Null)));
                        }
                    }
                }
                // Server-push events without matching pending id are ignored
                // (the rust-client doesn't subscribe to events currently)
            }

            // Connection closed — reject all pending calls
            let mut map = pending_reader.lock().await;
            for (_, tx) in map.drain() {
                let _ = tx.send(Err(anyhow!("WebSocket connection closed")));
            }
        });

        Ok(Self {
            sender: Arc::new(Mutex::new(write)),
            pending,
        })
    }

    pub async fn call<T: DeserializeOwned>(
        &self,
        msg_type: &str,
        params: Value,
    ) -> Result<T> {
        let id = next_id();
        let msg = RpcMessage {
            id: id.clone(),
            msg_type: msg_type.to_string(),
            params,
        };

        let (tx, rx) = oneshot::channel();
        self.pending.lock().await.insert(id.clone(), tx);

        let json = serde_json::to_string(&msg)?;
        let send_result = self
            .sender
            .lock()
            .await
            .send(Message::Text(json))
            .await;

        if let Err(e) = send_result {
            self.pending.lock().await.remove(&id);
            return Err(anyhow!("Failed to send WS message: {}", e));
        }

        let result = rx
            .await
            .map_err(|_| anyhow!("Response channel closed"))??;

        serde_json::from_value(result.clone()).map_err(|e| {
            anyhow!(
                "Failed to deserialize response for '{}': {} (raw: {})",
                msg_type,
                e,
                result
            )
        })
    }

    /// Fire-and-forget call that discards the result.
    pub async fn call_void(&self, msg_type: &str, params: Value) -> Result<()> {
        let _: Value = self.call(msg_type, params).await?;
        Ok(())
    }
}
