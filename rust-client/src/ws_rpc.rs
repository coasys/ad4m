use anyhow::{anyhow, Result};
use futures_util::{SinkExt, StreamExt};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::net::TcpStream;
use tokio::sync::{broadcast, oneshot, Mutex};
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
type WsSink = futures_util::stream::SplitSink<WebSocketStream<MaybeTlsStream<TcpStream>>, Message>;

pub struct WsRpcClient {
    sender: Arc<Mutex<WsSink>>,
    pending: PendingMap,
    events: broadcast::Sender<Value>,
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
        format!("{}/api/v1/ws?token={}", ws_base, urlencoding::encode(token))
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

        let (events_tx, _) = broadcast::channel::<Value>(256);
        let events_tx_reader = events_tx.clone();

        // Background reader task: dispatches RPC responses to pending callers
        // and broadcasts server-push events to subscribers.
        tokio::spawn(async move {
            while let Some(msg) = read.next().await {
                let text = match msg {
                    Ok(Message::Text(t)) => t,
                    Ok(Message::Close(_)) => break,
                    Err(_) => break,
                    _ => continue,
                };

                let parsed: Value = match serde_json::from_str(&text) {
                    Ok(v) => v,
                    Err(_) => continue,
                };

                // RPC responses have an "id" field; events do not.
                if let Some(id) = parsed.get("id").and_then(|v| v.as_str()).map(str::to_owned) {
                    let resp: RpcResponse = match serde_json::from_value(parsed) {
                        Ok(r) => r,
                        Err(_) => continue,
                    };
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
                } else {
                    // Server-push event — broadcast to subscribers
                    let _ = events_tx_reader.send(parsed);
                }
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
            events: events_tx,
        })
    }

    pub async fn call<T: DeserializeOwned>(&self, msg_type: &str, params: Value) -> Result<T> {
        let id = next_id();
        let msg = RpcMessage {
            id: id.clone(),
            msg_type: msg_type.to_string(),
            params,
        };

        let (tx, rx) = oneshot::channel();
        self.pending.lock().await.insert(id.clone(), tx);

        let json = serde_json::to_string(&msg)?;
        let send_result = self.sender.lock().await.send(Message::Text(json)).await;

        if let Err(e) = send_result {
            self.pending.lock().await.remove(&id);
            return Err(anyhow!("Failed to send WS message: {}", e));
        }

        let result = rx.await.map_err(|_| anyhow!("Response channel closed"))??;

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

    /// Subscribe to server-push events.
    /// Returns a broadcast receiver that yields raw event JSON values.
    /// Each event has a `"type"` field indicating the event kind.
    pub fn subscribe_events(&self) -> broadcast::Receiver<Value> {
        self.events.subscribe()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_ws_url_converts_http_to_ws() {
        let url = to_ws_url("http://localhost:12000", "");
        assert_eq!(url, "ws://localhost:12000/api/v1/ws");
    }

    #[test]
    fn to_ws_url_converts_https_to_wss() {
        let url = to_ws_url("https://remote.host:443", "");
        assert_eq!(url, "wss://remote.host:443/api/v1/ws");
    }

    #[test]
    fn to_ws_url_appends_token_query_param() {
        let url = to_ws_url("http://localhost:12000", "my-jwt-token");
        assert_eq!(url, "ws://localhost:12000/api/v1/ws?token=my-jwt-token");
    }

    #[test]
    fn to_ws_url_encodes_special_chars_in_token() {
        let url = to_ws_url("http://localhost:12000", "tok en+special=chars&more");
        assert!(url.contains("token=tok%20en%2Bspecial%3Dchars%26more"));
    }

    #[test]
    fn to_ws_url_strips_trailing_slash() {
        let url = to_ws_url("http://localhost:12000/", "");
        assert_eq!(url, "ws://localhost:12000/api/v1/ws");
    }

    #[test]
    fn to_ws_url_empty_token_no_query_param() {
        let url = to_ws_url("http://localhost:12000", "");
        assert!(!url.contains("?token="));
        assert!(!url.contains("?token"));
    }

    #[test]
    fn next_id_increments() {
        let id1 = next_id();
        let id2 = next_id();
        let n1: u64 = id1.parse().unwrap();
        let n2: u64 = id2.parse().unwrap();
        assert_eq!(n2, n1 + 1);
    }

    #[test]
    fn rpc_message_serializes_correctly() {
        let msg = RpcMessage {
            id: "42".to_string(),
            msg_type: "agent.status".to_string(),
            params: serde_json::json!({}),
        };
        let json = serde_json::to_value(&msg).unwrap();
        assert_eq!(json["id"], "42");
        assert_eq!(json["type"], "agent.status");
        assert_eq!(json["params"], serde_json::json!({}));
    }

    #[test]
    fn rpc_response_deserializes_success() {
        let json = r#"{"id": "1", "result": {"status": "ok"}}"#;
        let resp: RpcResponse = serde_json::from_str(json).unwrap();
        assert_eq!(resp.id, Some("1".to_string()));
        assert!(resp.result.is_some());
        assert!(resp.error.is_none());
    }

    #[test]
    fn rpc_response_deserializes_error() {
        let json = r#"{"id": "2", "error": {"code": 401, "message": "Unauthorized"}}"#;
        let resp: RpcResponse = serde_json::from_str(json).unwrap();
        assert_eq!(resp.id, Some("2".to_string()));
        assert!(resp.result.is_none());
        let err = resp.error.unwrap();
        assert_eq!(err.code, Some(401));
        assert_eq!(err.message, Some("Unauthorized".to_string()));
    }

    #[tokio::test]
    async fn call_fails_on_closed_channel() {
        // Create a client with a mock sender that simulates a closed connection
        use tokio::net::TcpListener;
        use tokio_tungstenite::accept_async;

        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();

        // Spawn a WS server that immediately closes
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let ws = accept_async(stream).await.unwrap();
            drop(ws); // Close immediately
        });

        let url = format!("http://127.0.0.1:{}", addr.port());
        let client = WsRpcClient::connect(&url, "").await.unwrap();

        // Wait for server to close
        server.await.unwrap();
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;

        // Call should fail because connection is closed
        let result: Result<Value> = client.call("test.method", serde_json::json!({})).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn call_receives_response() {
        use tokio::net::TcpListener;
        use tokio_tungstenite::accept_async;

        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();

        // Spawn a WS server that echoes back a result
        tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut ws = accept_async(stream).await.unwrap();
            if let Some(Ok(Message::Text(text))) = ws.next().await {
                let req: Value = serde_json::from_str(&text).unwrap();
                let id = req["id"].as_str().unwrap().to_string();
                let response = serde_json::json!({
                    "id": id,
                    "result": {"hello": "world"}
                });
                ws.send(Message::Text(response.to_string())).await.unwrap();
            }
        });

        let url = format!("http://127.0.0.1:{}", addr.port());
        let client = WsRpcClient::connect(&url, "").await.unwrap();

        let result: Value = client
            .call("test.echo", serde_json::json!({"key": "value"}))
            .await
            .unwrap();
        assert_eq!(result, serde_json::json!({"hello": "world"}));
    }

    #[tokio::test]
    async fn call_returns_error_from_server() {
        use tokio::net::TcpListener;
        use tokio_tungstenite::accept_async;

        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();

        // Spawn a WS server that returns an error response
        tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut ws = accept_async(stream).await.unwrap();
            if let Some(Ok(Message::Text(text))) = ws.next().await {
                let req: Value = serde_json::from_str(&text).unwrap();
                let id = req["id"].as_str().unwrap().to_string();
                let response = serde_json::json!({
                    "id": id,
                    "error": {"code": 403, "message": "Forbidden"}
                });
                ws.send(Message::Text(response.to_string())).await.unwrap();
            }
        });

        let url = format!("http://127.0.0.1:{}", addr.port());
        let client = WsRpcClient::connect(&url, "").await.unwrap();

        let result: Result<Value> = client.call("test.forbidden", serde_json::json!({})).await;
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("403"),
            "Error should contain code: {}",
            err_msg
        );
        assert!(
            err_msg.contains("Forbidden"),
            "Error should contain message: {}",
            err_msg
        );
    }
}
