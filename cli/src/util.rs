use crate::startup::get_executor_url;
use anyhow::{anyhow, Result};
use async_tungstenite::tungstenite::{client::IntoClientRequest, http::HeaderValue, Message};
use futures::StreamExt;
use graphql_client::{QueryBody, Response};
use graphql_ws_client::{graphql::GraphQLClient, AsyncWebsocketClient, GraphQLClientClientBuilder};
use serde::{de::DeserializeOwned, Serialize};

pub const DATETIME_FORMAT: &str = "%Y-%m-%dT%H:%M:%S%.fZ";

pub async fn query<Q, R>(cap_token: String, query: QueryBody<Q>) -> Result<R>
where
    Q: Serialize,
    R: DeserializeOwned,
{
    let response_body: Response<R> = reqwest::Client::new()
        .post(get_executor_url()?)
        .header("Authorization", cap_token)
        .json(&query)
        .send()
        .await?
        .json()
        .await?;
    let response_data = response_body
        .data
        .ok_or_else(|| anyhow!("No data in response! Errors: {:?}", response_body.errors))?;
    Ok(response_data)
}

struct TokioSpawner(tokio::runtime::Handle);

impl TokioSpawner {
    pub fn new(handle: tokio::runtime::Handle) -> Self {
        TokioSpawner(handle)
    }

    pub fn current() -> Self {
        TokioSpawner::new(tokio::runtime::Handle::current())
    }
}

impl futures::task::Spawn for TokioSpawner {
    fn spawn_obj(
        &self,
        obj: futures::task::FutureObj<'static, ()>,
    ) -> Result<(), futures::task::SpawnError> {
        self.0.spawn(obj);
        Ok(())
    }
}

pub async fn create_websocket_client(
    cap_token: String,
) -> Result<AsyncWebsocketClient<GraphQLClient, Message>> {
    let url = get_executor_url()?.replace("http", "ws");
    let mut request = url.into_client_request().unwrap();
    request.headers_mut().insert(
        "Sec-WebSocket-Protocol",
        HeaderValue::from_str("graphql-transport-ws").unwrap(),
    );
    request
        .headers_mut()
        .insert("Authorization", HeaderValue::from_str(&cap_token).unwrap());
    let (connection, _) = async_tungstenite::tokio::connect_async(request).await?;

    let (sink, stream) = connection.split();
    Ok(GraphQLClientClientBuilder::new()
        .build(stream, sink, TokioSpawner::current())
        .await
        .unwrap())
}

pub fn maybe_parse_datetime(
    maybe_date: Option<String>,
) -> Result<Option<chrono::naive::NaiveDateTime>> {
    Ok(
        match maybe_date
            .clone()
            .map(|s| chrono::naive::NaiveDateTime::parse_from_str(&s, DATETIME_FORMAT))
        {
            Some(Err(e)) => {
                return Err(anyhow!(e).context(format!(
                    "Couldn't parse datetime '{}' with expected format: {}",
                    maybe_date.unwrap(),
                    DATETIME_FORMAT
                )));
            }
            Some(Ok(x)) => Some(x),
            None => None,
        },
    )
}

pub fn readline_masked(prompt: &str) -> Result<String> {
    use rustyline::completion::Completer;
    use rustyline::config::Configurer;
    use rustyline::highlight::Highlighter;
    use rustyline::hint::Hinter;
    use rustyline::validate::Validator;
    use rustyline::{ColorMode, Editor, Helper};
    use std::borrow::Cow::{self, Borrowed, Owned};

    struct MaskingHighlighter {
        masking: bool,
    }

    impl Highlighter for MaskingHighlighter {
        fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
            use unicode_width::UnicodeWidthStr;
            if self.masking {
                Owned("*".repeat(line.width()))
            } else {
                Borrowed(line)
            }
        }

        fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
            self.masking
        }
    }

    impl Completer for MaskingHighlighter {
        type Candidate = String;
    }
    impl Hinter for MaskingHighlighter {
        type Hint = String;
    }
    impl Validator for MaskingHighlighter {}
    impl Helper for MaskingHighlighter {}

    let mut rl = Editor::new()?;
    let h = MaskingHighlighter { masking: false };
    rl.set_helper(Some(h));
    rl.helper_mut().expect("No helper").masking = true;
    rl.set_color_mode(ColorMode::Forced); // force masking
    rl.set_auto_add_history(false); // make sure password is not added to history
    let passwd = rl.readline(prompt)?;
    Ok(passwd)
}
