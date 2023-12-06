use std::convert::Infallible;
use std::net::SocketAddr;

use http_body_util::Full;
use hyper::body::Bytes;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response};
use hyper::header::HeaderValue;
use hyper_util::rt::TokioIo;
use tokio::net::TcpListener;
use log::info;

use rust_embed::*;

#[derive(RustEmbed)]
#[folder = "dapp/"]
struct Asset;


async fn serve_file(req: Request<hyper::body::Incoming>) -> Result<Response<Full<Bytes>>, Infallible> {
    let mut path = req.uri().path()[1..].to_string();

    if path == "" {
        path = String::from("index.html");
    }

    match Asset::get(&path) {
        Some(content) => {
            if path != String::from("index.html") && path != String::from("favicon.ico") && !path.contains(".css") {
                let mut response = Response::new(Full::new(Bytes::from(content.data.into_owned())));
                response.headers_mut().insert("Content-Type", HeaderValue::from_static("application/javascript"));
                Ok(response)
            } else if path.contains(".css") {
                let mut response = Response::new(Full::new(Bytes::from(content.data.into_owned())));
                response.headers_mut().insert("Content-Type", HeaderValue::from_static("text/css"));
                Ok(response)
            } else {
                let response = Response::new(Full::new(Bytes::from(content.data.into_owned())));
                Ok(response)
            }
        },
        None => {
            let response = Response::new(Full::new(Bytes::from("File not found")));
            Ok(response)
        },
    }
}

pub(crate) async fn serve_dapp(port: u16) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let addr = SocketAddr::from(([127, 0, 0, 1], port));

    let listener = TcpListener::bind(addr).await?;

    info!("Listening dapp on http://{}", addr);

    loop {
        let (stream, _) = listener.accept().await?;

        let io = TokioIo::new(stream);

        tokio::task::spawn(async move {
            if let Err(err) = http1::Builder::new()
                .serve_connection(io, service_fn(serve_file))
                .await
            {
                println!("Error serving connection: {:?}", err);
            }
        });
    }
}