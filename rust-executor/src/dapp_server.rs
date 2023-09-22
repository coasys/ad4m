use std::convert::Infallible;
use std::net::SocketAddr;

use http_body_util::Full;
use hyper::body::Bytes;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response};
use hyper_util::rt::TokioIo;
use tokio::net::TcpListener;
use log::info;

use rust_embed::*;

#[derive(RustEmbed)]
#[folder = "dapp/"]
struct Asset;


async fn serve_file(req: Request<hyper::body::Incoming>) -> Result<Response<Full<Bytes>>, Infallible> {
    let path = req.uri().path();
    let path_clone = path.clone().replace("/", "");
    let mut base = path_clone.as_str();

    if base == "" {
        base = "index.html";
    }

    match Asset::get(base) {
        Some(content) => {
            let response = Response::new(Full::new(Bytes::from(content.data.into_owned())));
            Ok(response)
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