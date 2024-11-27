use ad4m_client::{
    agent, perspectives,
    types::{Capability, Resource},
};
use anyhow::{Context, Result};
use rustyline::Editor;
use std::path::PathBuf;

pub fn data_path() -> Result<PathBuf> {
    let home_dir = dirs::home_dir().expect("Could not get home directory");
    let data_path = home_dir.join(".ad4m-cli");
    if !data_path.exists() {
        std::fs::create_dir_all(&data_path)
            .with_context(|| format!("Could not create directory `{}`", data_path.display()))?;
    }
    Ok(data_path)
}

pub fn executor_data_path() -> PathBuf {
    let home_dir = dirs::home_dir().expect("Could not get home directory");

    home_dir.join(".ad4m")
}

pub fn get_executor_port() -> Result<u16> {
    let executor_data_path = executor_data_path();
    let file_path = executor_data_path.join("executor-port");
    let executor_port = std::fs::read_to_string(file_path.clone()).with_context(|| {
        format!(
            "Could not get executor port file `{}`!\nIs AD4M executor running?",
            file_path.display()
        )
    });
    match executor_port {
        Ok(port) => Ok(port.parse::<u16>().unwrap()),
        Err(err) => {
            println!("{}", err);
            println!("Attempting to connect on default port 12000...\n");
            Ok(12000)
        }
    }
}

pub fn get_executor_url() -> Result<String> {
    let port = get_executor_port()?;
    Ok(format!("http://127.0.0.1:{}/graphql", port))
}

pub async fn get_cap_token(executor_url: String) -> Result<String> {
    let cap_token;

    let cap_token_file = data_path()?.join("cap_token");
    if cap_token_file.exists() {
        cap_token = std::fs::read_to_string(&cap_token_file)
            .with_context(|| format!("Could not read file `{}`", cap_token_file.display()))?;
        if (perspectives::all(executor_url.clone(), cap_token.clone()).await).is_ok() {
            return Ok(cap_token);
        }
    }

    println!("No cap token found in file or token not valid. Requesting one...");

    let app_name = "AD4M cli".to_string();
    let app_desc = "Command line administration tool for AD4M".to_string();
    let app_domain = "org.perspect3vism.ad4m.cli".to_string();
    let capabilities = Capability {
        with: Resource {
            domain: "*".to_string(),
            pointers: vec!["*".to_string()],
        },
        can: vec!["*".to_string()],
    };
    let request_id = agent::request_capability(
        executor_url.clone(),
        app_name,
        app_desc,
        app_domain,
        None,
        None,
        Some(vec![capabilities]),
    )
    .await?;
    println!(
        "Successfully started a new Capability Token request with id: {:#?}",
        request_id
    );
    println!("Please open the AD4M UI and approve the request. And then...");

    let mut rl = Editor::<()>::new()?;
    let rand = rl.readline("Enter the 6-digit 2FA number from AD4M UI: ")?;
    let jwt = agent::retrieve_capability(executor_url, request_id, rand)
        .await
        .with_context(|| "Error generating capability token!".to_string())?;

    let cap_token = jwt.clone();
    std::fs::write(&cap_token_file, jwt)
        .with_context(|| format!("Could not write file `{}`", cap_token_file.display()))?;
    println!("Wrote cap token to file.");

    Ok(cap_token)
}
