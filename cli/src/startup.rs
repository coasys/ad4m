use crate::{agent, perspectives};
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
            "Could not executor port file `{}`!\nIs AD4M executor running?",
            file_path.display()
        )
    })?;
    let port = executor_port.trim().parse::<u16>()?;
    Ok(port)
}

pub fn get_executor_url() -> Result<String> {
    let port = get_executor_port()?;
    Ok(format!("http://localhost:{}/graphql", port))
}

pub async fn get_cap_token() -> Result<String> {
    let cap_token;

    let cap_token_file = data_path()?.join("cap_token");
    if cap_token_file.exists() {
        cap_token = std::fs::read_to_string(&cap_token_file)
            .with_context(|| format!("Could not read file `{}`", cap_token_file.display()))?;
        if (perspectives::run_all(cap_token.clone()).await).is_ok() {
            return Ok(cap_token);
        }
    }

    println!("No cap token found in file or token not valid. Requesting one...");

    let request_id = agent::run_request_capability().await?;
    println!("Successfully started a new Capability Token request with id: {:#?}", request_id);
    println!("Please open the AD4M UI and approve the request. And then...");

    let mut rl = Editor::<()>::new()?;
    let rand = rl.readline("Enter the 6-digit 2FA number from AD4M UI: ")?;
    let jwt = agent::run_retrieve_capability(request_id, rand)
        .await
        .with_context(|| "Error generating capability token!".to_string())?;

    let cap_token = jwt.clone();
    std::fs::write(&cap_token_file, jwt)
        .with_context(|| format!("Could not write file `{}`", cap_token_file.display()))?;
    println!("Wrote cap token to file.");

    Ok(cap_token)
}
