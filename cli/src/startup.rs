use anyhow::{Context, Result};
use std::path::PathBuf;

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
    Ok(format!("http://127.0.0.1:{}", port))
}
