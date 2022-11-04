use anyhow::{Context, Result};
use rustyline::{Editor};
use crate::agent;

pub async fn get_cap_token() -> Result<String> {
    let home_dir = dirs::home_dir().expect("Could not get home directory");
    let data_path = home_dir.join(".ad4m-cli");
    if !data_path.exists() {
        std::fs::create_dir_all(&data_path).with_context(|| format!("Could not create directory `{}`", data_path.display()))?;
    }

    let mut cap_token = String::new();

    let cap_token_file = data_path.join("cap_token");
    if cap_token_file.exists() {
        cap_token = std::fs::read_to_string(&cap_token_file).with_context(|| format!("Could not read file `{}`", cap_token_file.display()))?;
        println!("Found cap token in file.");
    } else {
        println!("No cap token found in file. Requesting one...");

        if let Ok(request_id) = agent::run_request_capability().await {
            println!("Got request id: {:#?}", request_id);
    
            let mut rl = Editor::<()>::new()?;
            if let Ok(rand) = rl.readline("Enter random string: ") {
                let result = agent::run_retrieve_capability(request_id, rand).await;
                match result {
                    Ok(jwt) => {
                        cap_token = jwt.clone();
                        std::fs::write(&cap_token_file, jwt).with_context(|| format!("Could not write file `{}`", cap_token_file.display()))?;
                        println!("Wrote cap token to file.");
                    },
                    Err(e) => {
                        eprintln!("Error generating capability token: {:#?}", e);
                    }
                }
            }
        } else {
            println!("Error requesting capability");
        }
    }

    Ok(cap_token)
}
