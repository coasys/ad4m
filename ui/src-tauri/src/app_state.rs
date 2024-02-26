use std::fs::{File, OpenOptions};
use std::io::prelude::*;
use serde::{Serialize, Deserialize};
use tauri::api::path::home_dir;

#[derive(Serialize, Deserialize)]
pub struct LauncherState {
    pub dev_mode: bool,
}

pub fn save_state(state: &LauncherState) -> std::io::Result<()> {
    let path = home_dir().expect("Could not get home dir").join("ad4m-state.json");
    let mut file = File::create(&path)?;
    let data = serde_json::to_string(&state).unwrap();
    file.write_all(data.as_bytes())?;
    Ok(())
}

pub fn load_state() -> std::io::Result<LauncherState> {
    let path = home_dir().expect("Could not get home dir").join("ad4m-state.json");
    let mut file = OpenOptions::new().read(true).write(true).create(true).open(&path)?;
    let mut data = String::new();
    file.read_to_string(&mut data)?;
    let state: LauncherState = if data.is_empty() {
        LauncherState {
            dev_mode: false,
        }
    } else {
        serde_json::from_str(&data).unwrap()
    };

    Ok(state)
}