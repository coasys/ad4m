use std::fs::{File, OpenOptions};
use std::io::prelude::*;
use std::path::PathBuf;
use serde::{Serialize, Deserialize};
use tauri::api::path::home_dir;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct AgentList {
    pub name: String,
    pub path: PathBuf,
    pub bootstrap: Option<PathBuf>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct LauncherState {
    pub agent_list: Vec<AgentList>,
    pub selected_agent: Option<AgentList>,
}

impl LauncherState {
    pub fn save(&mut self) -> std::io::Result<()> {
        let path = home_dir().expect("Could not get home dir").join("ad4m-state.json");
        let mut file = File::create(&path)?;
        let data = serde_json::to_string(&self).unwrap();
        file.write_all(data.as_bytes())?;
        Ok(())
    }

    pub fn load() -> std::io::Result<LauncherState> {
        let path = home_dir().expect("Could not get home dir").join("ad4m-state.json");
        let mut file = OpenOptions::new().read(true).write(true).create(true).open(&path)?;
        let mut data = String::new();
        file.read_to_string(&mut data)?;
        let state: LauncherState = if data.is_empty() {
            let agent = AgentList {
                name: "Main Net".to_string(),
                path: PathBuf::from(home_dir().expect("Could not get home dir")).join(".ad4m".to_string()),
                bootstrap: None
            };
    
            LauncherState {
                agent_list: vec![{
                    agent.clone()
                }],
                selected_agent: Some(agent)
            }
        } else {
            serde_json::from_str(&data).unwrap()
        };
    
        Ok(state)
    }

    pub fn add_agent(&mut self, agent: AgentList) {
        if !self.is_agent_taken(&agent.name, &agent.path) {
            self.agent_list.push(agent);
        }
    }

    pub fn remove_agent(&mut self, agent: AgentList) {
        self.agent_list.retain(|a| a.name != agent.name && &a.path != &agent.path);
    }

    pub fn is_agent_taken(&self, new_name: &str, new_path: &PathBuf) -> bool {
        self.agent_list.iter().any(|agent| agent.name == new_name && (&agent.path == new_path))
    }
}



