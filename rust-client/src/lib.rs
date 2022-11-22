extern crate anyhow;
extern crate async_tungstenite;
extern crate chrono;
extern crate clap;
extern crate dirs;
extern crate graphql_client;
extern crate rand;
extern crate regex;
extern crate reqwest;
extern crate rustyline;
extern crate tokio;

pub mod agent;
pub mod languages;
pub mod neighbourhoods;
pub mod perspectives;
pub mod runtime;
pub mod types;
mod util;


static mut EXECUTOR_URL: String = String::new();

pub fn get_executor_url() -> String {
    unsafe { EXECUTOR_URL.clone() }
}

pub fn set_executor_url(url: String) {
    unsafe {
        EXECUTOR_URL = url;
    }
}
