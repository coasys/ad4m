use log::LevelFilter;
use log4rs::{
    append::file::FileAppender,
    config::{Appender, Root},
    encode::pattern::PatternEncoder,
    Config,
};
use std::fs;

use crate::config::{log_path};

pub fn setup_logs() -> Result<(), String> {
    let _ = fs::remove_file(log_path());

    let logfile = FileAppender::builder()
      .encoder(Box::new(PatternEncoder::new("[{d}] {l} - {m}\n")))
      .build(log_path())
      .map_err(|err| format!("Could not build log config: {:?}", err))?;
  
    let config = Config::builder()
      .appender(Appender::builder().build("logfile", Box::new(logfile)))
      .build(Root::builder().appender("logfile").build(LevelFilter::Info))
      .map_err(|err| format!("Could not init log config: {:?}", err))?;
  
    log4rs::init_config(config).map_err(|err| format!("Could not init log config: {:?}", err))?;
  
    Ok(())
}