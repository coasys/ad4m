use dirs::home_dir;
use portpicker;
use std::path::PathBuf;

pub(crate) fn ad4m_data_directory() -> PathBuf {
    home_dir().unwrap().join(".ad4m")
}

pub fn find_port(start_port: u16, end_port: u16) -> u16 {
    for x in start_port..end_port {
        if portpicker::is_free(x) {
            return x;
        }
    }

    panic!(
        "No open port found between: [{:?}, {:?}]",
        start_port, end_port
    );
}
