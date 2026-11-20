//! `rt::fs` on wasm32 keeps files in memory: what is written reads back, by path.
#![cfg(target_arch = "wasm32")]

use rust_executor::rt::fs;
use std::io;
use wasm_bindgen_test::wasm_bindgen_test;

#[wasm_bindgen_test]
fn a_written_file_reads_back_and_exists() {
    let path = "/rt-fs-test/agent.json";
    assert!(!fs::exists(path).unwrap());
    assert_eq!(
        fs::read_to_string(path).unwrap_err().kind(),
        io::ErrorKind::NotFound
    );
    fs::create_dir_all("/rt-fs-test").unwrap();
    fs::write(path, "{}").unwrap();
    assert!(fs::exists(path).unwrap());
    assert_eq!(fs::read_to_string(path).unwrap(), "{}");
    fs::write(path, "[1]").unwrap();
    assert_eq!(fs::read_to_string(path).unwrap(), "[1]");
}
