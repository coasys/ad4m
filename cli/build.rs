use std::fs;
use std::path::Path;
use std::thread::sleep;
use std::time::Duration;

fn main() {
    wait_for_dependencies();

    if cfg!(target_os = "macos") {
        println!("cargo:rustc-link-arg=-Wl,-rpath,@loader_path/gn_out");
    } else if cfg!(target_os = "linux") {
        println!("cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN/gn_out");
    } else if cfg!(target_os = "windows") {
    } else {
        panic!("Unsupported target OS");
    }
}

fn wait_for_dependencies() {
    let dependencies = if cfg!(target_os = "macos") {
        vec![
            "../target/release/gn_out/libc++_chrome.dylib",
            "../target/release/gn_out/libicuuc.dylib",
            "../target/release/gn_out/libthird_party_abseil-cpp_absl.dylib",
            "../target/release/gn_out/libthird_party_icu_icui18n.dylib",
            "../target/release/gn_out/libv8_libbase.dylib",
            "../target/release/gn_out/libv8_libplatform.dylib",
            "../target/release/gn_out/libv8.dylib",
        ]
    } else if cfg!(target_os = "linux") {
        vec![
            "../target/release/gn_out/libc++.so",
            "../target/release/gn_out/libicuuc.so",
            "../target/release/gn_out/libthird_party_abseil-cpp_absl.so",
            "../target/release/gn_out/libthird_party_icu_icui18n.so",
            "../target/release/gn_out/libv8_libbase.so",
            "../target/release/gn_out/libv8_libplatform.so",
            "../target/release/gn_out/libv8.so",
        ]
    } else if cfg!(target_os = "windows") {
        vec![
            // Add Windows-specific dependencies here if needed
        ]
    } else {
        panic!("Unsupported target OS");
    };

    let timeout = Duration::from_secs(3600); // 5 minutes timeout
    let check_interval = Duration::from_secs(5); // Check every 5 seconds

    let start_time = std::time::Instant::now();
    for dependency in &dependencies {
        let dependency_path = Path::new(dependency);
        while !dependency_path.exists() {
            if start_time.elapsed() > timeout {
                panic!("Timeout waiting for dependency {} to be built", dependency);
            }
            println!("Waiting for dependency {} to be built...", dependency);
            sleep(check_interval);
        }
    }

    println!("All dependencies are built and available");
}
