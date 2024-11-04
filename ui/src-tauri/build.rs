use serde_json::Value;
use std::fs;
use std::path::Path;
use std::thread::sleep;
use std::time::Duration;

fn main() {
    let config_path = Path::new("./tauri.conf.json");
    let config_str = fs::read_to_string(config_path).expect("Unable to read tauri.conf.json");
    let mut config: Value =
        serde_json::from_str(&config_str).expect("Unable to parse tauri.conf.json");

    if let Some(resources) = config["tauri"]["bundle"]["resources"].as_array_mut() {
        resources.clear();
        if cfg!(target_os = "macos") {
            resources.push(Value::String(
                "../../target/release/gn_out/libc++_chrome.dylib".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/libicuuc.dylib".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/libthird_party_abseil-cpp_absl.dylib".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/libthird_party_icu_icui18n.dylib".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/libv8_libbase.dylib".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/libv8_libplatform.dylib".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/libv8.dylib".to_string(),
            ));
        } else if cfg!(target_os = "linux") {
            resources.push(Value::String(
                "../../target/release/gn_out/x86_64/libc++.so".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/x86_64/libc++_chrome.so".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/x86_64/libicuuc.so".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/x86_64/libthird_party_abseil-cpp_absl.so".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/x86_64/libthird_party_icu_icui18n.so".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/x86_64/libv8_libbase.so".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/x86_64/libv8_libplatform.so".to_string(),
            ));
            resources.push(Value::String(
                "../../target/release/gn_out/x86_64/libv8.so".to_string(),
            ));
        } else if cfg!(target_os = "windows") {
        } else {
            panic!("Unsupported target OS");
        }
    }

    fs::write(
        config_path,
        serde_json::to_string_pretty(&config).expect("Unable to serialize tauri.conf.json"),
    )
    .expect("Unable to write tauri.conf.json");

    println!("Updated tauri.conf.json");

    wait_for_dependencies();

    if cfg!(target_os = "macos") {
        println!("cargo:rustc-link-arg=-Wl,-rpath,@loader_path/../Resources/_up_/_up_/target/release/gn_out");
    } else if cfg!(target_os = "linux") {
        println!("cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN/../lib/adam-launcher/_up_/_up_/target/release/gn_out/x86_64");
    } else if cfg!(target_os = "windows") {
    } else {
        panic!("Unsupported target OS");
    }

    tauri_build::build();
}

fn wait_for_dependencies() {
    let dependencies = if cfg!(target_os = "macos") {
        vec![
            "../../target/release/gn_out/libc++_chrome.dylib",
            "../../target/release/gn_out/libicuuc.dylib",
            "../../target/release/gn_out/libthird_party_abseil-cpp_absl.dylib",
            "../../target/release/gn_out/libthird_party_icu_icui18n.dylib",
            "../../target/release/gn_out/libv8_libbase.dylib",
            "../../target/release/gn_out/libv8_libplatform.dylib",
            "../../target/release/gn_out/libv8.dylib",
        ]
    } else if cfg!(target_os = "linux") {
        vec![
            "../../target/release/gn_out/x86_64/libc++.so",
            "../../target/release/gn_out/x86_64/libicuuc.so",
            "../../target/release/gn_out/x86_64/libthird_party_abseil-cpp_absl.so",
            "../../target/release/gn_out/x86_64/libthird_party_icu_icui18n.so",
            "../../target/release/gn_out/x86_64/libv8_libbase.so",
            "../../target/release/gn_out/x86_64/libv8_libplatform.so",
            "../../target/release/gn_out/x86_64/libv8.so",
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

    if cfg!(target_os = "linux") {
        let source_path = Path::new("../target/release/gn_out/x86_64/libc++.so");
        let dest_path = Path::new("../target/release/gn_out/x86_64/libc++_chrome.so");

        if source_path.exists() {
            match std::fs::copy(source_path, dest_path) {
                Ok(_) => println!("Successfully copied libc++.so to libc++_chrome.so"),
                Err(e) => eprintln!("Failed to copy libc++.so to libc++_chrome.so: {}", e),
            }
        } else {
            eprintln!("Source file libc++.so not found");
        }
    }

    println!("All dependencies are built and available");
}
