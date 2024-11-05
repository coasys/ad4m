use serde_json::Value;
use std::env;
use std::fs;
use std::path::Path;
use std::thread::sleep;
use std::time::Duration;

fn main() {
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap();
    let config_path = Path::new("./tauri.conf.json");
    let config_str = fs::read_to_string(config_path).expect("Unable to read tauri.conf.json");
    let mut config: Value =
        serde_json::from_str(&config_str).expect("Unable to parse tauri.conf.json");

        if let Some(resources) = config.get_mut("tauri").and_then(|t| t.get_mut("bundle")).and_then(|b| b.get_mut("resources")).and_then(|r| r.as_array_mut()) {
        resources.clear();
        if cfg!(target_os = "macos") {
            resources.push(Value::String(in_target_dir(
                "libc++_chrome.dylib",
                &target_arch,
            )));
            resources.push(Value::String(in_target_dir("libicuuc.dylib", &target_arch)));
            resources.push(Value::String(in_target_dir(
                "libthird_party_abseil-cpp_absl.dylib",
                &target_arch,
            )));
            resources.push(Value::String(in_target_dir(
                "libthird_party_icu_icui18n.dylib",
                &target_arch,
            )));
            resources.push(Value::String(in_target_dir(
                "libv8_libbase.dylib",
                &target_arch,
            )));
            resources.push(Value::String(in_target_dir(
                "libv8_libplatform.dylib",
                &target_arch,
            )));
            resources.push(Value::String(in_target_dir("libv8.dylib", &target_arch)));
        } else if cfg!(target_os = "linux") {
            /*
            resources.push(Value::String(in_target_dir("libc++.so", &target_arch)));
            resources.push(Value::String(in_target_dir(
                "libc++_chrome.so",
                &target_arch,
            )));
            resources.push(Value::String(in_target_dir("libicuuc.so", &target_arch)));
                "libthird_party_abseil-cpp_absl.so",
            resources.push(Value::String(in_target_dir(
                &target_arch,
            )));
                "libthird_party_icu_icui18n.so",
            resources.push(Value::String(in_target_dir(
                &target_arch,
            resources.push(Value::String(in_target_dir(
            )));
                "libv8_libbase.so",
                &target_arch,
            )));
            resources.push(Value::String(in_target_dir(
                &target_arch,
                "libv8_libplatform.so",
            )));
        } else if cfg!(target_os = "windows") {
            resources.push(Value::String(in_target_dir("libv8.so", &target_arch)));
        } else {
             */
            panic!("Unsupported target OS");
        }
    }

    fs::write(
        config_path,
        serde_json::to_string_pretty(&config).expect("Unable to serialize tauri.conf.json"),
    )
    .expect("Unable to write tauri.conf.json");

    println!("Updated tauri.conf.json");

    wait_for_dependencies(&target_arch);

    if cfg!(target_os = "macos") {
        println!("cargo:rustc-link-arg=-Wl,-rpath,@loader_path/../Resources/_up_/_up_/target/release/gn_out");
    } else if cfg!(target_os = "linux") {
        //println!("cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN/../lib/adam-launcher/_up_/_up_/target/release/gn_out/x86_64");
    } else if cfg!(target_os = "windows") {
    } else {
        panic!("Unsupported target OS");
    }

    tauri_build::build();
}

fn in_target_dir(target: &str, target_arch: &String) -> String {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let out_dir = Path::new(&out_dir)
        .parent() // .../build/ad4m-launcher-163f64../out
        .and_then(|p| p.parent()) // .../build/ad4m-launcher-163f64..
        .and_then(|p| p.parent()) // .../target/release
        .unwrap();

    format!("{}/gn_out/{}/{}", out_dir.display(), target_arch, target)
}

fn wait_for_dependencies(target_arch: &String) {
    let dependencies = if cfg!(target_os = "macos") {
        vec![
            "libc++_chrome.dylib",
            "libicuuc.dylib",
            "libthird_party_abseil-cpp_absl.dylib",
            "libthird_party_icu_icui18n.dylib",
            "libv8_libbase.dylib",
            "libv8_libplatform.dylib",
            "libv8.dylib",
        ]
    } else if cfg!(target_os = "linux") {
        vec![
            //"libc++.so",
            //"libicuuc.so",
            //"libthird_party_abseil-cpp_absl.so",
            //"libthird_party_icu_icui18n.so",
            //"libv8_libbase.so",
            //"libv8_libplatform.so",
            //"libv8.so",
        ]
    } else if cfg!(target_os = "windows") {
        vec![
            // Add Windows-specific dependencies here if needed
        ]
    } else {
        panic!("Unsupported target OS");
    };

    let dependencies: Vec<String> = dependencies
        .into_iter()
        .map(|d| in_target_dir(d, target_arch))
        .collect();

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
/*
    if cfg!(target_os = "linux") {
        let source = in_target_dir("libc++.so", target_arch);
        let source_path = Path::new(&source);
        let dest = in_target_dir("libc++_chrome.so", target_arch);
        let dest_path = Path::new(&dest);

        // Print the absolute path of libc++.so for debugging
        match source_path.canonicalize() {
            Ok(absolute_path) => {
                println!("Absolute path of libc++.so: {}", absolute_path.display())
            }
            Err(e) => println!("Failed to get absolute path for libc++.so: {}", e),
        };

        if source_path.exists() {
            println!("Found source file: {}", source_path.display());
            match std::fs::copy(&source_path, &dest_path) {
                Ok(_) => println!("Successfully copied libc++.so to libc++_chrome.so"),
                Err(e) => panic!("Failed to copy libc++.so to libc++_chrome.so: {}", e),
            }
        } else {
            panic!(
                "Source file libc++.so not found at {}",
                source_path.display()
            );
        }
    }
 */
    println!("All dependencies are built and available");
}
