use std::env;
use std::path::Path;
use std::thread::sleep;
use std::time::Duration;

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap();
    wait_for_dependencies(&target_arch);
    println!(
        "cargo:rustc-link-search=native={}/gn_out/{}",
        out_dir, target_arch
    );
    println!(
        "cargo:rustc-link-search=native=./target/release/gn_out/{}",
        target_arch
    );
    println!(
        "cargo:rustc-link-search=native=./target/debug/gn_out/{}",
        target_arch
    );

    if cfg!(target_os = "macos") {
        println!(
            "cargo:rustc-link-arg=-Wl,-rpath,@loader_path/gn_out/{}",
            &target_arch
        );
    } else if cfg!(target_os = "linux") {
        println!(
            "cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN/gn_out/{}",
            &target_arch
        );
    } else if cfg!(target_os = "windows") {
    } else {
        panic!("Unsupported target OS");
    }
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

        if source_path.exists() {
            match std::fs::copy(source_path, dest_path) {
                Ok(_) => println!("Successfully copied libc++.so to libc++_chrome.so"),
                Err(e) => eprintln!("Failed to copy libc++.so to libc++_chrome.so: {}", e),
            }
        } else {
            eprintln!("Source file libc++.so not found");
        }
    } */

    println!("All dependencies are built and available");
}
