use std::path::Path;
use std::process::Command;

const ALLIANCE_DNA_VERSION: &str = "0.64.0";
const ALLIANCE_DNA_URL: &str =
    "https://github.com/unytco/unyt-sandbox/releases/download/v0.64.0/alliance.dna";

fn main() {
    // Try to detect if CUDA is installed by checking if `nvcc` is available
    let cuda_available = Command::new("nvcc").arg("--version").output().is_ok();

    // If CUDA is available, enable the `cuda` feature
    if cuda_available {
        println!("cargo:rustc-cfg=feature=\"cuda\"");
    }

    // If building on macOS, enable `metal` feature by default
    if cfg!(target_os = "macos") {
        println!("cargo:rustc-cfg=feature=\"metal\"");
    }

    download_alliance_dna();
}

fn download_alliance_dna() {
    // Only re-run this build script when build.rs itself changes
    // (i.e. when the DNA version/URL is updated).
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = std::env::var("OUT_DIR").unwrap();
    let dest = Path::new(&out_dir).join("alliance.dna");

    // Skip download if the file already exists (cached across incremental builds).
    if dest.exists() {
        let metadata = std::fs::metadata(&dest).unwrap();
        if metadata.len() > 0 {
            return;
        }
    }

    println!(
        "cargo:warning=Downloading alliance DNA v{} from GitHub...",
        ALLIANCE_DNA_VERSION
    );

    let status = Command::new("curl")
        .args([
            "-L",
            "--fail",
            "--silent",
            "--show-error",
            "-o",
            dest.to_str().unwrap(),
            ALLIANCE_DNA_URL,
        ])
        .status()
        .expect("failed to run curl – is it installed?");

    assert!(
        status.success(),
        "Failed to download alliance DNA from {}",
        ALLIANCE_DNA_URL
    );

    let size = std::fs::metadata(&dest).unwrap().len();
    println!(
        "cargo:warning=Downloaded alliance DNA v{} ({} bytes)",
        ALLIANCE_DNA_VERSION, size
    );
}
