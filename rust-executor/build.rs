use std::process::Command;

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
}
