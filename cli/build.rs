use std::process::Command;

fn main() {
    // Capture git commit information
    let git_hash = Command::new("git")
        .args(&["rev-parse", "--short", "HEAD"])
        .output()
        .ok()
        .and_then(|output| String::from_utf8(output.stdout).ok())
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    let git_dirty = Command::new("git")
        .args(&["diff", "--quiet"])
        .status()
        .map(|status| !status.success())
        .unwrap_or(false);

    // Set environment variables for the main code
    println!("cargo:rustc-env=GIT_COMMIT_HASH={}", git_hash);
    println!(
        "cargo:rustc-env=GIT_DIRTY={}",
        if git_dirty { "dirty" } else { "clean" }
    );

    // Re-run if git information changes
    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-changed=.git/index");
}
