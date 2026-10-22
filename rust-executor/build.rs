use std::path::Path;
use std::process::Command;

const ALLIANCE_DNA_VERSION: &str = "0.61.0";
const ALLIANCE_DNA_URL: &str =
    "https://github.com/unytco/unyt-sandbox/releases/download/v0.61.0/alliance.dna";

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

    println!(
        "cargo:rustc-env=ALLIANCE_DNA_VERSION={}",
        ALLIANCE_DNA_VERSION
    );

    embed_git_commit();
    download_alliance_dna();
}

/// The commit this executor is built from, as `AD4M_GIT_COMMIT` (empty when unknown), so
/// `runtime.info` can report it next to the version. The version alone says which release a build
/// descends from; every build between two releases reports the same one.
///
/// An explicit `AD4M_GIT_COMMIT` wins, then CI's own variables, then `git rev-parse HEAD`. A build
/// from a source tarball has no `.git` and reports no commit rather than failing.
fn embed_git_commit() {
    println!("cargo:rerun-if-env-changed=AD4M_GIT_COMMIT");
    println!("cargo:rerun-if-env-changed=GITHUB_SHA");
    println!("cargo:rerun-if-env-changed=CIRCLE_SHA1");

    let from_env = ["AD4M_GIT_COMMIT", "GITHUB_SHA", "CIRCLE_SHA1"]
        .iter()
        .find_map(|name| std::env::var(name).ok().filter(|v| !v.is_empty()));

    let commit = from_env.or_else(|| {
        let git = |args: &[&str]| {
            Command::new("git")
                .args(args)
                .output()
                .ok()
                .filter(|o| o.status.success())
                .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                .filter(|s| !s.is_empty())
        };
        let commit = git(&["rev-parse", "HEAD"])?;
        // Re-run when HEAD moves: a checkout or a commit changes HEAD itself or the ref it points
        // at, and a ref that has been packed lives in packed-refs. `--git-path` finds each file in a
        // worktree as well as in a plain checkout. `rerun-if-changed=build.rs` above narrows the
        // triggers to the files named, so without these a rebuild would keep the old commit.
        for path in ["HEAD", "packed-refs"] {
            if let Some(file) = git(&["rev-parse", "--git-path", path]) {
                println!("cargo:rerun-if-changed={}", file);
            }
        }
        if let Some(reference) = git(&["symbolic-ref", "-q", "HEAD"]) {
            if let Some(file) = git(&["rev-parse", "--git-path", &reference]) {
                println!("cargo:rerun-if-changed={}", file);
            }
        }
        Some(commit)
    });

    println!(
        "cargo:rustc-env=AD4M_GIT_COMMIT={}",
        commit.unwrap_or_default()
    );
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
