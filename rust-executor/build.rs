use sha2::{Digest, Sha256};
use std::path::Path;
use std::process::Command;

const ALLIANCE_DNA_VERSION: &str = "0.61.0";
const ALLIANCE_DNA_URL: &str =
    "https://github.com/unytco/unyt-sandbox/releases/download/v0.61.0/alliance.dna";

/// WE's agent tools (`@we/agent-tools`), as a WE release publishes them: the bundle and its
/// manifest, which `src/tool_bundles/builtin.rs` compiles in. To take a new release, set the
/// version, and the two hashes to the ones in that release's `SHA256SUMS`
/// (https://github.com/coasys/we/releases/tag/agent-tools-v<version>).
const WE_AGENT_TOOLS_VERSION: &str = "0.2.0";
const WE_AGENT_TOOLS_FILES: [(&str, &str); 2] = [
    (
        "agent-tools.bundle.js",
        "9e08d496a84e98314c022c943b4dfabbbd6f3987e388153fcd9db486eb9fb272",
    ),
    (
        "agent-tools.json",
        "bdd7505f85c73912dabbc04a69b9b746849e7e29f261706a250ecc3e5d69cf26",
    ),
];

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
    download_we_agent_tools();
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

/// Downloads WE's agent tools release into `OUT_DIR`, and fails the build unless every file has
/// the sha256 pinned in `WE_AGENT_TOOLS_FILES`. A file already there with that hash is kept, so
/// once both are, they need no network again.
///
/// The script re-runs whenever either file changes. `include_str!` makes them inputs of the
/// executor crate only, so without that a cached file changed after its check would be compiled
/// in unchecked; and one that was deleted would fail the build until the script ran again.
fn download_we_agent_tools() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    for (name, pinned) in WE_AGENT_TOOLS_FILES {
        let dest = Path::new(&out_dir).join(name);
        println!("cargo:rerun-if-changed={}", dest.display());
        if sha256_of(&dest).as_deref() == Some(pinned) {
            continue;
        }

        let url = format!(
            "https://github.com/coasys/we/releases/download/agent-tools-v{}/{}",
            WE_AGENT_TOOLS_VERSION, name
        );
        println!(
            "cargo:warning=Downloading WE agent tools v{} ({}) from GitHub...",
            WE_AGENT_TOOLS_VERSION, name
        );
        // Into a file of its own first, so a download that fails or does not match is never
        // taken for the cached file.
        let partial = Path::new(&out_dir).join(format!("{}.part", name));
        let status = Command::new("curl")
            .args([
                "-L",
                "--fail",
                "--silent",
                "--show-error",
                "-o",
                partial.to_str().unwrap(),
                &url,
            ])
            .status()
            .expect("failed to run curl – is it installed?");
        assert!(status.success(), "Failed to download {} from {}", name, url);

        let got = sha256_of(&partial).unwrap_or_default();
        if got != pinned {
            let _ = std::fs::remove_file(&partial);
            panic!(
                "{} from {} has sha256 {}, but build.rs pins {} for WE agent tools v{}. \
                 Set WE_AGENT_TOOLS_FILES to the hashes in that release's SHA256SUMS.",
                name, url, got, pinned, WE_AGENT_TOOLS_VERSION
            );
        }
        // Cargo takes a file newer than the start of this run for a change, so one written now
        // would re-run the script, and recompile the crate, on the next build as well. Date it
        // back to this script's binary, which was built before the run started.
        let built = std::env::current_exe()
            .and_then(std::fs::metadata)
            .and_then(|m| m.modified())
            .unwrap();
        std::fs::File::options()
            .write(true)
            .open(&partial)
            .and_then(|f| f.set_modified(built))
            .unwrap();
        std::fs::rename(&partial, &dest).unwrap();
    }
}

/// The sha256 of a file as lowercase hex, or None when it cannot be read.
fn sha256_of(path: &Path) -> Option<String> {
    let bytes = std::fs::read(path).ok()?;
    Some(format!("{:x}", Sha256::digest(bytes)))
}
