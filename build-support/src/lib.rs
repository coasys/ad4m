//! Shared helpers for the `build.rs` of the ad4m workspace crates.
//!
//! The one job here is [`emit_git_commit`]: it records the commit a build was
//! made from so a running executor, CLI or launcher can say which one it is.
//! The version (`AD4M_VERSION`, `CARGO_PKG_VERSION`) is hand-set and the same
//! for every build of a release, so it cannot tell a staging build, a PR build
//! and a local build apart. The commit can.
//!
//! The commit goes next to the version, never into it: `init.rs` compares the
//! version against `OLDEST_VERSION`, and npm and the release tags use it.
//!
//! Contract: the build never fails because of this module. Without git (a
//! crate built from a source tarball) the commit is [`UNKNOWN`].

use std::path::{Path, PathBuf};
use std::process::Command;

/// The commit reported when the build directory has no git history.
pub const UNKNOWN: &str = "unknown";

/// Suffix appended to the commit when tracked files have uncommitted changes.
pub const DIRTY_SUFFIX: &str = "-dirty";

/// Hex digits kept in the short form (`AD4M_GIT_COMMIT_SHORT`, `--version`).
const SHORT_LEN: usize = 8;

/// Emits `AD4M_GIT_COMMIT` and `AD4M_GIT_COMMIT_SHORT` for the crate whose
/// `build.rs` calls this, plus the `rerun-if-*` lines that keep them current.
///
/// `AD4M_GIT_COMMIT` is the full commit from `GITHUB_SHA` when CI sets it,
/// otherwise from `git rev-parse HEAD`; `-dirty` is appended when the tree has
/// uncommitted changes to tracked files (the same rule as `git describe
/// --dirty`). `AD4M_GIT_COMMIT_SHORT` is the first eight hex digits with the
/// same suffix. Both are `unknown` without git.
///
/// Rerun triggers: `GITHUB_SHA`, the per-worktree `HEAD`, the branch ref it
/// points to (loose or packed), the index, and the crate's `src` directory, so
/// that an edit, `git add`, commit or checkout updates the value. An edit in
/// another crate of the workspace does not rerun this crate's script; the
/// dirty flag then reflects the tree as of the last run.
pub fn emit_git_commit() {
    let dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into()));
    let github_sha = std::env::var("GITHUB_SHA").ok();
    let commit = git_commit(&dir, github_sha.as_deref());
    println!("cargo:rustc-env=AD4M_GIT_COMMIT={commit}");
    println!("cargo:rustc-env=AD4M_GIT_COMMIT_SHORT={}", short(&commit));
    println!("cargo:rerun-if-env-changed=GITHUB_SHA");
    for path in rerun_paths(&dir) {
        println!("cargo:rerun-if-changed={}", path.display());
    }
}

/// The full commit for the repository containing `dir`, with `-dirty` when
/// tracked files are modified, or [`UNKNOWN`] when there is no git history.
///
/// `github_sha` wins over `git rev-parse HEAD` when it is set and non-empty:
/// on a `pull_request` event GitHub builds the merge commit it names.
pub fn git_commit(dir: &Path, github_sha: Option<&str>) -> String {
    let head = match github_sha.map(str::trim).filter(|sha| !sha.is_empty()) {
        Some(sha) => sha.to_string(),
        None => match git(dir, &["rev-parse", "HEAD"]) {
            Some(head) => head,
            None => return UNKNOWN.to_string(),
        },
    };
    match git(dir, &["status", "--porcelain", "--untracked-files=no"]) {
        Some(changes) if !changes.is_empty() => format!("{head}{DIRTY_SUFFIX}"),
        _ => head,
    }
}

/// The first eight hex digits of `commit`, keeping a `-dirty` suffix.
/// [`UNKNOWN`] stays as it is.
pub fn short(commit: &str) -> String {
    let (hash, suffix) = match commit.strip_suffix(DIRTY_SUFFIX) {
        Some(hash) => (hash, DIRTY_SUFFIX),
        None => (commit, ""),
    };
    if hash == UNKNOWN || hash.len() <= SHORT_LEN {
        return commit.to_string();
    }
    format!("{}{suffix}", &hash[..SHORT_LEN])
}

/// The git files whose change means the commit may have changed, plus the
/// crate's `src` directory for the dirty flag. Empty without git.
///
/// Paths come from `git rev-parse --git-path`, which resolves worktrees: a
/// linked worktree's `.git` is a file pointing at the per-worktree directory
/// (`HEAD`, `index`), while branch refs live in the common directory.
fn rerun_paths(dir: &Path) -> Vec<PathBuf> {
    let git_path = |name: &str| git(dir, &["rev-parse", "--git-path", name]).map(|p| dir.join(p));
    let mut paths = Vec::new();
    let Some(head) = git_path("HEAD") else {
        return paths;
    };
    paths.push(head);
    if let Some(index) = git_path("index") {
        paths.push(index);
    }
    // A detached HEAD has no symbolic ref; HEAD itself then holds the commit.
    if let Some(branch) = git(dir, &["symbolic-ref", "-q", "HEAD"]) {
        if let Some(loose) = git_path(&branch).filter(|p| p.exists()) {
            paths.push(loose);
        }
    }
    if let Some(packed) = git_path("packed-refs").filter(|p| p.exists()) {
        paths.push(packed);
    }
    let src = dir.join("src");
    if src.is_dir() {
        paths.push(src);
    }
    paths
}

/// Runs git in `dir` and returns its trimmed stdout, or `None` when git is
/// missing, fails, or prints nothing useful.
fn git(dir: &Path, args: &[&str]) -> Option<String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(dir)
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let text = String::from_utf8(output.stdout).ok()?;
    Some(text.trim().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(dir: &Path, args: &[&str]) {
        let status = Command::new("git")
            .args(args)
            .current_dir(dir)
            .status()
            .expect("git runs");
        assert!(status.success(), "git {args:?} failed");
    }

    /// A fresh repository with one commit of `tracked.txt`.
    fn repo() -> tempfile::TempDir {
        let dir = tempfile::tempdir().expect("tempdir");
        run(dir.path(), &["init", "-q", "-b", "main"]);
        run(dir.path(), &["config", "user.email", "build@example.org"]);
        run(dir.path(), &["config", "user.name", "build"]);
        std::fs::write(dir.path().join("tracked.txt"), "one\n").unwrap();
        run(dir.path(), &["add", "tracked.txt"]);
        run(dir.path(), &["commit", "-q", "-m", "one"]);
        dir
    }

    fn is_full_hash(s: &str) -> bool {
        s.len() == 40 && s.chars().all(|c| c.is_ascii_hexdigit())
    }

    #[test]
    fn clean_tree_reports_head() {
        let dir = repo();
        let commit = git_commit(dir.path(), None);
        assert!(is_full_hash(&commit), "{commit}");
        let head = git(dir.path(), &["rev-parse", "HEAD"]).unwrap();
        assert_eq!(commit, head);
    }

    #[test]
    fn modified_tracked_file_marks_dirty() {
        let dir = repo();
        std::fs::write(dir.path().join("tracked.txt"), "two\n").unwrap();
        let commit = git_commit(dir.path(), None);
        let hash = commit.strip_suffix(DIRTY_SUFFIX).expect("dirty suffix");
        assert!(is_full_hash(hash), "{commit}");
    }

    #[test]
    fn untracked_file_is_not_dirty() {
        let dir = repo();
        std::fs::write(dir.path().join("scratch.txt"), "x\n").unwrap();
        let commit = git_commit(dir.path(), None);
        assert!(is_full_hash(&commit), "{commit}");
    }

    #[test]
    fn github_sha_wins_over_head() {
        let dir = repo();
        let sha = "0123456789abcdef0123456789abcdef01234567";
        assert_eq!(git_commit(dir.path(), Some(sha)), sha);
        // An empty variable is "not set".
        assert!(is_full_hash(&git_commit(dir.path(), Some(""))));
    }

    #[test]
    fn github_sha_still_gets_the_dirty_suffix() {
        let dir = repo();
        std::fs::write(dir.path().join("tracked.txt"), "two\n").unwrap();
        let sha = "0123456789abcdef0123456789abcdef01234567";
        assert_eq!(
            git_commit(dir.path(), Some(sha)),
            format!("{sha}{DIRTY_SUFFIX}")
        );
    }

    #[test]
    fn no_repository_is_unknown() {
        let dir = tempfile::tempdir().expect("tempdir");
        // Keep git from finding a repository above the temp dir.
        let outside = dir.path().join("outside");
        std::fs::create_dir(&outside).unwrap();
        let commit = Command::new("git")
            .args(["rev-parse", "HEAD"])
            .current_dir(&outside)
            .env("GIT_CEILING_DIRECTORIES", dir.path())
            .output()
            .unwrap();
        assert!(!commit.status.success(), "temp dir must not be a repo");
        assert_eq!(git_commit(&outside, None), UNKNOWN);
        assert!(rerun_paths(&outside).is_empty());
    }

    #[test]
    fn short_keeps_suffix_and_unknown() {
        assert_eq!(
            short("0123456789abcdef0123456789abcdef01234567"),
            "01234567"
        );
        assert_eq!(
            short("0123456789abcdef0123456789abcdef01234567-dirty"),
            "01234567-dirty"
        );
        assert_eq!(short(UNKNOWN), UNKNOWN);
        assert_eq!(short("abc"), "abc");
    }

    #[test]
    fn rerun_paths_name_head_index_and_branch_ref() {
        let dir = repo();
        let paths = rerun_paths(dir.path());
        let names: Vec<String> = paths
            .iter()
            .map(|p| {
                p.strip_prefix(dir.path())
                    .unwrap_or(p)
                    .display()
                    .to_string()
            })
            .collect();
        assert!(names.contains(&".git/HEAD".to_string()), "{names:?}");
        assert!(names.contains(&".git/index".to_string()), "{names:?}");
        assert!(
            names.contains(&".git/refs/heads/main".to_string()),
            "{names:?}"
        );
        assert!(
            paths.iter().all(|p| p.exists()),
            "every rerun path exists: {names:?}"
        );
    }

    #[test]
    fn rerun_paths_follow_a_linked_worktree() {
        let dir = repo();
        let linked = dir.path().join("linked");
        run(
            dir.path(),
            &[
                "worktree",
                "add",
                "-q",
                "-b",
                "side",
                linked.to_str().unwrap(),
            ],
        );
        let paths = rerun_paths(&linked);
        let names: Vec<String> = paths.iter().map(|p| p.display().to_string()).collect();
        let worktree_head = dir.path().join(".git/worktrees/linked/HEAD");
        let common_ref = dir.path().join(".git/refs/heads/side");
        assert!(
            paths.iter().any(|p| same_file(p, &worktree_head)),
            "{names:?}"
        );
        assert!(paths.iter().any(|p| same_file(p, &common_ref)), "{names:?}");
        assert!(is_full_hash(&git_commit(&linked, None)));
    }

    fn same_file(a: &Path, b: &Path) -> bool {
        match (a.canonicalize(), b.canonicalize()) {
            (Ok(a), Ok(b)) => a == b,
            _ => false,
        }
    }
}
