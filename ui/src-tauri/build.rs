fn main() {
    // AD4M_GIT_COMMIT / AD4M_GIT_COMMIT_SHORT: the commit this build is from.
    ad4m_build_support::emit_git_commit();
    tauri_build::build();
}
