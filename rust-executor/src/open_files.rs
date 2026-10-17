//! The executor's open-file limit.
//!
//! Holochain (DNA packing, the conductor's databases), the per-language
//! runtimes and every client socket hold descriptors, and a multi-user
//! executor runs out under the common soft limit of 1024 ("Too many open
//! files (os error 24)" while packing a DNA). The hard limit is what an
//! unprivileged process may raise its soft limit to, so the executor raises it
//! that far at startup.

/// The most open files the kernel lets one process ask for, whatever the hard
/// limit says: macOS refuses a soft limit above `OPEN_MAX` (10240), and Linux
/// one above `fs.nr_open` (1048576 by default) — and a hard limit of
/// `RLIM_INFINITY` exceeds both.
const PLATFORM_CEILING: u64 = if cfg!(target_os = "macos") {
    10_240
} else {
    1_048_576
};

/// The soft limit to set given the current `soft` and `hard` limits, or `None`
/// to leave it: the hard limit, capped at what the platform accepts (which
/// covers `RLIM_INFINITY`), and never lower than it already is.
pub fn raised_open_file_limit(soft: u64, hard: u64) -> Option<u64> {
    let target = hard.min(PLATFORM_CEILING);
    (soft < target).then_some(target)
}

/// Raise this process's soft open-file limit to its hard limit, and log the
/// limits before and after. A failure is logged; the executor runs on with
/// the limit it had.
// `rlim_t` is `u64` on Linux and macOS but not on every unix, hence the casts.
#[cfg(unix)]
#[allow(clippy::unnecessary_cast)]
pub fn raise_open_file_limit() {
    let mut limit = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    // SAFETY: getrlimit writes only into the rlimit it is given.
    if unsafe { libc::getrlimit(libc::RLIMIT_NOFILE, &mut limit) } != 0 {
        log::warn!(
            "Could not read the open-file limit: {}",
            std::io::Error::last_os_error()
        );
        return;
    }
    let (soft, hard) = (limit.rlim_cur as u64, limit.rlim_max as u64);
    let Some(target) = raised_open_file_limit(soft, hard) else {
        log::info!("Open-file limit: {} (hard {})", soft, show(hard));
        return;
    };
    limit.rlim_cur = target as libc::rlim_t;
    // SAFETY: setrlimit only reads the rlimit it is given.
    if unsafe { libc::setrlimit(libc::RLIMIT_NOFILE, &limit) } != 0 {
        log::warn!(
            "Could not raise the open-file limit from {} to {} (hard {}): {}",
            soft,
            target,
            show(hard),
            std::io::Error::last_os_error()
        );
        return;
    }
    log::info!(
        "Raised the open-file limit from {} to {} (hard {})",
        soft,
        target,
        show(hard)
    );
}

#[cfg(unix)]
#[allow(clippy::unnecessary_cast)]
fn show(limit: u64) -> String {
    if limit == libc::RLIM_INFINITY as u64 {
        "unlimited".to_string()
    } else {
        limit.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn raises_the_soft_limit_to_the_hard_limit() {
        assert_eq!(raised_open_file_limit(1024, 4096), Some(4096));
    }

    #[test]
    fn never_lowers_the_limit() {
        assert_eq!(raised_open_file_limit(4096, 4096), None);
        assert_eq!(raised_open_file_limit(8192, 4096), None);
        assert_eq!(raised_open_file_limit(u64::MAX, u64::MAX), None);
    }

    #[test]
    fn an_unlimited_hard_limit_raises_to_what_the_platform_accepts() {
        assert_eq!(
            raised_open_file_limit(1024, u64::MAX),
            Some(PLATFORM_CEILING)
        );
        assert_eq!(
            raised_open_file_limit(PLATFORM_CEILING, u64::MAX),
            None,
            "already at the platform's ceiling"
        );
    }

    #[cfg(unix)]
    #[test]
    #[allow(clippy::unnecessary_cast)]
    fn rlim_infinity_raises_to_what_the_platform_accepts() {
        let unlimited = libc::RLIM_INFINITY as u64;
        assert_eq!(
            raised_open_file_limit(1024, unlimited),
            Some(PLATFORM_CEILING)
        );
        assert_eq!(raised_open_file_limit(unlimited, unlimited), None);
    }
}
