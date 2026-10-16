//! Raise the process's open-file soft limit (`RLIMIT_NOFILE`) to what the
//! system allows.
//!
//! The executor keeps one RocksDB-backed SPARQL store open per perspective
//! (see `perspectives::sparql_store::SPARQL_STORE_FD_BUDGET`), plus Holochain,
//! SQLite, sockets and the language runtime. A soft limit of 256 (macOS) or
//! 1024 (many Linux shells) runs out after a handful of perspectives, with
//! "Too many open files". The hard limit is the ceiling an unprivileged
//! process may raise its own soft limit to, so startup lifts the soft limit
//! to it.

/// What [`raise_nofile_limit`] found and did.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NofileLimit {
    /// Soft limit before the call.
    pub before: u64,
    /// Soft limit after the call (equal to `before` when nothing changed).
    pub after: u64,
    /// Hard limit (`u64::MAX` for `RLIM_INFINITY`).
    pub hard: u64,
}

/// The soft limit to ask for: the hard limit, clamped to the kernel's
/// per-process ceiling when there is one, and never below the current soft
/// limit.
///
/// `kernel_cap` is `fs.nr_open` on Linux and `kern.maxfilesperproc` on
/// macOS. macOS reports a hard limit of `RLIM_INFINITY`, yet `setrlimit`
/// refuses a soft limit above `kern.maxfilesperproc`; Linux refuses one above
/// `fs.nr_open`.
#[cfg_attr(not(unix), allow(dead_code))]
pub fn nofile_target(current: u64, hard: u64, kernel_cap: Option<u64>) -> u64 {
    let ceiling = match kernel_cap {
        Some(cap) => hard.min(cap),
        None => hard,
    };
    ceiling.max(current)
}

/// Raise the soft `RLIMIT_NOFILE` to [`nofile_target`]. Never lowers it.
#[cfg(unix)]
pub fn raise_nofile_limit() -> std::io::Result<NofileLimit> {
    let (before, hard) = get_nofile()?;
    let target = nofile_target(before, hard, kernel_cap());
    if target > before {
        if let Err(e) = set_nofile(target, hard) {
            // macOS without a readable kern.maxfilesperproc: OPEN_MAX is the
            // documented ceiling for the soft limit.
            #[cfg(target_os = "macos")]
            {
                let open_max = nofile_target(before, hard, Some(OPEN_MAX));
                if open_max > before && set_nofile(open_max, hard).is_ok() {
                    let (after, hard) = get_nofile()?;
                    return Ok(NofileLimit {
                        before,
                        after,
                        hard,
                    });
                }
            }
            return Err(e);
        }
    }
    let (after, hard) = get_nofile()?;
    Ok(NofileLimit {
        before,
        after,
        hard,
    })
}

/// No `RLIMIT_NOFILE` outside Unix: nothing to raise.
#[cfg(not(unix))]
pub fn raise_nofile_limit() -> std::io::Result<NofileLimit> {
    Ok(NofileLimit {
        before: 0,
        after: 0,
        hard: 0,
    })
}

/// [`raise_nofile_limit`], logging the outcome. Called once at startup.
pub fn raise_nofile_limit_and_log() {
    match raise_nofile_limit() {
        #[cfg(unix)]
        Ok(limit) => log::info!(
            "Open file limit (RLIMIT_NOFILE): soft {} -> {}, hard {}",
            limit.before,
            limit.after,
            render_limit(limit.hard)
        ),
        #[cfg(not(unix))]
        Ok(_) => {}
        Err(e) => log::warn!("Could not raise the open file limit (RLIMIT_NOFILE): {e}"),
    }
}

#[cfg(unix)]
fn render_limit(value: u64) -> String {
    if value == u64::MAX {
        "unlimited".to_string()
    } else {
        value.to_string()
    }
}

#[cfg(target_os = "macos")]
const OPEN_MAX: u64 = 10240;

/// Convert an `rlim_t` (`RLIM_INFINITY` included) to `u64`.
#[cfg(unix)]
fn from_rlim(value: libc::rlim_t) -> u64 {
    if value == libc::RLIM_INFINITY {
        u64::MAX
    } else {
        u64::try_from(value).unwrap_or(u64::MAX)
    }
}

#[cfg(unix)]
fn to_rlim(value: u64) -> libc::rlim_t {
    if value == u64::MAX {
        libc::RLIM_INFINITY
    } else {
        libc::rlim_t::try_from(value).unwrap_or(libc::RLIM_INFINITY)
    }
}

#[cfg(unix)]
fn get_nofile() -> std::io::Result<(u64, u64)> {
    let mut limit = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    // SAFETY: `limit` is a valid, writable `rlimit` for the call's duration.
    if unsafe { libc::getrlimit(libc::RLIMIT_NOFILE, &mut limit) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok((from_rlim(limit.rlim_cur), from_rlim(limit.rlim_max)))
}

#[cfg(unix)]
fn set_nofile(soft: u64, hard: u64) -> std::io::Result<()> {
    let limit = libc::rlimit {
        rlim_cur: to_rlim(soft),
        rlim_max: to_rlim(hard),
    };
    // SAFETY: `limit` is a valid `rlimit` for the call's duration.
    if unsafe { libc::setrlimit(libc::RLIMIT_NOFILE, &limit) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(())
}

#[cfg(target_os = "linux")]
fn kernel_cap() -> Option<u64> {
    std::fs::read_to_string("/proc/sys/fs/nr_open")
        .ok()?
        .trim()
        .parse()
        .ok()
}

#[cfg(target_os = "macos")]
fn kernel_cap() -> Option<u64> {
    let mut value: libc::c_int = 0;
    let mut size = std::mem::size_of::<libc::c_int>();
    // SAFETY: the name is NUL-terminated, `value`/`size` describe a writable
    // c_int, and no new value is written.
    let rc = unsafe {
        libc::sysctlbyname(
            c"kern.maxfilesperproc".as_ptr(),
            (&mut value as *mut libc::c_int).cast(),
            &mut size,
            std::ptr::null_mut(),
            0,
        )
    };
    if rc != 0 || value <= 0 {
        return None;
    }
    u64::try_from(value).ok()
}

#[cfg(all(unix, not(any(target_os = "linux", target_os = "macos"))))]
fn kernel_cap() -> Option<u64> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn target_is_the_hard_limit_when_the_kernel_allows_it() {
        assert_eq!(nofile_target(256, 524_288, Some(1_048_576)), 524_288);
        assert_eq!(nofile_target(1024, 4096, None), 4096);
    }

    #[test]
    fn target_is_clamped_to_the_kernel_cap() {
        // macOS: hard limit RLIM_INFINITY, kern.maxfilesperproc 61440.
        assert_eq!(nofile_target(256, u64::MAX, Some(61_440)), 61_440);
        // Linux: hard limit above fs.nr_open.
        assert_eq!(nofile_target(1024, 2_000_000, Some(1_048_576)), 1_048_576);
    }

    #[test]
    fn target_never_lowers_the_soft_limit() {
        assert_eq!(nofile_target(8192, 4096, None), 8192);
        assert_eq!(nofile_target(8192, u64::MAX, Some(1024)), 8192);
    }

    #[cfg(unix)]
    #[test]
    fn raising_leaves_the_soft_limit_at_the_target() {
        let limit = raise_nofile_limit().expect("raise RLIMIT_NOFILE");
        let (soft, hard) = get_nofile().unwrap();
        assert_eq!(limit.after, soft);
        assert_eq!(limit.hard, hard);
        assert!(limit.after >= limit.before);
        assert_eq!(limit.after, nofile_target(limit.before, hard, kernel_cap()));
    }

    #[cfg(unix)]
    #[test]
    fn a_lowered_soft_limit_is_raised_back_to_the_hard_limit() {
        let (original_soft, hard) = get_nofile().unwrap();
        let target = nofile_target(0, hard, kernel_cap());
        // Below the target, but high enough for the rest of the test process.
        let lowered = target.min(original_soft).min(512);
        if lowered >= target {
            return; // hard limit already this low: nothing to raise
        }
        set_nofile(lowered, hard).unwrap();
        let limit = raise_nofile_limit().expect("raise RLIMIT_NOFILE");
        assert_eq!(limit.before, lowered);
        assert_eq!(limit.after, target);
        assert_eq!(get_nofile().unwrap().0, target);
    }
}
