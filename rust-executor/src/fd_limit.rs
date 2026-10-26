//! Raises the soft open-file limit (`RLIMIT_NOFILE`) at startup.
//!
//! Every perspective keeps its SPARQL store's RocksDB files open, and the Deno
//! runtimes, Holochain and the sockets hold more. The usual soft limit (1024 on
//! Linux, 256 on macOS) runs out at a few dozen perspectives: Deno then panics
//! on `EMFILE` and writes fail. The hard limit is usually much higher, and a
//! process may raise its own soft limit up to the hard limit without privileges.

use log::{info, warn};
use std::io;

/// The soft limit the executor asks for. Higher than any executor needs today,
/// low enough that a runaway fd leak still ends in an error, not a host-wide
/// file-table exhaustion.
pub const TARGET_OPEN_FILES: libc::rlim_t = 65536;

fn get_nofile() -> io::Result<libc::rlimit> {
    let mut limit = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    // SAFETY: `limit` is a valid, writable `rlimit`.
    if unsafe { libc::getrlimit(libc::RLIMIT_NOFILE, &mut limit) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(limit)
}

fn set_nofile(limit: &libc::rlimit) -> io::Result<()> {
    // SAFETY: `limit` is a valid `rlimit`.
    if unsafe { libc::setrlimit(libc::RLIMIT_NOFILE, limit) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// macOS refuses a soft limit above `kern.maxfilesperproc` with `EINVAL`, even
/// when the hard limit reads as unlimited.
#[cfg(target_os = "macos")]
fn per_process_cap() -> Option<libc::rlim_t> {
    let mut value: libc::c_int = 0;
    let mut size = std::mem::size_of::<libc::c_int>();
    // SAFETY: the name is NUL-terminated and `value`/`size` describe a writable c_int.
    let ret = unsafe {
        libc::sysctlbyname(
            c"kern.maxfilesperproc".as_ptr(),
            &mut value as *mut libc::c_int as *mut libc::c_void,
            &mut size,
            std::ptr::null_mut(),
            0,
        )
    };
    (ret == 0 && value > 0).then_some(value as libc::rlim_t)
}

#[cfg(not(target_os = "macos"))]
fn per_process_cap() -> Option<libc::rlim_t> {
    None
}

/// Raises the soft `RLIMIT_NOFILE` to `target`, capped at the hard limit (and
/// on macOS at `kern.maxfilesperproc`). Never lowers it and never touches the
/// hard limit. Returns the soft limit before and after.
pub fn raise_fd_limit(target: libc::rlim_t) -> io::Result<(libc::rlim_t, libc::rlim_t)> {
    let limit = get_nofile()?;
    let before = limit.rlim_cur;
    let mut wanted = target.min(limit.rlim_max);
    if let Some(cap) = per_process_cap() {
        wanted = wanted.min(cap);
    }
    if wanted <= before {
        return Ok((before, before));
    }
    set_nofile(&libc::rlimit {
        rlim_cur: wanted,
        rlim_max: limit.rlim_max,
    })?;
    Ok((before, get_nofile()?.rlim_cur))
}

/// Startup hook: raises the limit to [`TARGET_OPEN_FILES`] and logs the result.
/// A failure is logged and the executor carries on with the limit it has.
pub fn raise_fd_limit_at_startup() {
    match raise_fd_limit(TARGET_OPEN_FILES) {
        Ok((before, after)) if after > before => {
            info!("Raised the open-file limit (RLIMIT_NOFILE soft) from {before} to {after}")
        }
        Ok((before, _)) => info!("Open-file limit (RLIMIT_NOFILE soft) stays at {before}"),
        Err(e) => warn!("Could not raise the open-file limit (RLIMIT_NOFILE): {e}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Puts the process's original limit back, even when an assertion fails.
    struct Restore(libc::rlimit);
    impl Drop for Restore {
        fn drop(&mut self) {
            set_nofile(&self.0).expect("restore RLIMIT_NOFILE");
        }
    }

    fn set_soft(soft: libc::rlim_t) {
        let hard = get_nofile().unwrap().rlim_max;
        set_nofile(&libc::rlimit {
            rlim_cur: soft.min(hard),
            rlim_max: hard,
        })
        .unwrap();
    }

    #[test]
    fn raise_fd_limit_lifts_a_low_soft_limit_and_never_lowers_it() {
        let original = get_nofile().unwrap();
        let _restore = Restore(original);
        let hard = original.rlim_max;
        let low: libc::rlim_t = 256.min(hard);

        // A low soft limit, as on macOS or under a tight unit, is raised.
        set_soft(low);
        let target: libc::rlim_t = 4096;
        let (before, after) = raise_fd_limit(target).unwrap();
        let mut expected = target.min(hard);
        if let Some(cap) = per_process_cap() {
            expected = expected.min(cap).max(low);
        }
        assert_eq!(before, low);
        assert_eq!(after, expected, "soft limit after raising");
        assert_eq!(get_nofile().unwrap().rlim_cur, expected);
        assert_eq!(get_nofile().unwrap().rlim_max, hard, "hard limit untouched");

        // A soft limit already above the target is left alone.
        let high: libc::rlim_t = 2048.min(hard);
        set_soft(high);
        let (before, after) = raise_fd_limit(1024.min(high)).unwrap();
        assert_eq!((before, after), (high, high));
        assert_eq!(get_nofile().unwrap().rlim_cur, high, "never lowered");
    }
}
