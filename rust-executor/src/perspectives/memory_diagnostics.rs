//! Periodic memory diagnostics for tracking down leaks.
//!
//! Spawns a background task that logs process RSS, jemalloc stats, and
//! per-perspective data-structure sizes every 30 seconds.
//!
//! By default, output goes to `log::debug!`.  Set the environment variable
//! `AD4M_MEMORY_DIAGNOSTICS=1` to promote all output to `log::info!`.
//!
//! Activate by calling `start_memory_diagnostics()` after perspectives
//! are initialised (done automatically in lib.rs).

use super::perspective_instance::PerspectiveMemoryStats;
use super::{get_app_data_path, PERSPECTIVES};
use std::time::Duration;
use tokio::time::sleep;

/// Log at info or debug level depending on the `verbose` flag.
macro_rules! mem_log {
    ($verbose:expr, $($arg:tt)*) => {
        if $verbose {
            log::info!($($arg)*);
        } else {
            log::debug!($($arg)*);
        }
    };
}

/// Read process RSS from /proc/self/statm (Linux only).
/// Returns (rss_bytes, virt_bytes) or (0, 0) on non-Linux / error.
fn process_memory() -> (usize, usize) {
    #[cfg(target_os = "linux")]
    {
        if let Ok(contents) = std::fs::read_to_string("/proc/self/statm") {
            let parts: Vec<&str> = contents.split_whitespace().collect();
            if parts.len() >= 2 {
                let page_size = 4096usize;
                let virt_pages = parts[0].parse::<usize>().unwrap_or(0);
                let rss_pages = parts[1].parse::<usize>().unwrap_or(0);
                return (rss_pages * page_size, virt_pages * page_size);
            }
        }
        (0, 0)
    }
    #[cfg(not(target_os = "linux"))]
    {
        (0, 0)
    }
}

/// Parse /proc/self/status for detailed memory breakdown (Linux only).
/// Returns a formatted string with key memory fields.
fn proc_status_memory() -> String {
    #[cfg(target_os = "linux")]
    {
        if let Ok(contents) = std::fs::read_to_string("/proc/self/status") {
            let fields = [
                "VmRSS", "RssAnon", "RssFile", "RssShmem", "VmData", "VmStk", "VmLib", "VmSwap",
            ];
            let mut parts = Vec::new();
            for line in contents.lines() {
                for field in &fields {
                    if line.starts_with(field) {
                        let value = line.split_whitespace().collect::<Vec<_>>();
                        if value.len() >= 2 {
                            parts.push(format!("{}:{}", field, value[1..].join("")));
                        }
                    }
                }
            }
            return parts.join(" | ");
        }
        String::new()
    }
    #[cfg(not(target_os = "linux"))]
    {
        String::new()
    }
}

/// Recursively compute directory size in bytes.
fn dir_size(path: &std::path::Path) -> u64 {
    let mut total = 0u64;
    if let Ok(entries) = std::fs::read_dir(path) {
        for entry in entries.flatten() {
            let ft = match entry.file_type() {
                Ok(ft) => ft,
                Err(_) => continue,
            };
            if ft.is_file() {
                total += entry.metadata().map(|m| m.len()).unwrap_or(0);
            } else if ft.is_dir() {
                total += dir_size(&entry.path());
            }
        }
    }
    total
}

fn format_bytes(bytes: usize) -> String {
    if bytes >= 1_073_741_824 {
        format!("{:.2} GB", bytes as f64 / 1_073_741_824.0)
    } else if bytes >= 1_048_576 {
        format!("{:.1} MB", bytes as f64 / 1_048_576.0)
    } else if bytes >= 1024 {
        format!("{:.1} KB", bytes as f64 / 1024.0)
    } else {
        format!("{} B", bytes)
    }
}

fn format_bytes_u64(bytes: u64) -> String {
    format_bytes(bytes as usize)
}

fn format_bytes_signed(bytes: isize) -> String {
    let abs = bytes.unsigned_abs();
    let sign = if bytes >= 0 { "+" } else { "-" };
    if abs >= 1_073_741_824 {
        format!("{}{:.2} GB", sign, abs as f64 / 1_073_741_824.0)
    } else if abs >= 1_048_576 {
        format!("{}{:.1} MB", sign, abs as f64 / 1_048_576.0)
    } else if abs >= 1024 {
        format!("{}{:.1} KB", sign, abs as f64 / 1024.0)
    } else {
        format!("{}{} B", sign, abs)
    }
}

/// Read jemalloc stats via the `tikv-jemalloc-ctl` crate.
/// Returns (allocated, active, metadata, resident, mapped) in bytes.
fn jemalloc_stats() -> (usize, usize, usize, usize, usize) {
    #[cfg(not(target_env = "msvc"))]
    {
        use tikv_jemalloc_ctl::{epoch, stats};
        let _ = epoch::advance();
        let allocated = stats::allocated::read().unwrap_or(0);
        let active = stats::active::read().unwrap_or(0);
        let metadata = stats::metadata::read().unwrap_or(0);
        let resident = stats::resident::read().unwrap_or(0);
        let mapped = stats::mapped::read().unwrap_or(0);
        (allocated, active, metadata, resident, mapped)
    }
    #[cfg(target_env = "msvc")]
    {
        (0, 0, 0, 0, 0)
    }
}

/// Collect per-perspective stats on a blocking thread.
///
/// The global `PERSPECTIVES` lock is held only long enough to collect
/// in-memory stats from each perspective.  Disk-size computation (filesystem
/// I/O) happens after the lock is released so mutations aren't blocked.
fn collect_perspective_stats() -> Vec<(PerspectiveMemoryStats, u64)> {
    // Phase 1: collect in-memory stats under the PERSPECTIVES lock.
    let mem_stats: Vec<PerspectiveMemoryStats> = {
        let perspectives = match PERSPECTIVES.read() {
            Ok(p) => p,
            Err(_) => return vec![],
        };
        perspectives
            .iter()
            .filter_map(|(_uuid, perspective_lock)| {
                perspective_lock
                    .read()
                    .ok()
                    .map(|p| p.memory_diagnostics_sync())
            })
            .collect()
    }; // PERSPECTIVES lock released here

    // Phase 2: compute on-disk sizes without holding any global lock.
    let base_path = get_app_data_path().map(|p| std::path::PathBuf::from(&p).join("perspectives"));

    mem_stats
        .into_iter()
        .map(|mem| {
            let disk_bytes = base_path
                .as_ref()
                .map(|base| {
                    let store_dir = base.join(&mem.uuid).join("sparql_store");
                    dir_size(&store_dir)
                })
                .unwrap_or(0);
            (mem, disk_bytes)
        })
        .collect()
}

/// Spawn a tokio task that logs memory diagnostics every 30 seconds.
///
/// Logs at `debug` level by default.  Set `AD4M_MEMORY_DIAGNOSTICS=1`
/// to log at `info` level instead.
pub fn start_memory_diagnostics() {
    let verbose = std::env::var("AD4M_MEMORY_DIAGNOSTICS")
        .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
        .unwrap_or(false);

    tokio::spawn(async move {
        mem_log!(verbose, "Memory diagnostics started (reporting every 30s)");
        let mut prev_rss: usize = 0;
        let mut prev_allocated: usize = 0;

        loop {
            sleep(Duration::from_secs(30)).await;

            // Skip all expensive work when the target log level is disabled.
            let enabled = if verbose {
                log::log_enabled!(log::Level::Info)
            } else {
                log::log_enabled!(log::Level::Debug)
            };
            if !enabled {
                continue;
            }

            // Process-level memory
            let (rss, virt) = process_memory();
            let (allocated, _active, _metadata, resident, _mapped) = jemalloc_stats();

            let rss_delta = rss as isize - prev_rss as isize;
            let alloc_delta = allocated as isize - prev_allocated as isize;
            prev_rss = rss;
            prev_allocated = allocated;

            // How much RSS is NOT accounted for by jemalloc
            let non_jemalloc = if rss > resident { rss - resident } else { 0 };

            mem_log!(
                verbose,
                "MEMORY | RSS: {} ({}) | jemalloc alloc: {} ({}) resident: {} | non-jemalloc RSS: {} | VIRT: {}",
                format_bytes(rss),
                format_bytes_signed(rss_delta),
                format_bytes(allocated),
                format_bytes_signed(alloc_delta),
                format_bytes(resident),
                format_bytes(non_jemalloc),
                format_bytes(virt),
            );

            // Detailed /proc/self/status breakdown
            let status = proc_status_memory();
            if !status.is_empty() {
                mem_log!(verbose, "  /proc/self/status: {}", status);
            }

            // Per-perspective stats
            let perspective_stats = tokio::task::spawn_blocking(collect_perspective_stats)
                .await
                .unwrap_or_default();

            if !perspective_stats.is_empty() {
                let mut total_quads = 0usize;
                let mut total_disk = 0u64;

                for (stats, disk_bytes) in &perspective_stats {
                    total_quads += stats.quad_count;
                    total_disk += disk_bytes;

                    if stats.quad_count > 0 || stats.subscriptions > 0 || *disk_bytes > 0 {
                        mem_log!(
                            verbose,
                            "  [{}] \"{}\" | quads: {} | disk: {} | subs: {} ({}) | batches: {}",
                            &stats.uuid[..8.min(stats.uuid.len())],
                            stats.name,
                            stats.quad_count,
                            format_bytes_u64(*disk_bytes),
                            stats.subscriptions,
                            format_bytes(stats.sub_result_bytes),
                            stats.batches,
                        );
                    }
                }

                mem_log!(
                    verbose,
                    "  TOTALS | perspectives: {} | quads: {} | disk: {}",
                    perspective_stats.len(),
                    total_quads,
                    format_bytes_u64(total_disk),
                );
            }
        }
    });
}
