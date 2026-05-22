//! Periodic memory diagnostics for tracking down leaks.
//!
//! Spawns a background task that logs process RSS, jemalloc stats, and
//! per-perspective data-structure sizes every 30 seconds. Output goes to
//! the standard `log::info!` logger so it appears in the executor log.
//!
//! Activate by calling `start_memory_diagnostics()` after perspectives
//! are initialised (done automatically in lib.rs).

use super::perspective_instance::PerspectiveMemoryStats;
use super::{get_app_data_path, PERSPECTIVES};
use std::time::Duration;
use tokio::time::sleep;

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
fn collect_perspective_stats() -> Vec<(PerspectiveMemoryStats, u64)> {
    let perspectives = match PERSPECTIVES.read() {
        Ok(p) => p,
        Err(_) => return vec![],
    };

    let base_path = get_app_data_path().map(|p| std::path::PathBuf::from(&p).join("perspectives"));

    let mut stats = Vec::with_capacity(perspectives.len());
    for (_uuid, perspective_lock) in perspectives.iter() {
        if let Ok(perspective) = perspective_lock.read() {
            let mem = perspective.memory_diagnostics_sync();
            // Compute on-disk size of this perspective's RocksDB store
            let disk_bytes = base_path
                .as_ref()
                .map(|base| {
                    let store_dir = base.join(&mem.uuid).join("sparql_store");
                    dir_size(&store_dir)
                })
                .unwrap_or(0);
            stats.push((mem, disk_bytes));
        }
    }
    stats
}

/// Spawn a tokio task that logs memory diagnostics every 30 seconds.
pub fn start_memory_diagnostics() {
    tokio::spawn(async {
        log::info!("Memory diagnostics started (reporting every 30s)");
        let mut prev_rss: usize = 0;
        let mut prev_allocated: usize = 0;

        loop {
            sleep(Duration::from_secs(30)).await;

            // Process-level memory
            let (rss, virt) = process_memory();
            let (allocated, _active, _metadata, resident, _mapped) = jemalloc_stats();

            let rss_delta = rss as isize - prev_rss as isize;
            let alloc_delta = allocated as isize - prev_allocated as isize;
            prev_rss = rss;
            prev_allocated = allocated;

            // How much RSS is NOT accounted for by jemalloc
            let non_jemalloc = if rss > resident { rss - resident } else { 0 };

            log::info!(
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
                log::info!("  /proc/self/status: {}", status);
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
                        log::info!(
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

                log::info!(
                    "  TOTALS | perspectives: {} | quads: {} | disk: {}",
                    perspective_stats.len(),
                    total_quads,
                    format_bytes_u64(total_disk),
                );
            }
        }
    });
}
