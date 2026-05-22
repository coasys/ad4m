//! Periodic memory diagnostics for tracking down leaks.
//!
//! Spawns a background task that logs process RSS, jemalloc stats, and
//! per-perspective data-structure sizes every 30 seconds. Output goes to
//! the standard `log::info!` logger so it appears in the executor log.
//!
//! Activate by calling `start_memory_diagnostics()` after perspectives
//! are initialised (done automatically in lib.rs).

use super::perspective_instance::PerspectiveMemoryStats;
use super::PERSPECTIVES;
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
                let page_size = 4096usize; // typical Linux page size
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
        // Advance jemalloc's stats epoch so values are fresh.
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

/// Collect per-perspective stats on a blocking thread (since PERSPECTIVES
/// uses std::sync::RwLock and the inner tokio locks need blocking_lock).
fn collect_perspective_stats() -> Vec<PerspectiveMemoryStats> {
    let perspectives = match PERSPECTIVES.read() {
        Ok(p) => p,
        Err(_) => return vec![],
    };

    let mut stats = Vec::with_capacity(perspectives.len());
    for (_uuid, perspective_lock) in perspectives.iter() {
        if let Ok(perspective) = perspective_lock.read() {
            stats.push(perspective.memory_diagnostics_sync());
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
            let (allocated, active, metadata, resident, mapped) = jemalloc_stats();

            let rss_delta = rss as isize - prev_rss as isize;
            let alloc_delta = allocated as isize - prev_allocated as isize;
            prev_rss = rss;
            prev_allocated = allocated;

            log::info!(
                "MEMORY | RSS: {} ({}) | VIRT: {} | jemalloc alloc: {} ({}) active: {} meta: {} resident: {} mapped: {}",
                format_bytes(rss),
                format_bytes_signed(rss_delta),
                format_bytes(virt),
                format_bytes(allocated),
                format_bytes_signed(alloc_delta),
                format_bytes(active),
                format_bytes(metadata),
                format_bytes(resident),
                format_bytes(mapped),
            );

            // Per-perspective stats — collected on a blocking thread to avoid
            // holding std::sync::RwLock across .await points.
            let perspective_stats = tokio::task::spawn_blocking(collect_perspective_stats)
                .await
                .unwrap_or_default();

            if !perspective_stats.is_empty() {
                let mut total_quads = 0usize;
                let mut total_subs = 0usize;
                let mut total_sub_bytes = 0usize;
                let mut total_batches = 0usize;

                for stats in &perspective_stats {
                    total_quads += stats.quad_count;
                    total_subs += stats.subscriptions;
                    total_sub_bytes += stats.sub_result_bytes;
                    total_batches += stats.batches;

                    // Only log details for perspectives with non-trivial data
                    if stats.quad_count > 0 || stats.subscriptions > 0 || stats.batches > 0 {
                        log::info!(
                            "  [{}] \"{}\" | quads: {} | subs: {} (result_bytes: {}) | batches: {} (links: {})",
                            &stats.uuid[..8.min(stats.uuid.len())],
                            stats.name,
                            stats.quad_count,
                            stats.subscriptions,
                            format_bytes(stats.sub_result_bytes),
                            stats.batches,
                            stats.batch_links,
                        );
                    }
                }

                log::info!(
                    "  TOTALS | perspectives: {} | quads: {} | subs: {} (result_bytes: {}) | batches: {}",
                    perspective_stats.len(),
                    total_quads,
                    total_subs,
                    format_bytes(total_sub_bytes),
                    total_batches,
                );
            }
        }
    });
}
