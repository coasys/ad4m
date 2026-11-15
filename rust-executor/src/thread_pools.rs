//! Worker counts of the executor's tokio runtimes.
//!
//! A tokio multi-thread runtime starts one worker per CPU unless told
//! otherwise. The executor builds several (the binary's main runtime, the API
//! server's, the MCP server's, the dapp server's), so an idle executor on a
//! 32-CPU host held two pools of 32 workers, and each executor on a shared
//! host did the same. These constants size them.
//!
//! Blocking work does not need more workers. A call that blocks for long
//! (a SPARQL query through `SparqlStore::query_cancellable`, subscription
//! re-checks, shared-database requests) runs on the runtime's blocking pool
//! (`spawn_blocking`), which grows on demand and is not capped here. No
//! production code calls `block_in_place`, which would hold a worker. Short
//! synchronous store reads (`get_link`, `query_links`) still run on a worker:
//! eight of them at once already keep eight CPUs busy.

use tokio::runtime::{Builder, Runtime};

/// Most workers of the main runtime (the `ad4m-executor` binary's): startup,
/// the perspectives' background tasks, link-language syncs, pub/sub fan-out
/// and the events socket.
pub const MAIN_RUNTIME_MAX_WORKERS: usize = 8;

/// Most workers of the API runtime (`rest_server` threads): every WS RPC
/// call and the OpenAI-compatible `/v1` routes, and the tasks they spawn.
pub const API_RUNTIME_MAX_WORKERS: usize = 8;

/// Most workers of the MCP server's runtime (`mcp_server` threads), which
/// runs only with `enable_mcp`.
pub const MCP_RUNTIME_MAX_WORKERS: usize = 2;

/// Workers of the dapp server's runtime (`dapp_server` threads), which serves
/// static files only with `run_dapp_server`.
pub const DAPP_RUNTIME_WORKERS: usize = 1;

/// The host's CPUs, at most `max` (at least 1). A host with fewer CPUs than
/// `max` keeps one worker per CPU, as before.
pub fn capped_workers(max: usize) -> usize {
    std::thread::available_parallelism()
        .map_or(1, |n| n.get())
        .min(max)
        .max(1)
}

/// A multi-thread runtime with I/O and timers, `workers` workers, and its
/// threads (workers and blocking pool) named `name`.
pub fn multi_thread_runtime(name: &str, workers: usize) -> std::io::Result<Runtime> {
    Builder::new_multi_thread()
        .worker_threads(workers.max(1))
        .thread_name(name)
        .enable_all()
        .build()
}

/// The main runtime of an executor binary: [`MAIN_RUNTIME_MAX_WORKERS`] at
/// most, threads named `tokio-rt-worker` as tokio names them by default.
pub fn main_runtime() -> std::io::Result<Runtime> {
    multi_thread_runtime("tokio-rt-worker", capped_workers(MAIN_RUNTIME_MAX_WORKERS))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cpus() -> usize {
        std::thread::available_parallelism().map_or(1, |n| n.get())
    }

    #[test]
    fn capped_workers_never_exceeds_the_cap_or_the_cpus() {
        assert_eq!(capped_workers(8), cpus().min(8));
        assert_eq!(capped_workers(1), 1);
        assert_eq!(capped_workers(0), 1, "a runtime needs one worker");
        assert_eq!(capped_workers(usize::MAX), cpus());
    }

    /// A runtime built here has exactly the workers asked for, whatever the
    /// host's CPU count.
    #[test]
    fn a_runtime_has_the_workers_it_was_given() {
        for workers in [1, 2, 3] {
            let runtime = multi_thread_runtime("pool-test", workers).unwrap();
            assert_eq!(runtime.metrics().num_workers(), workers);
            let name = runtime.block_on(async {
                tokio::spawn(async { std::thread::current().name().map(str::to_string) })
                    .await
                    .unwrap()
            });
            assert_eq!(name.as_deref(), Some("pool-test"));
        }
    }

    #[test]
    fn the_main_runtime_is_capped() {
        let runtime = main_runtime().unwrap();
        assert_eq!(
            runtime.metrics().num_workers(),
            cpus().min(MAIN_RUNTIME_MAX_WORKERS)
        );
    }

    /// Two workers and 32 blocking calls in flight at once: the blocking
    /// pool (not capped here) takes them, so the workers stay free for async
    /// tasks.
    #[test]
    fn blocking_calls_do_not_starve_a_capped_runtime() {
        let runtime = multi_thread_runtime("pool-test", 2).unwrap();
        runtime.block_on(async {
            let gate = std::sync::Arc::new(std::sync::Barrier::new(33));
            let blocked: Vec<_> = (0..32)
                .map(|_| {
                    let gate = gate.clone();
                    tokio::task::spawn_blocking(move || {
                        gate.wait();
                    })
                })
                .collect();
            // All 32 blocking calls are parked; an async task still runs.
            let ran = tokio::time::timeout(
                std::time::Duration::from_secs(5),
                tokio::spawn(async { 1 + 1 }),
            )
            .await;
            assert_eq!(ran.unwrap().unwrap(), 2);
            let gate_last = gate.clone();
            tokio::task::spawn_blocking(move || {
                gate_last.wait();
            })
            .await
            .unwrap();
            for task in blocked {
                task.await.unwrap();
            }
        });
    }
}
