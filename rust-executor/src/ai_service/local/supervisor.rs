//! Runs a local runtime as a child process of the executor and keeps it running.
//!
//! - It listens on `127.0.0.1` only, on a port [`choose_port`] picks: the one it had, when
//!   free, else a free one, never a port `ai.detectEndpoints` probes (11434, 9931, 8080,
//!   8000, 5092), so it never shadows a user's own server.
//! - Ready means an HTTP probe answers 200 (llama-server's public `/health`, nemo-speech's
//!   `/ready`, which answers 503 until its model is loaded and warmed up) or, for a server
//!   without one, a TCP connect.
//! - A child that exits, or stops answering three probes in a row, is restarted with the
//!   budget WE's executor supervisor uses: 3 restarts, given back after 5 healthy minutes.
//!   After that it stays down, failed, with the reason.
//! - It stops with the executor: on Linux the kernel sends it SIGKILL when the executor dies
//!   (`PR_SET_PDEATHSIG`, set from a thread that lives as long as the process; not SIGTERM:
//!   a server may catch it and not exit, and an executor that is gone cannot follow
//!   up; llama-server's router takes its model processes down with it); everywhere
//!   the executor's shutdown stops it, and the next start stops one a crash left behind
//!   (the pid file, checked against the binary's path).
//! - Its stdout and stderr go to `<name>.log` in the executor's `local-ai` directory.

use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};
use tokio::sync::watch;
use ts_rs::TS;

/// Ports a managed runtime never takes: what `ai.detectEndpoints` probes (the upstream
/// defaults of llama-server, whisper.cpp's server, Ollama, vLLM, speaches).
pub fn avoided_ports() -> Vec<u16> {
    crate::ai_service::endpoints::detect::PROBED_PORTS.to_vec()
}

/// `preferred` when it is free and allowed, else a free loopback port outside `avoid`.
pub fn choose_port(preferred: Option<u16>, avoid: &[u16]) -> std::io::Result<u16> {
    let free = |port: u16| std::net::TcpListener::bind(("127.0.0.1", port)).is_ok();
    if let Some(port) = preferred.filter(|p| *p != 0 && !avoid.contains(p)) {
        if free(port) {
            return Ok(port);
        }
    }
    for _ in 0..32 {
        let port = std::net::TcpListener::bind(("127.0.0.1", 0))?
            .local_addr()?
            .port();
        if !avoid.contains(&port) && Some(port) != preferred {
            return Ok(port);
        }
    }
    Err(std::io::Error::new(
        std::io::ErrorKind::AddrNotAvailable,
        "no free loopback port",
    ))
}

/// How many restarts a child gets, and when they are given back.
#[derive(Debug, Clone)]
pub struct RestartBudget {
    max: u32,
    reset_after: Duration,
    used: u32,
    healthy_since: Option<Instant>,
}

impl RestartBudget {
    pub fn new(max: u32, reset_after: Duration) -> Self {
        RestartBudget {
            max,
            reset_after,
            used: 0,
            healthy_since: None,
        }
    }

    /// WE's policy (`executorProcess.js` `restartBudget`).
    pub fn standard() -> Self {
        RestartBudget::new(3, Duration::from_secs(5 * 60))
    }

    pub fn used(&self) -> u32 {
        self.used
    }

    /// The child answers at `now`; it has since the first such call after a start.
    pub fn healthy(&mut self, now: Instant) {
        self.healthy_since.get_or_insert(now);
    }

    /// The child died at `now`: whether it may start again. A child that was healthy for
    /// `reset_after` gets its whole budget back first.
    pub fn crashed(&mut self, now: Instant) -> bool {
        if self
            .healthy_since
            .is_some_and(|since| now.duration_since(since) >= self.reset_after)
        {
            self.used = 0;
        }
        self.healthy_since = None;
        if self.used < self.max {
            self.used += 1;
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Readiness {
    /// `GET http://127.0.0.1:<port><path>` answers 200.
    Http(String),
    /// A TCP connect to the port succeeds.
    Tcp,
}

#[derive(Debug, Clone)]
pub struct ChildSpec {
    /// For logs and the log file: `llama-server`.
    pub name: String,
    pub program: PathBuf,
    pub args: Vec<String>,
    pub env: Vec<(String, String)>,
    pub cwd: PathBuf,
    pub port: u16,
    pub readiness: Readiness,
    /// Where stdout and stderr go (appended).
    pub log_file: PathBuf,
}

#[derive(Debug, Clone)]
pub struct Timing {
    /// How long a start may take to get ready. macOS checks a fresh unsigned binary for
    /// up to about 40 s on its first run.
    pub start_timeout: Duration,
    pub probe_interval: Duration,
    /// The first restart waits this long, each next one twice as long, at most 30 s.
    pub backoff: Duration,
}

impl Default for Timing {
    fn default() -> Self {
        Timing {
            start_timeout: Duration::from_secs(if cfg!(target_os = "macos") { 120 } else { 90 }),
            probe_interval: Duration::from_secs(10),
            backoff: Duration::from_secs(1),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(rename = "LocalAiPhase")]
pub enum Phase {
    Starting,
    Running,
    Restarting,
    Stopped,
    Failed,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcessStatus {
    pub phase: Phase,
    pub pid: Option<u32>,
    pub port: u16,
    pub restarts: u32,
    pub detail: Option<String>,
}

/// A supervised child. Dropping it does not stop the child: call [`Supervised::stop`].
pub struct Supervised {
    pub spec: ChildSpec,
    status: watch::Receiver<ProcessStatus>,
    stop: watch::Sender<bool>,
    task: tokio::task::JoinHandle<()>,
}

impl Supervised {
    pub fn start(spec: ChildSpec, timing: Timing, budget: RestartBudget) -> Supervised {
        let (status_tx, status) = watch::channel(ProcessStatus {
            phase: Phase::Starting,
            pid: None,
            port: spec.port,
            restarts: 0,
            detail: None,
        });
        let (stop, stop_rx) = watch::channel(false);
        let task = tokio::spawn(supervise(spec.clone(), timing, budget, status_tx, stop_rx));
        Supervised {
            spec,
            status,
            stop,
            task,
        }
    }

    pub fn status(&self) -> ProcessStatus {
        self.status.borrow().clone()
    }

    pub fn watch(&self) -> watch::Receiver<ProcessStatus> {
        self.status.clone()
    }

    /// Wait until the child is running, or failed or stopped (the answer), or `timeout`.
    pub async fn wait_until_settled(&self, timeout: Duration) -> ProcessStatus {
        let mut status = self.status.clone();
        let settled =
            |s: &ProcessStatus| matches!(s.phase, Phase::Running | Phase::Failed | Phase::Stopped);
        let _ = tokio::time::timeout(timeout, status.wait_for(settled)).await;
        let current = status.borrow().clone();
        current
    }

    /// Ask the supervision to stop the child and not restart it, without waiting.
    pub fn request_stop(&self) {
        let _ = self.stop.send(true);
    }

    /// Stop the child (SIGTERM, then SIGKILL after 5 s) and the supervision.
    pub async fn stop(self) {
        let _ = self.stop.send(true);
        let _ = self.task.await;
    }
}

/// Probe `port` once.
pub async fn probe(port: u16, readiness: &Readiness) -> bool {
    match readiness {
        Readiness::Tcp => tokio::time::timeout(
            Duration::from_secs(1),
            tokio::net::TcpStream::connect(("127.0.0.1", port)),
        )
        .await
        .is_ok_and(|r| r.is_ok()),
        Readiness::Http(path) => {
            let Ok(client) = reqwest::Client::builder()
                .timeout(Duration::from_secs(2))
                .no_proxy()
                .build()
            else {
                return false;
            };
            client
                .get(format!("http://127.0.0.1:{port}{path}"))
                .send()
                .await
                .is_ok_and(|r| r.status() == reqwest::StatusCode::OK)
        }
    }
}

/// How a run of the child ended.
enum Ended {
    Stopped,
    Exited(String),
}

async fn supervise(
    spec: ChildSpec,
    timing: Timing,
    mut budget: RestartBudget,
    status: watch::Sender<ProcessStatus>,
    mut stop: watch::Receiver<bool>,
) {
    let set = |phase: Phase, pid: Option<u32>, detail: Option<String>, restarts: u32| {
        status.send_replace(ProcessStatus {
            phase,
            pid,
            port: spec.port,
            restarts,
            detail,
        });
    };
    loop {
        if *stop.borrow() {
            set(Phase::Stopped, None, None, budget.used());
            return;
        }
        let mut child = match spawn(&spec).await {
            Ok(child) => child,
            Err(e) => {
                // A binary that does not start will not start next time either.
                let detail = format!("{} did not start: {e}", spec.name);
                log::error!("Local AI: {detail}");
                set(Phase::Failed, None, Some(detail), budget.used());
                return;
            }
        };
        let pid = child.id();
        log::info!(
            "Local AI: {} started (pid {pid}, port {})",
            spec.name,
            spec.port
        );
        set(Phase::Starting, Some(pid), None, budget.used());

        let ended = run_once(
            &spec,
            &timing,
            &mut budget,
            &mut child,
            &mut stop,
            |phase| {
                set(phase, Some(pid), None, budget_used_snapshot(&status));
            },
        )
        .await;
        match ended {
            Ended::Stopped => {
                terminate(&mut child).await;
                log::info!("Local AI: {} stopped", spec.name);
                set(Phase::Stopped, None, None, budget.used());
                return;
            }
            Ended::Exited(reason) => {
                terminate(&mut child).await;
                log::warn!("Local AI: {} {reason}", spec.name);
                if !budget.crashed(Instant::now()) {
                    let detail = format!(
                        "{} {reason}; it was restarted {} times and stays down",
                        spec.name,
                        budget.used()
                    );
                    log::error!("Local AI: {detail}");
                    set(Phase::Failed, None, Some(detail), budget.used());
                    return;
                }
                set(Phase::Restarting, None, Some(reason), budget.used());
                let wait = timing
                    .backoff
                    .saturating_mul(1 << (budget.used().saturating_sub(1)).min(5))
                    .min(Duration::from_secs(30));
                tokio::select! {
                    _ = tokio::time::sleep(wait) => {}
                    _ = stop.wait_for(|stop| *stop) => {}
                }
            }
        }
    }
}

fn budget_used_snapshot(status: &watch::Sender<ProcessStatus>) -> u32 {
    status.borrow().restarts
}

/// One run of the child: until it exits, stops answering, or the supervisor stops it.
async fn run_once(
    spec: &ChildSpec,
    timing: &Timing,
    budget: &mut RestartBudget,
    child: &mut Child,
    stop: &mut watch::Receiver<bool>,
    phase: impl Fn(Phase),
) -> Ended {
    let started = Instant::now();
    let poll = Duration::from_millis(200);
    // Starting: until the first probe answers.
    loop {
        if *stop.borrow() {
            return Ended::Stopped;
        }
        if let Ok(Some(exit)) = child.try_wait() {
            return Ended::Exited(format!("exited while starting ({exit})"));
        }
        if probe(spec.port, &spec.readiness).await {
            break;
        }
        if started.elapsed() > timing.start_timeout {
            return Ended::Exited(format!(
                "did not answer within {} s of its start",
                timing.start_timeout.as_secs()
            ));
        }
        tokio::select! {
            _ = tokio::time::sleep(poll) => {}
            _ = stop.changed() => {}
        }
    }
    log::info!("Local AI: {} is ready on port {}", spec.name, spec.port);
    budget.healthy(Instant::now());
    phase(Phase::Running);

    // Running: watch the process, probe it now and then.
    let mut last_probe = Instant::now();
    let mut failed_probes = 0;
    loop {
        if *stop.borrow() {
            return Ended::Stopped;
        }
        if let Ok(Some(exit)) = child.try_wait() {
            return Ended::Exited(format!("exited ({exit})"));
        }
        if last_probe.elapsed() >= timing.probe_interval {
            last_probe = Instant::now();
            if probe(spec.port, &spec.readiness).await {
                failed_probes = 0;
                budget.healthy(Instant::now());
            } else {
                failed_probes += 1;
                if failed_probes >= 3 {
                    return Ended::Exited("stopped answering".into());
                }
            }
        }
        tokio::select! {
            _ = tokio::time::sleep(poll) => {}
            _ = stop.changed() => {}
        }
    }
}

/// SIGTERM, then SIGKILL after 5 s; then reap.
async fn terminate(child: &mut Child) {
    if let Ok(Some(_)) = child.try_wait() {
        return;
    }
    #[cfg(unix)]
    {
        // SAFETY: kill(2) on the pid of a child this process spawned and has not reaped.
        unsafe {
            libc::kill(child.id() as libc::pid_t, libc::SIGTERM);
        }
        let deadline = Instant::now() + Duration::from_secs(5);
        while Instant::now() < deadline {
            if let Ok(Some(_)) = child.try_wait() {
                return;
            }
            tokio::time::sleep(Duration::from_millis(100)).await;
        }
    }
    let _ = child.kill();
    let _ = child.wait();
}

/// Requests to spawn, served by one thread that lives as long as the process: Linux sends
/// a child's `PDEATHSIG` when the thread that spawned it ends, not only the process.
type SpawnRequest = (
    Command,
    tokio::sync::oneshot::Sender<std::io::Result<Child>>,
);

lazy_static! {
    static ref SPAWNER: std::sync::Mutex<std::sync::mpsc::Sender<SpawnRequest>> = {
        let (tx, rx) = std::sync::mpsc::channel::<SpawnRequest>();
        std::thread::Builder::new()
            .name("local-ai-spawner".into())
            .spawn(move || {
                for (mut command, reply) in rx {
                    let _ = reply.send(command.spawn());
                }
            })
            .expect("the local AI spawner thread starts");
        std::sync::Mutex::new(tx)
    };
}

/// The log file, emptied first when it has grown past 20 MB.
fn log_file(path: &std::path::Path) -> std::io::Result<std::fs::File> {
    if let Some(dir) = path.parent() {
        std::fs::create_dir_all(dir)?;
    }
    let too_big = path.metadata().is_ok_and(|m| m.len() > 20 * 1024 * 1024);
    std::fs::OpenOptions::new()
        .create(true)
        .append(!too_big)
        .write(true)
        .truncate(too_big)
        .open(path)
}

async fn spawn(spec: &ChildSpec) -> std::io::Result<Child> {
    let log = log_file(&spec.log_file)?;
    let mut command = Command::new(&spec.program);
    command
        .args(&spec.args)
        .current_dir(&spec.cwd)
        .stdin(Stdio::null())
        .stdout(Stdio::from(log.try_clone()?))
        .stderr(Stdio::from(log));
    for (key, value) in &spec.env {
        command.env(key, value);
    }
    #[cfg(target_os = "linux")]
    {
        use std::os::unix::process::CommandExt;
        // SAFETY: prctl is async-signal-safe; nothing else runs between fork and exec.
        unsafe {
            command.pre_exec(|| {
                libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGKILL);
                Ok(())
            });
        }
    }
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        const CREATE_NO_WINDOW: u32 = 0x0800_0000;
        command.creation_flags(CREATE_NO_WINDOW);
    }
    let (reply, answer) = tokio::sync::oneshot::channel();
    SPAWNER
        .lock()
        .map_err(|_| std::io::Error::other("the local AI spawner is gone"))?
        .send((command, reply))
        .map_err(|_| std::io::Error::other("the local AI spawner is gone"))?;
    answer
        .await
        .map_err(|_| std::io::Error::other("the local AI spawner is gone"))?
}

/// Stop processes a crashed executor left: each pid of `pids` that still runs `binary`.
pub fn reap_stale(pids: &[(u32, PathBuf)]) {
    use sysinfo::{Pid, ProcessRefreshKind, ProcessesToUpdate, System, UpdateKind};
    if pids.is_empty() {
        return;
    }
    let mut system = System::new();
    let wanted: Vec<Pid> = pids.iter().map(|(pid, _)| Pid::from_u32(*pid)).collect();
    system.refresh_processes_specifics(
        ProcessesToUpdate::Some(&wanted),
        true,
        ProcessRefreshKind::nothing().with_exe(UpdateKind::Always),
    );
    for (pid, binary) in pids {
        if let Some(process) = system.process(Pid::from_u32(*pid)) {
            if process.exe() == Some(binary.as_path()) {
                log::warn!(
                    "Local AI: stopping {} (pid {pid}) left by an earlier run",
                    binary.display()
                );
                process.kill();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_budget_allows_three_restarts_and_gives_them_back_after_healthy_minutes() {
        let start = Instant::now();
        let mut budget = RestartBudget::new(3, Duration::from_secs(300));
        assert!(budget.crashed(start));
        assert!(budget.crashed(start));
        assert!(budget.crashed(start));
        assert!(!budget.crashed(start), "a fourth crash in a row stays down");

        let mut budget = RestartBudget::new(3, Duration::from_secs(300));
        for _ in 0..3 {
            assert!(budget.crashed(start));
        }
        // Healthy for less than five minutes: nothing back.
        budget.healthy(start);
        assert!(!budget.crashed(start + Duration::from_secs(299)));

        let mut budget = RestartBudget::new(3, Duration::from_secs(300));
        for _ in 0..3 {
            assert!(budget.crashed(start));
        }
        budget.healthy(start);
        assert!(budget.crashed(start + Duration::from_secs(300)));
        assert_eq!(budget.used(), 1);
    }

    #[test]
    fn a_port_is_kept_when_free_and_never_one_to_avoid() {
        let free = choose_port(None, &[]).unwrap();
        assert_eq!(choose_port(Some(free), &[]).unwrap(), free);

        let taken = std::net::TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let taken_port = taken.local_addr().unwrap().port();
        let other = choose_port(Some(taken_port), &[]).unwrap();
        assert_ne!(other, taken_port);

        assert_ne!(choose_port(Some(free), &[free]).unwrap(), free);
        let avoid = avoided_ports();
        assert!(avoid.contains(&9931) && avoid.contains(&8080));
        for _ in 0..20 {
            assert!(!avoid.contains(&choose_port(None, &avoid).unwrap()));
        }
    }

    fn fast() -> Timing {
        Timing {
            start_timeout: Duration::from_secs(5),
            probe_interval: Duration::from_millis(100),
            backoff: Duration::from_millis(20),
        }
    }

    fn spec(dir: &std::path::Path, script: &str, port: u16) -> ChildSpec {
        ChildSpec {
            name: "fake".into(),
            program: PathBuf::from("/bin/sh"),
            args: vec!["-c".into(), script.into()],
            env: vec![],
            cwd: dir.to_path_buf(),
            port,
            readiness: Readiness::Tcp,
            log_file: dir.join("fake.log"),
        }
    }

    fn alive(pid: u32) -> bool {
        // SAFETY: signal 0 only checks that the process exists.
        unsafe { libc::kill(pid as libc::pid_t, 0) == 0 }
    }

    /// A child that crashes at once is restarted its budget's worth, then stays down,
    /// failed, saying why.
    #[cfg(unix)]
    #[tokio::test]
    async fn a_child_that_keeps_crashing_is_restarted_three_times_then_fails() {
        let dir = tempfile::tempdir().unwrap();
        let port = choose_port(None, &[]).unwrap();
        let supervised = Supervised::start(
            spec(dir.path(), "echo crashing; exit 3", port),
            fast(),
            RestartBudget::standard(),
        );
        let status = supervised.wait_until_settled(Duration::from_secs(20)).await;
        assert_eq!(status.phase, Phase::Failed);
        assert_eq!(status.restarts, 3);
        let detail = status.detail.unwrap();
        assert!(
            detail.contains("exit status: 3") && detail.contains("restarted 3 times"),
            "{detail}"
        );
        let log = std::fs::read_to_string(dir.path().join("fake.log")).unwrap();
        assert_eq!(log.matches("crashing").count(), 4, "{log}");
    }

    /// A child that is ready runs; killed from outside, it comes back; stopped, it is gone.
    #[cfg(unix)]
    #[tokio::test]
    async fn a_ready_child_runs_comes_back_after_a_kill_and_stops_with_the_supervisor() {
        let dir = tempfile::tempdir().unwrap();
        // The fake child does not listen itself: the test holds the port, so it answers.
        let listener = std::net::TcpListener::bind(("127.0.0.1", 0)).unwrap();
        let port = listener.local_addr().unwrap().port();
        let supervised = Supervised::start(
            spec(dir.path(), "exec sleep 60", port),
            fast(),
            RestartBudget::standard(),
        );
        let status = supervised.wait_until_settled(Duration::from_secs(10)).await;
        assert_eq!(status.phase, Phase::Running);
        let first = status.pid.unwrap();
        assert!(alive(first));

        unsafe {
            libc::kill(first as libc::pid_t, libc::SIGKILL);
        }
        let mut watch = supervised.watch();
        let back = tokio::time::timeout(
            Duration::from_secs(10),
            watch.wait_for(|s| s.phase == Phase::Running && s.pid != Some(first)),
        )
        .await
        .expect("it runs again")
        .unwrap()
        .clone();
        assert_eq!(back.restarts, 1);
        let second = back.pid.unwrap();

        supervised.stop().await;
        assert!(!alive(second), "the child stops with the supervisor");
        drop(listener);
    }

    /// A start that never answers counts as a crash.
    #[cfg(unix)]
    #[tokio::test]
    async fn a_child_that_never_answers_is_restarted_after_the_start_timeout() {
        let dir = tempfile::tempdir().unwrap();
        let port = choose_port(None, &[]).unwrap();
        let timing = Timing {
            start_timeout: Duration::from_millis(300),
            ..fast()
        };
        let supervised = Supervised::start(
            spec(dir.path(), "exec sleep 60", port),
            timing,
            RestartBudget::new(1, Duration::from_secs(300)),
        );
        let status = supervised.wait_until_settled(Duration::from_secs(10)).await;
        assert_eq!(status.phase, Phase::Failed);
        assert!(status.detail.unwrap().contains("did not answer"));
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn a_missing_binary_fails_without_restarts() {
        let dir = tempfile::tempdir().unwrap();
        let mut spec = spec(dir.path(), "", 1);
        spec.program = dir.path().join("no-such-server");
        let supervised = Supervised::start(spec, fast(), RestartBudget::standard());
        let status = supervised.wait_until_settled(Duration::from_secs(5)).await;
        assert_eq!(status.phase, Phase::Failed);
        assert_eq!(status.restarts, 0);
    }

    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn a_stale_child_of_an_earlier_run_is_stopped_by_its_binary() {
        let mut child = std::process::Command::new("/bin/sleep")
            .arg("60")
            .spawn()
            .unwrap();
        let pid = child.id();
        // Another binary at that pid is left alone.
        reap_stale(&[(pid, PathBuf::from("/bin/true"))]);
        assert!(child.try_wait().unwrap().is_none());
        let exe = std::fs::read_link(format!("/proc/{pid}/exe")).unwrap();
        reap_stale(&[(pid, exe)]);
        let status = child.wait().unwrap();
        assert!(!status.success());
    }
}
