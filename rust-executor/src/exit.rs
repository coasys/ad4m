//! How the executor process ends, for whatever supervises it (the desktop app, systemd, a
//! host): the exit codes, the graceful stop, the lock on the data folder, and the stop when
//! the supervisor's pipe closes.
//!
//! | Code | [`ExitReason`] | When |
//! |---|---|---|
//! | 0 | `Stopped` | `runtime.quit`, SIGTERM, SIGINT, stdin closed with `--watch-stdin` |
//! | 75 | `RestartRequested` | `runtime.restart`: start the executor again with the same arguments |
//! | 78 | `ConfigRefused` | the config file or a flag is invalid, or two settings refuse each other |
//! | 79 | `DataFolderBusy` | another executor holds the data folder's lock |
//! | 80 | `BindFailed` | the API listener cannot bind its port |
//!
//! Any other code, or a stop by a signal, is a crash.

use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use deno_core::error::AnyError;

/// Why the executor process ends. The code is the process's exit status.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(i32)]
pub enum ExitReason {
    /// A requested stop: `runtime.quit`, SIGTERM, SIGINT, or stdin closed with `--watch-stdin`.
    Stopped = 0,
    /// `runtime.restart`: the supervisor starts the executor again with the same arguments.
    /// Not zero, so systemd's `Restart=on-failure` restarts it too.
    RestartRequested = 75,
    /// The config file or a flag is invalid, or two settings refuse each other. A supervisor
    /// does not retry it: the same start fails the same way.
    ConfigRefused = 78,
    /// Another executor holds the data folder ([`DataFolderLock`]).
    DataFolderBusy = 79,
    /// The API listener cannot bind its port.
    BindFailed = 80,
}

impl ExitReason {
    pub fn code(self) -> i32 {
        self as i32
    }
}

/// A start the executor refuses, with the exit code that says why.
#[derive(Debug)]
pub struct StartRefused {
    pub reason: ExitReason,
    pub message: String,
}

impl StartRefused {
    pub fn new(reason: ExitReason, message: impl Into<String>) -> Self {
        StartRefused {
            reason,
            message: message.into(),
        }
    }
}

impl std::fmt::Display for StartRefused {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for StartRefused {}

/// The exit status of an executor whose API thread ended with `error`: the code of a
/// [`StartRefused`], else 1 (a crash).
pub fn exit_code_of(error: &AnyError) -> i32 {
    error
        .downcast_ref::<StartRefused>()
        .map_or(1, |refused| refused.reason.code())
}

/// The sender of the graceful stop: `run` sets it, and the stop exits with the reason sent.
pub(crate) static SHUTDOWN_TX: Mutex<Option<tokio::sync::oneshot::Sender<ExitReason>>> =
    Mutex::new(None);

/// Starts the graceful stop (Holochain shut down, PID file removed), which then exits with
/// `reason`'s code. A second request while one runs changes nothing. Before `run` set up the
/// stop, the process exits at once.
pub fn request_shutdown(reason: ExitReason) {
    let sender = SHUTDOWN_TX
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .take();
    match sender {
        Some(sender) => {
            let _ = sender.send(reason);
        }
        // Set up, and its sender taken: a stop runs already.
        None if STOP_SET_UP.load(std::sync::atomic::Ordering::SeqCst) => {}
        None => std::process::exit(reason.code()),
    }
}

/// Whether `run` set up the graceful stop.
static STOP_SET_UP: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Sets up the graceful stop and answers its receiver; `run` awaits it.
pub(crate) fn graceful_stop_channel() -> tokio::sync::oneshot::Receiver<ExitReason> {
    let (sender, receiver) = tokio::sync::oneshot::channel();
    *SHUTDOWN_TX
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner()) = Some(sender);
    STOP_SET_UP.store(true, std::sync::atomic::Ordering::SeqCst);
    receiver
}

/// Reads `input` until it ends or fails, then calls `on_close`. With `--watch-stdin`, `input`
/// is the executor's stdin: a supervisor holds the pipe's write end open and never writes, and
/// the operating system closes it when the supervisor ends, so the executor never outlives it.
pub fn stop_when_closed(
    mut input: impl Read + Send + 'static,
    on_close: impl FnOnce() + Send + 'static,
) -> std::thread::JoinHandle<()> {
    std::thread::Builder::new()
        .name("watch-stdin".to_string())
        .spawn(move || {
            let mut buffer = [0u8; 256];
            loop {
                match input.read(&mut buffer) {
                    Ok(0) => break,
                    Ok(_) => continue,
                    Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                    Err(_) => break,
                }
            }
            on_close();
        })
        .expect("start the stdin watcher")
}

/// The exclusive lock on a data folder: one executor per folder. Held until the process
/// ends; the operating system frees it then, also after a crash.
pub struct DataFolderLock {
    _file: File,
}

impl DataFolderLock {
    /// The lock file in the data folder.
    pub const FILE_NAME: &'static str = "executor.lock";

    /// Takes the lock on `folder` (made if missing). Another holder, in this process or
    /// another, is [`ExitReason::DataFolderBusy`]; a folder that cannot hold the file is a
    /// crash (1).
    pub fn take(folder: &Path) -> Result<DataFolderLock, AnyError> {
        std::fs::create_dir_all(folder)?;
        let path = folder.join(Self::FILE_NAME);
        let file = std::fs::OpenOptions::new()
            .create(true)
            .truncate(false)
            .write(true)
            .open(&path)?;
        match file.try_lock() {
            Ok(()) => Ok(DataFolderLock { _file: file }),
            Err(std::fs::TryLockError::WouldBlock) => Err(StartRefused::new(
                ExitReason::DataFolderBusy,
                format!(
                    "Refusing to start: another executor uses the data folder {} (it holds {}). \
                     Stop it, or start this one with another --app-data-path.",
                    folder.display(),
                    path.display()
                ),
            )
            .into()),
            Err(std::fs::TryLockError::Error(e)) => Err(e.into()),
        }
    }
}

/// The lock `run` took, kept for the life of the process.
static HELD_LOCK: Mutex<Option<(PathBuf, DataFolderLock)>> = Mutex::new(None);

/// Takes the lock on `folder` for the life of the process. A second start in the same process
/// on the same folder keeps the lock it holds.
pub(crate) fn hold_data_folder(folder: &Path) -> Result<(), AnyError> {
    let mut held = HELD_LOCK
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    if held.as_ref().is_some_and(|(path, _)| path == folder) {
        return Ok(());
    }
    let lock = DataFolderLock::take(folder)?;
    *held = Some((folder.to_path_buf(), lock));
    Ok(())
}

/// No core file and no debugger attach for the executor: its memory holds the wallet's keys
/// once unlocked. Linux only; macOS writes no core file by default.
#[cfg(target_os = "linux")]
pub fn forbid_core_dumps() -> std::io::Result<()> {
    let none = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    // SAFETY: setrlimit and prctl take plain values and change only this process.
    if unsafe { libc::setrlimit(libc::RLIMIT_CORE, &none) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    if unsafe { libc::prctl(libc::PR_SET_DUMPABLE, 0, 0, 0, 0) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_exit_codes_are_the_supervisors_contract() {
        assert_eq!(ExitReason::Stopped.code(), 0);
        assert_eq!(ExitReason::RestartRequested.code(), 75);
        assert_eq!(ExitReason::ConfigRefused.code(), 78);
        assert_eq!(ExitReason::DataFolderBusy.code(), 79);
        assert_eq!(ExitReason::BindFailed.code(), 80);
    }

    #[test]
    fn a_refused_start_exits_with_its_code_and_any_other_failure_with_1() {
        let busy: AnyError = StartRefused::new(ExitReason::DataFolderBusy, "busy").into();
        assert_eq!(exit_code_of(&busy), 79);
        let wrapped = busy.context("the API thread ended");
        assert_eq!(exit_code_of(&wrapped), 79);
        let other = deno_core::anyhow::anyhow!("something else");
        assert_eq!(exit_code_of(&other), 1);
    }

    /// `--watch-stdin`: the watcher calls back once its input ends, and not before.
    #[test]
    fn the_watcher_stops_the_executor_when_its_input_closes() {
        let (reader, writer) = std::io::pipe().expect("a pipe");
        let (closed_tx, closed_rx) = std::sync::mpsc::channel();
        let watcher = stop_when_closed(reader, move || closed_tx.send(()).unwrap());
        let mut writer = writer;
        std::io::Write::write_all(
            &mut writer,
            b"a supervisor never writes, but data is no stop",
        )
        .unwrap();
        assert!(closed_rx
            .recv_timeout(std::time::Duration::from_millis(200))
            .is_err());
        drop(writer);
        closed_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .expect("the watcher calls back when its input closes");
        watcher.join().unwrap();
    }

    /// One executor per data folder: a second taker gets `DataFolderBusy` (79) while the first
    /// holds the lock, and the lock again once the first lets go.
    #[test]
    fn a_second_executor_on_a_locked_data_folder_is_refused_with_79() {
        let folder = tempfile::tempdir().unwrap();
        let first = DataFolderLock::take(folder.path()).expect("the first taker gets the lock");
        let refused = DataFolderLock::take(folder.path())
            .err()
            .expect("the second taker is refused");
        assert_eq!(exit_code_of(&refused), ExitReason::DataFolderBusy.code());
        assert!(
            refused.to_string().contains("another executor"),
            "{refused}"
        );
        drop(first);
        DataFolderLock::take(folder.path()).expect("free again once the first lets go");
    }

    #[test]
    fn the_lock_makes_a_missing_data_folder() {
        let parent = tempfile::tempdir().unwrap();
        let folder = parent.path().join("new").join("adam");
        let _lock = DataFolderLock::take(&folder).unwrap();
        assert!(folder.join(DataFolderLock::FILE_NAME).exists());
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn the_executor_is_not_dumpable_and_writes_no_core_file() {
        forbid_core_dumps().unwrap();
        let dumpable = unsafe { libc::prctl(libc::PR_GET_DUMPABLE, 0, 0, 0, 0) };
        assert_eq!(dumpable, 0);
        let mut limit = libc::rlimit {
            rlim_cur: 1,
            rlim_max: 1,
        };
        assert_eq!(unsafe { libc::getrlimit(libc::RLIMIT_CORE, &mut limit) }, 0);
        assert_eq!((limit.rlim_cur, limit.rlim_max), (0, 0));
    }
}
