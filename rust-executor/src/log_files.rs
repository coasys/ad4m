//! The executor's own log files (`--log-dir`): one file per run, the newest runs kept, each
//! file capped in size, and the newest lines read back for `runtime.logTail`.
//!
//! ```text
//! <log_dir>/executor-0.log   this run
//! <log_dir>/executor-1.log   the run before
//! ...
//! <log_dir>/executor-4.log   the oldest run kept
//! ```

use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

/// The runs whose files stay: this one and the four before it.
pub const KEPT_RUNS: usize = 5;
/// A run's file stops growing here: a loop that logs without end cannot fill the disk.
pub const MAX_FILE_BYTES: u64 = 50 * 1024 * 1024;
/// The most lines `runtime.logTail` returns.
pub const MAX_TAIL_LINES: usize = 2000;
/// How far back from the end a tail reads: enough for [`MAX_TAIL_LINES`] long lines.
const MAX_TAIL_BYTES: u64 = 4 * 1024 * 1024;

/// The file of the run `index` runs ago (0: this run).
pub fn log_file(dir: &Path, index: usize) -> PathBuf {
    dir.join(format!("executor-{index}.log"))
}

/// Moves each kept run's file one index up and drops the oldest, so `executor-0.log` is free
/// for a new run.
pub fn rotate(dir: &Path, keep: usize) -> io::Result<()> {
    if keep == 0 {
        return Ok(());
    }
    let oldest = log_file(dir, keep - 1);
    if oldest.exists() {
        std::fs::remove_file(&oldest)?;
    }
    for index in (0..keep - 1).rev() {
        let from = log_file(dir, index);
        if from.exists() {
            std::fs::rename(&from, log_file(dir, index + 1))?;
        }
    }
    Ok(())
}

/// A run's log file: appends until [`MAX_FILE_BYTES`], then writes one line that says so and
/// drops the rest of the run's lines.
pub struct LogFile {
    file: File,
    written: u64,
    max_bytes: u64,
    capped: bool,
}

impl LogFile {
    /// Makes `dir` if missing, rotates the runs before, and opens this run's file.
    pub fn open(dir: &Path) -> io::Result<LogFile> {
        Self::open_with(dir, KEPT_RUNS, MAX_FILE_BYTES)
    }

    fn open_with(dir: &Path, keep: usize, max_bytes: u64) -> io::Result<LogFile> {
        std::fs::create_dir_all(dir)?;
        rotate(dir, keep)?;
        let file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(log_file(dir, 0))?;
        Ok(LogFile {
            file,
            written: 0,
            max_bytes,
            capped: false,
        })
    }
}

impl Write for LogFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.capped {
            return Ok(buf.len());
        }
        if self.written + buf.len() as u64 > self.max_bytes {
            self.capped = true;
            self.file.write_all(
                b"[log size limit reached: the rest of this run is not written to this file]\n",
            )?;
            return Ok(buf.len());
        }
        self.file.write_all(buf)?;
        self.written += buf.len() as u64;
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        self.file.flush()
    }
}

/// The folder this run logs to, set once its file is open.
static LOG_DIR: OnceLock<PathBuf> = OnceLock::new();

/// Records that this run logs to `dir`, for [`tail`].
pub(crate) fn set_log_dir(dir: &Path) {
    let _ = LOG_DIR.set(dir.to_path_buf());
}

/// The folder this run logs to; `None` when it logs to stdout.
pub fn log_dir() -> Option<&'static Path> {
    LOG_DIR.get().map(PathBuf::as_path)
}

/// The newest `lines` lines (at most [`MAX_TAIL_LINES`]) of this run's file in `dir`, oldest
/// first. Reads at most the last 4 MiB of the file.
pub fn tail(dir: &Path, lines: usize) -> io::Result<Vec<String>> {
    let lines = lines.min(MAX_TAIL_LINES);
    if lines == 0 {
        return Ok(Vec::new());
    }
    let mut file = File::open(log_file(dir, 0))?;
    let size = file.metadata()?.len();
    let start = size.saturating_sub(MAX_TAIL_BYTES);
    file.seek(SeekFrom::Start(start))?;
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes)?;
    let text = String::from_utf8_lossy(&bytes);
    let mut all: Vec<&str> = text.lines().collect();
    // A read that starts inside the file may start inside a line: drop that part line.
    if start > 0 && !all.is_empty() {
        all.remove(0);
    }
    let from = all.len().saturating_sub(lines);
    Ok(all[from..].iter().map(|line| line.to_string()).collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read(dir: &Path, index: usize) -> String {
        std::fs::read_to_string(log_file(dir, index)).unwrap_or_default()
    }

    #[test]
    fn each_run_gets_a_new_file_and_the_five_newest_runs_stay() {
        let dir = tempfile::tempdir().unwrap();
        for run in 0..7 {
            let mut file = LogFile::open(dir.path()).unwrap();
            writeln!(file, "run {run}").unwrap();
        }
        assert_eq!(read(dir.path(), 0), "run 6\n");
        assert_eq!(read(dir.path(), 4), "run 2\n");
        assert!(!log_file(dir.path(), 5).exists());
    }

    #[test]
    fn a_file_stops_at_its_size_limit_and_says_so_once() {
        let dir = tempfile::tempdir().unwrap();
        let mut file = LogFile::open_with(dir.path(), KEPT_RUNS, 16).unwrap();
        file.write_all(b"0123456789\n").unwrap();
        file.write_all(b"0123456789\n").unwrap();
        file.write_all(b"0123456789\n").unwrap();
        let text = read(dir.path(), 0);
        assert!(
            text.starts_with("0123456789\n[log size limit reached"),
            "{text}"
        );
        assert_eq!(text.matches("limit reached").count(), 1);
    }

    #[test]
    fn the_tail_is_the_newest_lines_oldest_first_and_at_most_2000() {
        let dir = tempfile::tempdir().unwrap();
        let mut file = LogFile::open(dir.path()).unwrap();
        for n in 0..2500 {
            writeln!(file, "line {n}").unwrap();
        }
        assert_eq!(
            tail(dir.path(), 3).unwrap(),
            vec!["line 2497", "line 2498", "line 2499"]
        );
        let most = tail(dir.path(), 5000).unwrap();
        assert_eq!(most.len(), MAX_TAIL_LINES);
        assert_eq!(most[0], "line 500");
        assert!(tail(dir.path(), 0).unwrap().is_empty());
    }

    #[test]
    fn the_logger_makes_a_missing_log_folder() {
        let parent = tempfile::tempdir().unwrap();
        let dir = parent.path().join("adam").join("logs");
        LogFile::open(&dir).unwrap();
        assert!(log_file(&dir, 0).exists());
    }
}
