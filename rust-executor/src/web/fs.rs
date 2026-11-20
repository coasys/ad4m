//! `crate::rt::fs` on wasm32: files in memory, keyed by path, for as long as the Worker lives.
//!
//! The same signatures as the `std::fs` functions native re-exports. A Worker has one thread,
//! so the map is a thread-local; there are no directories (`create_dir_all` does nothing, and
//! a file needs no parent to exist).

use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

thread_local! {
    static FILES: RefCell<HashMap<PathBuf, Vec<u8>>> = RefCell::new(HashMap::new());
}

/// Whether a file is stored at `path`.
pub fn exists<P: AsRef<Path>>(path: P) -> io::Result<bool> {
    Ok(FILES.with(|files| files.borrow().contains_key(path.as_ref())))
}

/// The file at `path` as UTF-8; `NotFound` when there is none, `InvalidData` when it is not
/// UTF-8.
pub fn read_to_string<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let bytes = FILES
        .with(|files| files.borrow().get(path.as_ref()).cloned())
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("{}: no such file", path.as_ref().display()),
            )
        })?;
    String::from_utf8(bytes).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

/// Stores `contents` at `path`, replacing what was there. One step: no reader sees half a file.
pub fn write<P: AsRef<Path>, C: AsRef<[u8]>>(path: P, contents: C) -> io::Result<()> {
    FILES.with(|files| {
        files
            .borrow_mut()
            .insert(path.as_ref().to_path_buf(), contents.as_ref().to_vec())
    });
    Ok(())
}

/// Nothing to do: there are no directories.
pub fn create_dir_all<P: AsRef<Path>>(_path: P) -> io::Result<()> {
    Ok(())
}
