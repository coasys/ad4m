//! Files and folders only the executor's user may read: the data folder is made 0700, the
//! files the executor writes in it 0600. A mode is set where the file or folder is made, so
//! no other user can open it in the moment between its creation and a later `chmod`.

use std::io::{self, Write};
use std::path::Path;

/// The mode of a folder the executor makes in its data folder.
pub const DIR_MODE: u32 = 0o700;
/// The mode of a file the executor writes in its data folder.
pub const FILE_MODE: u32 = 0o600;

/// Makes `path` and any missing parent, each new one with mode 0700. A folder that exists
/// keeps its mode: [`close_if_open`] closes the data folder itself.
pub fn create_private_dir_all(path: &Path) -> io::Result<()> {
    let mut builder = std::fs::DirBuilder::new();
    builder.recursive(true);
    #[cfg(unix)]
    std::os::unix::fs::DirBuilderExt::mode(&mut builder, DIR_MODE);
    builder.create(path)
}

/// Opens `path` to write, made with mode 0600 when it does not exist yet. With `new_only`
/// an existing file is an error (`AlreadyExists`).
pub fn open_private_file(path: &Path, new_only: bool) -> io::Result<std::fs::File> {
    let mut options = std::fs::OpenOptions::new();
    options.write(true);
    if new_only {
        options.create_new(true);
    } else {
        options.create(true).truncate(true);
    }
    #[cfg(unix)]
    std::os::unix::fs::OpenOptionsExt::mode(&mut options, FILE_MODE);
    options.open(path)
}

/// Sets `path`'s mode to 0700 (a folder) or 0600 (a file). For the files and folders a
/// library makes with the default mode (the SPARQL stores).
pub fn restrict(path: &Path) -> io::Result<()> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mode = if path.is_dir() { DIR_MODE } else { FILE_MODE };
        std::fs::set_permissions(path, std::fs::Permissions::from_mode(mode))?;
    }
    #[cfg(not(unix))]
    let _ = path;
    Ok(())
}

/// The mode bits of `path` that let group or others in, if any (unix).
pub fn open_bits(path: &Path) -> io::Result<u32> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        Ok(std::fs::metadata(path)?.permissions().mode() & 0o077)
    }
    #[cfg(not(unix))]
    {
        let _ = path;
        Ok(0)
    }
}

/// Sets the executor's own data folder to 0700 when it lets group or others in: one made by
/// an older executor, by hand, or by an earlier step with the default mode. A folder whose
/// mode cannot be changed is logged.
pub fn close_if_open(path: &Path) {
    match open_bits(path) {
        Ok(0) => {}
        Ok(bits) => match restrict(path) {
            Ok(()) => log::info!(
                "{} let other users of this machine in (mode bits {bits:03o} for group and \
                 others); it is now 0700",
                path.display()
            ),
            Err(e) => log::warn!(
                "{} can be read by other users of this machine (mode bits {bits:03o} for \
                 group and others) and could not be restricted: {e}; restrict it with chmod 700",
                path.display()
            ),
        },
        Err(e) => log::warn!("Could not read the mode of {}: {e}", path.display()),
    }
}

/// Writes `bytes` to `path` so that a reader sees the old content or the new, never part of
/// either: a temporary file beside it (mode 0600), flushed to disk, renamed over `path`, then
/// the folder flushed so the rename itself survives a crash. On any failure the temporary
/// file is removed and `path` keeps its old content.
pub fn write_atomic(path: &Path, bytes: &[u8]) -> io::Result<()> {
    write_atomic_with(path, bytes, |from, to| std::fs::rename(from, to))
}

/// [`write_atomic`] with the step that puts the new file in place given: tests make it fail.
pub(crate) fn write_atomic_with(
    path: &Path,
    bytes: &[u8],
    rename: impl FnOnce(&Path, &Path) -> io::Result<()>,
) -> io::Result<()> {
    let folder = path
        .parent()
        .ok_or_else(|| io::Error::other("the file has no folder"))?;
    let name = path
        .file_name()
        .ok_or_else(|| io::Error::other("the path names no file"))?
        .to_string_lossy();
    let temp = folder.join(format!(".{name}.{}.tmp", uuid::Uuid::new_v4().simple()));
    let written = (|| {
        let mut file = open_private_file(&temp, true)?;
        file.write_all(bytes)?;
        file.sync_all()?;
        drop(file);
        rename(&temp, path)
    })();
    if let Err(e) = written {
        let _ = std::fs::remove_file(&temp);
        return Err(e);
    }
    sync_dir(folder)
}

/// Flushes a folder's entries to disk (unix); a no-op elsewhere.
fn sync_dir(folder: &Path) -> io::Result<()> {
    #[cfg(unix)]
    std::fs::File::open(folder)?.sync_all()?;
    #[cfg(not(unix))]
    let _ = folder;
    Ok(())
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;

    fn mode(path: &Path) -> u32 {
        std::fs::metadata(path).unwrap().permissions().mode() & 0o777
    }

    #[test]
    fn new_folders_are_0700_and_new_files_0600() {
        let root = tempfile::tempdir().unwrap();
        let nested = root.path().join("a").join("b");
        create_private_dir_all(&nested).unwrap();
        assert_eq!(mode(&root.path().join("a")), 0o700);
        assert_eq!(mode(&nested), 0o700);
        let file = nested.join("f");
        open_private_file(&file, true).unwrap();
        assert_eq!(mode(&file), 0o600);
        write_atomic(&nested.join("g"), b"x").unwrap();
        assert_eq!(mode(&nested.join("g")), 0o600);
    }

    #[test]
    fn restrict_closes_a_folder_and_a_file_made_with_the_default_mode() {
        let root = tempfile::tempdir().unwrap();
        let folder = root.path().join("store");
        std::fs::create_dir(&folder).unwrap();
        std::fs::set_permissions(&folder, std::fs::Permissions::from_mode(0o755)).unwrap();
        let file = folder.join("data");
        std::fs::write(&file, b"x").unwrap();
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o644)).unwrap();
        assert_eq!(open_bits(&folder).unwrap(), 0o055);
        restrict(&folder).unwrap();
        restrict(&file).unwrap();
        assert_eq!(mode(&folder), 0o700);
        assert_eq!(mode(&file), 0o600);
        assert_eq!(open_bits(&folder).unwrap(), 0);
    }

    /// The data folder is 0700 even when an earlier step, or an older executor, made it with
    /// the default mode; a closed one is left alone.
    #[test]
    fn the_data_folder_is_closed_when_it_was_made_open() {
        let root = tempfile::tempdir().unwrap();
        let folder = root.path().join("data");
        std::fs::create_dir(&folder).unwrap();
        std::fs::set_permissions(&folder, std::fs::Permissions::from_mode(0o775)).unwrap();
        close_if_open(&folder);
        assert_eq!(mode(&folder), 0o700);
        close_if_open(&folder);
        assert_eq!(mode(&folder), 0o700);
    }

    #[test]
    fn a_failed_write_leaves_the_old_content_and_no_temporary_file() {
        let root = tempfile::tempdir().unwrap();
        let path = root.path().join("keyring.json");
        write_atomic(&path, b"old").unwrap();
        let failed = write_atomic_with(&path, b"new", |_, _| Err(io::Error::other("disk full")));
        assert!(failed.is_err());
        assert_eq!(std::fs::read(&path).unwrap(), b"old");
        let names: Vec<_> = std::fs::read_dir(root.path())
            .unwrap()
            .map(|entry| entry.unwrap().file_name())
            .collect();
        assert_eq!(names, vec![std::ffi::OsString::from("keyring.json")]);
    }
}
