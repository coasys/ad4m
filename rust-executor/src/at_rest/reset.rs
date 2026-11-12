//! `ad4m-executor storage reset`: gives up the data a lost storage secret locked, so the
//! executor can start again from nothing.
//!
//! Without the secret nothing under the keyring opens again, but a secret that turns up later
//! still would: so nothing is deleted. Every piece of data the keyring guards, and the agent
//! made with it, moves into one folder `lost-<UTC time>` in the data folder, each at the path it
//! had. The next start makes a new keyring, and the operator makes a new agent.
//!
//! What moves ([`plan`]): the keyring file; the database file with its sidecars; the agent
//! file; the managed users' profiles; Holochain's and lair's folder; the executor's sealed
//! secrets; the perspectives' stores; and every sealed file of the languages' storage (the
//! languages' bundles, metadata and settings stay). What stays: the config, the network seed,
//! the apps' data, the logs and the data folder's lock.
//!
//! The reset runs offline: it takes the data folder's lock, so it is refused while an executor
//! runs on the folder. It is refused unless `confirm` is the keyring's id (printed by
//! `storage status` and in the log of a sealed start). It never touches a path outside the data
//! folder: a piece reached through a link to elsewhere is refused, and a link is moved as a
//! link, never followed.

use std::path::{Path, PathBuf};

use chrono::Utc;

use super::keyring_file::{self, KeyringFile};
use super::lang_storage;
use super::sqlite;

/// What confirms the reset of a data folder whose keyring file does not read.
pub const UNREADABLE_KEYRING: &str = "unreadable-keyring";

/// What a reset of a data folder would move.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResetPlan {
    /// The keyring's id, hex, or [`UNREADABLE_KEYRING`]: the confirmation the reset asks for.
    pub confirmation: String,
    /// What moves, relative to the data folder, sorted.
    pub moves: Vec<PathBuf>,
}

/// Why a reset did not run.
#[derive(Debug, thiserror::Error)]
pub enum ResetError {
    #[error("there is no keyring in {0}: nothing is sealed, there is nothing to reset")]
    NoKeyring(PathBuf),
    #[error(
        "the reset is not confirmed: run it again with `--confirm {expected}` to move the data \
         listed above into a lost- folder"
    )]
    NotConfirmed { expected: String },
    #[error("{0}")]
    Busy(String),
    #[error("{} is reached through a link to outside the data folder: refused", .0.display())]
    Outside(PathBuf),
    #[error("{}: {error}", path.display())]
    Io {
        path: PathBuf,
        error: std::io::Error,
    },
}

fn io(path: &Path) -> impl FnOnce(std::io::Error) -> ResetError + '_ {
    move |error| ResetError::Io {
        path: path.to_path_buf(),
        error,
    }
}

/// The fixed pieces a reset moves, relative to the data folder.
fn fixed_pieces() -> Vec<PathBuf> {
    let database = PathBuf::from("ad4m_db.sqlite");
    let mut pieces = vec![
        PathBuf::from("ad4m").join("keyring.json"),
        PathBuf::from("ad4m").join("agent.json"),
        PathBuf::from("ad4m").join("users"),
        PathBuf::from("ad4m").join("h"),
        PathBuf::from("ad4m").join("lan"),
        PathBuf::from("perspectives"),
    ];
    for file in [database.clone(), sqlite::migration_file(&database)] {
        for suffix in ["", "-wal", "-shm", "-journal"] {
            let mut name = file.clone().into_os_string();
            name.push(suffix);
            pieces.push(PathBuf::from(name));
        }
    }
    pieces
}

/// The keyring's id, hex; [`UNREADABLE_KEYRING`] for a keyring file that does not read.
fn confirmation(data_dir: &Path) -> Result<String, ResetError> {
    let path = keyring_file::path_in(data_dir);
    match KeyringFile::read(&path) {
        Ok(Some(file)) => Ok(file.keyring_id.to_hex()),
        Ok(None) => Err(ResetError::NoKeyring(data_dir.to_path_buf())),
        Err(keyring_file::FileError::Corrupt { .. }) => Ok(UNREADABLE_KEYRING.to_string()),
        Err(keyring_file::FileError::Io { path, error }) => Err(ResetError::Io { path, error }),
    }
}

/// Refuses `piece` (relative to `data_dir`) when the folder that holds it is reached through a
/// link to outside the data folder.
fn inside(data_dir: &Path, root: &Path, piece: &Path) -> Result<(), ResetError> {
    let full = data_dir.join(piece);
    let parent = full.parent().unwrap_or(data_dir);
    let real = parent.canonicalize().map_err(io(parent))?;
    if real.starts_with(root) {
        Ok(())
    } else {
        Err(ResetError::Outside(full))
    }
}

/// What a reset of `data_dir` would move.
pub fn plan(data_dir: &Path) -> Result<ResetPlan, ResetError> {
    let confirmation = confirmation(data_dir)?;
    let root = data_dir.canonicalize().map_err(io(data_dir))?;
    let mut moves = Vec::new();
    for piece in fixed_pieces() {
        let full = data_dir.join(&piece);
        if std::fs::symlink_metadata(&full).is_ok() {
            inside(data_dir, &root, &piece)?;
            moves.push(piece);
        }
    }
    let languages = data_dir.join("ad4m").join("languages");
    if std::fs::symlink_metadata(&languages).is_ok() {
        inside(data_dir, &root, Path::new("ad4m/languages/x"))?;
        for file in lang_storage::sealed_files(&languages).map_err(io(&languages))? {
            let piece = file
                .strip_prefix(data_dir)
                .map_err(|_| ResetError::Outside(file.clone()))?
                .to_path_buf();
            inside(data_dir, &root, &piece)?;
            moves.push(piece);
        }
    }
    moves.sort();
    Ok(ResetPlan {
        confirmation,
        moves,
    })
}

/// What a reset did.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResetReport {
    /// The folder everything moved into.
    pub lost: PathBuf,
    /// What moved, relative to the data folder (and to `lost`).
    pub moved: Vec<PathBuf>,
}

/// Moves what [`plan`] lists into `lost-<UTC time>` in `data_dir`, when `confirm` is the plan's
/// confirmation and no executor holds the data folder.
pub fn reset(data_dir: &Path, confirm: &str) -> Result<ResetReport, ResetError> {
    let _lock =
        crate::exit::DataFolderLock::take(data_dir).map_err(|e| ResetError::Busy(e.to_string()))?;
    let plan = plan(data_dir)?;
    if confirm != plan.confirmation {
        return Err(ResetError::NotConfirmed {
            expected: plan.confirmation,
        });
    }
    let lost = data_dir.join(format!("lost-{}", Utc::now().format("%Y%m%dT%H%M%S%.3fZ")));
    super::files::create_private_dir_all(&lost).map_err(io(&lost))?;
    let mut moved = Vec::new();
    for piece in plan.moves {
        let from = data_dir.join(&piece);
        let to = lost.join(&piece);
        if let Some(parent) = to.parent() {
            super::files::create_private_dir_all(parent).map_err(io(parent))?;
        }
        std::fs::rename(&from, &to).map_err(io(&from))?;
        moved.push(piece);
    }
    if let Ok(dir) = std::fs::File::open(data_dir) {
        let _ = dir.sync_all();
    }
    log::warn!(
        "Storage reset: {} pieces of sealed data moved into {}",
        moved.len(),
        lost.display()
    );
    Ok(ResetReport { lost, moved })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::at_rest::DataKeys;

    /// A data folder that ran sealed: a keyring, a keyed database with its sidecar, an agent,
    /// a user's profile, Holochain's folder, a sealed executor secret, a perspective store, and
    /// a language with a sealed storage file beside its bundle; plus what a reset keeps.
    fn sealed_data_folder(data: &Path) -> (String, PathBuf) {
        let keys = DataKeys::load(keyring_file::path_in(data)).unwrap();
        keys.unlock(zeroize::Zeroizing::new([3u8; 32])).unwrap();
        let ad4m = data.join("ad4m");
        let write = |path: PathBuf, bytes: &[u8]| {
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, bytes).unwrap();
        };
        write(data.join("ad4m_db.sqlite"), b"keyed database");
        write(data.join("ad4m_db.sqlite-wal"), b"wal");
        write(ad4m.join("agent.json"), b"{}");
        write(ad4m.join("users/alice@example.test/profile.json"), b"{}");
        write(ad4m.join("h/d/conductor.db"), b"holochain");
        write(ad4m.join("lan/key.sealed"), b"sealed secret");
        write(data.join("perspectives/some-uuid/store"), b"sparql");
        let language = ad4m.join("languages/QmLanguage");
        let sealed = lang_storage::seal(
            &keys,
            &keyring_file::StoreId::Executor,
            "QmLanguage",
            "items.json",
            b"[1,2]",
        )
        .unwrap();
        write(language.join("items.json"), &sealed);
        write(language.join("bundle.js"), b"the bundle");
        write(language.join("meta.json"), b"{}");
        write(data.join("mainnet_seed.seed"), b"{}");
        write(data.join("ad4m.log"), b"log");
        write(data.join("apps_data.json"), b"{}");
        (keys.keyring_id().to_hex(), language)
    }

    fn expected_moves() -> Vec<PathBuf> {
        let mut moves: Vec<PathBuf> = [
            "ad4m/agent.json",
            "ad4m/h",
            "ad4m/keyring.json",
            "ad4m/lan",
            "ad4m/languages/QmLanguage/items.json",
            "ad4m/users",
            "ad4m_db.sqlite",
            "ad4m_db.sqlite-wal",
            "perspectives",
        ]
        .into_iter()
        .map(PathBuf::from)
        .collect();
        moves.sort();
        moves
    }

    /// Without the keyring's id as confirmation nothing moves; with it exactly the sealed data
    /// moves into one lost- folder at the paths it had, and the rest stays.
    #[test]
    fn a_reset_moves_exactly_the_sealed_data_and_only_when_confirmed() {
        let data = tempfile::tempdir().unwrap();
        let (keyring_id, language) = sealed_data_folder(data.path());

        assert_eq!(plan(data.path()).unwrap().moves, expected_moves());
        assert_eq!(plan(data.path()).unwrap().confirmation, keyring_id);

        for wrong in ["", "yes", &"0".repeat(keyring_id.len())] {
            let refused = reset(data.path(), wrong).unwrap_err();
            assert!(
                matches!(&refused, ResetError::NotConfirmed { expected } if *expected == keyring_id),
                "{refused}"
            );
        }
        for piece in expected_moves() {
            assert!(
                data.path().join(&piece).exists(),
                "{piece:?} moved unconfirmed"
            );
        }

        let report = reset(data.path(), &keyring_id).unwrap();
        assert_eq!(report.moved, expected_moves());
        assert!(report
            .lost
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .starts_with("lost-"));
        for piece in expected_moves() {
            assert!(!data.path().join(&piece).exists(), "{piece:?} stayed");
            assert!(report.lost.join(&piece).exists(), "{piece:?} is not kept");
        }
        assert_eq!(
            std::fs::read(report.lost.join("ad4m/h/d/conductor.db")).unwrap(),
            b"holochain"
        );
        for kept in ["mainnet_seed.seed", "ad4m.log", "apps_data.json"] {
            assert!(data.path().join(kept).exists(), "{kept} moved");
        }
        assert_eq!(
            std::fs::read(language.join("bundle.js")).unwrap(),
            b"the bundle"
        );
        assert!(language.join("meta.json").exists());

        // The next start makes a new keyring: there is nothing left to reset.
        assert!(matches!(
            reset(data.path(), &keyring_id),
            Err(ResetError::NoKeyring(_))
        ));
    }

    /// A reset is refused while an executor holds the data folder.
    #[test]
    fn a_reset_is_refused_while_an_executor_runs() {
        let data = tempfile::tempdir().unwrap();
        let (keyring_id, _) = sealed_data_folder(data.path());
        let running = crate::exit::DataFolderLock::take(data.path()).unwrap();
        assert!(matches!(
            reset(data.path(), &keyring_id),
            Err(ResetError::Busy(_))
        ));
        assert!(keyring_file::path_in(data.path()).exists());
        drop(running);
        reset(data.path(), &keyring_id).unwrap();
    }

    /// A keyring file that does not read is reset with the fixed confirmation.
    #[test]
    fn an_unreadable_keyring_is_reset_with_the_fixed_confirmation() {
        let data = tempfile::tempdir().unwrap();
        sealed_data_folder(data.path());
        std::fs::write(keyring_file::path_in(data.path()), b"not json").unwrap();
        assert_eq!(plan(data.path()).unwrap().confirmation, UNREADABLE_KEYRING);
        reset(data.path(), UNREADABLE_KEYRING).unwrap();
        assert!(!keyring_file::path_in(data.path()).exists());
    }

    /// A piece reached through a link to outside the data folder is refused, and nothing
    /// outside moves.
    #[cfg(unix)]
    #[test]
    fn a_piece_behind_a_link_to_outside_is_refused() {
        let data = tempfile::tempdir().unwrap();
        let elsewhere = tempfile::tempdir().unwrap();
        let (keyring_id, _) = sealed_data_folder(data.path());
        std::fs::create_dir_all(elsewhere.path().join("inner")).unwrap();
        std::fs::write(elsewhere.path().join("inner/agent.json"), b"outside").unwrap();
        let users = data.path().join("ad4m/users");
        std::fs::remove_dir_all(&users).unwrap();
        // `ad4m/users` itself a link: moved as a link, its target untouched.
        std::os::unix::fs::symlink(elsewhere.path().join("inner"), &users).unwrap();
        let report = reset(data.path(), &keyring_id).unwrap();
        assert!(report.moved.contains(&PathBuf::from("ad4m/users")));
        assert_eq!(
            std::fs::read(elsewhere.path().join("inner/agent.json")).unwrap(),
            b"outside"
        );

        // A data folder whose `ad4m` folder is a link to elsewhere: refused.
        let linked = tempfile::tempdir().unwrap();
        let target = tempfile::tempdir().unwrap();
        sealed_data_folder(target.path());
        std::os::unix::fs::symlink(target.path().join("ad4m"), linked.path().join("ad4m")).unwrap();
        assert!(matches!(plan(linked.path()), Err(ResetError::Outside(_))));
        assert!(keyring_file::path_in(target.path()).exists());
    }
}
