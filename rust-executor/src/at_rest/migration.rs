//! The move of a plaintext executor's data under its keys: one ordered, resumable run at the
//! first unlock of an executor that ran before at-rest encryption.
//!
//! The steps run in a fixed order ([`MigrationStep::ALL`]), each through the module that owns
//! its store:
//!
//! 1. [`MigrationStep::Database`]: the database file moves under the database key and its
//!    sealed columns are sealed ([`database::key_file`]). Every store the rows belong to is
//!    made on the way, the perspectives' included.
//! 2. [`MigrationStep::Persons`]: every managed user in the database gets its person and its
//!    person store ([`persons::prepare`]).
//! 3. [`MigrationStep::LanguageStorage`]: every plaintext file of the languages' storage
//!    folders is sealed under the key of the store its language serves
//!    ([`lang_storage::migrate_with`]). The link-language routing is read from the database
//!    first, so a link language's files go under its neighbourhood's perspective store.
//!
//! The keyring's migration record ([`MigrationRecord`]) is written before the first step and
//! after each one. A start that finds it unfinished runs the steps it does not name; a start
//! that finds plaintext data (a plaintext database file, a plaintext language file) begins a
//! new run. Nothing else runs it: on an executor with neither, [`run`] does nothing.
//!
//! Each step is safe to run again after a kill at any point: the database file is replaced
//! only by a keyed copy that was checked (row counts, `user_version`, `quick_check`), and a
//! keyed file is never moved again; a language file is replaced only by a sealed copy that
//! opens to what the file held, and a sealed file is never sealed again; a person is made only
//! when missing. The plaintext stays until its sealed copy is checked.
//!
//! The gate is [`StorageState::Migrating`](super::StorageState::Migrating) meanwhile and
//! answers only the status and the unlock. A failure keeps the plaintext and sends the gate
//! back to sealed with `migration-failed`; the next unlock runs the steps not done.

use std::collections::BTreeSet;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};

use chrono::Utc;

use super::database::{self, DatabaseStep};
use super::keyring_file::{MigrationRecord, MigrationStep};
use super::lang_storage::{self, LanguageStorageStep};
use super::sqlite::{self, DatabaseFile};
use super::{persons, DataKeys, StorageGate, StorageMigrationProgress};
use crate::db::Ad4mDb;
use crate::types::PerspectiveHandle;

/// At most one `storage-migration-progress` a second, the first and the last always sent.
pub const PROGRESS_INTERVAL: Duration = Duration::from_secs(1);

/// The files the migration moves, in a data folder.
#[derive(Clone, Debug)]
pub struct Paths {
    pub database: PathBuf,
    pub languages: PathBuf,
}

impl Paths {
    /// The executor's files in the data folder `data_dir`.
    pub fn in_data_dir(data_dir: &Path) -> Self {
        Paths {
            database: data_dir.join("ad4m_db.sqlite"),
            languages: data_dir.join("ad4m").join("languages"),
        }
    }
}

/// What [`run`] did.
#[derive(Debug, PartialEq, Eq)]
pub enum Outcome {
    /// No plaintext data and no unfinished record: nothing ran, nothing was written.
    NothingToDo,
    /// These steps ran and every store is under its key.
    Migrated { steps: Vec<MigrationStep> },
}

/// Why the migration stopped: the plaintext of the step that failed, and of the steps after
/// it, stays as it was.
#[derive(Debug, thiserror::Error)]
#[error("the move of the plaintext data under its keys failed{}: {error}", step.map(|s| format!(" at the {s:?} step")).unwrap_or_default())]
pub struct MigrationFailure {
    pub step: Option<MigrationStep>,
    pub error: String,
}

/// What a run has to do.
#[derive(Debug, PartialEq, Eq)]
pub struct Plan {
    /// The steps to run, in order; empty when there is nothing to do.
    pub pending: Vec<MigrationStep>,
    /// The language folders that hold a plaintext file.
    pub language_folders: Vec<String>,
}

/// What the migration of `paths` has to do, from the keyring's record and the files.
pub fn plan(keys: &DataKeys, paths: &Paths) -> Result<Plan, String> {
    let record = keys
        .migration()
        .map_err(|e| format!("Could not read the migration record: {e}"))?;
    let unfinished = record.filter(|r| !r.finished());
    let database_plain = matches!(
        sqlite::database_file(&paths.database)
            .map_err(|e| format!("Could not read {}: {e}", paths.database.display()))?,
        DatabaseFile::Plaintext
    );
    let language_folders = lang_storage::plaintext_folders(&paths.languages)
        .map_err(|e| format!("Could not list {}: {e}", paths.languages.display()))?;
    let pending = if unfinished.is_none() && !database_plain && language_folders.is_empty() {
        vec![]
    } else {
        let done: BTreeSet<MigrationStep> = unfinished
            .map(|r| r.done.into_iter().collect())
            .unwrap_or_default();
        MigrationStep::ALL
            .into_iter()
            .filter(|step| !done.contains(step))
            .collect()
    };
    Ok(Plan {
        pending,
        language_folders,
    })
}

/// Sends the progress to the status and, spaced out, to `storage-migration-progress`.
pub(crate) struct Progress<'a> {
    gate: &'a StorageGate,
    every: Duration,
    last_sent: Option<Instant>,
    send: Box<dyn FnMut(&StorageMigrationProgress) + 'a>,
}

impl<'a> Progress<'a> {
    /// Progress published to the operator as `storage-migration-progress`.
    pub(crate) fn published(gate: &'a StorageGate) -> Self {
        Self::with(gate, PROGRESS_INTERVAL, |progress| {
            crate::pubsub::get_global_pubsub_sync().publish_event(crate::events::Event::new(
                crate::events::Topic::StorageMigrationProgress,
                crate::events::Audience::Operator,
                progress.clone(),
            ))
        })
    }

    /// Progress sent to `send`, at most once every `every` but for the first and the last.
    pub(crate) fn with(
        gate: &'a StorageGate,
        every: Duration,
        send: impl FnMut(&StorageMigrationProgress) + 'a,
    ) -> Self {
        Progress {
            gate,
            every,
            last_sent: None,
            send: Box::new(send),
        }
    }

    fn begin(&mut self, progress: StorageMigrationProgress) {
        self.gate.begin_migration(progress.clone());
        self.send_now(progress);
    }

    fn report(&mut self, progress: StorageMigrationProgress) {
        self.gate.set_migration_progress(progress.clone());
        if self.last_sent.is_none_or(|at| at.elapsed() >= self.every) {
            self.send_now(progress);
        }
    }

    fn finish(&mut self, progress: StorageMigrationProgress) {
        self.gate.set_migration_progress(progress.clone());
        self.send_now(progress);
    }

    fn send_now(&mut self, progress: StorageMigrationProgress) {
        (self.send)(&progress);
        self.last_sent = Some(Instant::now());
    }
}

/// Where a test stops a run, standing for a kill.
pub(crate) struct Hooks<'a> {
    /// Called once a step's work is done, before the record names it.
    pub after_step: &'a dyn Fn(MigrationStep) -> Result<(), String>,
    /// Puts each sealed language file in place.
    pub rename: &'a dyn Fn(&Path, &Path) -> io::Result<()>,
}

impl Default for Hooks<'_> {
    fn default() -> Self {
        Hooks {
            after_step: &|_| Ok(()),
            rename: &|from, to| std::fs::rename(from, to),
        }
    }
}

/// Moves the plaintext data of `paths` under the keys of `gate`'s keyring, which must be open.
/// Does nothing when [`plan`] finds nothing to do. On failure the gate is sealed with
/// `migration-failed` and the plaintext stays.
pub fn run(gate: &StorageGate, paths: &Paths) -> Result<Outcome, MigrationFailure> {
    run_with(
        gate,
        paths,
        &mut Progress::published(gate),
        &Hooks::default(),
    )
}

/// [`run`] with the progress sink and the test hooks given.
pub(crate) fn run_with(
    gate: &StorageGate,
    paths: &Paths,
    progress: &mut Progress,
    hooks: &Hooks,
) -> Result<Outcome, MigrationFailure> {
    let keys = gate.keys();
    let fail = |step: Option<MigrationStep>, error: String| {
        let failure = MigrationFailure { step, error };
        log::error!("{failure}; the plaintext stays and the next unlock tries again");
        gate.migration_failed();
        failure
    };
    let plan = plan(keys, paths).map_err(|e| fail(None, e))?;
    let Some(&first) = plan.pending.first() else {
        return Ok(Outcome::NothingToDo);
    };
    let mut record = match keys.migration() {
        Ok(Some(record)) if !record.finished() => record,
        _ => MigrationRecord {
            started_at: Utc::now(),
            done: vec![],
            finished_at: None,
        },
    };
    keys.set_migration(record.clone())
        .map_err(|e| fail(None, format!("Could not write the migration record: {e}")))?;
    log::info!(
        "Moving this executor's plaintext data under its keys: {:?}",
        plan.pending
    );

    let units = |step: MigrationStep| match step {
        MigrationStep::Database | MigrationStep::Persons => 1,
        MigrationStep::LanguageStorage => plan.language_folders.len().max(1) as u32,
    };
    let total: u32 = MigrationStep::ALL.into_iter().map(units).sum();
    let mut done: u32 = MigrationStep::ALL
        .into_iter()
        .filter(|s| !plan.pending.contains(s))
        .map(units)
        .sum();
    let at = |step, done, store: Option<String>| StorageMigrationProgress {
        step,
        stores_done: done,
        stores_total: total,
        store,
    };
    progress.begin(at(first, done, Some(store_name(first, &plan))));

    for &step in &plan.pending {
        // The first step's start went out with the beginning.
        if step != first {
            progress.report(at(step, done, Some(store_name(step, &plan))));
        }
        match step {
            MigrationStep::Database => {
                match database::key_file(keys, &paths.database, Some(gate)) {
                    Ok(DatabaseStep::Keyed(_)) => {}
                    Ok(DatabaseStep::KeptPlain(e)) => {
                        return Err(fail(Some(step), e.to_string()));
                    }
                    Err(e) => return Err(fail(Some(step), e)),
                }
                done += 1;
            }
            MigrationStep::Persons => {
                let usernames = match keyed_database(keys, &paths.database) {
                    Ok(Some(db)) => db
                        .list_users()
                        .map_err(|e| {
                            fail(Some(step), format!("Could not list the managed users: {e}"))
                        })?
                        .into_iter()
                        .map(|u| u.username)
                        .collect(),
                    Ok(None) => vec![],
                    Err(e) => return Err(fail(Some(step), e)),
                };
                let made = persons::prepare(keys, &usernames).map_err(|e| fail(Some(step), e))?;
                log::info!("Made the data keys of {made} managed users");
                done += 1;
            }
            MigrationStep::LanguageStorage => {
                match keyed_database(keys, &paths.database) {
                    Ok(Some(db)) => match db.get_all_perspectives() {
                        Ok(handles) => route_link_languages(&handles),
                        Err(e) => {
                            return Err(fail(
                                Some(step),
                                format!("Could not list the perspectives: {e}"),
                            ))
                        }
                    },
                    Ok(None) => {}
                    Err(e) => return Err(fail(Some(step), e)),
                }
                let start = done;
                // The first folder's start went out with the step's.
                let mut current: Option<String> = plan.language_folders.first().cloned();
                let sealed = lang_storage::migrate_with(
                    keys,
                    &paths.languages,
                    Some(gate),
                    hooks.rename,
                    |address| {
                        if current.as_deref() == Some(address) {
                            return;
                        }
                        if current.is_some() {
                            done += 1;
                        }
                        current = Some(address.to_string());
                        progress.report(at(step, done, Some(format!("language:{address}"))));
                    },
                );
                match sealed {
                    Ok(LanguageStorageStep::Sealed { sealed }) => {
                        log::info!("Sealed {sealed} files of the languages' storage");
                    }
                    Ok(LanguageStorageStep::KeptPlain { error, .. }) | Err(error) => {
                        return Err(fail(Some(step), error));
                    }
                }
                done = start + units(step);
            }
        }
        (hooks.after_step)(step).map_err(|e| fail(Some(step), e))?;
        record.done.push(step);
        keys.set_migration(record.clone()).map_err(|e| {
            fail(
                Some(step),
                format!("Could not write the migration record: {e}"),
            )
        })?;
    }

    record.finished_at = Some(Utc::now());
    keys.set_migration(record)
        .map_err(|e| fail(None, format!("Could not write the migration record: {e}")))?;
    let last = *plan.pending.last().unwrap_or(&first);
    progress.finish(at(last, total, None));
    gate.end_migration();
    log::info!("Every store of this executor is under its key");
    Ok(Outcome::Migrated {
        steps: plan.pending,
    })
}

/// The store a step starts with, as the progress names it.
fn store_name(step: MigrationStep, plan: &Plan) -> String {
    match step {
        MigrationStep::Database => "executor".into(),
        MigrationStep::Persons => "persons".into(),
        MigrationStep::LanguageStorage => plan
            .language_folders
            .first()
            .map_or_else(|| "languages".into(), |a| format!("language:{a}")),
    }
}

/// A connection of the migration's own to the keyed database; `None` while there is no
/// database file. The process's database is not open yet.
fn keyed_database(keys: &Arc<DataKeys>, path: &Path) -> Result<Option<Ad4mDb>, String> {
    let file = sqlite::database_file(path)
        .map_err(|e| format!("Could not read {}: {e}", path.display()))?;
    match file {
        DatabaseFile::New => Ok(None),
        DatabaseFile::Plaintext => Err(format!(
            "The database {} is still in the clear",
            path.display()
        )),
        DatabaseFile::Encrypted => {
            let key = sqlite::database_key(keys).map_err(|e| e.to_string())?;
            Ad4mDb::open(path, Some((&key, keys.clone())))
                .map(Some)
                .map_err(|e| format!("Could not open the database {}: {e}", path.display()))
        }
    }
}

/// Tells the link-language routing which perspective each neighbourhood's link language
/// serves, so the languages' storage seals a link language's files under its perspective.
pub(crate) fn route_link_languages(handles: &[PerspectiveHandle]) {
    for handle in handles {
        if let Some(nh) = handle.neighbourhood.as_ref() {
            crate::perspectives::register_link_language_perspective(
                nh.data.link_language.clone(),
                handle.clone(),
            );
        }
    }
}

#[cfg(test)]
#[path = "migration_tests.rs"]
mod tests;
