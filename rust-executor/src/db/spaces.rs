//! The link-language choice of each space (`space_choice`), read by `crate::spaces`.
//!
//! A space's choice (which link language syncs it, through which server, made by whom) is
//! fixed for the life of its perspective: `neighbourhood.create` and `neighbourhood.join`
//! write it once, and a second write for the same perspective changes nothing. The choice is
//! stored as the JSON `crate::spaces::LinkLanguageChoice` serialises to, sealed under the
//! perspective's store (`sealed::SPACE_CHOICE`): it names the server and the room the
//! perspective syncs through, as `perspective_handle.shared_url` names the neighbourhood.

use rusqlite::{params, Connection, OptionalExtension};

use super::{sealed, Ad4mDb, Ad4mDbResult};

pub(crate) fn create_space_tables(conn: &Connection) -> rusqlite::Result<()> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS space_choice (
            perspective TEXT PRIMARY KEY,
            choice TEXT NOT NULL
         );",
    )
}

impl Ad4mDb {
    pub(super) fn create_space_tables(conn: &Connection) -> rusqlite::Result<()> {
        create_space_tables(conn)
    }

    /// Records `choice` (JSON) as the link-language choice of `perspective`, once: `false`,
    /// and nothing written, when the perspective has a choice already.
    pub fn record_space_choice(&self, perspective: &str, choice: &str) -> Ad4mDbResult<bool> {
        let sealed = self
            .writer(
                &sealed::SPACE_CHOICE,
                Some(perspective),
                &[perspective],
                None,
            )?
            .seal("choice", choice)?;
        let added = self.conn.execute(
            "INSERT OR IGNORE INTO space_choice (perspective, choice) VALUES (?1, ?2)",
            params![perspective, sealed],
        )?;
        Ok(added > 0)
    }

    /// The link-language choice of `perspective` (JSON), if one was recorded.
    pub fn space_choice(&self, perspective: &str) -> Ad4mDbResult<Option<String>> {
        let stored: Option<String> = self
            .conn
            .query_row(
                "SELECT choice FROM space_choice WHERE perspective = ?1",
                [perspective],
                |row| row.get(0),
            )
            .optional()?;
        let Some(stored) = stored else {
            return Ok(None);
        };
        let opened = self
            .reader(&sealed::SPACE_CHOICE, Some(perspective), &[perspective])?
            .open("choice", stored)?;
        Ok(Some(opened))
    }

    /// Forgets the choice of `perspective`: part of [`Ad4mDb::remove_perspective`].
    pub(super) fn remove_space_choice(&self, perspective: &str) -> Ad4mDbResult<()> {
        self.conn.execute(
            "DELETE FROM space_choice WHERE perspective = ?1",
            [perspective],
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_choice_is_recorded_once_and_goes_with_its_perspective() {
        let db = Ad4mDb::new(":memory:").unwrap();
        assert_eq!(db.space_choice("p1").unwrap(), None);
        assert!(db
            .record_space_choice("p1", r#"{"kind":"linkServer"}"#)
            .unwrap());
        assert!(
            !db.record_space_choice("p1", r#"{"kind":"holochain"}"#)
                .unwrap(),
            "a second choice for the same perspective is not written"
        );
        assert_eq!(
            db.space_choice("p1").unwrap().as_deref(),
            Some(r#"{"kind":"linkServer"}"#)
        );
        assert_eq!(db.space_choice("p2").unwrap(), None);

        db.remove_perspective("p1").unwrap();
        assert_eq!(db.space_choice("p1").unwrap(), None);
    }
}
