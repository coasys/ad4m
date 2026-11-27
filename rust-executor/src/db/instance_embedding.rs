//! The `instance_embedding` table: the vector of an instance's title, for one embedding model.
//!
//! The key is (perspective, model, instance id, sha256 of the embedded text). A row whose
//! hash is not the hash of the instance's title now is stale: the reader embeds the title
//! again and [`Ad4mDb::put_instance_embeddings`] replaces every row of that instance and model.
//! `perspectives::interpretation::embedding_cache` is the one reader and writer.
//!
//! `vector` is TEXT (base64 of the little-endian `f32`s), not a BLOB, for the sealing at rest
//! of #1691 (not merged): it seals a perspective's data as text, under the perspective's
//! store key, and a sealed column is never in a `WHERE`. `vector` is the only column that
//! holds the perspective's data in a form that can give back its text, and the reads look
//! rows up by the key columns only. When #1691 merges, this table joins its
//! `SEALED_TABLES`: owner `perspective`, key the four key columns, sealed column `vector`.
//!
//! TODO(#1804): the rows of a removed instance stay until its perspective is removed. No
//! single hook sees every removal (`instance_remove`, the SDK's `removeSubject`, raw link
//! removals, a peer's synced diff), so this needs a sweep: drop the rows whose instance id
//! no longer has a title in the perspective. Search never returns them: its candidates come
//! from the live graph.

use rusqlite::{params, Connection};

use super::{Ad4mDb, Ad4mDbResult};

/// The most ids one `IN (…)` lookup binds: well under SQLite's bound-parameter limit.
const IDS_PER_LOOKUP: usize = 500;

/// One cached vector: the instance, the sha256 (hex) of the text it embeds, the vector.
#[derive(Debug, Clone, PartialEq)]
pub struct InstanceEmbeddingRow {
    pub instance_id: String,
    pub text_sha256: String,
    pub vector: Vec<f32>,
}

pub(super) fn create_instance_embedding_table(conn: &Connection) -> rusqlite::Result<()> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS instance_embedding (
            perspective TEXT NOT NULL,
            model TEXT NOT NULL,
            instance_id TEXT NOT NULL,
            text_sha256 TEXT NOT NULL,
            vector TEXT NOT NULL,
            PRIMARY KEY (perspective, model, instance_id, text_sha256)
        )",
    )
}

fn encode_vector(vector: &[f32]) -> String {
    use base64::Engine;
    let bytes: Vec<u8> = vector.iter().flat_map(|f| f.to_le_bytes()).collect();
    base64::engine::general_purpose::STANDARD.encode(bytes)
}

fn decode_vector(text: &str) -> Option<Vec<f32>> {
    use base64::Engine;
    let bytes = base64::engine::general_purpose::STANDARD
        .decode(text)
        .ok()?;
    if bytes.len() % 4 != 0 {
        return None;
    }
    Some(
        bytes
            .chunks_exact(4)
            .map(|c| f32::from_le_bytes([c[0], c[1], c[2], c[3]]))
            .collect(),
    )
}

impl Ad4mDb {
    /// The cached vectors of `instance_ids` on `perspective` for `model`, every hash. A row
    /// whose vector does not decode is left out (the reader embeds that title again).
    pub fn get_instance_embeddings(
        &self,
        perspective: &str,
        model: &str,
        instance_ids: &[String],
    ) -> Ad4mDbResult<Vec<InstanceEmbeddingRow>> {
        let mut out = Vec::new();
        for ids in instance_ids.chunks(IDS_PER_LOOKUP) {
            let marks = vec!["?"; ids.len()].join(", ");
            let sql = format!(
                "SELECT instance_id, text_sha256, vector FROM instance_embedding
                 WHERE perspective = ? AND model = ? AND instance_id IN ({marks})"
            );
            let mut stmt = self.conn.prepare(&sql)?;
            let mut bound: Vec<&dyn rusqlite::ToSql> = vec![&perspective, &model];
            bound.extend(ids.iter().map(|id| id as &dyn rusqlite::ToSql));
            let rows = stmt.query_map(bound.as_slice(), |row| {
                Ok((
                    row.get::<_, String>(0)?,
                    row.get::<_, String>(1)?,
                    row.get::<_, String>(2)?,
                ))
            })?;
            for row in rows {
                let (instance_id, text_sha256, vector) = row?;
                if let Some(vector) = decode_vector(&vector) {
                    out.push(InstanceEmbeddingRow {
                        instance_id,
                        text_sha256,
                        vector,
                    });
                }
            }
        }
        Ok(out)
    }

    /// Store `rows` for `model` on `perspective`. Each replaces every row of its instance and
    /// model, so a title that changed leaves no stale vector behind.
    pub fn put_instance_embeddings(
        &self,
        perspective: &str,
        model: &str,
        rows: &[InstanceEmbeddingRow],
    ) -> Ad4mDbResult<()> {
        let tx = self.conn.unchecked_transaction()?;
        {
            let mut delete = tx.prepare(
                "DELETE FROM instance_embedding
                 WHERE perspective = ?1 AND model = ?2 AND instance_id = ?3",
            )?;
            let mut insert = tx.prepare(
                "INSERT INTO instance_embedding
                 (perspective, model, instance_id, text_sha256, vector)
                 VALUES (?1, ?2, ?3, ?4, ?5)",
            )?;
            for row in rows {
                delete.execute(params![perspective, model, row.instance_id])?;
                insert.execute(params![
                    perspective,
                    model,
                    row.instance_id,
                    row.text_sha256,
                    encode_vector(&row.vector)
                ])?;
            }
        }
        tx.commit()?;
        Ok(())
    }

    /// Every cached vector of `perspective`. Part of [`Self::remove_perspective`].
    pub fn remove_instance_embeddings(&self, perspective: &str) -> Ad4mDbResult<()> {
        self.conn.execute(
            "DELETE FROM instance_embedding WHERE perspective = ?1",
            [perspective],
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn row(id: &str, sha: &str, vector: &[f32]) -> InstanceEmbeddingRow {
        InstanceEmbeddingRow {
            instance_id: id.into(),
            text_sha256: sha.into(),
            vector: vector.to_vec(),
        }
    }

    #[test]
    fn a_vector_comes_back_as_it_was_stored() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.put_instance_embeddings("p", "m", &[row("a", "h1", &[0.25, -1.5, 3.0e-7])])
            .unwrap();
        let rows = db.get_instance_embeddings("p", "m", &["a".into()]).unwrap();
        assert_eq!(rows, vec![row("a", "h1", &[0.25, -1.5, 3.0e-7])]);
    }

    #[test]
    fn a_new_hash_replaces_the_instances_old_row() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.put_instance_embeddings("p", "m", &[row("a", "old", &[1.0]), row("b", "b", &[2.0])])
            .unwrap();
        db.put_instance_embeddings("p", "m", &[row("a", "new", &[3.0])])
            .unwrap();
        let mut rows = db
            .get_instance_embeddings("p", "m", &["a".into(), "b".into()])
            .unwrap();
        rows.sort_by(|x, y| x.instance_id.cmp(&y.instance_id));
        assert_eq!(rows, vec![row("a", "new", &[3.0]), row("b", "b", &[2.0])]);
    }

    #[test]
    fn rows_are_apart_by_perspective_and_model() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.put_instance_embeddings("p", "m", &[row("a", "h", &[1.0])])
            .unwrap();
        db.put_instance_embeddings("p", "other", &[row("a", "h", &[2.0])])
            .unwrap();
        db.put_instance_embeddings("q", "m", &[row("a", "h", &[3.0])])
            .unwrap();
        let rows = db.get_instance_embeddings("p", "m", &["a".into()]).unwrap();
        assert_eq!(rows, vec![row("a", "h", &[1.0])]);

        db.remove_perspective("p").unwrap();
        assert!(db
            .get_instance_embeddings("p", "m", &["a".into()])
            .unwrap()
            .is_empty());
        assert_eq!(
            db.get_instance_embeddings("q", "m", &["a".into()]).unwrap(),
            vec![row("a", "h", &[3.0])]
        );
    }

    #[test]
    fn a_lookup_of_more_ids_than_one_statement_binds_finds_them_all() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let rows: Vec<_> = (0..1203)
            .map(|i| row(&format!("id{i}"), "h", &[i as f32]))
            .collect();
        db.put_instance_embeddings("p", "m", &rows).unwrap();
        let ids: Vec<String> = (0..1203).map(|i| format!("id{i}")).collect();
        assert_eq!(
            db.get_instance_embeddings("p", "m", &ids).unwrap().len(),
            1203
        );
    }
}
