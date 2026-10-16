//! The expression cache and the queue of expressions waiting to be published.
//!
//! Both are keyed by the full expression URL (`<language hash>://<address>`):
//! two content-addressed languages can mint the same address for different
//! data, so the address alone does not identify an expression.

use rusqlite::{params, Connection, OptionalExtension};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use super::{Ad4mDb, Ad4mDbResult};
use crate::types::Expression;

/// An expression `prepare` minted whose `publish` has not succeeded yet.
#[derive(Debug, Clone, PartialEq)]
pub struct PendingPublish {
    pub url: String,
    pub language_address: String,
    pub expression_address: String,
    pub expression: JsonValue,
    pub attempts: u32,
}

/// A publish-queue row as `export_all_to_json` writes it. `expression` is
/// the stored text verbatim, so a round trip restores even a row that no
/// longer parses.
#[derive(Serialize, Deserialize)]
pub(super) struct QueuedPublishSchema {
    pub(super) url: String,
    language_address: String,
    expression_address: String,
    expression: String,
    attempts: u32,
    next_attempt_at: i64,
    last_error: Option<String>,
}

pub(super) fn create_tables(conn: &Connection) -> Ad4mDbResult<()> {
    conn.execute(
        "CREATE TABLE IF NOT EXISTS expression (
            id INTEGER PRIMARY KEY,
            url TEXT NOT NULL UNIQUE,
            data TEXT NOT NULL
         )",
        [],
    )?;
    // Rows from before the cache was keyed by URL are keyed by the bare
    // address, and nothing reads them any more.
    conn.execute("DELETE FROM expression WHERE url NOT LIKE '%://%'", [])?;
    conn.execute(
        "CREATE TABLE IF NOT EXISTS expression_publish_queue (
            url TEXT PRIMARY KEY,
            language_address TEXT NOT NULL,
            expression_address TEXT NOT NULL,
            expression TEXT NOT NULL,
            attempts INTEGER NOT NULL DEFAULT 0,
            next_attempt_at INTEGER NOT NULL,
            last_error TEXT
         )",
        [],
    )?;
    Ok(())
}

impl Ad4mDb {
    /// Cache an immutable expression. Caching one already cached is a no-op:
    /// the URL names one expression forever.
    pub fn cache_expression(
        &self,
        url: &str,
        expression: &Expression<JsonValue>,
    ) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT OR IGNORE INTO expression (url, data) VALUES (?1, ?2)",
            params![url, serde_json::to_string(expression)?],
        )?;
        Ok(())
    }

    /// Look up cached expressions, one result per URL, in order. A row that
    /// no longer parses reads as a miss rather than an error, so the caller
    /// fetches the expression again.
    pub fn cached_expressions(
        &self,
        urls: &[String],
    ) -> Ad4mDbResult<Vec<Option<Expression<JsonValue>>>> {
        let mut stmt = self
            .conn
            .prepare_cached("SELECT data FROM expression WHERE url = ?1")?;
        urls.iter()
            .map(|url| {
                let data: Option<String> =
                    stmt.query_row(params![url], |row| row.get(0)).optional()?;
                Ok(data.and_then(|d| serde_json::from_str(&d).ok()))
            })
            .collect()
    }

    /// Queue an expression for publishing. Queuing one already queued keeps
    /// the existing row and its backoff.
    pub fn queue_expression_publish(
        &self,
        url: &str,
        language_address: &str,
        expression_address: &str,
        expression: &JsonValue,
        next_attempt_at: i64,
    ) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT OR IGNORE INTO expression_publish_queue
                (url, language_address, expression_address, expression, next_attempt_at)
             VALUES (?1, ?2, ?3, ?4, ?5)",
            params![
                url,
                language_address,
                expression_address,
                serde_json::to_string(expression)?,
                next_attempt_at
            ],
        )?;
        Ok(())
    }

    /// Queued publishes due at or before `now`, oldest due first.
    pub fn due_expression_publishes(
        &self,
        now: i64,
        limit: u32,
    ) -> Ad4mDbResult<Vec<PendingPublish>> {
        let mut stmt = self.conn.prepare(
            "SELECT url, language_address, expression_address, expression, attempts
             FROM expression_publish_queue
             WHERE next_attempt_at <= ?1
             ORDER BY next_attempt_at
             LIMIT ?2",
        )?;
        let rows = stmt.query_map(params![now, limit], |row| {
            Ok((
                row.get::<_, String>(0)?,
                row.get::<_, String>(1)?,
                row.get::<_, String>(2)?,
                row.get::<_, String>(3)?,
                row.get::<_, u32>(4)?,
            ))
        })?;
        let mut pending = Vec::new();
        for row in rows {
            let (url, language_address, expression_address, expression, attempts) = row?;
            // Skipped, not dropped: with `?` one corrupt row, always due
            // first, would fail every poll and stop the whole queue.
            match serde_json::from_str(&expression) {
                Ok(expression) => pending.push(PendingPublish {
                    url,
                    language_address,
                    expression_address,
                    expression,
                    attempts,
                }),
                Err(e) => log::warn!("Skipping unparseable publish-queue row {}: {}", url, e),
            }
        }
        Ok(pending)
    }

    pub fn complete_expression_publish(&self, url: &str) -> Ad4mDbResult<()> {
        self.conn.execute(
            "DELETE FROM expression_publish_queue WHERE url = ?1",
            params![url],
        )?;
        Ok(())
    }

    pub fn reschedule_expression_publish(
        &self,
        url: &str,
        attempts: u32,
        next_attempt_at: i64,
        error: &str,
    ) -> Ad4mDbResult<()> {
        self.conn.execute(
            "UPDATE expression_publish_queue
             SET attempts = ?2, next_attempt_at = ?3, last_error = ?4
             WHERE url = ?1",
            params![url, attempts, next_attempt_at, error],
        )?;
        Ok(())
    }

    /// Every publish-queue row, for `export_all_to_json`.
    pub(super) fn export_expression_publish_queue(&self) -> Ad4mDbResult<Vec<QueuedPublishSchema>> {
        let mut stmt = self.conn.prepare(
            "SELECT url, language_address, expression_address, expression, attempts,
                    next_attempt_at, last_error
             FROM expression_publish_queue",
        )?;
        let rows = stmt
            .query_map([], |row| {
                Ok(QueuedPublishSchema {
                    url: row.get(0)?,
                    language_address: row.get(1)?,
                    expression_address: row.get(2)?,
                    expression: row.get(3)?,
                    attempts: row.get(4)?,
                    next_attempt_at: row.get(5)?,
                    last_error: row.get(6)?,
                })
            })?
            .collect::<Result<Vec<_>, _>>()?;
        Ok(rows)
    }

    /// Restore one exported publish-queue row, for `import_from_json`. A row
    /// already queued keeps its own state, as in `queue_expression_publish`.
    pub(super) fn import_queued_publish(&self, row: &QueuedPublishSchema) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT OR IGNORE INTO expression_publish_queue
                (url, language_address, expression_address, expression, attempts,
                 next_attempt_at, last_error)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
            params![
                row.url,
                row.language_address,
                row.expression_address,
                row.expression,
                row.attempts,
                row.next_attempt_at,
                row.last_error
            ],
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::ExpressionProof;
    use serde_json::json;

    fn expression(data: JsonValue) -> Expression<JsonValue> {
        Expression {
            author: "did:key:test".to_string(),
            timestamp: "2026-10-05T00:00:00Z".to_string(),
            data,
            proof: ExpressionProof {
                signature: "sig".to_string(),
                key: "key".to_string(),
            },
        }
    }

    #[test]
    fn cache_is_keyed_by_url_and_first_write_wins() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.cache_expression("a://x", &expression(json!(1))).unwrap();
        db.cache_expression("a://x", &expression(json!(2))).unwrap();
        db.cache_expression("b://x", &expression(json!(3))).unwrap();

        let urls = vec![
            "a://x".to_string(),
            "b://x".to_string(),
            "c://x".to_string(),
        ];
        let cached = db.cached_expressions(&urls).unwrap();
        assert_eq!(cached[0].as_ref().unwrap().data, json!(1));
        assert_eq!(cached[1].as_ref().unwrap().data, json!(3));
        assert!(cached[2].is_none());
    }

    #[test]
    fn publish_queue_returns_only_due_rows_until_completed() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let expr = json!({ "data": "x" });
        db.queue_expression_publish("a://1", "a", "1", &expr, 100)
            .unwrap();
        db.queue_expression_publish("a://2", "a", "2", &expr, 200)
            .unwrap();
        // Re-queuing keeps the original schedule.
        db.queue_expression_publish("a://1", "a", "1", &expr, 999)
            .unwrap();

        let due = db.due_expression_publishes(150, 10).unwrap();
        assert_eq!(due.len(), 1);
        assert_eq!(due[0].url, "a://1");
        assert_eq!(due[0].expression, expr);
        assert_eq!(due[0].attempts, 0);

        db.reschedule_expression_publish("a://1", 1, 300, "offline")
            .unwrap();
        let due = db.due_expression_publishes(250, 10).unwrap();
        assert_eq!(
            due.iter().map(|p| p.url.as_str()).collect::<Vec<_>>(),
            vec!["a://2"]
        );

        db.complete_expression_publish("a://2").unwrap();
        let due = db.due_expression_publishes(1000, 10).unwrap();
        assert_eq!(due.len(), 1);
        assert_eq!(due[0].url, "a://1");
        assert_eq!(due[0].attempts, 1);
    }

    #[test]
    fn rows_keyed_by_bare_address_are_dropped_on_open() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.cache_expression("a://x", &expression(json!(1))).unwrap();
        db.conn
            .execute(
                "INSERT INTO expression (url, data) VALUES ('x', ?1)",
                params![serde_json::to_string(&expression(json!(2))).unwrap()],
            )
            .unwrap();

        create_tables(&db.conn).unwrap();

        let urls: Vec<String> = db
            .conn
            .prepare("SELECT url FROM expression")
            .unwrap()
            .query_map([], |row| row.get(0))
            .unwrap()
            .collect::<Result<_, _>>()
            .unwrap();
        assert_eq!(urls, vec!["a://x".to_string()]);
    }

    #[test]
    fn importing_a_cached_expression_is_omitted_not_failed() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.cache_expression("a://x", &expression(json!(1))).unwrap();

        let result = db
            .import_from_json(db.export_all_to_json().unwrap())
            .unwrap();
        assert_eq!(result.expressions.failed, 0);
        assert_eq!(result.expressions.omitted, 1);
    }

    #[test]
    fn an_unparseable_queue_row_is_skipped_not_fatal() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.queue_expression_publish("a://2", "a", "2", &json!({ "data": "x" }), 200)
            .unwrap();
        db.conn
            .execute(
                "INSERT INTO expression_publish_queue
                    (url, language_address, expression_address, expression, next_attempt_at)
                 VALUES ('a://1', 'a', '1', '{not json', 100)",
                [],
            )
            .unwrap();

        let due = db.due_expression_publishes(1000, 10).unwrap();
        assert_eq!(
            due.iter().map(|p| p.url.as_str()).collect::<Vec<_>>(),
            vec!["a://2"]
        );
        assert_eq!(db.export_expression_publish_queue().unwrap().len(), 2);
    }

    #[test]
    fn export_and_import_carry_the_publish_queue() {
        let db = Ad4mDb::new(":memory:").unwrap();
        db.queue_expression_publish("a://1", "a", "1", &json!({ "data": "x" }), 100)
            .unwrap();
        db.reschedule_expression_publish("a://1", 2, 300, "offline")
            .unwrap();

        let imported = Ad4mDb::new(":memory:").unwrap();
        imported
            .import_from_json(db.export_all_to_json().unwrap())
            .unwrap();

        let due = imported.due_expression_publishes(300, 10).unwrap();
        assert_eq!(due, db.due_expression_publishes(300, 10).unwrap());
        assert_eq!(due.len(), 1);
        let rows = |db: &Ad4mDb| {
            serde_json::to_value(db.export_expression_publish_queue().unwrap()).unwrap()
        };
        assert_eq!(rows(&imported), rows(&db));
    }
}
