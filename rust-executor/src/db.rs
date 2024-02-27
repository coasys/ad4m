use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use rusqlite::{params, Connection, OptionalExtension};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use crate::types::{Expression, PerspectiveDiff, LinkExpression, LinkStatus};
use crate::graphql::graphql_types::PerspectiveHandle;

#[derive(Serialize, Deserialize)]
struct LinkSchema {
    perspective: String,
    link_expression: JsonValue,
    source: String,
    predicate: String,
    target: String,
    author: String,
    timestamp: String,
    status: String,
}

#[derive(Serialize, Deserialize)]
struct ExpressionSchema {
    url: String,
    data: JsonValue,
}

pub type Ad4mDbResult<T> = Result<T, AnyError>;


use std::sync::{Arc, Mutex};

lazy_static! {
    static ref AD4M_DB_INSTANCE: Arc<Mutex<Option<Ad4mDb>>> = Arc::new(Mutex::new(None));
}

pub struct Ad4mDb {
    conn: Connection,
}

impl Ad4mDb {
    pub fn init_global_instance(db_path: &str) -> Ad4mDbResult<()> {
        let mut db_instance = AD4M_DB_INSTANCE.lock().unwrap();
        *db_instance = Some(Ad4mDb::new(db_path)?);
        Ok(())
    }

    pub fn global_instance() -> Arc<Mutex<Option<Ad4mDb>>> {
        AD4M_DB_INSTANCE.clone()
    }

    fn new(db_path: &str) -> Ad4mDbResult<Self> {
        let conn = Connection::open(db_path)?;

        // Create tables if they don't exist

        conn.execute(
            "CREATE TABLE IF NOT EXISTS perspective_handle (
                uuid TEXT PRIMARY KEY,
                name TEXT,
                neighbourhood TEXT,
                shared_url TEXT,
                state TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS link (
                id INTEGER PRIMARY KEY,
                perspective TEXT NOT NULL,
                link_expression TEXT NOT NULL,
                source TEXT NOT NULL,
                predicate TEXT NOT NULL,
                target TEXT NOT NULL,
                author TEXT NOT NULL,
                timestamp TEXT NOT NULL,
                status TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS expression (
                id INTEGER PRIMARY KEY,
                url TEXT NOT NULL UNIQUE,
                data TEXT NOT NULL
             )",
            [],
        )?;

        conn.execute(
            "CREATE TABLE IF NOT EXISTS perspective_diff (
                id INTEGER PRIMARY KEY,
                perspective TEXT NOT NULL,
                additions TEXT NOT NULL,
                removals TEXT NOT NULL,
                is_pending BOOLEAN NOT NULL
             )",
            [],
        )?;

        Ok(Self { conn })
    }

    pub fn add_perspective(&self, perspective: &PerspectiveHandle) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT INTO perspective_handle (name, uuid, neighbourhood, shared_url, state)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![
                perspective.name,
                perspective.uuid,
                perspective.neighbourhood.as_ref().map(|n| serde_json::to_string(n).ok()).flatten(),
                perspective.shared_url,
                serde_json::to_string(&perspective.state)?,
            ],
        )?;
        Ok(())
    }

    pub fn get_perspective(&self, uuid: &str) -> Ad4mDbResult<Option<PerspectiveHandle>> {
        let mut stmt = self.conn.prepare(
            "SELECT name, uuid, neighbourhood, shared_url, state FROM perspective_handle WHERE uuid = ?1",
        )?;

        let found_perspective = stmt
            .query_map([uuid], |row| {
                Ok(PerspectiveHandle {
                    name: row.get(0)?,
                    uuid: row.get(1)?,
                    neighbourhood: row.get::<usize, Option<String>>(3)?.map(|n| serde_json::from_str(&n).ok()).flatten(),
                    shared_url: row.get(4)?,
                    state: serde_json::from_str(row.get::<usize, String>(5)?.as_str()).expect("Could not deserialize perspective state from DB"),
                })
            })?
            .map(|p| p.ok())
            .next()
            .ok_or(anyhow!("No perspective found with given uuid"))?
            .clone();
        
        Ok(found_perspective)
    }

    pub fn get_all_perspectives(&self) -> Ad4mDbResult<Vec<PerspectiveHandle>> {
        let mut stmt = self.conn.prepare(
            "SELECT name, uuid, neighbourhood, shared_url, state FROM perspective_handle",
        )?;
        let perspective_iter = stmt.query_map([], |row| {
            Ok(PerspectiveHandle {
                name: row.get(0)?,
                uuid: row.get(1)?,
                neighbourhood: row.get::<usize, Option<String>>(4)?.map(|n| serde_json::from_str(&n).ok()).flatten(),
                shared_url: row.get(5)?,
                state: serde_json::from_str(row.get::<usize, String>(6)?.as_str()).expect("Could not deserialize perspective state from DB"),
            })
        })?;

        let mut perspectives = Vec::new();
        for perspective in perspective_iter {
            perspectives.push(perspective?);
        }

        Ok(perspectives)
    }

    pub fn update_perspective(&self, perspective: &PerspectiveHandle) -> Ad4mDbResult<()> {
        self.conn.execute(
            "UPDATE perspective_handle SET name = ?1, neighbourhood = ?3, shared_url = ?4, state = ?5 WHERE uuid = ?6",
            params![
                perspective.name,
                perspective.neighbourhood.as_ref().map(|n| serde_json::to_string(n).ok()).flatten(),
                perspective.shared_url,
                serde_json::to_string(&perspective.state)?,
                perspective.uuid,
            ],
        )?;
        Ok(())
    }

    pub fn remove_perspective(&self, uuid: &str) -> Ad4mDbResult<()> {
        self.conn.execute(
            "DELETE FROM perspective_handle WHERE uuid = ?1",
            [uuid],
        )?;
        Ok(())
    }

    pub fn add_link(&self, perspective_uuid: &str, link: &LinkExpression, status: &LinkStatus) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT INTO link (perspective, link_expression, source, predicate, target, author, timestamp, status)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
            params![
                perspective_uuid,
                serde_json::to_string(link)?,
                link.data.source,
                link.data.predicate.as_ref().unwrap_or(&"".to_string()),
                link.data.target,
                link.author,
                link.timestamp,
                serde_json::to_string(status)?,
            ],
        )?;
        Ok(())
    }

    pub fn add_many_links(&self, perspective_uuid: &str, links: Vec<LinkExpression>, status: &LinkStatus) -> Ad4mDbResult<()> {
        for link in links.iter() {
            self.conn.execute(
                "INSERT INTO link (perspective, link_expression, source, predicate, target, author, timestamp, status)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
                params![
                    perspective_uuid,
                    serde_json::to_string(link)?,
                    link.data.source,
                    link.data.predicate.as_ref().unwrap_or(&"".to_string()),
                    link.data.target,
                    link.author,
                    link.timestamp,
                    serde_json::to_string(&status)?,
                ],
            )?;
        }
        Ok(())
    }

    pub fn update_link(&self, perspective_uuid: &str, old_link: &LinkExpression, new_link: &LinkExpression) -> Ad4mDbResult<()> {
        self.conn.execute(
            "UPDATE link SET link_expression = ?1, source = ?2, predicate = ?3, target = ?4, author = ?5, timestamp = ?6
             WHERE perspective = ?7 AND link_expression = ?8",
            params![
                serde_json::to_string(new_link)?,
                new_link.data.source,
                new_link.data.predicate.as_ref().unwrap_or(&"".to_string()),
                new_link.data.target,
                new_link.author,
                new_link.timestamp,
                perspective_uuid,
                serde_json::to_string(old_link)?,
            ],
        )?;
        Ok(())
    }

    pub fn remove_link(&self, perspective_uuid: &str, link: &LinkExpression) -> Ad4mDbResult<()> {
        self.conn.execute(
            "DELETE FROM link WHERE perspective = ?1 AND link_expression = ?2",
            params![
                perspective_uuid,
                serde_json::to_string(link)?,
            ],
        )?;
        Ok(())
    }

    pub fn get_link(&self, perspective_uuid: &str, link: &LinkExpression) -> Ad4mDbResult<Option<LinkExpression>> {
        let mut stmt = self.conn.prepare(
            "SELECT link_expression FROM link WHERE perspective = ?1 AND link_expression = ?2",
        )?;
        let link_expression: Option<String> = stmt.query_row(
            params![perspective_uuid, serde_json::to_string(link)?],
            |row| row.get(0),
        ).optional()?;
        Ok(link_expression.map(|le| serde_json::from_str(&le).unwrap()))
    }

    pub fn get_all_links(&self, perspective_uuid: &str) -> Ad4mDbResult<Vec<LinkExpression>> {
        let mut stmt = self.conn.prepare(
            "SELECT link_expression FROM link WHERE perspective = ?1",
        )?;
        let link_iter = stmt.query_map(
            params![perspective_uuid],
            |row| serde_json::from_str(&row.get::<_, String>(0).unwrap())
                .map_err(|e| rusqlite::Error::FromSqlConversionFailure(
                    0,
                    rusqlite::types::Type::Text,
                    Box::new(e)
                ))
        )?;
        let links: Result<Vec<_>, _> = link_iter.collect();
        Ok(links?)
    }

    pub fn get_links_by_source(&self, perspective_uuid: &str, source: &str) -> Ad4mDbResult<Vec<LinkExpression>> {
        let mut stmt = self.conn.prepare(
            "SELECT link_expression FROM link WHERE perspective = ?1 AND source = ?2",
        )?;
        let link_iter = stmt.query_map(
            params![perspective_uuid, source],
            |row| serde_json::from_str(&row.get::<_, String>(0).unwrap())
                .map_err(|e| rusqlite::Error::FromSqlConversionFailure(
                    0,
                    rusqlite::types::Type::Text,
                    Box::new(e)
                ))

        )?;
        let links: Result<Vec<_>, _> = link_iter.collect();
        Ok(links?)
    }

    pub fn get_links_by_target(&self, perspective_uuid: &str, target: &str) -> Ad4mDbResult<Vec<LinkExpression>> {
        let mut stmt = self.conn.prepare(
            "SELECT link_expression FROM link WHERE perspective = ?1 AND target = ?2",
        )?;
        let link_iter = stmt.query_map(
            params![perspective_uuid, target],
            |row| serde_json::from_str(&row.get::<_, String>(0).unwrap())
                .map_err(|e| rusqlite::Error::FromSqlConversionFailure(
                    0,
                    rusqlite::types::Type::Text,
                    Box::new(e)
                ))
        )?;
        let links: Result<Vec<_>, _> = link_iter.collect();
        Ok(links?)
    }

    pub fn add_pending_diff(&self, perspective_uuid: &str, diff: &PerspectiveDiff) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT INTO perspective_diff (perspective, additions, removals, is_pending)
             VALUES (?1, ?2, ?3, ?4)",
            params![
                perspective_uuid,
                serde_json::to_string(&diff.additions)?,
                serde_json::to_string(&diff.removals)?,
                true,
            ],
        )?;
        Ok(())
    }

    pub fn get_pending_diffs(&self, perspective_uuid: &str) -> Ad4mDbResult<PerspectiveDiff> {
        let mut stmt = self.conn.prepare(
            "SELECT additions, removals FROM perspective_diff WHERE perspective = ?1 AND is_pending = ?2",
        )?;
        let diffs_iter = stmt.query_map(
            params![perspective_uuid, true],
            |row| {
                let additions: Vec<LinkExpression> = serde_json::from_str(&row.get::<_, String>(0).unwrap()).unwrap();
                let removals: Vec<LinkExpression> = serde_json::from_str(&row.get::<_, String>(1).unwrap()).unwrap();
                Ok(PerspectiveDiff { additions, removals })
            },
        )?;
        let mut diffs = Vec::new();
        for diff in diffs_iter {
            diffs.push(diff?);
        }
        // Assuming we want to concatenate all additions and removals from different diffs
        let mut all_additions = Vec::new();
        let mut all_removals = Vec::new();
        for diff in diffs {
            all_additions.extend(diff.additions);
            all_removals.extend(diff.removals);
        }
        Ok(PerspectiveDiff {
            additions: all_additions,
            removals: all_removals,
        })
    }

    pub fn clear_pending_diffs(&self, perspective_uuid: &str) -> Ad4mDbResult<()> {
        self.conn.execute(
            "DELETE FROM perspective_diff WHERE perspective = ?1 AND is_pending = ?2",
            params![perspective_uuid, true],
        )?;
        Ok(())
    }

    // Expression Methods

    pub fn add_expression<T: Serialize>(&self, url: &str, expression: &Expression<T>) -> Ad4mDbResult<()> {
        self.conn.execute(
            "INSERT INTO expression (url, data)
             VALUES (?1, ?2)",
            params![
                url,
                serde_json::to_string(expression)?,
            ],
        )?;
        Ok(())
    }

    pub fn get_expression(&self, url: &str) -> Ad4mDbResult<Option<Expression<serde_json::Value>>> {
        let mut stmt = self.conn.prepare(
            "SELECT data FROM expression WHERE url = ?1",
        )?;
        let expression: Option<String> = stmt.query_row(
            params![url],
            |row| row.get(0),
        ).optional()?;
        Ok(expression.map(|e| serde_json::from_str(&e).unwrap()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::Ad4mDb;
    use uuid::Uuid;
    use fake::{Fake, Faker};
    use chrono::Utc;
    use crate::types::{LinkExpression, Link, LinkStatus, ExpressionProof};

    fn construct_dummy_link_expression() -> LinkExpression {
        LinkExpression {
            data: Link {
                source: Faker.fake::<String>(),
                target: Faker.fake::<String>(),
                predicate: Some(Faker.fake::<String>()),
            },
            proof: ExpressionProof {
            signature: "signature".to_string(),
                key: "key".to_string(),
            },
            author: "did:test:key".to_string(),
            timestamp: Utc::now().to_rfc3339(),
            status: Some(LinkStatus::Shared),
        }
    }

    #[test]
    fn can_store_and_retrieve_links() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link = construct_dummy_link_expression();
        db.add_link(&p_uuid, &link, &LinkStatus::Shared).unwrap();

        let result: Option<LinkExpression> = db.get_link(&p_uuid, &link).unwrap();
        assert!(result.is_some());
        assert_eq!(result.unwrap(), link);
    }

    #[test]
    fn can_store_and_get_link_with_missing_predicate() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let mut link = construct_dummy_link_expression();
        link.data.predicate = None;
        db.add_link(&p_uuid, &link, &LinkStatus::Shared).unwrap();

        let result = db.get_link(&p_uuid, &link).unwrap();
        assert_eq!(result, Some(link));
    }

    #[test]
    fn can_call_get_link_multiple_times() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link = construct_dummy_link_expression();
        db.add_link(&p_uuid, &link, &LinkStatus::Shared).unwrap();

        for _ in 0..3 {
            let result = db.get_link(&p_uuid, &link).unwrap();
            assert_eq!(result, Some(link.clone()));
        }
    }

    #[test]
    fn can_get_all_links() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression();
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();
        let link2 = construct_dummy_link_expression();
        db.add_link(&p_uuid, &link2, &LinkStatus::Shared).unwrap();

        let all_links = db.get_all_links(&p_uuid).unwrap();
        assert_eq!(all_links, vec![link1, link2]);
    }

    #[test]
    fn can_call_get_all_links_multiple_times() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression();
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();

        for _ in 0..3 {
            let all_links = db.get_all_links(&p_uuid).unwrap();
            assert_eq!(all_links, vec![link1.clone()]);
        }
    }

    #[test]
    fn can_get_links_by_source() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression();
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();
        let link2 = construct_dummy_link_expression();
        db.add_link(&p_uuid, &link2,  &LinkStatus::Shared).unwrap();

        let result = db.get_links_by_source(&p_uuid, &link1.data.source).unwrap();
        assert_eq!(result, vec![link1]);
    }

    #[test]
    fn can_get_links_by_target() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression();
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();
        let link2 = construct_dummy_link_expression();
        db.add_link(&p_uuid, &link2, &LinkStatus::Shared).unwrap();

        let result = db.get_links_by_target(&p_uuid, &link1.data.target).unwrap();
        assert_eq!(result, vec![link1]);
    }

    #[test]
    fn can_update_link() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression();
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();
        let link2 = construct_dummy_link_expression();
        db.update_link(&p_uuid, &link1, &link2).unwrap();

        assert!(db.get_link(&p_uuid, &link1).unwrap().is_none());
        let result = db.get_link(&p_uuid, &link2).unwrap();
        assert_eq!(result, Some(link2));
    }

    #[test]
    fn can_remove_link() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let link1 = construct_dummy_link_expression();
        db.add_link(&p_uuid, &link1, &LinkStatus::Shared).unwrap();

        let result = db.get_link(&p_uuid, &link1).unwrap();
        assert_eq!(result, Some(link1.clone()));
        db.remove_link(&p_uuid, &link1).unwrap();
        assert!(db.get_link(&p_uuid, &link1).unwrap().is_none());
    }

    #[test]
    fn can_get_and_remove_pending_diffs() {
        let db = Ad4mDb::new(":memory:").unwrap();
        let p_uuid = Uuid::new_v4().to_string();
        let addition = construct_dummy_link_expression();
        let removal = construct_dummy_link_expression();
        db.add_pending_diff(&p_uuid, &PerspectiveDiff {
            additions: vec![addition.clone()],
            removals: vec![removal.clone()],
        }).unwrap();

        let get = db.get_pending_diffs(&p_uuid).unwrap();
        assert_eq!(get.additions.len(), 1);
        assert_eq!(get.removals.len(), 1);
        assert_eq!(get, PerspectiveDiff {
            additions: vec![addition],
            removals: vec![removal],
        });

        db.clear_pending_diffs(&p_uuid).unwrap();
        let get2 = db.get_pending_diffs(&p_uuid).unwrap();
        assert_eq!(get2.additions.len(), 0);
    }
}


