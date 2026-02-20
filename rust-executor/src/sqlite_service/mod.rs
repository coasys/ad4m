use crate::graphql::graphql_types::LinkStatus;
use crate::types::{DecoratedExpressionProof, DecoratedLinkExpression, Link};
use deno_core::anyhow::{anyhow, Error};
use log::warn;
use rusqlite::functions::FunctionFlags;
use rusqlite::types::ValueRef;
use rusqlite::Connection;
use serde_json::Value;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::Mutex;

/// SQLite storage representation of a link
#[derive(Debug, Clone)]
struct SqliteLink {
    source: String,
    target: String,
    predicate: String,
    author: String,
    timestamp: String,
    proof_key: String,
    proof_signature: String,
    status: Option<String>,
}

impl From<SqliteLink> for DecoratedLinkExpression {
    fn from(sqlite_link: SqliteLink) -> Self {
        let status = sqlite_link
            .status
            .and_then(|s| match s.to_lowercase().as_str() {
                "shared" => Some(LinkStatus::Shared),
                "local" => Some(LinkStatus::Local),
                _ => None,
            });

        DecoratedLinkExpression {
            author: sqlite_link.author,
            timestamp: sqlite_link.timestamp,
            data: Link {
                source: sqlite_link.source,
                predicate: if sqlite_link.predicate.is_empty() {
                    None
                } else {
                    Some(sqlite_link.predicate)
                },
                target: sqlite_link.target,
            },
            proof: DecoratedExpressionProof {
                key: sqlite_link.proof_key,
                signature: sqlite_link.proof_signature,
                valid: None,
                invalid: None,
            },
            status,
        }
    }
}

impl SqliteLink {
    fn from_row(row: &rusqlite::Row) -> rusqlite::Result<Self> {
        Ok(SqliteLink {
            source: row.get("source")?,
            target: row.get("target")?,
            predicate: row.get("predicate")?,
            author: row.get("author")?,
            timestamp: row.get("timestamp")?,
            proof_key: row.get("proof_key")?,
            proof_signature: row.get("proof_signature")?,
            status: row.get("status")?,
        })
    }
}

/// Register custom SQL functions that mirror SurrealDB's `fn::*` scripting functions.
///
/// - `parse_literal(url)` — extracts typed values from `literal://` URIs
/// - `strip_html(html)` — removes HTML tags from a string
/// - `json_path(json_text, path)` — traverses a JSON value by dot-separated path
/// - `contains(str, substring)` — checks if `str` contains `substring`
/// - `regex_match(str, pattern)` — checks if `str` matches a regex `pattern`
fn register_custom_functions(conn: &Connection) -> Result<(), Error> {
    // parse_literal
    conn.create_scalar_function(
        "parse_literal",
        1,
        FunctionFlags::SQLITE_UTF8 | FunctionFlags::SQLITE_DETERMINISTIC,
        |ctx| {
            let raw = ctx.get_raw(0);
            let url = match raw {
                ValueRef::Null => return Ok(rusqlite::types::Value::Null),
                ValueRef::Text(b) => std::str::from_utf8(b).unwrap_or(""),
                _ => return Ok(ctx.get_raw(0).into()),
            };

            if !url.starts_with("literal://") {
                return Ok(rusqlite::types::Value::Text(url.to_string()));
            }

            let body = &url[10..];

            if let Some(rest) = body.strip_prefix("string:") {
                let decoded = urlencoding::decode(rest).unwrap_or_else(|_| rest.into());
                return Ok(rusqlite::types::Value::Text(decoded.into_owned()));
            }

            if let Some(rest) = body.strip_prefix("number:") {
                if let Ok(n) = rest.parse::<f64>() {
                    return Ok(rusqlite::types::Value::Real(n));
                }
            }

            if let Some(rest) = body.strip_prefix("boolean:") {
                return Ok(rusqlite::types::Value::Integer(if rest == "true" {
                    1
                } else {
                    0
                }));
            }

            if let Some(rest) = body.strip_prefix("json:") {
                let decoded = urlencoding::decode(rest).unwrap_or_else(|_| rest.into());
                if let Ok(parsed) = serde_json::from_str::<Value>(&decoded) {
                    if let Some(data) = parsed.get("data") {
                        return Ok(rusqlite::types::Value::Text(data.to_string()));
                    }
                    return Ok(rusqlite::types::Value::Text(parsed.to_string()));
                }
            }

            Ok(rusqlite::types::Value::Text(url.to_string()))
        },
    )
    .map_err(|e| anyhow!("Failed to register parse_literal: {}", e))?;

    // strip_html
    conn.create_scalar_function(
        "strip_html",
        1,
        FunctionFlags::SQLITE_UTF8 | FunctionFlags::SQLITE_DETERMINISTIC,
        |ctx| {
            let raw = ctx.get_raw(0);
            let html = match raw {
                ValueRef::Null => return Ok(rusqlite::types::Value::Null),
                ValueRef::Text(b) => std::str::from_utf8(b).unwrap_or(""),
                _ => return Ok(ctx.get_raw(0).into()),
            };

            let mut result = String::with_capacity(html.len());
            let mut inside_tag = false;
            for ch in html.chars() {
                if ch == '<' {
                    inside_tag = true;
                } else if ch == '>' {
                    inside_tag = false;
                } else if !inside_tag {
                    result.push(ch);
                }
            }
            Ok(rusqlite::types::Value::Text(result))
        },
    )
    .map_err(|e| anyhow!("Failed to register strip_html: {}", e))?;

    // json_path
    conn.create_scalar_function(
        "json_path",
        2,
        FunctionFlags::SQLITE_UTF8 | FunctionFlags::SQLITE_DETERMINISTIC,
        |ctx| {
            let json_raw = ctx.get_raw(0);
            let path_raw = ctx.get_raw(1);

            let json_str = match json_raw {
                ValueRef::Null => return Ok(rusqlite::types::Value::Null),
                ValueRef::Text(b) => std::str::from_utf8(b).unwrap_or(""),
                _ => return Ok(rusqlite::types::Value::Null),
            };

            let path = match path_raw {
                ValueRef::Null => return Ok(rusqlite::types::Value::Null),
                ValueRef::Text(b) => std::str::from_utf8(b).unwrap_or(""),
                _ => return Ok(rusqlite::types::Value::Null),
            };

            let parsed: Value = match serde_json::from_str(json_str) {
                Ok(v) => v,
                Err(_) => return Ok(rusqlite::types::Value::Null),
            };

            let mut current = &parsed;
            for part in path.split('.') {
                match current.get(part) {
                    Some(v) => current = v,
                    None => return Ok(rusqlite::types::Value::Null),
                }
            }

            match current {
                Value::String(s) => Ok(rusqlite::types::Value::Text(s.clone())),
                Value::Number(n) => {
                    if let Some(i) = n.as_i64() {
                        Ok(rusqlite::types::Value::Integer(i))
                    } else if let Some(f) = n.as_f64() {
                        Ok(rusqlite::types::Value::Real(f))
                    } else {
                        Ok(rusqlite::types::Value::Text(n.to_string()))
                    }
                }
                Value::Bool(b) => Ok(rusqlite::types::Value::Integer(if *b { 1 } else { 0 })),
                Value::Null => Ok(rusqlite::types::Value::Null),
                other => Ok(rusqlite::types::Value::Text(other.to_string())),
            }
        },
    )
    .map_err(|e| anyhow!("Failed to register json_path: {}", e))?;

    // contains
    conn.create_scalar_function(
        "contains",
        2,
        FunctionFlags::SQLITE_UTF8 | FunctionFlags::SQLITE_DETERMINISTIC,
        |ctx| {
            let str_raw = ctx.get_raw(0);
            let sub_raw = ctx.get_raw(1);

            let haystack = match str_raw {
                ValueRef::Text(b) => std::str::from_utf8(b).unwrap_or(""),
                _ => return Ok(rusqlite::types::Value::Integer(0)),
            };

            let needle = match sub_raw {
                ValueRef::Text(b) => std::str::from_utf8(b).unwrap_or(""),
                _ => return Ok(rusqlite::types::Value::Integer(0)),
            };

            Ok(rusqlite::types::Value::Integer(
                if haystack.contains(needle) {
                    1
                } else {
                    0
                },
            ))
        },
    )
    .map_err(|e| anyhow!("Failed to register contains: {}", e))?;

    // regex_match
    conn.create_scalar_function(
        "regex_match",
        2,
        FunctionFlags::SQLITE_UTF8 | FunctionFlags::SQLITE_DETERMINISTIC,
        |ctx| {
            let str_raw = ctx.get_raw(0);
            let pat_raw = ctx.get_raw(1);

            let text = match str_raw {
                ValueRef::Text(b) => std::str::from_utf8(b).unwrap_or(""),
                _ => return Ok(rusqlite::types::Value::Integer(0)),
            };

            let pattern = match pat_raw {
                ValueRef::Text(b) => std::str::from_utf8(b).unwrap_or(""),
                _ => return Ok(rusqlite::types::Value::Integer(0)),
            };

            match regex::Regex::new(pattern) {
                Ok(re) => Ok(rusqlite::types::Value::Integer(
                    if re.is_match(text) { 1 } else { 0 },
                )),
                Err(_) => Ok(rusqlite::types::Value::Integer(0)),
            }
        },
    )
    .map_err(|e| anyhow!("Failed to register regex_match: {}", e))?;

    Ok(())
}

/// SQLite-based link storage service for perspectives.
///
/// Each perspective gets its own isolated SQLite database file (or in-memory for tests).
/// WAL mode is enabled for concurrent reads with a single writer.
///
/// This is a drop-in replacement for `SurrealDBService` behind the `sqlite-links` feature flag.
#[derive(Clone)]
pub struct SqliteLinkService {
    conn: Arc<Mutex<Connection>>,
}

impl SqliteLinkService {
    /// Validates that a query is read-only.
    fn validate_readonly_query(query: &str) -> Result<(), Error> {
        let query_trimmed = query.trim();
        let query_upper = query_trimmed.to_uppercase();

        let mutating_operations = [
            "INSERT", "UPDATE", "DELETE", "CREATE", "DROP", "ALTER", "BEGIN", "COMMIT", "ROLLBACK",
            "REPLACE", "ATTACH", "DETACH", "REINDEX", "VACUUM",
        ];

        for operation in &mutating_operations {
            let mut search_pos = 0;
            while let Some(pos) = query_upper[search_pos..].find(operation) {
                let absolute_pos = search_pos + pos;

                let before_ok = if absolute_pos == 0 {
                    true
                } else {
                    let before_char = query_upper.as_bytes().get(absolute_pos - 1).copied();
                    matches!(
                        before_char,
                        Some(b' ')
                            | Some(b'\t')
                            | Some(b'\n')
                            | Some(b'\r')
                            | Some(b';')
                            | Some(b'(')
                    )
                };

                let after_pos = absolute_pos + operation.len();
                let after_ok = if after_pos >= query_upper.len() {
                    true
                } else {
                    let after_char = query_upper.as_bytes().get(after_pos).copied();
                    matches!(
                        after_char,
                        Some(b' ')
                            | Some(b'\t')
                            | Some(b'\n')
                            | Some(b'\r')
                            | Some(b';')
                            | Some(b'(')
                    )
                };

                if before_ok && after_ok {
                    return Err(anyhow!(
                        "Query contains mutating operation '{}' which is not allowed. \
                         Only read-only queries (SELECT, etc.) are permitted.",
                        operation
                    ));
                }

                search_pos = absolute_pos + 1;
            }
        }

        let first_word = query_upper.split_whitespace().next().unwrap_or("");
        if !first_word.is_empty()
            && !matches!(first_word, "SELECT" | "WITH" | "EXPLAIN" | "VALUES")
        {
            log::warn!(
                "Query starts with '{}' which is unusual for a read-only query: {}",
                first_word,
                query_trimmed
            );
        }

        Ok(())
    }

    /// Translate a SurrealQL query into SQLite SQL.
    ///
    /// Handles the key SurrealQL patterns used in AD4M:
    /// - `in.uri` → `source` (graph traversal on source node)
    /// - `out.uri` → `target` (graph traversal on target node)
    /// - `fn::*` function calls → bare function name
    /// - `IN [...]` → `IN (...)`
    /// - `CONTAINS` operator → `contains()` function call
    fn translate_surreal_to_sqlite(query: &str) -> String {
        let mut result = query.to_string();

        // Replace graph traversal patterns
        result = result.replace("in.uri", "source");
        result = result.replace("out.uri", "target");

        // Replace SurrealDB function calls with SQLite custom function calls
        result = result.replace("fn::parse_literal", "parse_literal");
        result = result.replace("fn::strip_html", "strip_html");
        result = result.replace("fn::json_path", "json_path");
        result = result.replace("fn::contains", "contains");
        result = result.replace("fn::regex_match", "regex_match");

        // Replace type::thing($var)
        let thing_re = regex::Regex::new(r"type::thing\(([^)]+)\)").unwrap();
        result = thing_re.replace_all(&result, "$1").to_string();

        // string::contains → contains
        result = result.replace("string::contains", "contains");

        // CONTAINS operator → contains() function
        let contains_op_re =
            regex::Regex::new(r#"(\w+(?:\.\w+)*)\s+CONTAINS\s+('[^']*'|"[^"]*"|\w+)"#).unwrap();
        result = contains_op_re
            .replace_all(&result, "contains($1, $2)")
            .to_string();

        // IN [...] → IN (...)
        let in_bracket_re = regex::Regex::new(r"\bIN\s*\[([^\]]*)\]").unwrap();
        result = in_bracket_re.replace_all(&result, "IN ($1)").to_string();

        result
    }

    /// Create a new SQLite-backed link service.
    ///
    /// # Arguments
    /// * `_namespace` — ignored (API compatibility with SurrealDBService)
    /// * `database` — perspective UUID, used to name the SQLite file
    /// * `data_path` — base directory for database files; `None` means in-memory (tests)
    pub async fn new(
        _namespace: &str,
        database: &str,
        data_path: Option<&str>,
    ) -> Result<Self, Error> {
        let conn = if let Some(path) = data_path {
            let db_dir = PathBuf::from(path).join(format!("sqlite_perspectives/{}", database));
            std::fs::create_dir_all(&db_dir)?;
            let db_file = db_dir.join("links.sqlite");
            Connection::open(&db_file)
                .map_err(|e| anyhow!("Failed to open SQLite database at {:?}: {}", db_file, e))?
        } else {
            Connection::open_in_memory()
                .map_err(|e| anyhow!("Failed to open in-memory SQLite database: {}", e))?
        };

        // WAL mode for concurrent readers
        conn.pragma_update(None, "journal_mode", "WAL")
            .map_err(|e| anyhow!("Failed to set WAL mode: {}", e))?;

        // Reasonable busy timeout for concurrent access
        conn.pragma_update(None, "busy_timeout", 5000)
            .map_err(|e| anyhow!("Failed to set busy_timeout: {}", e))?;

        // Foreign keys
        conn.pragma_update(None, "foreign_keys", "ON")
            .map_err(|e| anyhow!("Failed to enable foreign keys: {}", e))?;

        // Create schema
        conn.execute_batch(
            "
            CREATE TABLE IF NOT EXISTS node (
                uri TEXT PRIMARY KEY NOT NULL
            );

            CREATE TABLE IF NOT EXISTS link (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                source TEXT NOT NULL,
                target TEXT NOT NULL,
                predicate TEXT NOT NULL DEFAULT '',
                author TEXT NOT NULL DEFAULT '',
                timestamp TEXT NOT NULL DEFAULT '',
                proof_key TEXT NOT NULL DEFAULT '',
                proof_signature TEXT NOT NULL DEFAULT '',
                status TEXT,
                FOREIGN KEY (source) REFERENCES node(uri),
                FOREIGN KEY (target) REFERENCES node(uri)
            );

            CREATE INDEX IF NOT EXISTS idx_link_source ON link(source);
            CREATE INDEX IF NOT EXISTS idx_link_target ON link(target);
            CREATE INDEX IF NOT EXISTS idx_link_predicate ON link(predicate);
            CREATE INDEX IF NOT EXISTS idx_link_source_predicate ON link(source, predicate);
            CREATE INDEX IF NOT EXISTS idx_link_target_predicate ON link(target, predicate);

            CREATE UNIQUE INDEX IF NOT EXISTS idx_link_unique
                ON link(source, target, predicate, author, timestamp);
            ",
        )
        .map_err(|e| anyhow!("Failed to create schema: {}", e))?;

        register_custom_functions(&conn)?;

        Ok(SqliteLinkService {
            conn: Arc::new(Mutex::new(conn)),
        })
    }

    /// Ensure a node record exists for the given URI.
    fn ensure_node_sync(conn: &Connection, uri: &str) -> Result<(), Error> {
        conn.execute(
            "INSERT OR IGNORE INTO node (uri) VALUES (?1)",
            rusqlite::params![uri],
        )
        .map_err(|e| anyhow!("Failed to ensure node for URI '{}': {}", uri, e))?;
        Ok(())
    }

    pub async fn add_link(
        &self,
        _perspective_uuid: &str,
        link: &DecoratedLinkExpression,
    ) -> Result<(), Error> {
        let source = link.data.source.clone();
        let target = link.data.target.clone();
        let predicate = link.data.predicate.clone().unwrap_or_default();
        let author = link.author.clone();
        let timestamp = link.timestamp.clone();
        let proof_key = link.proof.key.clone();
        let proof_signature = link.proof.signature.clone();
        let status_str = link.status.as_ref().map(|s| match s {
            LinkStatus::Shared => "Shared".to_string(),
            LinkStatus::Local => "Local".to_string(),
        });

        let conn = self.conn.lock().await;
        Self::ensure_node_sync(&conn, &source)?;
        Self::ensure_node_sync(&conn, &target)?;

        let result = conn.execute(
            "INSERT OR IGNORE INTO link \
             (source, target, predicate, author, timestamp, proof_key, proof_signature, status) \
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
            rusqlite::params![
                source,
                target,
                predicate,
                author,
                timestamp,
                proof_key,
                proof_signature,
                status_str,
            ],
        );

        match result {
            Ok(_) => Ok(()),
            Err(e) => {
                let msg = e.to_string().to_lowercase();
                if msg.contains("unique") || msg.contains("constraint") {
                    Ok(()) // idempotent
                } else {
                    Err(anyhow!("Failed to add link: {}", e))
                }
            }
        }
    }

    pub async fn remove_link(
        &self,
        _perspective_uuid: &str,
        link: &DecoratedLinkExpression,
    ) -> Result<(), Error> {
        let source = link.data.source.clone();
        let target = link.data.target.clone();
        let predicate = link.data.predicate.clone().unwrap_or_default();
        let author = link.author.clone();
        let timestamp = link.timestamp.clone();

        let conn = self.conn.lock().await;
        conn.execute(
            "DELETE FROM link \
             WHERE source = ?1 AND target = ?2 AND predicate = ?3 \
             AND author = ?4 AND timestamp = ?5",
            rusqlite::params![source, target, predicate, author, timestamp],
        )
        .map_err(|e| anyhow!("Failed to remove link: {}", e))?;

        Ok(())
    }

    pub async fn query_links(
        &self,
        _perspective_uuid: &str,
        query: &str,
    ) -> Result<Vec<Value>, Error> {
        let query = query.trim().to_string();

        Self::validate_readonly_query(&query)?;

        let sqlite_query = Self::translate_surreal_to_sqlite(&query);

        log::trace!(
            "🗄️ SQLite query (translated):\n  original: {}\n  sqlite:   {}",
            query,
            sqlite_query
        );

        let conn = self.conn.lock().await;

        let mut stmt = conn
            .prepare(&sqlite_query)
            .map_err(|e| anyhow!("Failed to prepare query '{}': {}", sqlite_query, e))?;

        let column_count = stmt.column_count();
        let column_names: Vec<String> = (0..column_count)
            .map(|i| stmt.column_name(i).unwrap_or("").to_string())
            .collect();

        let rows = stmt
            .query_map([], |row| {
                let mut map = serde_json::Map::new();
                for (i, name) in column_names.iter().enumerate() {
                    let value = match row.get_ref(i) {
                        Ok(ValueRef::Null) => Value::Null,
                        Ok(ValueRef::Integer(n)) => Value::Number(n.into()),
                        Ok(ValueRef::Real(f)) => Value::Number(
                            serde_json::Number::from_f64(f).unwrap_or_else(|| 0.into()),
                        ),
                        Ok(ValueRef::Text(b)) => {
                            Value::String(std::str::from_utf8(b).unwrap_or("").to_string())
                        }
                        Ok(ValueRef::Blob(b)) => {
                            Value::String(format!("<blob:{} bytes>", b.len()))
                        }
                        Err(_) => Value::Null,
                    };
                    map.insert(name.clone(), value);
                }
                Ok(Value::Object(map))
            })
            .map_err(|e| anyhow!("Failed to execute query '{}': {}", sqlite_query, e))?;

        let mut results = Vec::new();
        for row_result in rows {
            match row_result {
                Ok(value) => results.push(value),
                Err(e) => {
                    warn!("Error reading row from SQLite query: {}", e);
                }
            }
        }

        log::trace!("🗄️ SQLite query result count: {}", results.len());

        Ok(results)
    }

    #[allow(dead_code)]
    pub async fn clear_perspective(&self, _perspective_uuid: &str) -> Result<(), Error> {
        let conn = self.conn.lock().await;
        conn.execute("DELETE FROM link", [])
            .map_err(|e| anyhow!("Failed to clear links: {}", e))?;
        Ok(())
    }

    #[allow(dead_code)]
    pub async fn reload_perspective(
        &self,
        perspective_uuid: &str,
        links: Vec<DecoratedLinkExpression>,
    ) -> Result<(), Error> {
        {
            let conn = self.conn.lock().await;
            conn.execute("DELETE FROM link", [])
                .map_err(|e| anyhow!("Failed to clear links during reload: {}", e))?;
        }

        if links.is_empty() {
            return Ok(());
        }

        for link in links {
            self.add_link(perspective_uuid, &link).await?;
        }

        Ok(())
    }

    /// Get all links from the database for a perspective.
    pub async fn get_all_links(
        &self,
        _perspective_uuid: &str,
    ) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let conn = self.conn.lock().await;
        let mut stmt = conn
            .prepare(
                "SELECT source, target, predicate, author, timestamp, \
                 proof_key, proof_signature, status FROM link",
            )
            .map_err(|e| anyhow!("Failed to prepare get_all_links query: {}", e))?;

        let rows = stmt
            .query_map([], |row| SqliteLink::from_row(row))
            .map_err(|e| anyhow!("Failed to execute get_all_links query: {}", e))?;

        let mut links = Vec::new();
        for row_result in rows {
            match row_result {
                Ok(sqlite_link) => links.push(sqlite_link.into()),
                Err(e) => {
                    warn!("Failed to deserialize link in get_all_links: {}", e);
                }
            }
        }

        Ok(links)
    }

    /// Get a specific link by its unique constraint fields.
    pub async fn get_link(
        &self,
        _perspective_uuid: &str,
        source: &str,
        predicate: Option<&str>,
        target: &str,
        author: &str,
        timestamp: &str,
    ) -> Result<Option<DecoratedLinkExpression>, Error> {
        let predicate_str = predicate.unwrap_or("").to_string();

        let conn = self.conn.lock().await;
        let mut stmt = conn
            .prepare(
                "SELECT source, target, predicate, author, timestamp, \
                 proof_key, proof_signature, status \
                 FROM link \
                 WHERE source = ?1 AND target = ?2 AND predicate = ?3 \
                 AND author = ?4 AND timestamp = ?5 \
                 LIMIT 1",
            )
            .map_err(|e| anyhow!("Failed to prepare get_link query: {}", e))?;

        let mut rows = stmt
            .query_map(
                rusqlite::params![source, target, predicate_str, author, timestamp],
                |row| SqliteLink::from_row(row),
            )
            .map_err(|e| anyhow!("Failed to execute get_link query: {}", e))?;

        match rows.next() {
            Some(Ok(sqlite_link)) => Ok(Some(sqlite_link.into())),
            Some(Err(e)) => {
                warn!("Failed to deserialize link in get_link: {}", e);
                Ok(None)
            }
            None => Ok(None),
        }
    }

    /// Get all links matching a specific source address.
    pub async fn get_links_by_source(
        &self,
        _perspective_uuid: &str,
        source: &str,
    ) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let conn = self.conn.lock().await;
        let mut stmt = conn
            .prepare(
                "SELECT source, target, predicate, author, timestamp, \
                 proof_key, proof_signature, status \
                 FROM link WHERE source = ?1",
            )
            .map_err(|e| anyhow!("Failed to prepare get_links_by_source query: {}", e))?;

        let rows = stmt
            .query_map(rusqlite::params![source], |row| SqliteLink::from_row(row))
            .map_err(|e| anyhow!("Failed to execute get_links_by_source query: {}", e))?;

        let mut links = Vec::new();
        for row_result in rows {
            match row_result {
                Ok(sqlite_link) => links.push(sqlite_link.into()),
                Err(e) => {
                    warn!(
                        "Failed to deserialize link in get_links_by_source: {}",
                        e
                    );
                }
            }
        }

        Ok(links)
    }

    /// Get all links matching a specific target address.
    pub async fn get_links_by_target(
        &self,
        _perspective_uuid: &str,
        target: &str,
    ) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let conn = self.conn.lock().await;
        let mut stmt = conn
            .prepare(
                "SELECT source, target, predicate, author, timestamp, \
                 proof_key, proof_signature, status \
                 FROM link WHERE target = ?1",
            )
            .map_err(|e| anyhow!("Failed to prepare get_links_by_target query: {}", e))?;

        let rows = stmt
            .query_map(rusqlite::params![target], |row| SqliteLink::from_row(row))
            .map_err(|e| anyhow!("Failed to execute get_links_by_target query: {}", e))?;

        let mut links = Vec::new();
        for row_result in rows {
            match row_result {
                Ok(sqlite_link) => links.push(sqlite_link.into()),
                Err(e) => {
                    warn!(
                        "Failed to deserialize link in get_links_by_target: {}",
                        e
                    );
                }
            }
        }

        Ok(links)
    }

    /// Get all links matching a specific predicate.
    pub async fn get_links_by_predicate(
        &self,
        _perspective_uuid: &str,
        predicate: &str,
    ) -> Result<Vec<DecoratedLinkExpression>, Error> {
        let conn = self.conn.lock().await;
        let mut stmt = conn
            .prepare(
                "SELECT source, target, predicate, author, timestamp, \
                 proof_key, proof_signature, status \
                 FROM link WHERE predicate = ?1",
            )
            .map_err(|e| anyhow!("Failed to prepare get_links_by_predicate query: {}", e))?;

        let rows = stmt
            .query_map(rusqlite::params![predicate], |row| {
                SqliteLink::from_row(row)
            })
            .map_err(|e| anyhow!("Failed to execute get_links_by_predicate query: {}", e))?;

        let mut links = Vec::new();
        for row_result in rows {
            match row_result {
                Ok(sqlite_link) => links.push(sqlite_link.into()),
                Err(e) => {
                    warn!(
                        "Failed to deserialize link in get_links_by_predicate: {}",
                        e
                    );
                }
            }
        }

        Ok(links)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{DecoratedExpressionProof, Link};

    fn create_test_link(
        source: &str,
        predicate: Option<&str>,
        target: &str,
        author: &str,
        timestamp: &str,
    ) -> DecoratedLinkExpression {
        DecoratedLinkExpression {
            author: author.to_string(),
            timestamp: timestamp.to_string(),
            data: Link {
                source: source.to_string(),
                predicate: predicate.map(|s| s.to_string()),
                target: target.to_string(),
            },
            proof: DecoratedExpressionProof {
                key: "test_key".to_string(),
                signature: "test_signature".to_string(),
                valid: Some(true),
                invalid: Some(false),
            },
            status: None,
        }
    }

    #[tokio::test]
    async fn test_new_service_initializes_successfully() {
        let service = SqliteLinkService::new("ad4m", "test_init", None).await;
        assert!(service.is_ok(), "Service should initialize successfully");
    }

    #[tokio::test]
    async fn test_add_single_link() {
        let service = SqliteLinkService::new("ad4m", "test_add", None)
            .await
            .unwrap();
        let link = create_test_link(
            "source1",
            Some("predicate1"),
            "target1",
            "author1",
            "2024-01-01T00:00:00Z",
        );

        let result = service.add_link("test", &link).await;
        assert!(result.is_ok(), "Adding link should succeed");

        let results = service
            .query_links("test", "SELECT * FROM link")
            .await
            .unwrap();
        assert_eq!(results.len(), 1);
    }

    #[tokio::test]
    async fn test_add_duplicate_link_is_idempotent() {
        let service = SqliteLinkService::new("ad4m", "test_dup", None)
            .await
            .unwrap();
        let link = create_test_link("s1", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z");

        service.add_link("test", &link).await.unwrap();
        let result = service.add_link("test", &link).await;
        assert!(result.is_ok(), "Adding duplicate link should not error");

        let results = service
            .query_links("test", "SELECT * FROM link")
            .await
            .unwrap();
        assert_eq!(results.len(), 1);
    }

    #[tokio::test]
    async fn test_remove_link() {
        let service = SqliteLinkService::new("ad4m", "test_remove", None)
            .await
            .unwrap();
        let link = create_test_link("s1", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z");

        service.add_link("test", &link).await.unwrap();
        service.remove_link("test", &link).await.unwrap();

        let results = service
            .query_links("test", "SELECT * FROM link")
            .await
            .unwrap();
        assert_eq!(results.len(), 0);
    }

    #[tokio::test]
    async fn test_remove_nonexistent_link() {
        let service = SqliteLinkService::new("ad4m", "test_rem_none", None)
            .await
            .unwrap();
        let link = create_test_link("s1", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z");
        let result = service.remove_link("test", &link).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_query_by_source_surreal_syntax() {
        let service = SqliteLinkService::new("ad4m", "test_qsrc", None)
            .await
            .unwrap();

        let l1 = create_test_link("common", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z");
        let l2 = create_test_link("common", Some("p2"), "t2", "a2", "2024-01-01T00:00:01Z");
        let l3 = create_test_link("other", Some("p3"), "t3", "a3", "2024-01-01T00:00:02Z");

        service.add_link("t", &l1).await.unwrap();
        service.add_link("t", &l2).await.unwrap();
        service.add_link("t", &l3).await.unwrap();

        // SurrealQL graph traversal syntax
        let results = service
            .query_links("t", "SELECT * FROM link WHERE in.uri = 'common'")
            .await
            .unwrap();
        assert_eq!(results.len(), 2);
    }

    #[tokio::test]
    async fn test_query_by_target_surreal_syntax() {
        let service = SqliteLinkService::new("ad4m", "test_qtgt", None)
            .await
            .unwrap();

        let l1 = create_test_link("s1", Some("p1"), "common", "a1", "2024-01-01T00:00:00Z");
        let l2 = create_test_link("s2", Some("p2"), "common", "a2", "2024-01-01T00:00:01Z");
        let l3 = create_test_link("s3", Some("p3"), "other", "a3", "2024-01-01T00:00:02Z");

        service.add_link("t", &l1).await.unwrap();
        service.add_link("t", &l2).await.unwrap();
        service.add_link("t", &l3).await.unwrap();

        let results = service
            .query_links("t", "SELECT * FROM link WHERE out.uri = 'common'")
            .await
            .unwrap();
        assert_eq!(results.len(), 2);
    }

    #[tokio::test]
    async fn test_query_in_bracket_syntax() {
        let service = SqliteLinkService::new("ad4m", "test_qin", None)
            .await
            .unwrap();

        let l1 = create_test_link("s1", Some("type_a"), "t1", "a1", "2024-01-01T00:00:00Z");
        let l2 = create_test_link("s2", Some("type_b"), "t2", "a2", "2024-01-01T00:00:01Z");
        let l3 = create_test_link("s3", Some("type_c"), "t3", "a3", "2024-01-01T00:00:02Z");

        service.add_link("t", &l1).await.unwrap();
        service.add_link("t", &l2).await.unwrap();
        service.add_link("t", &l3).await.unwrap();

        let results = service
            .query_links(
                "t",
                "SELECT * FROM link WHERE predicate IN ['type_a', 'type_b']",
            )
            .await
            .unwrap();
        assert_eq!(results.len(), 2);
    }

    #[tokio::test]
    async fn test_clear_perspective() {
        let service = SqliteLinkService::new("ad4m", "test_clear", None)
            .await
            .unwrap();

        service
            .add_link(
                "t",
                &create_test_link("s1", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z"),
            )
            .await
            .unwrap();
        service
            .add_link(
                "t",
                &create_test_link("s2", Some("p2"), "t2", "a2", "2024-01-01T00:00:01Z"),
            )
            .await
            .unwrap();

        service.clear_perspective("t").await.unwrap();
        let results = service
            .query_links("t", "SELECT * FROM link")
            .await
            .unwrap();
        assert_eq!(results.len(), 0);
    }

    #[tokio::test]
    async fn test_perspective_isolation() {
        let s1 = SqliteLinkService::new("ad4m", "iso1", None).await.unwrap();
        let s2 = SqliteLinkService::new("ad4m", "iso2", None).await.unwrap();

        s1.add_link(
            "p1",
            &create_test_link("s1", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z"),
        )
        .await
        .unwrap();
        s2.add_link(
            "p2",
            &create_test_link("s2", Some("p2"), "t2", "a2", "2024-01-01T00:00:01Z"),
        )
        .await
        .unwrap();

        assert_eq!(
            s1.query_links("p1", "SELECT * FROM link")
                .await
                .unwrap()
                .len(),
            1
        );
        assert_eq!(
            s2.query_links("p2", "SELECT * FROM link")
                .await
                .unwrap()
                .len(),
            1
        );

        s1.clear_perspective("p1").await.unwrap();
        assert_eq!(
            s1.query_links("p1", "SELECT * FROM link")
                .await
                .unwrap()
                .len(),
            0
        );
        assert_eq!(
            s2.query_links("p2", "SELECT * FROM link")
                .await
                .unwrap()
                .len(),
            1
        );
    }

    #[tokio::test]
    async fn test_reload_perspective() {
        let service = SqliteLinkService::new("ad4m", "test_reload", None)
            .await
            .unwrap();

        service
            .add_link(
                "t",
                &create_test_link("s1", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z"),
            )
            .await
            .unwrap();

        service
            .reload_perspective(
                "t",
                vec![
                    create_test_link("s3", Some("p3"), "t3", "a3", "2024-01-01T00:00:02Z"),
                    create_test_link("s4", Some("p4"), "t4", "a4", "2024-01-01T00:00:03Z"),
                ],
            )
            .await
            .unwrap();

        let results = service
            .query_links("t", "SELECT * FROM link")
            .await
            .unwrap();
        assert_eq!(results.len(), 2);
    }

    #[tokio::test]
    async fn test_get_link() {
        let service = SqliteLinkService::new("ad4m", "test_getlink", None)
            .await
            .unwrap();

        let link = create_test_link("s1", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z");
        service.add_link("t", &link).await.unwrap();

        let found = service
            .get_link("t", "s1", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z")
            .await
            .unwrap();
        assert!(found.is_some());

        let not_found = service
            .get_link("t", "s1", Some("p1"), "t1", "a1", "1999-01-01T00:00:00Z")
            .await
            .unwrap();
        assert!(not_found.is_none());
    }

    #[tokio::test]
    async fn test_get_links_by_source() {
        let service = SqliteLinkService::new("ad4m", "test_bysrc", None)
            .await
            .unwrap();

        service
            .add_link(
                "t",
                &create_test_link("common", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z"),
            )
            .await
            .unwrap();
        service
            .add_link(
                "t",
                &create_test_link("common", Some("p2"), "t2", "a2", "2024-01-01T00:00:01Z"),
            )
            .await
            .unwrap();
        service
            .add_link(
                "t",
                &create_test_link("other", Some("p3"), "t3", "a3", "2024-01-01T00:00:02Z"),
            )
            .await
            .unwrap();

        let links = service.get_links_by_source("t", "common").await.unwrap();
        assert_eq!(links.len(), 2);
    }

    #[tokio::test]
    async fn test_status_stored_and_retrieved() {
        let service = SqliteLinkService::new("ad4m", "test_status", None)
            .await
            .unwrap();

        let mut link = create_test_link("s1", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z");
        link.status = Some(LinkStatus::Shared);
        service.add_link("t", &link).await.unwrap();

        let mut link2 = create_test_link("s2", Some("p2"), "t2", "a2", "2024-01-01T00:00:01Z");
        link2.status = Some(LinkStatus::Local);
        service.add_link("t", &link2).await.unwrap();

        let links = service.get_all_links("t").await.unwrap();
        assert_eq!(links[0].status, Some(LinkStatus::Shared));
        assert_eq!(links[1].status, Some(LinkStatus::Local));
    }

    #[tokio::test]
    async fn test_query_validation_blocks_mutations() {
        let service = SqliteLinkService::new("ad4m", "test_val", None)
            .await
            .unwrap();

        assert!(service
            .query_links("t", "DELETE FROM link")
            .await
            .is_err());
        assert!(service
            .query_links("t", "UPDATE link SET predicate = 'x'")
            .await
            .is_err());
        assert!(service
            .query_links("t", "INSERT INTO link VALUES (1)")
            .await
            .is_err());
        assert!(service.query_links("t", "DROP TABLE link").await.is_err());
    }

    #[tokio::test]
    async fn test_custom_function_parse_literal() {
        let service = SqliteLinkService::new("ad4m", "test_parslit", None)
            .await
            .unwrap();
        service
            .add_link(
                "t",
                &create_test_link(
                    "literal://string:hello%20world",
                    Some("p1"),
                    "t1",
                    "a1",
                    "2024-01-01T00:00:00Z",
                ),
            )
            .await
            .unwrap();

        let results = service
            .query_links("t", "SELECT parse_literal(source) as parsed FROM link")
            .await
            .unwrap();
        assert_eq!(results[0]["parsed"].as_str().unwrap(), "hello world");
    }

    #[tokio::test]
    async fn test_custom_function_strip_html() {
        let service = SqliteLinkService::new("ad4m", "test_striphtml", None)
            .await
            .unwrap();
        service
            .add_link(
                "t",
                &create_test_link(
                    "<b>bold</b> text",
                    Some("p1"),
                    "t1",
                    "a1",
                    "2024-01-01T00:00:00Z",
                ),
            )
            .await
            .unwrap();

        let results = service
            .query_links("t", "SELECT strip_html(source) as stripped FROM link")
            .await
            .unwrap();
        assert_eq!(results[0]["stripped"].as_str().unwrap(), "bold text");
    }

    #[tokio::test]
    async fn test_custom_function_contains() {
        let service = SqliteLinkService::new("ad4m", "test_contains", None)
            .await
            .unwrap();

        service
            .add_link(
                "t",
                &create_test_link("hello world", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z"),
            )
            .await
            .unwrap();
        service
            .add_link(
                "t",
                &create_test_link("goodbye", Some("p2"), "t2", "a2", "2024-01-01T00:00:01Z"),
            )
            .await
            .unwrap();

        let results = service
            .query_links(
                "t",
                "SELECT * FROM link WHERE contains(source, 'hello') = 1",
            )
            .await
            .unwrap();
        assert_eq!(results.len(), 1);
    }

    #[tokio::test]
    async fn test_custom_function_regex_match() {
        let service = SqliteLinkService::new("ad4m", "test_regex", None)
            .await
            .unwrap();

        service
            .add_link(
                "t",
                &create_test_link("test123", Some("p1"), "t1", "a1", "2024-01-01T00:00:00Z"),
            )
            .await
            .unwrap();
        service
            .add_link(
                "t",
                &create_test_link("no_numbers", Some("p2"), "t2", "a2", "2024-01-01T00:00:01Z"),
            )
            .await
            .unwrap();

        let results = service
            .query_links(
                "t",
                r"SELECT * FROM link WHERE regex_match(source, '\d+') = 1",
            )
            .await
            .unwrap();
        assert_eq!(results.len(), 1);
    }

    #[tokio::test]
    async fn test_translation() {
        assert_eq!(
            SqliteLinkService::translate_surreal_to_sqlite(
                "SELECT * FROM link WHERE in.uri = 'test'"
            ),
            "SELECT * FROM link WHERE source = 'test'"
        );
        assert_eq!(
            SqliteLinkService::translate_surreal_to_sqlite(
                "SELECT * FROM link WHERE out.uri = 'test'"
            ),
            "SELECT * FROM link WHERE target = 'test'"
        );
        assert_eq!(
            SqliteLinkService::translate_surreal_to_sqlite(
                "SELECT * FROM link WHERE predicate IN ['a', 'b']"
            ),
            "SELECT * FROM link WHERE predicate IN ('a', 'b')"
        );
    }

    #[tokio::test]
    async fn test_concurrent_operations() {
        let service = SqliteLinkService::new("ad4m", "test_concurrent", None)
            .await
            .unwrap();

        let links: Vec<_> = (0..10)
            .map(|i| {
                create_test_link(
                    &format!("source{}", i),
                    Some(&format!("predicate{}", i)),
                    &format!("target{}", i),
                    &format!("author{}", i),
                    "2024-01-01T00:00:00Z",
                )
            })
            .collect();

        let futures: Vec<_> = links
            .iter()
            .map(|link| service.add_link("t", link))
            .collect();

        let results = futures::future::join_all(futures).await;
        assert!(results.iter().all(|r| r.is_ok()));

        let all = service
            .query_links("t", "SELECT * FROM link")
            .await
            .unwrap();
        assert_eq!(all.len(), 10);
    }
}
