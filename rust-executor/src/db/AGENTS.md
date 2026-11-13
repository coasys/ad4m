# db/ — agent guide

Child modules of `db.rs` (`crate::db`). `db.rs` is being split: new SQL goes in a file here
with its own `impl Ad4mDb` block. Child modules can use the private `conn` field and
`Ad4mDb::new`.

## Files

| File | Content |
|---|---|
| `rekey.rs` | The rows `identity.rekey` moves to the main agent's new DID: notification owners, the flow bookkeeping (`flow_replica_state`, `flow_fired_mark`); and the claims the old key made, which it forgets |
| `bots.rs` | The `bots` table (a bot agent, its owner's identity, its offer and its grant; `set_bot_grant` is compare-and-set on the version, and a revoked grant never changes) and `bot_activity` (the bot's log, newest first, kept per bot and swept by age), read by `bots/` |
| `grants.rs` | The `grants` table: one row per token the executor issued, read by `agent::grants`; plus `pending_grant_writes`, the durable notes of the end-of-grant journal (see `agent::grants`' module doc): one pending op per grant id, `delete` outranking `revoke` in the note and the in-memory map alike — a revoke never downgrades a pending delete (`note_pending_grant_writes`'s upsert keeps a `delete` over a `revoke`). A grant is ended in memory before any persistence attempt; each pass writes the note, then the grant's own change, then clears the note — the note is cleared only when that op or a stronger one landed (`clear_pending_grant_writes` takes the landed op, so a landed revoke drops no `delete` note), and the pass retries the note's insert as well as the write. `agent::grants::load` ends every grant with a note and drives the ops again. Only a crash between the in-memory end and the first landed note loses the end |
| `endpoints.rs` | The `ai_endpoints` table (AI model servers: URL, key, protocol, capabilities, health). A model (`models`: `id, name, type, endpoint_id, remote_model, native_tools`) names its endpoint; `add_model` refuses an endpoint that does not exist and gives the endpoint the model type's capability. `ai_service::endpoints` calls them through `db_backend()` |
| `model_migration.rs` | The one-time move of a `models` table with `api_*` / `local_*` columns: each API model gets the endpoint with its protocol, URL and key (one is created, named by the host), each local model is deleted with one warning line (and its default), the table is rebuilt, `model_status` dropped. An import of an old export goes through the same linking. Runs only inside `Ad4mDb` (startup, import) |
| `tasks.rs` | Owner-scoped reads of the `tasks` table: `get_task_owner`, `get_tasks_owned_by` |
| `sealed.rs` | The sealed columns (table in its module doc): `SealedTable` per table (`SEALED_TABLES`), the row sealer (`writer`/`reader`, `readable` to skip a row that does not open), `seal_plain_values` (rows in the clear after the move under SQLCipher or an import), `reseal_store_values` (a store key rotation), `delete_person_rows` (a removed person's rows) |
| `ai_sessions.rs` | `ai_sessions` and `ai_messages`: AI chat sessions and their transcripts, owner-scoped (`owner_did IS ?`). Sealed under the owner's store: a session's `title`, `default_perspective`, `library_perspective`, `capabilities`; a message's `content` and `error` (its owner is its session's, `sealed::AI_MESSAGE_OWNER`). A content patch merges in Rust (`merge_patch`, RFC 7396), never with SQLite's `json_patch` |
| `identity.rs` | `operator_record`, `identity_agents`, `identity_joins` (created in `Ad4mDb::new`; its `accept` column, added to an older table on open, holds a claim for `identity.acceptJoin` and marks it cancelled; `add_join` writes a claim's row once and never replaces it, so a repeated claim keeps the decision), read by `identity/`. The operator record functions also take a bare connection for `ad4m-executor init --operator`, which runs before any executor. The key logs and `identity_heads`/`identity_fork_signers` are `identity::cache::KelCache`'s own connection to the same file |

Task CRUD (`add_task`, `get_task`, `get_tasks`, `update_task`, `remove_task`) and every
migration still live in `db.rs`.

## Executor code reaches the database through `DbBackend`

`crate::db_backend::db_backend()` is the executor's database. Executor code calls its methods,
never `Ad4mDb` directly: `Ad4mDb::with_global_instance` is `#[cfg(test)]`.

- A new `Ad4mDb` method that executor code calls gets one line in `ad4m_db_methods!` in
  `db_backend.rs`, with the same name, arguments and result. `LocalDb` and `SharedDb` get it
  from that line.
- `SharedDb` (shared mode) runs every domain method on the executor's own SQLite database.
  Only the key-value methods (`get`, `list`, `upsert`, `delete`) reach the platform Worker:
  the users and perspective metadata that executors share, under `shared:platform`.
- A check that must hold under the same lock as a write (a flag read, then a write) is one
  trait method, which runs both in one `on_ad4m_db` call: see
  `record_flow_replica_pass_unless_torn_down`. Two trait calls take the lock twice.
- A database that is not open is an error from every domain method, not a panic.

## Invariants

- `tasks.agent_did` is the DID of the managed agent whose session created the task (D16).
  `NULL` is a task of the main agent: admin credential, app token on the main agent, the
  executor's own interpretation tasks, and rows from before the column existed.
- A session reaches only its agent's tasks (`Guard::Owns { owner: Task }` in
  `api/guards.rs`; `ai.tasks` lists them): a user its own, a grant on the main agent the
  operator tasks. The operator reaches every task. Another agent's task is 404.
- Executor code that looks a task up by name reads operator tasks only
  (`get_tasks_owned_by(None)`). Otherwise a user can create a task under that name first and
  have it used for every user.
- `update_task` never writes `agent_did`: a rewrite keeps the owner. Export and import carry
  it, so a restore keeps each task's owner.
- A column that holds a person's or a perspective's data is sealed (`sealed.rs`): write it
  through `self.writer(&sealed::TABLE, owner, &[key], None)?.seal(..)`, read it through
  `self.reader(..)?.open(..)`, and wrap each row of a listing in `sealed::readable`. A new
  table of such data gets a `SealedTable` in `SEALED_TABLES` (a child table whose owner is its
  parent's comes before the parent: `delete_person_rows` reads the owner through it). A
  sealed column is never in a `WHERE`, an `ORDER BY`, an index or SQL that computes on it
  (`json_patch`, `COALESCE` with a literal): read the row, open, change, seal, write.

## Do not

- Add a column without a migration: an `alter_add_column` in `Ad4mDb::new`, or the
  table module's own migration that appends the columns last in one transaction (as
  `add_key_login_columns` in `db/grants.rs` does), so `SELECT *` positions match on
  fresh and migrated tables.
- Make a `SharedDb` domain method fail because the Worker does not serve it. A shared-mode
  executor serves it from SQLite.
