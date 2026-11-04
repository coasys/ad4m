# db/ — agent guide

Child modules of `db.rs` (`crate::db`). `db.rs` is being split: new SQL goes in a file here
with its own `impl Ad4mDb` block. Child modules can use the private `conn` field and
`Ad4mDb::new`.

## Files

| File | Content |
|---|---|
| `grants.rs` | The `grants` table: one row per token the executor issued, read by `agent::grants` |
| `tasks.rs` | Owner-scoped reads of the `tasks` table: `get_task_owner`, `get_tasks_owned_by` |

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

## Do not

- Add a column without an `alter_add_column` migration in `Ad4mDb::new`. Append it last, so
  `SELECT *` positions match on fresh and migrated tables.
- Make a `SharedDb` domain method fail because the Worker does not serve it. A shared-mode
  executor serves it from SQLite.
