# db/ — agent guide

Child modules of `db.rs` (`crate::db`). `db.rs` is being split: new SQL goes in a file here
with its own `impl Ad4mDb` block. Child modules can use the private `conn` field and
`Ad4mDb::new`.

## Files

| File | Content |
|---|---|
| `session_cutoffs.rs` | The users' session cutoffs (`users.sessions_valid_after`), read by `agent::revoked` |
| `tasks.rs` | Owner-scoped reads of the `tasks` table: `get_task_owner`, `get_tasks_owned_by` |

Task CRUD (`add_task`, `get_task`, `get_tasks`, `update_task`, `remove_task`) and every
migration still live in `db.rs`.

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
