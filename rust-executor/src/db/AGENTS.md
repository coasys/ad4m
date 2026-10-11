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

- `tasks.user_email` is the managed user who created the task. `NULL` is an operator task:
  admin credential, app token, the executor's own interpretation tasks, or any row older
  than the column.
- User sessions reach only their own tasks (`Guard::Task` in `api/guards.rs`, and `ai.tasks`
  lists the caller's own); operators and apps reach every task.
- Executor code that looks a task up by name reads operator tasks only
  (`get_tasks_owned_by(None)`). Otherwise a user can create a task under that name first and
  have it used for every user.
- `update_task` never writes `user_email`: a rewrite keeps the owner. Export and import carry
  it, so a restore keeps each task's owner.

## Do not

- Add a column without an `alter_add_column` migration in `Ad4mDb::new`. Append it last, so
  `SELECT *` positions match on fresh and migrated tables.
