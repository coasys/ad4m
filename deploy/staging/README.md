# deploy/staging

The staging executor behind `https://staging.ad4m.dev`: a headless
`ad4m-executor` that tracks the `staging` branch and redeploys itself when it
moves. It runs as `systemd --user` units of the `marvin` user on the marvin
box, next to the production node and does not replace it.

Setup, update, secrets, health checks and rollback are in the runbook,
[`docs-src/headless-executor.md`](../../docs-src/headless-executor.md).

| File | Installed to | What it does |
|---|---|---|
| `ad4m-staging.service` | `~/.config/systemd/user/` | Runs `current/ad4m-executor run --config …`; secrets through `LoadCredential=` |
| `ad4m-staging-update.service` | `~/.config/systemd/user/` | Runs `update.sh` once, at the lowest CPU and IO priority |
| `ad4m-staging-update.timer` | `~/.config/systemd/user/` | Starts the update 10 minutes after the last one finished |
| `update.sh` | `~/.local/share/ad4m-staging/bin/` | Fetch, build, snapshot, swap, gate, roll back; writes `status.json`. `update.sh rollback` goes back one build by hand |
| `agent.mjs` | `~/.local/share/ad4m-staging/bin/` | `status` / `generate` over the WebSocket RPC, secrets read from files |
| `executor-config.json` | `~/.config/ad4m-staging/` | Ports 12400/12401/12402, MCP 3003, all on 127.0.0.1; multi-user on; no SMTP; no dapp server |
| `nginx.conf.example` | `/etc/nginx/sites-available/staging-ad4m-dev` (root) | TLS vhost proxying to 127.0.0.1:12400, `/status.json` from `/var/www/ad4m-staging` |
| `test/update.test.sh` | — | Runs `update.sh` against a throwaway remote with stubbed build and systemd |

Paths at runtime:

| Path | Content |
|---|---|
| `~/ad4m-staging-src` | detached worktree of `~/nico/ad4m` (never of a CI workdir), own `target/` |
| `~/.local/share/ad4m-staging/releases/<sha>/` | `ad4m-executor` and `ad4m` of a build; `current` and `previous` link here |
| `~/.local/share/ad4m-staging/snapshots/` | data dir snapshots, `<time>-<sha>`: the newest of the running build and of the one before it |
| `~/.local/share/ad4m-staging/status.json` | deployed commit and last result; copied to `/var/www/ad4m-staging/status.json` |
| `~/.config/ad4m-staging/secrets/` | `admin-credential`, `unlock-passphrase` (dir 0700, files 0600) |
| `~/.ad4m-staging` | executor data dir |

`update.sh` and `agent.mjs` are installed copies: after changing them here,
install them again (runbook, "First-time setup", step 5).

Checks run in CI (`deploy-scripts` job):

```bash
shellcheck deploy/staging/update.sh deploy/staging/test/update.test.sh
deploy/staging/test/update.test.sh
```
