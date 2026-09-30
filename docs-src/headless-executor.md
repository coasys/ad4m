# Headless executor runbook

How to run `ad4m-executor` as a `systemd --user` service without the
launcher. The worked example is the **staging executor** on the marvin box
(`https://staging.ad4m.dev`), which follows the `staging` branch and redeploys
itself. The last section describes moving the **team node** off the launcher;
that section is documentation only and needs an approved window.

Every step is one command with the output to expect. Run them as the user
that owns the service (`marvin` on the marvin box), from a checkout of this
repository. Background: the config file and the `AD4M_*_FILE` secrets are
described in `pages/developer-guides/executor-config.mdx`; the executor itself
in `running-an-executor.md`. The files used here are in `deploy/staging/`.

**Never print a secret.** Secrets are generated straight into their files,
the executor and `agent.mjs` read them from there, and no command below takes
one as an argument.

## The staging executor at a glance

| What | Where |
|---|---|
| Public URL | `https://staging.ad4m.dev` (nginx, `*.ad4m.dev` certificate) → `127.0.0.1:12400` |
| Deployed commit | `https://staging.ad4m.dev/status.json` |
| Ports, all on 127.0.0.1 | RPC 12400, Holochain admin 12401, Holochain app 12402, MCP 3003 (not proxied) |
| Units | `ad4m-staging.service`, `ad4m-staging-update.service`, `ad4m-staging-update.timer` |
| Config | `~/.config/ad4m-staging/executor-config.json` |
| Secrets | `~/.config/ad4m-staging/secrets/admin-credential`, `…/unlock-passphrase` |
| Builds | `~/.local/share/ad4m-staging/releases/<sha>/`, links `current` and `previous` |
| Data | `~/.ad4m-staging`; snapshots in `~/.local/share/ad4m-staging/snapshots/` |
| Source | `~/ad4m-staging-src`, a detached worktree of `~/nico/ad4m` |
| Email | none: SMTP is off, accounts sign up with a password (see "Accounts") |

Every 10 minutes `update.sh` fetches the head of the `staging` branch (by
its full ref name; a tag called `origin/staging` does not count). When it has
moved, the script builds it, stops the executor, snapshots the data
directory, points `current` at the new build, runs `ad4m-executor init`,
starts the executor and waits up to 180 s for `/health` and an unlocked
agent. A node that never had an agent passes on `/health` alone; once an
agent has been unlocked, a build that finds none fails. If the new build
does not get there, the script stops it, restores the snapshot, points
`current` back at the build that ran before and starts that again. If a step
between the stop and the start fails (a full disk during the snapshot, say),
the build that ran before is started again. A commit that failed to build or
failed that check is not tried again until `staging` moves on.

Anyone who can merge to `staging` can run code on this box as `marvin`.
`staging` is protected (one approving review) for that reason. As of this
writing the protection still allows force pushes and does not apply to
admins; both are Nico's to change.

## First-time setup

The DNS record and the nginx vhost need Cloudflare and root, and were set up
once; steps 1 and 2 record what was done. Everything after runs as the
service user without root.

1. **DNS.** `staging.ad4m.dev` is a Cloudflare A record, DNS only (not
   proxied), to the box. Check:

   ```bash
   dig +short staging.ad4m.dev
   ```

   Expected: the box's public IP address.

2. **nginx (root).** The vhost in `deploy/staging/nginx.conf.example` is
   installed as `/etc/nginx/sites-available/staging-ad4m-dev` and linked
   from `sites-enabled/`; `/var/www/ad4m-staging/` exists and belongs to the
   service user, who writes `status.json` there. The vhost proxies `/` with
   WebSocket upgrade to `127.0.0.1:12400`, sets `X-Forwarded-*` (once
   coasys/ad4m#1179 lands, they make the executor see every proxied request
   as a network caller; until then it ignores them, which does not matter
   here because the admin credential is set), returns 404 for `/internal/`,
   sends `X-Robots-Tag: noindex`, and does not expose MCP. Check:

   ```bash
   curl -fsS https://staging.ad4m.dev/status.json
   ```

   Expected: a JSON object. Before `update.sh` first ran, it is whatever was
   put there by hand; afterwards it has `deployed_sha` (`null` until the
   first deploy) and `last_result`.

3. **Lingering**, so the user's units run without a login session:

   ```bash
   loginctl show-user "$USER" -p Linger
   ```

   Expected: `Linger=yes` (otherwise, as root: `loginctl enable-linger marvin`).

4. **Config and secrets directories:**

   ```bash
   install -d -m 0700 ~/.config/ad4m-staging ~/.config/ad4m-staging/secrets
   ```

   Expected: no output.

5. **Scripts:**

   ```bash
   install -D -m 0755 -t ~/.local/share/ad4m-staging/bin deploy/staging/update.sh deploy/staging/agent.mjs
   ```

   Expected: no output. Run this again whenever `update.sh` or `agent.mjs`
   changes: the timer runs these installed copies, not the files in the
   repository.

6. **Executor config.** The file sets the data directory, the ports,
   multi-user on, no SMTP, no TLS and no dapp server. Its `app_data_path` is
   `/home/marvin/.ad4m-staging`; the `sed` makes it the current user's:

   ```bash
   sed "s|/home/marvin|$HOME|" deploy/staging/executor-config.json > ~/.config/ad4m-staging/executor-config.json
   ```

   Expected: no output.

7. **The admin credential**, generated into its file (mode 0600, never
   printed):

   ```bash
   python3 -c 'import os,secrets,sys; fd=os.open(sys.argv[1], os.O_WRONLY|os.O_CREAT|os.O_EXCL, 0o600); os.write(fd, secrets.token_urlsafe(32).encode()); os.close(fd)' ~/.config/ad4m-staging/secrets/admin-credential
   ```

   Expected: no output. `FileExistsError` means there already is one; keep it.

8. **The unlock passphrase**, the same way:

   ```bash
   python3 -c 'import os,secrets,sys; fd=os.open(sys.argv[1], os.O_WRONLY|os.O_CREAT|os.O_EXCL, 0o600); os.write(fd, secrets.token_urlsafe(32).encode()); os.close(fd)' ~/.config/ad4m-staging/secrets/unlock-passphrase
   ```

   Expected: no output. Check both files:

   ```bash
   stat -c '%a %n' ~/.config/ad4m-staging/secrets/*
   ```

   Expected: `600 …/admin-credential` and `600 …/unlock-passphrase`.

9. **PATH for the build.** The update service does not read a login shell,
   so it gets the directories of `node`, `pnpm`, `cargo` and `deno` from a
   file:

   ```bash
   printf 'PATH=%s\n' "$(dirname "$(command -v node)"):$(dirname "$(command -v pnpm)"):$HOME/.cargo/bin:$HOME/.deno/bin:/usr/local/bin:/usr/bin:/bin" > ~/.config/ad4m-staging/update.env
   ```

   Expected: no output. `cat ~/.config/ad4m-staging/update.env` shows one
   `PATH=` line without an empty entry (`::`); an empty entry means `node`
   or `pnpm` was not found.

10. **Units:**

    ```bash
    install -m 0644 -t ~/.config/systemd/user deploy/staging/ad4m-staging.service deploy/staging/ad4m-staging-update.service deploy/staging/ad4m-staging-update.timer && systemctl --user daemon-reload
    ```

    Expected: no output.

11. **Enable** the executor (starts at boot once a build is deployed) and
    the timer:

    ```bash
    systemctl --user enable ad4m-staging.service && systemctl --user enable --now ad4m-staging-update.timer
    ```

    Expected: two `Created symlink …` lines.

12. **First deploy.** The timer starts it 10 minutes after boot or after the
    last run; to start it now:

    ```bash
    systemctl --user start --no-block ad4m-staging-update.service
    ```

    Expected: no output. The first build takes 30 to 90 minutes (a later
    one reuses `~/ad4m-staging-src/target`). Follow it with
    `journalctl --user -u ad4m-staging-update -f` and
    `tail -f ~/.local/share/ad4m-staging/logs/build-*.log`. When it is done:

    ```bash
    jq -r '.last_result, .deployed_sha' ~/.local/share/ad4m-staging/status.json
    ```

    Expected: `deployed (no agent yet)` and the SHA of `origin/staging`.
    `waiting: origin/staging lacks #1215 PR1 (--config)` means `staging`
    does not have the config-file support the unit needs yet; the timer
    deploys by itself once it does.

13. **Create the agent** (first deploy only). `agent.mjs` reads the admin
    credential and the passphrase from their files:

    ```bash
    AD4M_ADMIN_CREDENTIAL_FILE=~/.config/ad4m-staging/secrets/admin-credential AD4M_UNLOCK_PASSPHRASE_FILE=~/.config/ad4m-staging/secrets/unlock-passphrase node ~/.local/share/ad4m-staging/bin/agent.mjs generate
    ```

    Expected, after about two minutes (Holochain starts and the system
    languages install): `generated did:key:z6Mk…`. From now on every start
    unlocks the agent from the passphrase file.

14. **Check** with the health commands below.

## Health check

1. The service:

   ```bash
   systemctl --user is-active ad4m-staging.service ad4m-staging-update.timer
   ```

   Expected: `active` twice.

2. The HTTP server:

   ```bash
   curl -fsS http://127.0.0.1:12400/health
   ```

   Expected: `{"status":"ok"}`.

3. The agent:

   ```bash
   AD4M_ADMIN_CREDENTIAL_FILE=~/.config/ad4m-staging/secrets/admin-credential node ~/.local/share/ad4m-staging/bin/agent.mjs status
   ```

   Expected: `unlocked`. `locked` means the passphrase file does not match
   the agent (`journalctl --user -u ad4m-staging | grep 'Unlocking the agent at startup'`);
   `no-agent` means step 13 of the setup has not run.

4. The deployed commit, as the team sees it:

   ```bash
   curl -fsS https://staging.ad4m.dev/status.json | jq -r '.deployed_sha, .last_result, .checked_at'
   ```

   Expected: the SHA of `origin/staging`, `deployed`, and a time less than
   about 15 minutes ago (the timer's last check).

5. Nothing listens beyond loopback:

   ```bash
   ss -ltnH '( sport = :12400 or sport = :12401 or sport = :12402 or sport = :3003 )' | awk '{print $4}' | sort -u
   ```

   Expected: only `127.0.0.1:…` and `[::1]:…` addresses.

6. A caller without the credential gets no operator access through nginx
   (an empty token only reaches sign-up and login). `agent.mjs` refuses to
   send a real credential anywhere but 127.0.0.1, because the token travels
   in the URL and nginx logs URLs; this check sends an empty one:

   ```bash
   AD4M_URL=https://staging.ad4m.dev AD4M_ADMIN_CREDENTIAL_FILE=/dev/null node ~/.local/share/ad4m-staging/bin/agent.mjs status
   ```

   Expected: `agent.status: 403 Capability is not matched, …` and exit
   status 1.

7. Recent log lines:

   ```bash
   journalctl --user -u ad4m-staging -n 50 --no-pager
   ```

   Expected: no repeated `ERROR` lines from `rust_executor`.

## Update

Staging updates itself: merge to `staging` and wait for the timer, at most
10 minutes plus the build. To check now instead of waiting:

```bash
systemctl --user start ad4m-staging-update.service
```

Expected: returns when the check (and any build) is done;
`jq -r .last_result ~/.local/share/ad4m-staging/status.json` shows the
outcome.

| `last_result` | Meaning |
|---|---|
| `deployed` / `deployed (no agent yet)` | The new build passed the check and runs |
| `building <sha>` | A build is running |
| `build failed for <sha> (logs/build-<sha>.log in the state directory)` | The old build keeps running; read `~/.local/share/ad4m-staging/logs/build-<sha>.log` |
| `rolled_back: <sha> failed the gate; <previous> runs again` | The new build did not get healthy and unlocked in 180 s; the previous build runs on the data from before the deploy |
| `waiting: origin/staging lacks #1215 PR1 (--config)` | `staging` cannot run this unit yet; nothing deployed |
| `error: deploying <sha> failed before its check; <old sha> runs again` | A step after the stop failed (the snapshot, for example); the old build was started again; `journalctl --user -u ad4m-staging-update` has the error |
| `rolled_back: the deploy of <sha> was interrupted; <previous> runs again` | An update was killed during the check of `<sha>` (a reboot, the OOM killer, a stopped update service); the next run rolled it back as if it had failed the check |
| `error: current is <sha>, but status.json names no deployed build (an interrupted first deploy, or a lost status.json); left to the operator` | `status.json` has no `deployed_sha`, but `current` names a build. Nothing was stopped, moved or built, and every run stops here until this is resolved by hand; see "Roll back" |
| `rolling back by hand from <new sha> to <previous sha>` | A rollback by hand is running |
| `rolled_back by hand: <new sha>; <previous sha> failed the gate too (agent: …)` / `error: the rollback by hand from <new sha> to <previous sha> was interrupted; left to the operator` | A rollback by hand failed its check or was killed. After a failed check or a kill during it, `<previous sha>` is current, on its restored data; after a kill before that, `current` is still `<new sha>`, staging may be stopped, and the data directory may be half a snapshot. The timer deploys nothing until this is resolved by hand; see "Roll back" |
| `error: could not restore the data of <sha>; staging is stopped` | A rollback could not move the data directory aside or copy the snapshot back (a full disk, most likely). Nothing runs, `current` is removed, and the timer deploys nothing until the data is restored by hand; see "Roll back" |
| `error: …` (other) | A precondition failed (missing config or secret, wrong file mode, `git fetch` failed); nothing changed |

`update.sh` does not build a commit again that failed. After fixing what
made it fail (for example freeing disk space), build it again with:

```bash
systemd-run --user --wait --collect --pipe -p EnvironmentFile="$HOME/.config/ad4m-staging/update.env" -p Nice=19 -p IOSchedulingClass=idle -E AD4M_STAGING_RETRY=1 "$HOME/.local/share/ad4m-staging/bin/update.sh"
```

Expected, at the end: `update: deployed <sha>` and
`Finished with result: success`.

**A production node** is updated by hand: build the new
`ad4m-executor` (`pnpm install && pnpm build-dapp && pnpm run build-deno-snapshot && (cd cli && cargo build --release)`),
stop the service, copy the data directory
(`cp -a --reflink=auto <data> <data>.pre-<sha>`), install the binary, run
`ad4m-executor init --data-path <data>`, start the service, and run the
health check. Roll back with the old binary and the copy.

## Rotate secrets

**Admin credential.** Write a new one next to the old one, move it into
place, and restart (systemd copies the file again at every start):

```bash
python3 -c 'import os,secrets,sys; fd=os.open(sys.argv[1], os.O_WRONLY|os.O_CREAT|os.O_EXCL, 0o600); os.write(fd, secrets.token_urlsafe(32).encode()); os.close(fd)' ~/.config/ad4m-staging/secrets/admin-credential.new && mv ~/.config/ad4m-staging/secrets/admin-credential.new ~/.config/ad4m-staging/secrets/admin-credential && systemctl --user restart ad4m-staging.service
```

Expected: no output. Then health check 3 prints `unlocked` (it reads the new
file). Every script or client that held the old credential has to get the
new one; JWTs that users got by logging in stay valid.

**Unlock passphrase.** The passphrase encrypts the agent's keys, and the
executor has no call to change it, so replacing the file alone makes every
start fail to unlock. On staging, rotating it means a new, empty agent, and
no deploy may run in between: until step 13 has run again, `status.json`
still says an agent was unlocked, so a deploy would fail its check, and so
would its rollback.

1. Stop the timer and wait for a running update to end (or stop it):
   `systemctl --user stop ad4m-staging-update.timer ad4m-staging-update.service`
2. Stop the executor: `systemctl --user stop ad4m-staging`.
3. Move `~/.ad4m-staging` aside, write a new passphrase file as in setup
   step 8, and run `~/.local/share/ad4m-staging/current/ad4m-executor init --data-path ~/.ad4m-staging`.
4. Start the executor and repeat setup step 13.
5. Start the timer again: `systemctl --user start ad4m-staging-update.timer`.

All staging data and accounts are gone after that. On a production node,
keep the passphrase.

## Roll back

A failed deploy rolls back by itself. To go back to the previous build when
the new one passed the check but misbehaves, stop the timer and any update
that is running (stopping the timer alone leaves a running build alone, and
`rollback` then fails with `another update is running`), then roll back:

```bash
systemctl --user stop ad4m-staging-update.timer ad4m-staging-update.service && systemd-run --user --wait --collect --pipe -p EnvironmentFile="$HOME/.config/ad4m-staging/update.env" "$HOME/.local/share/ad4m-staging/bin/update.sh" rollback
```

Expected, at the end: `update: <previous sha> runs again` and
`Finished with result: success`; `status.json` then has `deployed_sha` =
the previous SHA and `last_result` =
`rolled_back by hand: <new sha>; <previous sha> runs again`.
`error: no previous build to roll back to` means there is only one build
(the first deploy, or a second rollback in a row).
`error: current is <…>, not the deployed <sha>; …` means a deploy or
rollback did not finish, or a restore failed; `rollback` changes nothing
then. After a deploy killed in its check, start the timer again: its next
run rolls that deploy back. Otherwise see the cases below.
`error: the unfinished rollback by hand keeps the data of <sha> in …`
means an earlier rollback moved the newer build's data aside and did not
finish; `rollback` changes nothing until it is resolved as below.
`error: no snapshot of the data of <previous sha>; …` means the snapshot is
missing; the previous build would start on data a newer build may have
migrated. Only if that is acceptable, run the same command with
`-E AD4M_STAGING_KEEP_DATA=1` after `--pipe`.
`error: could not restore the data of <sha>; staging is stopped` (also
written by an automatic rollback) means the data directory could not be
moved aside or the snapshot could not be copied back, most likely because
the disk is full. Nothing runs and `current` is gone, so neither a reboot
nor the timer starts a build on a half-copied data directory. The failed
build's data is in `~/.ad4m-staging.failed` (unless the move failed, then
it is still in `~/.ad4m-staging`), and the snapshot of `<sha>` is kept.
Free space, then restore by hand:

```bash
S=~/.local/share/ad4m-staging; sha=<sha>; snap=$(ls -d "$S"/snapshots/*-"$sha" | tail -n 1); [ ! -e ~/.ad4m-staging ] || mv ~/.ad4m-staging ~/.ad4m-staging.failed.2; cp -a --reflink=auto "$snap" ~/.ad4m-staging && ln -sfn "releases/$sha" "$S/current" && systemctl --user start ad4m-staging
```

Then run health check 3. If `<sha>` is not the `deployed_sha` in
`status.json` (a rollback by hand), also set it:
`jq --arg s "$sha" '.deployed_sha = $s | .previous_sha = null | .in_flight = null' "$S/status.json" > "$S/status.json.new" && mv "$S/status.json.new" "$S/status.json"`.

**A rollback by hand that did not finish** (`… failed the gate too` or
`error: the rollback by hand from <new sha> to <previous sha> was interrupted`).
Every run of the timer stops at it until `in_flight` is cleared. Find out
how far it got:

```bash
S=~/.local/share/ad4m-staging; readlink "$S/current"; jq -r .failed_data "$S/status.json"; ls -d ~/.ad4m-staging ~/.ad4m-staging.failed
```

- `current` is `releases/<previous sha>`: the rollback failed its check or
  was killed during it. The previous build is on the restored snapshot, and
  the newer build's data is in `~/.ad4m-staging.failed`. Run health check
  3. If the previous build is up and unlocked now, record it with the `jq`
  command above (`sha=<previous sha>`). Otherwise go forward again: stop
  `ad4m-staging`, move `~/.ad4m-staging` aside, move
  `~/.ad4m-staging.failed` back to `~/.ad4m-staging`, run
  `ln -sfn "releases/<new sha>" "$S/current"`, start `ad4m-staging`, and
  run
  `jq '.in_flight = null' "$S/status.json" > "$S/status.json.new" && mv "$S/status.json.new" "$S/status.json"`.
  `rollback` refuses in this state, because `current` is not `deployed_sha`.
- `current` is `releases/<new sha>` and `failed_data` is `<new sha>`: the
  rollback was killed after it moved the data aside, most likely while it
  copied the snapshot back. Staging is stopped, `~/.ad4m-staging` is half a
  snapshot or missing, and the newer build's data is in
  `~/.ad4m-staging.failed`. Go forward again as above, without the `ln`
  step (move `~/.ad4m-staging` aside only if it exists). `rollback` refuses
  until `in_flight` is cleared, because it would replace
  `~/.ad4m-staging.failed`; after going forward it can run again.
- `current` is `releases/<new sha>` and `failed_data` is `null`: the
  rollback was killed before it moved the data aside. Staging may be
  stopped (a kill before the stop leaves it running); `~/.ad4m-staging` is
  the newer build's data, whole (if it is missing, the kill came right
  after the move: move `~/.ad4m-staging.failed` back first). A
  `~/.ad4m-staging.failed` next to it is from an earlier rollback. Start
  `ad4m-staging` if it is not running and clear `in_flight` as in the first
  case, or run the rollback command again.

After going forward, the timer deploys the next commit on `staging` again.

**`error: current is <sha>, but status.json names no deployed build`**:
either `status.json` was lost or reset, or the very first deploy was killed
before its check. `update.sh` cannot tell the two apart, so it touches
nothing. Run health check 3.
- If `<sha>` runs and prints `unlocked`, `status.json` was lost. Write it
  again (drop `agent` if the check printed `no-agent`):
  `S=~/.local/share/ad4m-staging; p=$(readlink "$S/previous" || true); jq -n --arg s "<sha>" --arg p "${p#releases/}" '{deployed_sha: $s, previous_sha: (if $p == "" then null else $p end), agent: "unlocked"}' > "$S/status.json.new" && mv "$S/status.json.new" "$S/status.json"`.
  The next run then treats `<sha>` as deployed, and `rollback` can still
  go back to `previous`.
- If this was the first deploy, stop `ad4m-staging`, move `~/.ad4m-staging`
  aside (it holds only what the unchecked build wrote) and
  `rm ~/.local/share/ad4m-staging/current`. The next run deploys the head
  of `staging` again as the first deploy.

What it does: stops the executor, moves the data directory to
`~/.ad4m-staging.failed` (replacing an older one; `failed_data` in
`status.json` then names the build it holds), restores the snapshot
taken when the new build replaced the previous one, points `current` at the
previous build, starts it and checks it. **Everything written to staging
since that deploy is lost** (it stays readable in `~/.ad4m-staging.failed`).
The head of `staging` counts as failed, so the timer does not deploy it
again; the next commit on `staging` deploys normally. There is one step
back: after a rollback, `previous` is gone. Snapshots of failed deploys do
not push out the one a rollback needs: the newest snapshot of the running
build and of the one before it are kept. Restart the timer when you are done:

```bash
systemctl --user start ad4m-staging-update.timer
```

Expected: no output.

## Accounts on staging

Staging runs in multi-user mode without SMTP, so sign-up sends no email and
needs no code. A team member connects a multi-user client to
`https://staging.ad4m.dev` and gives an email address and a password; no
operator step is needed. Over the RPC the flow is:

| Call (empty token) | Answer |
|---|---|
| `user.requestVerification {email}` for a new address | `requiresPassword: true`, "No account found yet. Provide a password to create one." |
| `user.create {email, password}` | `success: true` and the user's `did:key` |
| `user.requestVerification {email}` again | `requiresPassword: true`, "Email not configured. Please log in with your password." |
| `user.login {email, password}` | a JWT; a wrong password gets `Invalid credentials` |

Nothing checks that the address belongs to the person, so anyone who can
reach the URL can create an account; the address is only a login name
here. An account holder gets a user agent of their own, not operator
access.

## Migrating the team node off the launcher

> **Not done. Needs a window approved by Nico.** The team node serves the
> team; this section is the plan for that window, not something to run
> ahead of it. Nothing here touches the node until step 5.

The team node runs today inside the ADAM Launcher: TLS on `0.0.0.0:12100`,
MCP on `0.0.0.0:3001`, RPC on `127.0.0.1:12000`, the dapp server on
`127.0.0.1:8080`, multi-user on, SMTP set. The launcher generates a new
admin credential at every start, so no client depends on it. Its settings
are in `~/.ad4m/launcher-state.json`; the config file uses the same key
names.

Before the window (no effect on the running node):

1. Write `~/.config/ad4m-team/executor-config.json` from the launcher's
   state: `app_data_path` = the path of the launcher's selected agent,
   `port: 12000`, `multi_user_config` with `enabled: true`, the
   `tls_config` (certificate and key paths, `tls_port: 12100`) and the
   `smtp_config` with `password_file` instead of the password,
   `mcp_enabled: true`, `mcp_port: 3001`, `run_dapp_server: true`, and the
   `log_config`. Show the non-secret launcher keys with:

   ```bash
   jq '{multi_user_config: (.multi_user_config | del(.smtp_config.password)), mcp_enabled, mcp_port, log_config, selected_agent}' ~/.ad4m/launcher-state.json
   ```

   Expected: the settings above; no password.

2. Put the secrets into `~/.config/ad4m-team/secrets/` (0700 / 0600): a new
   admin credential (setup step 7), the agent's existing passphrase as
   `unlock-passphrase` (the operator types it into the file with an editor;
   it is not generated), and the SMTP password as `smtp-password` (from the
   mail account; the launcher's copy is encrypted with a key in the desktop
   keyring).

3. Review what `run` would start with:

   ```bash
   AD4M_ADMIN_CREDENTIAL_FILE=~/.config/ad4m-team/secrets/admin-credential ad4m-executor config print --config ~/.config/ad4m-team/executor-config.json
   ```

   Expected: the settings of step 1, every secret shown as `"<redacted>"`.

4. Install `~/.config/systemd/user/ad4m-team.service`: a copy of
   `ad4m-staging.service` with the `ad4m-team` paths, `ExecStart=` on an
   `ad4m-executor` built from the commit the launcher runs (so no data
   migration happens in the window), a third `LoadCredential=` line for
   `smtp-password`, and without `Environment=MCP_HOST=127.0.0.1` (MCP stays
   on `0.0.0.0:3001`, now behind the admin credential). Do not enable it
   yet.

In the window:

5. Quit the launcher, then check that the ports are free:

   ```bash
   ss -ltnH '( sport = :12000 or sport = :12100 or sport = :3001 or sport = :8080 )'
   ```

   Expected: no output.

6. Copy the data directory:

   ```bash
   cp -a --reflink=auto ~/.ad4m ~/.ad4m.pre-headless
   ```

   Expected: no output (a few GB are copied).

7. Start the headless node:

   ```bash
   systemctl --user daemon-reload && systemctl --user start ad4m-team.service
   ```

   Expected: no output.

8. Check it: the health check above against port 12000 with the
   `ad4m-team` secret paths (expected `{"status":"ok"}` and `unlocked`),
   `ss` shows `0.0.0.0:12100` and `0.0.0.0:3001`, and a team member logs in
   from a client over `https://…:12100`.

9. Keep it:

   ```bash
   systemctl --user enable ad4m-team.service
   ```

   Expected: `Created symlink …`. Turn off the launcher's autostart, if any.

Back out, at any point in the window:

```bash
systemctl --user disable --now ad4m-team.service
```

Expected: `Removed …` or no output. Then start the launcher again. The data
directory is unchanged by a same-version start; if in doubt, restore
`~/.ad4m.pre-headless` before starting the launcher.
