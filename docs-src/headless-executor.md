# Running a headless executor

How to run `ad4m-executor` as a `systemd --user` service without the
launcher: the settings in a config file, the secrets in files that only the
service can read, a check after every start, and a way back to the previous
build. It applies to any machine; the names below (`ad4m-executor.service`,
`~/.config/ad4m`, `~/.local/share/ad4m`) are a layout, not a requirement.

Every step is one command with the output to expect. Run them as the user
that owns the service. Background: the config file and the `AD4M_*_FILE`
secrets are described in `pages/developer-guides/executor-config.mdx`; the
executor's flags in `running-an-executor.md`. The repository ships one
deployment that follows this guide, with an update script that builds a
branch, snapshots the data and rolls back by itself, and a test of that
script: `deploy/staging/` (its README is about that deployment).

**Never print a secret.** Secrets are generated straight into their files,
the executor and the helper script read them from there, and no command
below takes one as an argument.

## Layout

| What | Where |
|---|---|
| Unit | `~/.config/systemd/user/ad4m-executor.service` |
| Config | `~/.config/ad4m/executor-config.json` |
| Secrets | `~/.config/ad4m/secrets/admin-credential`, `…/unlock-passphrase` (dir 0700, files 0600) |
| Builds | `~/.local/share/ad4m/releases/<version>/ad4m-executor`, with a link `current` to the one that runs |
| Data | `~/.ad4m` (the executor's default `app_data_path`); copies next to it before an update |
| Ports | RPC 12000, Holochain admin 12001 and app 12002, MCP 3001 (if enabled), all on `127.0.0.1` |
| Helper | `deploy/staging/agent.mjs` from this repository: `status` and `generate` over the RPC, secrets read from files |

Pin the Holochain ports in the config file: without them the executor takes
the first free port from 2000 and 1337 at every start.

## Build

The service runs a binary from a release directory, so that an update is a
new directory and a rollback is the link pointing back. From a checkout of
this repository at the commit you want:

```bash
pnpm install && pnpm build-dapp && (cd core && pnpm install) && pnpm run build-deno-snapshot && (cd cli && cargo build --release)
```

Expected: a `Finished` line for the `release` profile at the end. The first
build takes 30 to 90 minutes; later ones reuse `target/`. Install it under
the commit it was built from:

```bash
sha=$(git rev-parse --short HEAD) && install -D -m 0755 -t ~/.local/share/ad4m/releases/$sha target/release/ad4m-executor target/release/ad4m && ln -sfn releases/$sha ~/.local/share/ad4m/current
```

Expected: no output. `readlink ~/.local/share/ad4m/current` prints
`releases/<sha>`.

## First-time setup

1. **Lingering**, so the user's units run without a login session:

   ```bash
   loginctl show-user "$USER" -p Linger
   ```

   Expected: `Linger=yes` (otherwise, as root: `loginctl enable-linger <user>`).

2. **Config and secrets directories:**

   ```bash
   install -d -m 0700 ~/.config/ad4m ~/.config/ad4m/secrets
   ```

   Expected: no output.

3. **Executor config.** The keys are in `executor-config.mdx`; this one
   pins the ports, keeps every listener on loopback, turns the dapp server
   off and multi-user on, with no TLS and no SMTP (a reverse proxy
   terminates TLS, see below; without SMTP, accounts sign up with a
   password, see "Accounts"):

   ```bash
   cat > ~/.config/ad4m/executor-config.json <<EOF
   {
     "app_data_path": "$HOME/.ad4m",
     "port": 12000,
     "hc_admin_port": 12001,
     "hc_app_port": 12002,
     "localhost": true,
     "run_dapp_server": false,
     "auto_permit_cap_requests": false,
     "multi_user_config": { "enabled": true, "tls_config": null, "smtp_config": null },
     "log_config": { "rust_executor": "info", "holochain": "warn" },
     "mcp_enabled": false
   }
   EOF
   ```

   Expected: no output. `multi_user_config.enabled` cannot be turned off
   again on the same data directory; leave it `false` for a single-user
   node.

4. **The admin credential**, generated into its file (mode 0600, never
   printed):

   ```bash
   python3 -c 'import os,secrets,sys; fd=os.open(sys.argv[1], os.O_WRONLY|os.O_CREAT|os.O_EXCL, 0o600); os.write(fd, secrets.token_urlsafe(32).encode()); os.close(fd)' ~/.config/ad4m/secrets/admin-credential
   ```

   Expected: no output. `FileExistsError` means there already is one; keep it.

5. **The unlock passphrase**, the same way:

   ```bash
   python3 -c 'import os,secrets,sys; fd=os.open(sys.argv[1], os.O_WRONLY|os.O_CREAT|os.O_EXCL, 0o600); os.write(fd, secrets.token_urlsafe(32).encode()); os.close(fd)' ~/.config/ad4m/secrets/unlock-passphrase
   ```

   Expected: no output. For an agent that already exists (a node moved off
   the launcher), write its passphrase into the file with an editor instead
   and `chmod 600` it. Check both files:

   ```bash
   stat -c '%a %n' ~/.config/ad4m/secrets/*
   ```

   Expected: `600 …/admin-credential` and `600 …/unlock-passphrase`. The
   executor refuses a secret file that group or others can read.

6. **The unit.** `LoadCredential=` copies each secret file into a directory
   only this unit can read, and the `AD4M_*_FILE` variables point the
   executor at the copies, so the secrets are in no environment or argument
   list. `ConditionPathExists=` lets the unit skip, instead of failing in a
   loop, until a build is installed. With an admin credential set, MCP
   binds `0.0.0.0` unless `MCP_HOST` says otherwise.

   ```bash
   mkdir -p ~/.config/systemd/user && cat > ~/.config/systemd/user/ad4m-executor.service <<'EOF'
   [Unit]
   Description=AD4M executor (headless)
   After=network-online.target
   Wants=network-online.target
   ConditionPathExists=%h/.local/share/ad4m/current/ad4m-executor

   [Service]
   Type=simple
   WorkingDirectory=%h
   LoadCredential=admin-credential:%h/.config/ad4m/secrets/admin-credential
   LoadCredential=unlock-passphrase:%h/.config/ad4m/secrets/unlock-passphrase
   Environment=AD4M_ADMIN_CREDENTIAL_FILE=%d/admin-credential
   Environment=AD4M_UNLOCK_PASSPHRASE_FILE=%d/unlock-passphrase
   Environment=MCP_HOST=127.0.0.1
   ExecStart=%h/.local/share/ad4m/current/ad4m-executor run --config %h/.config/ad4m/executor-config.json
   Restart=on-failure
   RestartSec=10
   TimeoutStopSec=60

   [Install]
   WantedBy=default.target
   EOF
   systemctl --user daemon-reload
   ```

   Expected: no output.

7. **Review** what `run` will start with, secrets redacted:

   ```bash
   AD4M_ADMIN_CREDENTIAL_FILE=~/.config/ad4m/secrets/admin-credential ~/.local/share/ad4m/current/ad4m-executor config print --config ~/.config/ad4m/executor-config.json
   ```

   Expected: the settings of step 3, every secret shown as `"<redacted>"`.

8. **Enable and start:**

   ```bash
   systemctl --user enable --now ad4m-executor.service
   ```

   Expected: one `Created symlink …` line. The first start takes about two
   minutes (Holochain starts and the system languages install).

9. **Create the agent** (first start only). The helper reads the admin
   credential and the passphrase from their files:

   ```bash
   AD4M_ADMIN_CREDENTIAL_FILE=~/.config/ad4m/secrets/admin-credential AD4M_UNLOCK_PASSPHRASE_FILE=~/.config/ad4m/secrets/unlock-passphrase node deploy/staging/agent.mjs generate
   ```

   Expected: `generated did:key:z6Mk…`. From now on every start unlocks the
   agent from the passphrase file (`Agent unlocked at startup` in the log).
   Skip this step for an agent that already exists.

10. **Check** with the health commands below.

## Health check

1. The service:

   ```bash
   systemctl --user is-active ad4m-executor.service
   ```

   Expected: `active`.

2. The HTTP server:

   ```bash
   curl -fsS http://127.0.0.1:12000/health
   ```

   Expected: `{"status":"ok"}`. This only shows that the HTTP server is up;
   the next check shows that the node is usable.

3. The agent:

   ```bash
   AD4M_ADMIN_CREDENTIAL_FILE=~/.config/ad4m/secrets/admin-credential node deploy/staging/agent.mjs status
   ```

   Expected: `unlocked`. `locked` means the passphrase file does not match
   the agent (`journalctl --user -u ad4m-executor | grep 'Unlocking the agent at startup'`);
   `no-agent` means step 9 of the setup has not run.

4. Nothing listens beyond loopback:

   ```bash
   ss -ltnH '( sport = :12000 or sport = :12001 or sport = :12002 or sport = :3001 )' | awk '{print $4}' | sort -u
   ```

   Expected: only `127.0.0.1:…` and `[::1]:…` addresses.

5. Through the reverse proxy, if there is one, a caller without the
   credential gets no operator access (an empty token only reaches sign-up
   and login). The helper refuses to send a real credential anywhere but
   loopback, because the token travels in the URL and proxies log URLs, so
   this check sends an empty one:

   ```bash
   AD4M_URL=https://<your host> AD4M_ADMIN_CREDENTIAL_FILE=/dev/null node deploy/staging/agent.mjs status
   ```

   Expected: `agent.status: 403 Capability is not matched, …` and exit
   status 1.

6. Recent log lines:

   ```bash
   journalctl --user -u ad4m-executor -n 50 --no-pager
   ```

   Expected: no repeated `ERROR` lines from `rust_executor`.

## Update

An update is: build the new commit, stop the service, copy the data
directory, point `current` at the new build, run `init`, start, check. The
copy is what makes a rollback possible: a newer build may migrate the data,
and the old build cannot read it afterwards.

1. Build and install the new commit as in "Build", without the `ln`:

   ```bash
   sha=$(git rev-parse --short HEAD) && install -D -m 0755 -t ~/.local/share/ad4m/releases/$sha target/release/ad4m-executor target/release/ad4m
   ```

   Expected: no output. The running build is not touched.

2. Stop, copy the data directory, swap, init, start:

   ```bash
   old=$(readlink ~/.local/share/ad4m/current) && systemctl --user stop ad4m-executor.service && cp -a --reflink=auto ~/.ad4m ~/.ad4m.pre-$sha && ln -sfn releases/$sha ~/.local/share/ad4m/current && ~/.local/share/ad4m/current/ad4m-executor init --data-path ~/.ad4m && systemctl --user start ad4m-executor.service
   ```

   Expected: no output. `init` writes the network seed of this build and
   clears state the build can no longer read, as the launcher does on every
   start. The copy takes a while on a large data directory; `--reflink=auto`
   makes it instant on btrfs and XFS.

3. Run the health check. If check 3 does not print `unlocked` within a few
   minutes, roll back (below) with `$old`.

4. Keep one copy of the data directory and the previous release; delete
   older ones.

To automate this (follow a branch, build, snapshot, swap, check, roll back
on failure, publish the result), see `deploy/staging/update.sh` in this
repository and its test, which exercise the failure paths with the build
and systemd stubbed out.

## Roll back

To go back to the previous build and the data from before its update:

```bash
systemctl --user stop ad4m-executor.service && mv ~/.ad4m ~/.ad4m.failed-$sha && cp -a --reflink=auto ~/.ad4m.pre-$sha ~/.ad4m && ln -sfn "$old" ~/.local/share/ad4m/current && systemctl --user start ad4m-executor.service
```

Expected: no output. Then run the health check. **Everything written since
the update is lost**; it stays readable in `~/.ad4m.failed-<sha>`. Each
step runs only if the one before it succeeded, so a `mv` that fails (a
full disk, a permission) stops the command before the copy, and nothing
starts on a half-restored data directory. If a step failed, read its
error, fix the cause, and run the remaining steps by hand.

Rolling back on the current data instead (without the `mv` and `cp`) is
only safe if the new build did not migrate anything; when in doubt, use the
copy.

## Rotate secrets

**Admin credential.** Write a new one next to the old one, move it into
place, and restart (systemd copies the file again at every start):

```bash
python3 -c 'import os,secrets,sys; fd=os.open(sys.argv[1], os.O_WRONLY|os.O_CREAT|os.O_EXCL, 0o600); os.write(fd, secrets.token_urlsafe(32).encode()); os.close(fd)' ~/.config/ad4m/secrets/admin-credential.new && mv ~/.config/ad4m/secrets/admin-credential.new ~/.config/ad4m/secrets/admin-credential && systemctl --user restart ad4m-executor.service
```

Expected: no output. Then health check 3 prints `unlocked` (it reads the
new file). Every script or client that held the old credential has to get
the new one; JWTs that users got by logging in stay valid.

**Unlock passphrase.** The passphrase encrypts the agent's keys, and the
executor has no call to change it, so replacing the file alone makes every
start fail to unlock. Keep the passphrase of an agent that holds data.
Rotating it means a new, empty agent: stop the service, move the data
directory aside, write a new passphrase file as in setup step 5, run
`~/.local/share/ad4m/current/ad4m-executor init --data-path ~/.ad4m`, start
the service and repeat setup step 9. All data and accounts of the node are
gone after that.

## Accounts in multi-user mode without SMTP

With `multi_user_config.enabled` and no `smtp_config`, sign-up sends no
email and needs no code. A user connects a multi-user client to the node
and gives an email address and a password; no operator step is needed. Over
the RPC the flow is:

| Call (empty token) | Answer |
|---|---|
| `user.requestVerification {email}` for a new address | `requiresPassword: true`, "No account found yet. Provide a password to create one." |
| `user.create {email, password}` | `success: true` and the user's `did:key` |
| `user.requestVerification {email}` again | `requiresPassword: true`, "Email not configured. Please log in with your password." |
| `user.login {email, password}` | a JWT; a wrong password gets `Invalid credentials` |

Nothing checks that the address belongs to the person, so anyone who can
reach the node can create an account; the address is only a login name.
An account holder gets a user agent of their own, not operator access. For
a public node, either configure SMTP or put the node behind something that
limits who can reach it.

## Behind a reverse proxy

The executor speaks plain HTTP and WebSocket on its RPC port. To publish it
over TLS, terminate TLS in a reverse proxy on the same machine and proxy to
`127.0.0.1:12000` with the WebSocket upgrade headers (`Upgrade`,
`Connection`) passed through and a long read timeout, since RPC connections
stay open. Set `X-Forwarded-For` and `X-Forwarded-Proto`. Return 404 for
`/internal/`, which is for loopback callers only, and do not proxy the MCP
port. Set the proxy's maximum request body to at least the executor's own
limits (10 MB, 50 MB for audio transcriptions); at a smaller limit the
proxy refuses larger bodies itself with a 413 that carries no CORS headers,
which a browser reports only as `Failed to fetch`. If the node is not meant
to be indexed, send `X-Robots-Tag: noindex`. An example vhost for nginx is
`deploy/staging/nginx.conf.example`.

## Moving a node off the launcher

The ADAM Launcher runs the same executor; moving a node to a headless
service keeps its agent and data. Its settings are in
`~/.ad4m/launcher-state.json`, and the config file uses the same key names.
The launcher generates a new admin credential at every start, so no client
depends on the old one. Everything before step 5 has no effect on the
running node.

1. Write the config file from the launcher's state: `app_data_path` = the
   path of the launcher's selected agent, the launcher's `port`,
   `multi_user_config` (with the `tls_config` paths and `tls_port`, and
   `smtp_config` with `password_file` in place of `password`),
   `mcp_enabled`, `mcp_port`, `run_dapp_server` and `log_config`. Show the
   non-secret launcher keys with:

   ```bash
   jq '{multi_user_config: (.multi_user_config | del(.smtp_config.password)), mcp_enabled, mcp_port, log_config, selected_agent}' ~/.ad4m/launcher-state.json
   ```

   Expected: the settings above; no password.

2. Put the secrets into `~/.config/ad4m/secrets/` (0700 / 0600): a new
   admin credential (setup step 4), the agent's existing passphrase as
   `unlock-passphrase` (typed into the file with an editor; it is not
   generated), and, if SMTP is set, the SMTP password as `smtp-password`
   (from the mail account; the launcher's copy is encrypted with a key in
   the desktop keyring).

3. Review what `run` would start with (setup step 7). Expected: the
   settings of step 1, every secret shown as `"<redacted>"`.

4. Install the unit (setup step 6) with `ExecStart=` on an
   `ad4m-executor` built from the commit the launcher runs, so no data
   migration happens during the switch; add a third `LoadCredential=` line
   and `AD4M_SMTP_PASSWORD_FILE=%d/smtp-password` if SMTP is set, and drop
   `Environment=MCP_HOST=127.0.0.1` if MCP is meant to stay reachable from
   outside (it is behind the admin credential). Do not enable it yet.

5. Quit the launcher, then check that its ports are free:

   ```bash
   ss -ltnH '( sport = :<rpc port> or sport = :<tls port> or sport = :<mcp port> or sport = :8080 )'
   ```

   Expected: no output.

6. Copy the data directory:

   ```bash
   cp -a --reflink=auto ~/.ad4m ~/.ad4m.pre-headless
   ```

   Expected: no output (a few GB are copied).

7. Start the service:

   ```bash
   systemctl --user daemon-reload && systemctl --user start ad4m-executor.service
   ```

   Expected: no output.

8. Run the health check against the launcher's RPC port (expected
   `{"status":"ok"}` and `unlocked`); `ss` shows the TLS and MCP ports
   where the launcher had them, and a client logs in over TLS as before.

9. Keep it:

   ```bash
   systemctl --user enable ad4m-executor.service
   ```

   Expected: `Created symlink …`. Turn off the launcher's autostart, if any.

Back out, at any point:

```bash
systemctl --user disable --now ad4m-executor.service
```

Expected: `Removed …` or no output. Then start the launcher again. The data
directory is unchanged by a same-version start; if in doubt, restore
`~/.ad4m.pre-headless` before starting the launcher.
