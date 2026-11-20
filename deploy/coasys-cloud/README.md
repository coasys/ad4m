# Coasys cloud deploy

Scripts and files for running the Coasys cloud executor (AR-09 4.12) from a
released ad4m tag. Modelled on `deploy/staging`, but it deploys **tags** only
and the executor always starts **sealed**: no start opens the data without a
person typing the storage secret.

| File | What |
|---|---|
| `coasys-cloud` | `install <tag>`, `deploy <tag>`, `rollback`, `unlock`, `status` (run with `sudo`) |
| `coasys-cloud.service` | System unit: `User=coasys-cloud`, restart rules for exit 75/78/79, hardening |
| `executor-config.json` | Settings: port 12500, multi-user on, `host.signup` `by_invite`, no per-tenant limits |
| `nginx.conf.example` | The `cloud.coasys.org` vhost: TLS, `/internal/` answers 404, no MCP |
| `test/coasys-cloud.test.sh` | The script against stubs (release download, systemd, executor) |

## Layout

| Path | What |
|---|---|
| `/opt/coasys-cloud/releases/<tag>/ad4m-executor` | One directory per release; `current` and `previous` link to them |
| `/var/lib/coasys-cloud/data` | The data directory, mode 0700, owned by `coasys-cloud` |
| `/var/lib/coasys-cloud/rollback` | One copy of the data taken before each deploy. Not a backup: it is lost with the disk |
| `/etc/coasys-cloud/executor-config.json` | Settings, root-owned |
| `/etc/credstore.encrypted/coasys-cloud-admin-credential` | The admin credential (`systemd-creds encrypt`) |

The storage secret is on no disk of the machine.

## The storage secret

`deploy`, `rollback` and `unlock` ask for it with no echo, check it is 64 hex
characters, keep it in a shell variable for the length of the command, and hand
it to `ad4m-executor storage unlock` through a pipe (`ad4m-executor storage
unlock` reads one line from stdin when stdin is not a terminal, and from the
terminal otherwise). It is never in an argument, the environment or a file.
Copy it from the keystore; there is no recovery code.

## Set up a machine

```bash
sudo ./coasys-cloud install v1.2.3     # user, directories, unit, vhost, release; no data
# make the admin credential:
#   head -c 32 /dev/urandom | base64 | sudo systemd-creds encrypt --name=admin-credential - \
#       /etc/credstore.encrypted/coasys-cloud-admin-credential
# set identity.storeKeys in /etc/coasys-cloud/executor-config.json, enable the vhost,
# then give the executor its first-run identity as `ad4m-executor init --operator <did:scid>`
# with --data-path /var/lib/coasys-cloud/data, as the coasys-cloud user
sudo ./coasys-cloud deploy v1.2.3
```

`deploy` refuses while the settings file holds a `REPLACE-WITH` value.

## Deploy, roll back, unlock

```bash
sudo coasys-cloud deploy v1.2.4   # fetch + sha256, stop, copy data, switch, start, unlock, gate
sudo coasys-cloud rollback        # previous release + the data copied before the last deploy
sudo coasys-cloud unlock          # after a reboot, a crash or exit 75: the executor is sealed
sudo coasys-cloud status          # run it once each working day: nobody is told of a sealed executor
```

The gate waits up to 300 s for `/health`, storage `open` and the agent
unlocked. On a failed gate the script stops the new release, restores the data
from the rollback copy (the failed run's data stays in `data.set-aside` until
the next restore), starts the previous release and unlocks it with the secret
it still holds. The next successful deploy replaces the rollback copy.

Not done by the script: `index.publish` after the gate (the hosting index is
outside this slice).

## The rebuild drill (AR-09 4.12.7)

On a clean machine that is not the production host, with staging's names and a
new drill storage secret: `install` the staging release, `init --operator` with
the drill identity, `deploy`, type the drill secret, start the drill compose
project, make a drill identity on the empty store, join as a new tenant with an
invite from `host.createInvite`, make a space, record the time. Start the
drill executor with `hosting_index.enabled = false` and a `link_server_url` of
the drill's own link server, then destroy the machine.

## Tests

`bash test/coasys-cloud.test.sh` (needs bash, flock, GNU coreutils). `shellcheck
coasys-cloud` when it is installed.
