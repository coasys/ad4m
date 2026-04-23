# Repository-managed git hooks

Opt-in pre-commit hooks that guard against committing transient test
artifacts.

## Activate

Run once per clone:

```sh
git config core.hooksPath .githooks
```

Unset with `git config --unset core.hooksPath`.

## What's here

### `pre-commit`

Rejects a commit that stages `tests/js/bootstrapSeed.json` or
`tests/js/publishBootstrapSeed.json` in their post-test-run populated
state (non-empty `languageLanguageBundle`, extra `trustedAgents`).

These files are read by the executor at boot. The test harness
populates them before a run and resets them at the end via
`scripts/cleanTestingData.js` — but if a run is interrupted
(failure, Ctrl-C, killed process) the reset doesn't happen and the
populated state gets accidentally staged by a broad `git add`. The
JSON diffs are huge and easy to miss in review; this hook catches
them deterministically.

If the hook rejects a commit, re-run the cleanup and re-stage:

```sh
(cd tests/js && node scripts/cleanTestingData.js)
git add tests/js/bootstrapSeed.json tests/js/publishBootstrapSeed.json
git commit ...
```

## Requirements

- `bash` (Linux, macOS, Git Bash on Windows).
- `jq` on `PATH` — already a dev dependency for the docs/test scripts.
