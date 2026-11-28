# bot/ — the SDK's `bot.*` calls

`BotClient` (`Ad4mClient.bot`): `createAgent`, `setGrant`, `pause`, `resume`, `revoke`,
`grant`, `list`, `log`. The executor side is `rust-executor/src/bots/`.

- The types (`BotAgent`, `CreatedBot`, `BotActivity`, `BotAction`, `GrantState`, ...) are
  generated from the executor into `src/generated/api/`; do not write them by hand.
- `pause`, `resume` and `revoke` read the grant and write it back with the version read, so a
  grant changed in between answers 409 instead of being overwritten.
- A bot's harness gives the reason for its writes as the frame's `meta.reason`:
  `ApiClient.setReason` / `Ad4mClient.setReason` for every call, `CallOptions.reason` for one.
- Tests: `src/Ad4mClient.test.ts` (`describe('BotClient')`); end to end in
  `tests/js/tests/identity.test.ts`.
