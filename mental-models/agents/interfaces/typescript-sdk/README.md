# TypeScript SDK

`@coasys/ad4m` (directory `core/`) is the TypeScript client of the [RPC API](../rpc-api.md). `Ad4mClient` groups the calls by namespace (`agent`, `perspective`, `neighbourhood`, `languages`, `expression`, `runtime`, `ai`). All namespaces share one `ApiClient`, which owns the WebSocket, matches replies to calls, sends `events.watch` and reconnects. The package also holds the shared types and the model class decorators.

**Why:** Apps call typed methods, not raw messages. The types come from the executor's own contracts (`core/src/generated/api/`), so a change on one side breaks the build on the other. A refused token is final for that client (`onAuthFailed`), so an app cannot loop on a dead credential.

**Parent:** [Executor interfaces](../README.md)

## Parts
- [ad4m-connect](ad4m-connect.md): the library that finds an executor, runs a login flow and gives an app a connected `Ad4mClient`.
- [Model classes](model-classes.md): `Ad4mModel` and its decorators: how an app class becomes a subject class, and how its queries and live updates map to RPC calls.

<details><summary>How it works</summary>

1. `new Ad4mClient(baseUrl, token, options?)` builds one `ApiClient(baseUrl, token)`. The socket URL is `<ws base>/api/v1/ws?token=<token>`.
2. `ApiClient.call(type, params, { signal?, timeoutMs? })` sends `{ id, type, params }` and waits for the reply. The default timeout is 30 s, and 20 min for `LONG_METHODS`. Aborting sends `request.cancel`. Errors are `RpcError { status, body }`.
3. `ApiClient.on(event, handler, { perspective? })` registers a handler and keeps `events.watch` in step with the registered events (retried up to 5 times).
4. Reconnect uses backoff (0.5 s to 30 s). Calls in `READ_METHODS` are retried after a reconnect, and `onReconnect` callbacks fire.
5. Close code 1008 (or a 401 reply) → `onAuthFailed(reason)`: every call fails with 401, and the client stops reconnecting. Close code 1013 (locked or sealed) → reconnect and keep the token.
6. `AgentClient` carries the key-login calls (`user.loginWithKey`, `user.renewSession`, …). The SDK holds no key: ad4m-connect or the app's shell signs the login and renews the 1-hour token ([Key login](../../access/key-login.md)).
7. Each sub-client takes `baseUrl` and `token`. When no `ApiClient` is passed in, it builds its own, so a sub-client created alone opens its own socket (`AgentClient` constructor).
8. Sub-clients map methods to RPC names, e.g. `agent.requestCapability` → `agent.requestCapability`, `agent.loginUser` → `user.login`, `agent.revokeToken(requestId)` → `agent.revokeToken { token: requestId }`.

</details>

<details><summary>Where in the code</summary>

- [core/src/Ad4mClient.ts](../../../../core/src/Ad4mClient.ts): `Ad4mClient`, the sub-client getters.
- [core/src/apiClient.ts](../../../../core/src/apiClient.ts): `ApiClient`, `RpcError`, `LONG_TIMEOUT_MS`, `onAuthFailed`, `onReconnect`.
- [core/src/agent/AgentClient.ts](../../../../core/src/agent/AgentClient.ts): agent, auth and user calls.
- `core/src/hosting/HostClient.ts` (new): the `host.*` calls.
- [core/src/generated/api/](../../../../core/src/generated/api): `RpcMethods.ts` (`RpcMethods`, `READ_METHODS`, `LONG_METHODS`), `Events.ts`, one file per wire type. `generate:api-types` writes these files; [index.ts](../../../../core/src/generated/api/index.ts) is written by hand and needs an export line for each new type.
- [core/src/perspectives/](../../../../core/src/perspectives), [core/src/model/](../../../../core/src/model), [core/src/shacl/](../../../../core/src/shacl): perspective proxy and models (see [Model classes](model-classes.md)).
- [ad4m-hooks/](../../../../ad4m-hooks): React and Vue hooks.

</details>
