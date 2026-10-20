# TypeScript SDK

`@coasys/ad4m` (directory `core/`) is the TypeScript client of the [RPC API](../rpc-api.md). `Ad4mClient` groups the calls by namespace (`agent`, `perspective`, `neighbourhood`, `languages`, `expression`, `runtime`, `ai`, and the identity and host calls). All namespaces share one `ApiClient`, which owns the WebSocket, matches replies to calls, sends `events.watch` and reconnects. The client holds one token. A key-session token lasts 1 hour; ad4m-connect or the app's shell renews it before expiry with a fresh signed statement. The package also holds the shared types and the model class decorators.

**Why:** Apps call typed methods, not raw messages. The types come from the executor's own contracts (`core/src/generated/api/`), so a change on one side breaks the build on the other. A refused token is final for that client (`onAuthFailed`), so an app cannot loop on a dead credential; a renewed token keeps the same grant, so open sockets and live queries stay. The SDK holds no key and signs nothing: the app's shell or Coasys Connect signs a key login, and the SDK only carries the calls.

**Parent:** [Executor interfaces](../README.md)

## Parts
- [ad4m-connect](ad4m-connect.md): the library that gets a web app a session on one of the person's executors through Coasys Connect, or through the older local and password routes.
- [Model classes](model-classes.md): `Ad4mModel` and its decorators: how an app class becomes a subject class, and how its queries and live updates map to RPC calls.

<details><summary>How it works</summary>

1. `new Ad4mClient(baseUrl, token, options?)` builds one `ApiClient(baseUrl, token)`. The socket URL is `<ws base>/api/v1/ws?token=<token>`. On the phone the socket runs over the shell's pinned transport.
2. `ApiClient.call(type, params, { signal?, timeoutMs? })` sends `{ id, type, params }` and waits for the reply. The default timeout is 30 s, and 20 min for `LONG_METHODS`. Aborting sends `request.cancel`. Errors are `RpcError { status, body }`.
3. `ApiClient.on(event, handler, { perspective? })` registers a handler and keeps `events.watch` in step with the registered events (retried up to 5 times).
4. Reconnect uses backoff (0.5 s to 30 s). Calls in `READ_METHODS` are retried after a reconnect, and `onReconnect` callbacks fire.
5. Close code 1008 (or a 401 reply) → `onAuthFailed(reason)`: every call fails with 401, and the client stops reconnecting. Close code 1013 (wallet locked, storage sealed) → reconnect and keep the token.
6. Key-login calls: `user.sessionChallenge`, `user.loginWithKey`, `user.redeemCode`, `user.renewSession`, `user.currentSession`, `user.logout`, `user.endAllSessions` ([Key login](../../access/key-login.md)). Host calls (`host.*`) live in `HostClient`.
7. Each sub-client takes `baseUrl` and `token`. When no `ApiClient` is passed in, it builds its own, so a sub-client created alone opens its own socket (`AgentClient` constructor).
8. Sub-clients map methods to RPC names, e.g. `agent.loginUser` → `user.login`, `agent.requestCapability(authInfo, agent?)` → `agent.requestCapability`, `agent.revokeToken(requestId)` → `agent.revokeToken { token: requestId }`.

</details>

<details><summary>Where in the code</summary>

- [core/src/Ad4mClient.ts](../../../../core/src/Ad4mClient.ts): `Ad4mClient`, the sub-client getters.
- [core/src/apiClient.ts](../../../../core/src/apiClient.ts): `ApiClient`, `RpcError`, `LONG_TIMEOUT_MS`, `onAuthFailed`, `onReconnect`.
- [core/src/agent/AgentClient.ts](../../../../core/src/agent/AgentClient.ts): agent, auth, key-login and user calls.
- [core/src/hosting/HostClient.ts](../../../../core/src/hosting/HostClient.ts) (new): tenant, join and host-policy calls.
- [core/src/generated/api/](../../../../core/src/generated/api): `RpcMethods.ts` (`RpcMethods`, `READ_METHODS`, `LONG_METHODS`), `Events.ts`, one file per wire type. `generate:api-types` writes these files; [index.ts](../../../../core/src/generated/api/index.ts) is written by hand and needs an export line for each new type.
- [core/src/perspectives/](../../../../core/src/perspectives), [core/src/model/](../../../../core/src/model), [core/src/shacl/](../../../../core/src/shacl): perspective proxy and models (see [Model classes](model-classes.md)).
- [ad4m-hooks/](../../../../ad4m-hooks): React and Vue hooks.

</details>
