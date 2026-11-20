# @coasys/connect

Sign a web app in with the person's own browser key. The library opens the Coasys Connect frame
(`https://coasys.id`, or `https://staging.coasys.id`), where the person picks one of their devices
and the browser key signs the login. The app gets an authenticated `Ad4mClient`. The token stays
in memory; the library renews it through the frame.

```ts
import { getAd4mConnect } from "@coasys/connect";

const { core, client } = getAd4mConnect({
  appId: "my-app", // the id the platform registry lists your origins under
  endpoints: { authDomain: "https://coasys.id" },
  capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
});

core.addEventListener("authstatechange", (e) => console.log((e as CustomEvent).detail));
const ad4m = await client;
```

Options:

| Option | Meaning |
| --- | --- |
| `appId` | The app's id in the platform registry (`GET /v1/connect/apps/{appId}`). The executor accepts a login only from one of the app's registered origins. |
| `endpoints.authDomain` | Where the Connect frame lives. Required. |
| `capabilities` | What the app asks for. |
| `appInfo` | Name, description and icon the frame shows. Default: the page's title. |
| `mode` | `iframe` (default) or `popup`. A popup needs a user gesture. |

`core.disconnect()` ends the session. `core.connectAsGuest(hostUrl)` connects to a hosted
multi-user node as a guest, with no frame.

Message types for the frame (`ConnectRequest`, `ConnectAnswer`, `ConnectRenew`, `ConnectRenewed`,
`ConnectDenied`, `ConnectReady`, `ConnectSignOut`) are exported from the package.

This package replaces `@coasys/ad4m-connect`. When it starts it removes the `ad4m-token` keys that
package left in `localStorage`.
