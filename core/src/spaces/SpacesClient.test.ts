import { ApiClient } from "../apiClient";
import { Ad4mClient } from "../Ad4mClient";
import type { LinkLanguageChoice } from "../generated/api/LinkLanguageChoice";
import type { LinkServerEntry } from "../generated/api/LinkServerEntry";
import type { SpaceHolding } from "../generated/api/SpaceHolding";
import { NeighbourhoodClient } from "../neighbourhood/NeighbourhoodClient";
import { RuntimeClient } from "../runtime/RuntimeClient";
import { SpacesClient } from "./SpacesClient";

const choice: LinkLanguageChoice = {
  kind: "linkServer",
  linkLanguage: "QmLink",
  serverUrl: "https://link.example",
  roomId: "G6zF1MMHd8RamYLF7ZGBVpCbDhfigXNvt8mP9Dv5BBhY",
  creator: "did:scid:coasys:1:QmCreator",
  fixedAt: "2026-11-01T10:00:00.000Z",
};
const holding: SpaceHolding = {
  perspectiveUuid: "p1",
  neighbourhoodUrl: "neighbourhood://QmSpace",
  name: "a space",
  how: "made",
  since: choice.fixedAt,
  choice,
  syncState: "SYNCED",
  myRole: "admin",
};
const servers: LinkServerEntry[] = [
  { url: "https://mine.example", name: "mine.example", source: "operator" },
  { url: "https://link.example", name: "Coasys", source: "seed" },
];

/** The executor's answers, by method: what each method's contract returns. */
const answers: Record<string, unknown> = {
  "neighbourhood.create": { url: "neighbourhood://QmSpace", choice },
  "neighbourhood.linkLanguageChoice": choice,
  "runtime.linkServers": servers,
  "spaces.list": [holding],
};

/** A socket that opens at once, records every request and answers it from `answers`. */
class FakeWebSocket {
  static sent: { type: string; params: Record<string, unknown> }[] = [];
  readyState = 0;
  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string }) => void) | null = null;
  onerror: ((e: unknown) => void) | null = null;
  onclose: (() => void) | null = null;
  constructor(public url: string) {
    queueMicrotask(() => { this.readyState = 1; this.onopen?.(); });
  }
  send(data: string) {
    const { id, type, params } = JSON.parse(data);
    FakeWebSocket.sent.push({ type, params });
    const result = type in answers ? answers[type] : true;
    queueMicrotask(() => this.onmessage?.({ data: JSON.stringify({ id, result }) }));
  }
  close() { this.readyState = 3; }
}

const base = "http://localhost:0";

describe("spaces by person: client shapes", () => {
  const api = new ApiClient(base, undefined, FakeWebSocket as unknown as new (url: string) => WebSocket);
  afterAll(() => api.closeAll());
  beforeEach(() => { FakeWebSocket.sent = []; });
  const last = () => FakeWebSocket.sent.filter((s) => !s.type.startsWith("events.")).pop();

  it("neighbourhood.create sends the perspective, the sync choice and the rules", async () => {
    const client = new NeighbourhoodClient(base, undefined, api);
    const created = await client.create({
      perspectiveUuid: "p1",
      sync: { kind: "ownLinkServer", url: "https://mine.example" },
      rules: { add: "admins", join: "open" },
    });
    expect(last()).toEqual({
      type: "neighbourhood.create",
      params: {
        perspectiveUuid: "p1",
        sync: { kind: "ownLinkServer", url: "https://mine.example" },
        rules: { add: "admins", join: "open" },
      },
    });
    expect(created).toEqual({ url: "neighbourhood://QmSpace", choice });
  });

  it("neighbourhood.linkLanguageChoice names the perspective", async () => {
    const client = new NeighbourhoodClient(base, undefined, api);
    expect(await client.linkLanguageChoice("p1")).toEqual(choice);
    expect(last()).toEqual({ type: "neighbourhood.linkLanguageChoice", params: { perspectiveUuid: "p1" } });
  });

  it("runtime.linkServers answers the directory in order", async () => {
    const client = new RuntimeClient(base, undefined, api);
    expect(await client.linkServers()).toEqual(servers);
    expect(last()).toEqual({ type: "runtime.linkServers", params: {} });
  });

  it("spaces.list answers the holdings", async () => {
    const client = new SpacesClient(base, undefined, api);
    expect(await client.list()).toEqual([holding]);
    expect(last()).toEqual({ type: "spaces.list", params: {} });
  });

  it("Ad4mClient has the spaces client", () => {
    const client = new Ad4mClient(base, undefined, {
      webSocketImpl: FakeWebSocket as unknown as new (url: string) => WebSocket,
    });
    expect(client.spaces).toBeInstanceOf(SpacesClient);
    client.close();
  });
});
