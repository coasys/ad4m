/**
 * A WebSocket that answers like an executor with managed users: the user is the token
 * (`did:test:<token>`), and `emit` pushes an event to the sockets of one token.
 */
export type RpcCall = { type: string; token: string; params: any };

export const rpcLog: RpcCall[] = [];
const sockets: FakeExecutorSocket[] = [];

/** `name` is the display name the profile of a DID has. */
export const profiles = new Map<string, string>();

export function profileOf(did: string, name = profiles.get(did) ?? "none") {
  return {
    did,
    directMessageLanguage: "lang://net",
    perspective: {
      links: [{
        author: did,
        timestamp: "2026-01-01T00:00:00.000Z",
        data: { source: did, predicate: "name", target: name },
        proof: { valid: true },
      }],
    },
  };
}

/** Methods that fail (500) while listed. */
export const failing = new Set<string>();

/** Methods whose replies wait while listed; `release()` sends them. */
export const holding = new Set<string>();
const held: (() => void)[] = [];
export function release() {
  holding.clear();
  for (const send of held.splice(0)) send();
}

/** The perspectives `perspective.all` lists, per token (wire handles). */
export const perspectives = new Map<string, object[]>();

export function handleOf(uuid: string, name = uuid, sharedUrl: string | null = null) {
  return { uuid, name, state: "SYNCED", sharedUrl, neighbourhood: null, owners: null };
}

export class FakeExecutorSocket {
  static CONNECTING = 0; static OPEN = 1; static CLOSING = 2; static CLOSED = 3;
  readonly CONNECTING = 0; readonly OPEN = 1; readonly CLOSING = 2; readonly CLOSED = 3;
  onopen: ((e: any) => void) | null = null;
  onmessage: ((e: { data: string }) => void) | null = null;
  onerror: ((e: any) => void) | null = null;
  onclose: ((e: any) => void) | null = null;
  readyState = 0;
  token: string;

  constructor(url: string) {
    this.token = new URL(url).searchParams.get("token") ?? "";
    sockets.push(this);
    setTimeout(() => { this.readyState = 1; this.onopen?.({}); }, 0);
  }

  close() { this.readyState = 3; this.onclose?.({}); }

  send(data: string) {
    const { id, type, params } = JSON.parse(data);
    if (type === "ping") { setTimeout(() => this.onmessage?.({ data: JSON.stringify({ type: "pong" }) }), 0); return; }
    rpcLog.push({ type, token: this.token, params });
    const did = `did:test:${this.token}`;
    const result =
      type === "agent.status" ? { did, isInitialized: true, isUnlocked: true }
      : type === "agent.get" ? profileOf(did)
      : type === "agent.byDIDs" ? params.dids.map((d: string) => profileOf(d))
      : type === "perspective.all" ? (perspectives.get(this.token) ?? [])
      : true;
    const reply = () => this.onmessage?.({
      data: JSON.stringify(failing.has(type) ? { id, error: { code: 500, message: `${type} failed` } } : { id, result }),
    });
    if (holding.has(type)) held.push(reply);
    else setTimeout(reply, 0);
  }
}

/** Push an event to every socket of the user with this token. */
export function emit(token: string, event: Record<string, unknown>) {
  for (const s of sockets) if (s.token === token) s.onmessage?.({ data: JSON.stringify(event) });
}

export function installFakeExecutor() {
  rpcLog.length = 0;
  sockets.length = 0;
  profiles.clear();
  perspectives.clear();
  failing.clear();
  holding.clear();
  held.length = 0;
  (globalThis as any).WebSocket = FakeExecutorSocket;
}

export async function until(condition: () => boolean): Promise<void> {
  while (!condition()) await new Promise((r) => setTimeout(r, 1));
}
