const AD4M_GQL = process.env.AD4M_GQL_URL ?? "http://localhost:4000/graphql";
const TIMEOUT_MS = 10_000;

export interface GqlResponse<T> {
  data?: T;
  errors?: Array<{ message: string }>;
}

export async function gql<T>(query: string, variables: Record<string, unknown> = {}): Promise<T> {
  let resp: Response;
  try {
    resp = await fetch(AD4M_GQL, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ query, variables }),
      signal: AbortSignal.timeout(TIMEOUT_MS),
    });
  } catch (e) {
    if (e instanceof Error && e.name === "TimeoutError") {
      throw new Error("AD4M executor timed out. Is it running? Check: curl http://localhost:4000/graphql");
    }
    throw new Error(`Cannot reach AD4M executor at ${AD4M_GQL}. Start it with: ad4m-executor run --gql-port 4000`);
  }

  if (!resp.ok) {
    throw new Error(`AD4M HTTP ${resp.status}. If 401, the agent keystore may be locked — unlock it first.`);
  }

  const json: GqlResponse<T> = await resp.json() as GqlResponse<T>;
  if (json.errors?.length) {
    const msg = json.errors[0].message;
    if (msg.includes("locked") || msg.includes("unlock")) {
      throw new Error(`Agent is locked. Unlock it: curl -X POST ${AD4M_GQL} -H 'Content-Type: application/json' -d '{"query":"mutation { agentUnlock(passphrase: \\"YOUR_PASS\\", holochain: true) { isUnlocked } }"}'`);
    }
    throw new Error(msg);
  }

  return json.data as T;
}

export function formatError(e: unknown): string {
  return `Error: ${e instanceof Error ? e.message : String(e)}`;
}
