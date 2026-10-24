/**
 * Shared A2 helper — provision an AD4M user identity on a multi-user node and
 * verify what a harness created, over the executor's MCP surface. Used by the
 * Hermes and Sovereign A2 verify scripts.
 *
 *   provision <base> <email> <pass>   -> prints  JWT=<jwt>  and  DID=<did>
 *   check     <base> <userJwt> <name> -> prints  PERSP=found|missing  and  DID=<did>
 *
 * `base` is the node's MCP URL (e.g. http://127.0.0.1:14420). Admin credential
 * from A2_ADMIN / A2H_ADMIN (default windtunnel-admin). Uses the harness's real
 * MCP client, located by walking up to the repo root.
 */
import { fileURLToPath } from "url";
import { dirname, resolve } from "path";
import { existsSync } from "fs";

let HARNESS = dirname(fileURLToPath(import.meta.url));
while (HARNESS !== "/" && !existsSync(resolve(HARNESS, "src/mcp-client.ts"))) {
  HARNESS = resolve(HARNESS, "..");
}
const ADMIN = process.env.A2_ADMIN || process.env.A2H_ADMIN || "windtunnel-admin";

function pick(o: any, ...keys: string[]): string {
  if (typeof o === "string") return o;
  for (const k of keys) if (o && typeof o[k] === "string") return o[k];
  return "";
}

(async () => {
  const { McpClient } = await import(`${HARNESS}/src/mcp-client.ts`);
  const [, , action, base, ...rest] = process.argv;

  if (action === "provision") {
    const [email, pass] = rest;
    const admin = new McpClient(base, ADMIN);
    await admin.initialize("a2");
    try {
      await admin.callToolJson("signup", { email, password: pass });
    } catch {
      /* user may already exist */
    }
    const login = await admin.callToolJson("login_email", { email, password: pass });
    const jwt = pick(login, "token", "jwt", "access_token");
    if (!jwt) {
      console.error("[a2] no JWT from login_email:", JSON.stringify(login).slice(0, 200));
      process.exit(1);
    }
    const user = new McpClient(base, jwt);
    await user.initialize("a2-u");
    const did = pick(await user.callToolJson("get_my_did", {}), "did", "result");
    console.log("JWT=" + jwt);
    console.log("DID=" + did);
  } else if (action === "check") {
    const [userJwt, name] = rest;
    const user = new McpClient(base, userJwt);
    await user.initialize("a2-c");
    const list = await user.callToolJson("list_perspectives", {});
    console.log("PERSP=" + (JSON.stringify(list).includes(name) ? "found" : "missing"));
    console.log("DID=" + pick(await user.callToolJson("get_my_did", {}), "did", "result"));
  } else {
    console.error("usage: ad4m-user.ts provision|check ...");
    process.exit(2);
  }
})().catch((e) => {
  console.error("[a2] FATAL", e?.stack || e);
  process.exit(2);
});
