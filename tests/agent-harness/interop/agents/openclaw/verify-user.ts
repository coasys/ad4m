/**
 * A2 assertion helper: confirm an agent identity exists on the node.
 *
 * Logs in as the given user (login_email is auth-exempt on the MCP surface),
 * then acting as that user checks get_my_did + auth_status. Exits 0 on a verified
 * identity, 1 otherwise. Used by verify-a2-openclaw.sh after the OpenClaw
 * assistant runs its onboarding turn.
 *
 * Usage: tsx verify-user.ts <email> <password> <mcpBaseUrl>
 */
const [, , email, password, base] = process.argv;
if (!email || !password || !base) {
  console.error("usage: verify-user.ts <email> <password> <mcpBaseUrl>");
  process.exit(2);
}

const { McpClient } = await import(new URL("../../../src/mcp-client.ts", import.meta.url).href);

const anon = new McpClient(base); // login_email needs no prior auth
await anon.initialize("a2-verify");
const login = await anon.callToolJson("login_email", { email, password });
const token = login?.token || login?.jwt || "";
if (!token) {
  console.error(`  FAIL: login returned no token for ${email} (identity not created)`);
  process.exit(1);
}

const user = new McpClient(base, token);
await user.initialize("a2-verify-user");
const did = await user.callToolJson("get_my_did", {});
const auth = await user.callToolJson("auth_status", {});
const ok =
  typeof did === "string" &&
  did.startsWith("did:key:") &&
  auth?.authenticated === true &&
  auth?.user_email === email;

console.log(`  identity: did=${did} user=${auth?.user_email} authed=${auth?.authenticated} caps=${auth?.has_capabilities}`);
if (!ok) {
  console.error("  FAIL: identity not verified");
  process.exit(1);
}
console.log("  PASS: OpenClaw-created ADAM agent verified on the node");
process.exit(0);
