/**
 * The world a call happens in, built as an operator and its users build it.
 *
 * One executor in multi-user mode, without Holochain; managed users with
 * their own tokens; neighbourhoods published on a fresh clone of the local
 * link language and joined by their members. Users of one executor share
 * the neighbourhood's perspective as its owners, so membership, the call
 * policy, presence and signalling stay on the executor, as they do for every
 * call it serves alone. The space's call policy is set by its creator through
 * the SDK. Nothing here reaches past what an operator or a user can do.
 */

import { ChildProcess } from "node:child_process";
import { randomUUID } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { Ad4mClient, Perspective, type SfuMode } from "@coasys/ad4m";
import { baseUrl, gracefulShutdown, pollUntil, spawnExecutor, startExecutor } from "../../../utils/utils.js";
import { deregisterPorts, getFreePorts, registerPorts } from "../../ports.js";

const TESTS_JS = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..", "..");
const REPO_ROOT = path.resolve(TESTS_JS, "..", "..");
const BOOTSTRAP_SEED = path.join(TESTS_JS, "bootstrapSeed.json");
const LINK_LANGUAGE_HASH = path.join(TESTS_JS, "scripts", "perspective-diff-sync-hash");
const TST_TMP = path.join(TESTS_JS, "tst-tmp");

export interface WorldSpec {
  /** A label for data directories and users, so worlds never share state. */
  id: string;
  users: { name: string }[];
  neighbourhoods: {
    name: string;
    owner: string;
    members: string[];
    /** The space's call policy, set by its owner. None stored means the SDK's own default. */
    call?: { mode: SfuMode; maxMeshParticipants?: number };
  }[];
}

export interface WorldUser {
  name: string;
  port: number;
  token: string;
  did: string;
  client: Ad4mClient;
}

export interface World {
  users: Record<string, WorldUser>;
  /** A neighbourhood's URL, by name. */
  url(name: string): string;
  /** Stop the executor and start it again on the same data, as an operator's restart does. */
  restartExecutor(): Promise<void>;
  /** The ports this world's traffic uses: the executor's API (TCP) and its SFU (UDP). */
  ports(): Promise<number[]>;
  stop(): Promise<void>;
}

/**
 * The local link language's template, or why it cannot be used. Fails fast
 * and says how to fix it when the test languages are not prepared in local
 * mode: prepared for Holochain, "perspective-diff-sync" is p-diff-sync.
 */
export function preparedLinkLanguage(): string {
  const prepare = "Prepare them first: `pnpm run prepare-test-local` in tests/js.";
  const seed = JSON.parse(readFileSync(BOOTSTRAP_SEED, "utf8"));
  if (!seed.languageLanguageBundle) {
    throw new Error(`${BOOTSTRAP_SEED} has no language-language bundle, so the test languages are not prepared. ${prepare}`);
  }
  const hash = existsSync(LINK_LANGUAGE_HASH) ? readFileSync(LINK_LANGUAGE_HASH, "utf8").trim() : "";
  if (!hash || !existsSync(path.join(TST_TMP, "languages", `meta-${hash}.json`))) {
    throw new Error(`The link language template "${hash}" is not published in ${TST_TMP}/languages. ${prepare}`);
  }
  const prepared = path.join(TST_TMP, "languages", "perspective-diff-sync", "build", "bundle.js");
  const local = path.join(REPO_ROOT, "bootstrap-languages", "local", "link-language.js");
  if (!existsSync(prepared) || !readFileSync(prepared).equals(readFileSync(local))) {
    throw new Error(`The test languages were prepared for Holochain, not in local mode. ${prepare}`);
  }
  return hash;
}

/** With `E2E_DEBUG` set, log how long each phase of building and stopping a world takes. */
function phases(id: string) {
  let last = Date.now();
  return (name: string) => {
    if (process.env.E2E_DEBUG) console.log(`[${id} world] ${name}: ${Date.now() - last} ms`);
    last = Date.now();
  };
}

export async function startWorld(spec: WorldSpec): Promise<World> {
  const phase = phases(spec.id);
  const linkLanguage = preparedLinkLanguage();
  const ports = await getFreePorts(3);
  registerPorts(ports);
  const [port, hcAdmin, hcApp] = ports;
  const clients: Ad4mClient[] = [];
  let executor: ChildProcess | null = null;
  const stop = async () => {
    phase("the scenario");
    for (const client of clients) client.close();
    await gracefulShutdown(executor, `${spec.id} executor`);
    phase("stop the executor");
    deregisterPorts(ports);
  };

  try {
    executor = await startExecutor(
      path.join(TST_TMP, "agents", spec.id), BOOTSTRAP_SEED, port, hcAdmin, hcApp,
      false, undefined, undefined, undefined, undefined, false, undefined, undefined,
      false, // runHolochain
    );
    phase("start the executor");
    const admin = new Ad4mClient(baseUrl(port));
    clients.push(admin);
    await admin.agent.generate("passphrase");
    await admin.runtime.setMultiUserEnabled(true);
    phase("generate the main agent");

    const users: Record<string, WorldUser> = {};
    for (const { name } of spec.users) {
      const email = `${spec.id}-${name}@example.com`;
      const created = await admin.agent.createUser(email, "password");
      if (!created.success) throw new Error(`creating ${name} failed: ${created.error}`);
      await admin.runtime.setUserFreeAccess(email, true);
      const token = await admin.agent.loginUser(email, "password");
      const client = new Ad4mClient(baseUrl(port), token);
      clients.push(client);
      users[name] = { name, port, token, did: (await client.agent.status()).did!, client };
    }
    phase(`create ${spec.users.length} users`);

    const urls = new Map<string, string>();
    for (const n of spec.neighbourhoods) {
      const owner = users[n.owner].client;
      const joiners = n.members.filter((m) => m !== n.owner).map((m) => users[m].client);
      const url = await shareNeighbourhood(linkLanguage, owner, joiners, `${spec.id} ${n.name}`);
      if (n.call) {
        const proxy = (await owner.perspective.all()).find((p) => p.sharedUrl === url)!.getNeighbourhoodProxy();
        await proxy.setSfuConfig(url, { mode: n.call.mode, fallback: "mesh", maxMeshParticipants: n.call.maxMeshParticipants ?? 6 });
      }
      urls.set(n.name, url);
      phase(`neighbourhood "${n.name}"`);
    }

    return {
      users,
      url: (name) => {
        const url = urls.get(name);
        if (!url) throw new Error(`no neighbourhood "${name}"`);
        return url;
      },
      ports: async () => {
        // The operator's view of the SFU, read only to aim the network impairment.
        const { bindAddress } = await admin.neighbourhood.sfuStatus();
        return [port, Number(bindAddress.split(":").pop())];
      },
      restartExecutor: async () => {
        const old = executor!;
        await gracefulShutdown(old, `${spec.id} executor`);
        // The same arguments, without the init that would wipe its data.
        executor = await spawnExecutor(old.spawnargs.slice(1), port);
        await admin.agent.unlock("passphrase", true);
      },
      stop,
    };
  } catch (error) {
    await stop();
    throw error;
  }
}

/** `owner` publishes a neighbourhood on a fresh clone of the link language; `joiners` join it. */
async function shareNeighbourhood(linkLanguage: string, owner: Ad4mClient, joiners: Ad4mClient[], name: string): Promise<string> {
  const perspective = await owner.perspective.add(name);
  const language = await owner.languages.applyTemplateAndPublish(linkLanguage, JSON.stringify({ uid: randomUUID(), name }));
  const url = await owner.neighbourhood.publishFromPerspective(perspective.uuid, language.address, new Perspective([]));
  for (const joiner of joiners) await joiner.neighbourhood.joinFromUrl(url);
  const ready = ["LINK_LANGUAGE_INSTALLED_BUT_NOT_SYNCED", "SYNCED"];
  await pollUntil(async () => {
    for (const client of [owner, ...joiners]) {
      const state = (await client.perspective.all()).find((p) => p.sharedUrl === url)?.state;
      if (!ready.includes(state as string)) return false;
    }
    return true;
  }, { timeoutMs: 60_000, intervalMs: 100, label: `link language of "${name}" installed for every member` });
  return url;
}
