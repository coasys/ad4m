import { Ad4mClient, ExpressionProof, Link, LinkExpression, Perspective, PerspectiveState } from "@coasys/ad4m";
import { ChildProcess } from 'child_process';
import { pollUntil } from "../utils/utils";

export class TestContext {
    /** False when the executors run with --run-holochain false: there are no
     *  Holochain agent infos to exchange, so makeAll*NodesKnown() do nothing. */
    holochain: boolean = true

    #alice: Ad4mClient | undefined
    #bob: Ad4mClient | undefined
    #jim: Ad4mClient | undefined

    #aliceCore: ChildProcess | undefined
    #bobCore: ChildProcess | undefined
    #jimCore: ChildProcess | undefined

    get ad4mClient(): Ad4mClient {
      return this.#alice!
    }

    get alice(): Ad4mClient {
      return this.#alice!
    }

    get bob(): Ad4mClient {
      return this.#bob!
    }

    get jim(): Ad4mClient {
      return this.#jim!
    }

    set alice(client: Ad4mClient) {
      this.#alice = client
    }

    set bob(client: Ad4mClient) {
      this.#bob = client
    }

    set jim(client: Ad4mClient) {
      this.#jim = client
    }

    set aliceCore(aliceCore: ChildProcess) {
      this.#aliceCore = aliceCore
    }

    set bobCore(bobCore: ChildProcess) {
      this.#bobCore = bobCore
    }

    set jimCore(jimCore: ChildProcess) {
      this.#jimCore = jimCore
    }

    /** Exchange Alice's and Bob's Holochain agent infos. A publish or join
     *  returns before its link language's cell exists, and an exchange before
     *  that leaves the new cell out: pass each such `[client, perspective uuid]`
     *  so the exchange waits until its link language is attached. */
    async makeAllNodesKnown(...perspectives: [Ad4mClient, string][]) {
      if (!this.holochain) return;
      await linkLanguagesAttached(perspectives);
      await pollUntil(async () => {
        const aliceAgentInfo = await this.#alice!.runtime.hcAgentInfos();
        const bobAgentInfo = await this.#bob!.runtime.hcAgentInfos();
        await this.#alice!.runtime.hcAddAgentInfos(bobAgentInfo);
        await this.#bob!.runtime.hcAddAgentInfos(aliceAgentInfo);
        console.log("Agent info exchange successful");
        return true;
      }, { timeoutMs: 15000, intervalMs: 3000, label: "agent info exchange (alice ↔ bob)" });
    }

    /** As `makeAllNodesKnown`, for Alice, Bob and Jim. */
    async makeAllThreeNodesKnown(...perspectives: [Ad4mClient, string][]) {
      if (!this.holochain) return;
      await linkLanguagesAttached(perspectives);
      await pollUntil(async () => {
        const aliceAgentInfo = await this.#alice!.runtime.hcAgentInfos();
        const bobAgentInfo = await this.#bob!.runtime.hcAgentInfos();
        const jimAgentInfo = await this.#jim!.runtime.hcAgentInfos();
        await this.#alice!.runtime.hcAddAgentInfos([...bobAgentInfo, ...jimAgentInfo]);
        await this.#bob!.runtime.hcAddAgentInfos([...aliceAgentInfo, ...jimAgentInfo]);
        await this.#jim!.runtime.hcAddAgentInfos([...aliceAgentInfo, ...bobAgentInfo]);
        console.log("Three-node agent info exchange successful");
        return true;
      }, { timeoutMs: 15000, intervalMs: 3000, label: "agent info exchange (alice ↔ bob ↔ jim)" });
    }
}

/** Wait until each perspective's link language is attached: its state has moved
 *  past `NeighbourhoodCreationInitiated` / `NeighbourhoodJoinInitiated`. */
export async function linkLanguagesAttached(perspectives: [Ad4mClient, string][]) {
  for (const [client, uuid] of perspectives) {
    await pollUntil(async () => {
      const state = (await client.perspective.byUUID(uuid))?.state;
      return state === PerspectiveState.LinkLanguageInstalledButNotSynced
        || state === PerspectiveState.Synced;
    }, { timeoutMs: 60000, label: `link language attached for ${uuid}` });
  }
}
