import { Ad4mClient, ExpressionProof, Link, LinkExpression, Perspective } from "@coasys/ad4m";
import { ChildProcess } from 'child_process';
import { pollUntil } from "../utils/utils";

export class TestContext {
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

    async makeAllNodesKnown() {
      await pollUntil(async () => {
        const aliceAgentInfo = await this.#alice!.runtime.hcAgentInfos();
        const bobAgentInfo = await this.#bob!.runtime.hcAgentInfos();
        await this.#alice!.runtime.hcAddAgentInfos(bobAgentInfo);
        await this.#bob!.runtime.hcAddAgentInfos(aliceAgentInfo);
        console.log("Agent info exchange successful");
        return true;
      }, { timeoutMs: 15000, intervalMs: 3000, label: "agent info exchange (alice ↔ bob)" });
    }

    async makeAllThreeNodesKnown() {
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
