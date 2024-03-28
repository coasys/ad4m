import * as path from "node:path";
import * as fs from "node:fs";
import {
  Language,
  Expression,
  PublicSharing,
  ReadOnlyLanguage,
} from "@coasys/ad4m";
import { Agent, ExpressionProof, AgentSignature } from "@coasys/ad4m";
import * as PubSubDefinitions from "../graphQL-interface/SubscriptionDefinitions";
import { resolver } from "@transmute/did-key.js";
import { getPubSub, tagExpressionSignatureStatus } from "../utils";


export default class AgentService {
  #did?: string;
  #didDocument?: string;
  #signingKeyId?: string;
  #file: string;
  #fileProfile: string;
  #agent?: Agent;
  #agentLanguage?: Language;
  #pubSub: PubSub;


  #readyPromise: Promise<void>;
  #readyPromiseResolve?: (value: void | PromiseLike<void>) => void;

  constructor(rootConfigPath: string, adminCredential?: string) {
    this.#file = path.join(rootConfigPath, "agent.json");
    this.#fileProfile = path.join(rootConfigPath, "agentProfile.json");
    this.#pubSub = getPubSub();
    this.#readyPromise = new Promise((resolve) => {
      this.#readyPromiseResolve = resolve;
    });
  }

  get did() {
    return this.#did;
  }

  get agent() {
    return this.getTaggedAgentCopy();
  }

  getTaggedAgentCopy(): Agent {
    const agent = this.#agent;
    if (!agent) throw new Error("No agent");
    const copy = JSON.parse(JSON.stringify(agent));
    if(copy.perspective) {
        for(let link of copy.perspective.links) {
            tagExpressionSignatureStatus(link)
        }
    }
    return copy;
  }

  get ready(): Promise<void> {
    return this.#readyPromise;
  }

  get signingKeyId(): string {
    if (!this.#signingKeyId) {
        throw new Error("No signing key id on AgentService")
    }
    return this.#signingKeyId!
  }

  signingChecks() {
    if (!this.isInitialized) {
      throw new Error("Can't sign without keystore");
    }
    if (!this.isUnlocked()) {
      throw new Error("Can't sign with locked keystore");
    }
    if (!this.#signingKeyId) {
      throw new Error("Can't sign without signingKeyId");
    }
  }

  createSignedExpression(data: any): Expression {
    this.signingChecks()
    return AGENT.createSignedExpression(data);
  }

  signString(data: string): string {
    this.signingChecks()
    return AGENT.signStringHex(data);
  }

  async updateAgent(a: Agent) {
    this.#agent = a;
    await this.storeAgentProfile();
    await this.#pubSub.publish(PubSubDefinitions.AGENT_UPDATED, this.getTaggedAgentCopy());
  }

  setAgentLanguage(lang: Language) {
    this.#agentLanguage = lang;
  }

  getAgentLanguage(): Language {
    if (!this.#agentLanguage) {
      throw new Error("AgentService ERROR: No agent language");
    }
    return this.#agentLanguage;
  }

  async ensureAgentExpression() {
    const currentAgent = this.agent;
    const agentDid = currentAgent?.did;
    if (!agentDid) throw Error("No agent did found");

    const agentLanguage = this.getAgentLanguage();

    if (!agentLanguage.expressionAdapter!) {
      throw Error("No expression adapter found");
    }

    const agentExpression = await agentLanguage.expressionAdapter!.get(
      agentDid
    );

    if (!agentExpression) {
      if (currentAgent) {
        await this.updateAgent(currentAgent);
      }
    }
  }

  async storeAgentProfile() {
    fs.writeFileSync(this.#fileProfile, JSON.stringify(this.#agent));

    const agentLanguage = this.getAgentLanguage();

    if (this.#agent?.did) {
      let adapter = agentLanguage.expressionAdapter!.putAdapter;

      let isPublic = function isPublic(
        adapter: PublicSharing | ReadOnlyLanguage
      ): adapter is PublicSharing {
        return (adapter as PublicSharing).createPublic !== undefined;
      };

      try {
        if (isPublic(adapter)) {
          await adapter.createPublic(this.#agent);
        } else {
          console.warn("Got a ReadOnlyLanguage for agent language");
        }
      } catch (e) {
        throw new Error(
          `Incompatible putAdapter in AgentLanguage}\nError was: ${e}`
        );
      }
    }
  }

  async createNewKeys() {
    WALLET.createMainKey()
    const didDocument = WALLET.getMainKeyDocument()
    const key = didDocument.verificationMethod[0]
    
    this.#did = key.controller;
    this.#didDocument = JSON.stringify(await resolver.resolve(this.#did));
    this.#agent = new Agent(this.#did);
    this.#signingKeyId = key.id;
  }

  isInitialized() {
    return fs.existsSync(this.#file);
  }

  isUnlocked() {
    return WALLET.isUnlocked()
  }

  async unlock(password: string) {
    // @ts-ignore
    WALLET.unlock(password);
    await this.#pubSub.publish(PubSubDefinitions.AGENT_STATUS_CHANGED, this.dump());
    this.#readyPromiseResolve!();
    try {
      await this.storeAgentProfile();
    } catch (e) {
      console.debug(
        "Error when trying to store agent profile during unlock: ",
        e
      );
      console.debug("Continuing anyway...");
    }
  }

  async lock(password: string) {
    // @ts-ignore
    WALLET.lock(password);
    await this.#pubSub.publish(PubSubDefinitions.AGENT_STATUS_CHANGED, this.dump());
  }

  async save(password: string) {
    const dump = {
      did: this.#did,
      didDocument: this.#didDocument,
      signingKeyId: this.#signingKeyId,
      // @ts-ignore
      keystore: WALLET.export(password),
      agent: this.#agent,
    };

    fs.writeFileSync(this.#file, JSON.stringify(dump));
    this.#readyPromiseResolve!();
  }

  load() {
    if (!this.isInitialized()) return;

    const dump = JSON.parse(fs.readFileSync(this.#file).toString());

    this.#did = dump.did;
    this.#didDocument = dump.didDocument;
    this.#signingKeyId = dump.signingKeyId;
    WALLET.load(dump.keystore);
    if (fs.existsSync(this.#fileProfile))
      this.#agent = JSON.parse(fs.readFileSync(this.#fileProfile).toString());
    else {
      this.#agent = new Agent(this.#did!);
    }
  }

  dump() {
    return {
      agent: this.#agent,
      isInitialized: this.isInitialized(),
      isUnlocked: WALLET.isUnlocked(),
      did: this.#did,
      didDocument: this.#didDocument,
    };
  }
}

export function init(
  rootConfigPath: string,
  adminCredential?: string
): AgentService {
  const agent = new AgentService(rootConfigPath, adminCredential);
  agent.load();
  return agent;
}
