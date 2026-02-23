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
  #agentLanguage?: Language;
  #pubSub: PubSub;
  #currentUserContext?: string; // Email of the current user context
  #cachedMainAgentDid?: string; // Cache the main agent's DID for non-context calls

  constructor(rootConfigPath: string, adminCredential?: string) {
    this.#pubSub = getPubSub();
    // Cache the main agent's DID on construction
    try {
      this.#cachedMainAgentDid = AGENT.did();
    } catch (e) {
      console.warn("Could not get main agent DID during construction:", e);
    }
  }

  // Set the current user context for this AgentService instance
  setUserContext(userEmail?: string) {
    this.#currentUserContext = userEmail;
  }

  // Get the current user context
  getUserContext(): string | undefined {
    return this.#currentUserContext;
  }

  // Refresh the cached main agent DID (called after AGENT.load())
  refreshMainAgentDid() {
    try {
      this.#cachedMainAgentDid = AGENT.did();
    } catch (e) {
      console.warn("Could not refresh main agent DID:", e);
    }
  }

  getTaggedAgentCopy(): Agent {
    const agent = AGENT.agent();
    if (!agent) throw new Error("No agent");
    const copy = JSON.parse(JSON.stringify(agent));
    if(copy.perspective) {
        for(let link of copy.perspective.links) {
            tagExpressionSignatureStatus(link)
        }
    }
    return copy;
  }

  createSignedExpression(data: any): Expression {
    if (this.#currentUserContext) {
      // @ts-ignore - New function from Rust
      return AGENT.createSignedExpressionForUser(this.#currentUserContext, data);
    }
    return AGENT.createSignedExpression(data);
  }

  get did(): string {
    if (this.#currentUserContext) {
      // @ts-ignore - New function from Rust
      return AGENT.didForUser(this.#currentUserContext);
    }
    // Return cached main agent DID, or fallback to calling AGENT.did()
    return this.#cachedMainAgentDid || AGENT.did();
  }

  get agent(): Agent {
    if (this.#currentUserContext) {
      // @ts-ignore - New function from Rust
      return AGENT.agentForUser(this.#currentUserContext);
    }
    return AGENT.agent();
  }

  // Return all local user DIDs (including main agent)
  getAllLocalUserDIDs(): string[] {
    // @ts-ignore - Rust implementation includes main agent + managed users
    return AGENT.getAllLocalUserDIDs();
  }

  async updateAgent(a: Agent) {
    AGENT.save_agent_profile(a);
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
    const currentAgent = AGENT.agent();
    const agentDid = AGENT.did();
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
    let agent = AGENT.agent();

    console.log("Storing agent profile", JSON.stringify(agent));

    const agentLanguage = this.getAgentLanguage();

    if (agent?.did) {
      let adapter = agentLanguage.expressionAdapter!.putAdapter;

      let isPublic = function isPublic(
        adapter: PublicSharing | ReadOnlyLanguage
      ): adapter is PublicSharing {
        return (adapter as PublicSharing).createPublic !== undefined;
      };

      try {
        if (isPublic(adapter)) {
          await adapter.createPublic(agent);
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

  // TODO: need to be removed once runtime stuff gets merged
  isInitialized() {
    return AGENT.isInitialized();
  }

  // TODO: need to be removed once runtime stuff gets merged
  isUnlocked() {
    return AGENT.isUnlocked();
  }

  async unlock(password: string) {
    AGENT.unlock(password);
    // Refresh cached main agent DID after unlocking
    this.refreshMainAgentDid();
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
}

export function init(
  rootConfigPath: string,
  adminCredential?: string
): AgentService {
  const agent = new AgentService(rootConfigPath, adminCredential);
  AGENT.load();
  // Update cached main agent DID after loading
  agent.refreshMainAgentDid();
  return agent;
}
