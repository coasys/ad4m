import * as path from "https://deno.land/std@0.177.0/path/mod.ts";
import * as fs from "https://deno.land/std@0.177.0/fs/mod.ts";
import { Key } from "../../wallet_extension.d.ts";
import {
  Language,
  Expression,
  PublicSharing,
  ReadOnlyLanguage,
  ExceptionType,
} from "@perspect3vism/ad4m";
import { Agent, ExpressionProof, AgentSignature, EntanglementProof } from "@perspect3vism/ad4m";
import Signatures from "./Signatures.ts";
import { v4 as uuidv4 } from "uuid";
import { ExceptionInfo } from "@perspect3vism/ad4m/lib/src/runtime/RuntimeResolver";
import {
  ALL_CAPABILITY,
  AuthInfo,
  AuthInfoExtended,
  DefaultTokenValidPeriod,
  genRequestKey,
  genRandomDigits,
  AGENT_AUTH_CAPABILITY,
  Capability,
} from "./Auth.ts";
import * as jose from "jose";
import * as crypto from "crypto";
import KeyEncoder from "key-encoder";
import * as secp from "@noble/secp256k1";


export default class AgentService {
  #did?: string;
  #didDocument?: string;
  #signingKeyId?: string;
  #file: string;
  #appsFile: string;
  #apps: AuthInfoExtended[];
  #requestingAuthInfo?: AuthInfoExtended;
  #fileProfile: string;
  #agent?: Agent;
  #agentLanguage?: Language;
  #requests: Map<string, AuthInfo>;
  #tokenValidPeriod: number;
  #adminCredential: string;

  #readyPromise: Promise<void>;
  #readyPromiseResolve?: (value: void | PromiseLike<void>) => void;

  constructor(rootConfigPath: string, adminCredential?: string) {
    this.#file = path.join(rootConfigPath, "agent.json");
    this.#fileProfile = path.join(rootConfigPath, "agentProfile.json");
    this.#appsFile = path.join(rootConfigPath, "apps.json");
    try {
      this.#apps = JSON.parse(fs.readFileSync(this.#appsFile).toString());
    } catch (e) {
      this.#apps = [];
    }
    this.#readyPromise = new Promise((resolve) => {
      this.#readyPromiseResolve = resolve;
    });
    this.#requests = new Map();
    this.#tokenValidPeriod = DefaultTokenValidPeriod;
    if (adminCredential) {
      this.#adminCredential = adminCredential;
    } else {
      console.warn(
        "adminCredential is not set or empty, empty token will possess admin capabililities."
      );
      this.#adminCredential = "";
    }
  }

  get did() {
    return this.#did;
  }

  get agent() {
    return this.#agent;
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

    const timestamp = new Date().toISOString();
    const payloadBytes = Signatures.buildMessage(data, timestamp);

    const signature = WALLET.sign(payloadBytes);
    const sigBuffer = Buffer.from(signature);
    const sigHex = sigBuffer.toString("hex");

    let proof = new ExpressionProof(sigHex.toString(), this.#signingKeyId!);
    proof.valid = true;
    proof.invalid = false;

    const signedExpresssion = {
      author: this.#did,
      timestamp,
      data,
      proof,
    } as Expression;

    return signedExpresssion;
  }

  signString(data: string): string {
    this.signingChecks()

    const payloadBytes = Signatures.buildMessageRaw(data)
    const signature = WALLET.sign(payloadBytes);
    const sigBuffer = Buffer.from(signature);
    const sigHex = sigBuffer.toString("hex");
    return sigHex
  }

  async updateAgent(a: Agent) {
    this.#agent = a;
    await this.storeAgentProfile();
    await PUBSUB.publish(PubSubInstance.AGENT_UPDATED, a);
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

  private getSigningKey(): Key {
    return WALLET.getMainKey();
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
    await PUBSUB.publish(PubSubInstance.AGENT_STATUS_CHANGED, this.dump());
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
    await PUBSUB.publish(PubSubInstance.AGENT_STATUS_CHANGED, this.dump());
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

  async getCapabilities(token: string) {
    if (token == this.#adminCredential) {
      return [ALL_CAPABILITY];
    }

    if (token === "") {
      return [AGENT_AUTH_CAPABILITY];
    }

    const payload = await JWT.verifyJwt(token);

    //@ts-ignore
    return payload.capabilities.capabilities;
  }

  isAdminCredential(token: string) {
    return token == this.#adminCredential;
  }

  async requestCapability(authInfo: AuthInfo) {
    let requestId = uuidv4();
    let authExtended = {
      requestId,
      auth: authInfo,
    } as AuthInfoExtended;

    await PUBSUB.publish(PubSubInstance.EXCEPTION_OCCURRED_TOPIC, {
      title: "Request to authenticate application",
      message: `${authInfo.appName} is waiting for authentication, go to ad4m launcher for more information.`,
      type: ExceptionType.CapabilityRequested,
      addon: JSON.stringify(authExtended),
    } as ExceptionInfo);

    return requestId;
  }

  // TODO, we may want to change the capability request workflow.
  // https://github.com/perspect3vism/ad4m-executor/issues/73
  permitCapability(authExt: string, capabilities: Capability[]) {
    console.log("AgentService.permitCapability(): admin user capabilities: ", capabilities);
    console.log("AgentService.permitCapability(): auth info: ", authExt);

    let { requestId, auth }: AuthInfoExtended = JSON.parse(authExt);
    let rand = genRandomDigits();
    this.#requests.set(genRequestKey(requestId, rand), auth);

    this.#requestingAuthInfo = JSON.parse(authExt);

    return rand;
  }

  async generateJwt(requestId: string, rand: string) {
    const authKey = genRequestKey(requestId, rand);
    const auth = this.#requests.get(authKey);

    if (!auth) {
      throw new Error("Can't find permitted request");
    }

    const jwt = await JWT.generateJwt(this.did || "", `${auth.appName}:${this.did || ""}`, this.#tokenValidPeriod, auth);

    this.#requests.delete(authKey);

    if (requestId === this.#requestingAuthInfo?.requestId) {
      const apps = [...this.#apps, { ...this.#requestingAuthInfo, token: jwt }];
      this.#apps = apps;
      fs.writeFileSync(this.#appsFile, JSON.stringify(apps));

      await PUBSUB.publish(PubSubInstance.APPS_CHANGED, null);
    }

    return jwt;
  }

  getApps(): AuthInfoExtended[] {
    return this.#apps;
  }

  async removeApp(requestId: string) {
    try {
      this.#apps = this.#apps.filter((app: any) => app.requestId !== requestId);

      fs.writeFileSync(this.#appsFile, JSON.stringify(this.#apps));

      await PUBSUB.publish(PubSubInstance.APPS_CHANGED, null);
    } catch (e) {
      console.error("Error while removing app", e);
    }
  }

  async revokeAppToken(requestId: string) {
    try {
      this.#apps = this.#apps.map((app: any) =>
        app.requestId === requestId ? { ...app, revoked: true } : app
      );

      fs.writeFileSync(this.#appsFile, JSON.stringify(this.#apps));

      await PUBSUB.publish(PubSubInstance.APPS_CHANGED, null);
    } catch (e) {
      console.error("Error while revoking token", e);
    }
  }

  async signMessage(msg: string) {
    const key = this.getSigningKey();
    const msgHash = await secp.utils.sha256(new TextEncoder().encode(msg));
    const signature = await secp.sign(msgHash, key.privateKey);
    const sigHex = Buffer.from(signature).toString("hex");
    return new AgentSignature(sigHex, key.publicKey);
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
