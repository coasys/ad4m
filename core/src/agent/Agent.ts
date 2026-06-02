import { Perspective } from "../perspectives/Perspective";
import { ExpressionGeneric } from "../expression/Expression";

/**  AD4M's representation of an Agent
 *
 * AD4M Agents are build around DIDs, which are used to identify and authenticate the Agent.
 * Conceptually, an Agent is regarded as something that can speak and that can listen.
 *
 * Agents speak by creating Expressions in AD4M Languages which are signed by the Agent's DID key,
 * And they also speak (broadcast) by putting semantic statements into their public "Agent Perspective".
 * They listen (can receive messages) through their "direct message Language".
 *
 * These three aspects are represented by the three fields of this class.
 *
 * This class is used as format for the Expressions in the Agent language.
 * Since AD4M treats DID URIs as addresses for the Agent Language,
 * DIDs are resolved to Expressions that are objects of this class.
 * Thus, this is how agents see (other) agents.
 */
export class Agent {
  /** The DID of the Agent
   * All epxressions authored by them are signed with the keys mentioned
   * in the DID document behind this DID URI.
   */
  did: string;

  /** The Perspective that holds the public-facing semantics/statements of the Agent
   * Holds and shares a Perspective that links all information
   * this agent wants to offer as public-facing semantics.
   * This should be used for any kind of user profile information.
   */
  perspective?: Perspective;

  /** Address of the Language by which the Agent will receive DMs */
  directMessageLanguage?: string;

  constructor(did: string, perspective?: Perspective) {
    this.did = did;
    if (perspective) {
      this.perspective = perspective;
    } else {
      this.perspective = new Perspective();
    }
  }
}
export class AgentExpression extends ExpressionGeneric(Agent) {}
export class EntanglementProof {
  did: string;
  didSigningKeyId: string;
  deviceKeyType: string;
  deviceKey: string;
  deviceKeySignedByDid: string;
  didSignedByDeviceKey?: string;

  constructor(
    did: string,
    didSigningKeyId: string,
    deviceKeyType: string,
    deviceKey: string,
    deviceKeySignedByDid: string,
    didSignedByDeviceKey?: string
  ) {
    this.did = did;
    this.didSigningKeyId = didSigningKeyId;
    this.deviceKeyType = deviceKeyType;
    this.deviceKey = deviceKey;
    this.deviceKeySignedByDid = deviceKeySignedByDid;
    this.didSignedByDeviceKey = didSignedByDeviceKey;
  }
}
export class EntanglementProofInput {
  did: string;
  didSigningKeyId: string;
  deviceKeyType: string;
  deviceKey: string;
  deviceKeySignedByDid: string;
  didSignedByDeviceKey: string;

  constructor(
    did: string,
    didSigningKeyId: string,
    deviceKeyType: string,
    deviceKey: string,
    deviceKeySignedByDid: string,
    didSignedByDeviceKey: string
  ) {
    this.did = did;
    this.didSigningKeyId = didSigningKeyId;
    this.deviceKeyType = deviceKeyType;
    this.deviceKey = deviceKey;
    this.deviceKeySignedByDid = deviceKeySignedByDid;
    this.didSignedByDeviceKey = didSignedByDeviceKey;
  }
}
export class AgentSignature {
  signature: string;
  publicKey: string;

  constructor(signature: string, publicKey: string) {
    this.signature = signature;
    this.publicKey = publicKey;
  }
}
export class Resource {
  domain: string;
  pointers: string[];

  constructor(domain: string, pointers: string[]) {
    this.domain = domain;
    this.pointers = pointers;
  }
}
export class Capability {
  with: Resource;
  can: string[];

  constructor(withF: Resource, can: string[]) {
    this.with = withF;
    this.can = can;
  }
}
export class AuthInfo {
  appName: string;
  appDesc: string;
  appUrl: string;
  appIconPath?: string;
  capabilities: Capability[];

  constructor(
    appName: string,
    appDesc: string,
    appUrl: string,
    capabilities: Capability[],
    appIconPath?: string
  ) {
    this.appName = appName;
    this.appDesc = appDesc;
    this.appIconPath = appIconPath;
    this.appUrl = appUrl;
    this.capabilities = capabilities;
  }
}
export class Apps {
  requestId: string;
  token: string;
  revoked?: boolean;
  auth: AuthInfo;

  constructor(
    requestId: string,
    auth: AuthInfo,
    token: string,
    revoked?: boolean
  ) {
    this.requestId = requestId;
    this.auth = auth;
    this.token = token;
    this.revoked = revoked;
  }
}
export class ResourceInput {
  domain: string;
  pointers: string[];

  constructor(domain: string, pointers: string[]) {
    this.domain = domain;
    this.pointers = pointers;
  }
}
export class CapabilityInput {
  with: ResourceInput;
  can: string[];

  constructor(withF: ResourceInput, can: string[]) {
    this.with = withF;
    this.can = can;
  }
}
export class AuthInfoInput {
  appName: string;
  appDesc: string;
  appDomain: string;
  appUrl?: string;
  appIconPath?: string;
  capabilities?: CapabilityInput[];

  constructor(
    appName: string,
    appDesc: string,
    appDomain: string,
    appUrl?: string,
    appIconPath?: string,
    capabilities?: CapabilityInput[]
  ) {
    this.appName = appName;
    this.appDesc = appDesc;
    this.appUrl = appUrl;
    this.appDomain = appDomain;
    this.capabilities = capabilities;
    this.appIconPath = appIconPath;
  }
}
export class UserCreationResult {
  did: string;
  success: boolean;
  error?: string;

  constructor(did: string, success: boolean, error?: string) {
    this.did = did;
    this.success = success;
    this.error = error;
  }
}
