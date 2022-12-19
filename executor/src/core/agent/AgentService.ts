import * as path from 'path';
import * as fs from 'fs';
import didWallet from '@transmute/did-wallet'
import { Language, Expression, PublicSharing, ReadOnlyLanguage, ExceptionType } from '@perspect3vism/ad4m';
import { Agent, ExpressionProof, AgentSignature } from '@perspect3vism/ad4m';
import secp256k1 from 'secp256k1'
import * as secp256k1DIDKey from '@transmute/did-key-secp256k1';
import Signatures from './Signatures';
import * as PubSubInstance from '../graphQL-interface/PubSub'
import type { PubSub } from 'graphql-subscriptions';
import { resolver } from '@transmute/did-key.js';
import { v4 as uuidv4 } from 'uuid';
import { ExceptionInfo } from '@perspect3vism/ad4m/lib/src/runtime/RuntimeResolver';
import { ALL_CAPABILITY, AuthInfo, AuthInfoExtended, DefaultTokenValidPeriod, genRequestKey, genRandomDigits, AGENT_AUTH_CAPABILITY, Capability } from './Auth';
import * as jose from 'jose'
import * as crypto from "crypto"
import KeyEncoder from 'key-encoder'
import * as secp from '@noble/secp256k1'

export default class AgentService {
    #did?: string
    #didDocument?: string
    #signingKeyId?: string
    #wallet?: object
    #file: string
    #appsFile: string;
    #requestingAuthInfo?: AuthInfoExtended;
    #fileProfile: string
    #agent?: Agent
    #agentLanguage?: Language
    #pubsub: PubSub
    #requests: Map<string, AuthInfo>
    #tokenValidPeriod: number
    #adminCredential: string
    

    #readyPromise: Promise<void>
    #readyPromiseResolve?: ((value: void | PromiseLike<void>) => void)

    constructor(rootConfigPath: string, reqCredential?: string) {
        this.#file = path.join(rootConfigPath, "agent.json")
        this.#fileProfile = path.join(rootConfigPath, "agentProfile.json")
        this.#appsFile = path.join(rootConfigPath, "apps.json")
        this.#pubsub = PubSubInstance.get()
        this.#readyPromise = new Promise(resolve => {
            this.#readyPromiseResolve = resolve
        })
        this.#requests = new Map()
        this.#tokenValidPeriod = DefaultTokenValidPeriod
        if(reqCredential) {
            this.#adminCredential = reqCredential
        } else {
            console.warn("reqCredential is not set or empty, empty token will possess admin capabililities.")
            this.#adminCredential = ""
        }
    }

    get did() {
        return this.#did
    }

    get agent() {
        return this.#agent
    }

    get ready(): Promise<void> {
        return this.#readyPromise
    }

    createSignedExpression(data: any): Expression {
        if(!this.isInitialized){
            throw new Error("Can't sign without keystore")
        }
        if(!this.isUnlocked()) {
            throw new Error("Can't sign with locked keystore")
        }
        if(!this.#signingKeyId) {
            throw new Error("Can't sign without signingKeyId")
        }

        const timestamp = new Date().toISOString()
        const payloadBytes = Signatures.buildMessage(data, timestamp)

        const key = this.getSigningKey()
        const privKey = Uint8Array.from(Buffer.from(key.privateKey, key.encoding))

        const sigObj = secp256k1.ecdsaSign(payloadBytes, privKey)
        const sigBuffer = Buffer.from(sigObj.signature)
        const sigHex = sigBuffer.toString('hex')
        let proof = new ExpressionProof(sigHex, this.#signingKeyId);
        proof.valid = true;
        proof.invalid = false;

        const signedExpresssion = {
            author: this.#did,
            timestamp,
            data,
            proof
        } as Expression

        return signedExpresssion
    }

    async updateAgent(a: Agent) {
        this.#agent = a
        await this.storeAgentProfile()
        this.#pubsub.publish(PubSubInstance.AGENT_UPDATED, a)
    }

    setAgentLanguage(lang: Language) {
        this.#agentLanguage = lang
    }

    async storeAgentProfile() {
        fs.writeFileSync(this.#fileProfile, JSON.stringify(this.#agent))

        if(!this.#agentLanguage) {
            console.error("AgentService ERROR: Can't store Agent profile. No AgentLanguage installed.")
            return
        }

        if (!this.#agentLanguage.expressionAdapter) {
            throw Error("Agent language does not have an expression adapater");
        }

        if(this.#agent?.did) {
            let adapter = this.#agentLanguage.expressionAdapter.putAdapter;

            let isPublic = function isPublic(adapter: PublicSharing | ReadOnlyLanguage): adapter is PublicSharing {
                return (adapter as PublicSharing).createPublic !== undefined;
            }
    
            try {
                if (isPublic(adapter)) {
                    await adapter.createPublic(this.#agent);
                } else {
                    console.warn("Got a ReadOnlyLanguage for agent language")
                }
            } catch (e) {
                throw new Error(`Incompatible putAdapter in AgentLanguage}\nError was: ${e}`)
            }
        }
    }

    private getSigningKey() {
        // @ts-ignore
        const keys = this.#wallet.extractByTags([this.#signingKeyId])
        if(keys.length === 0) {
            throw new Error(`Signing key '${this.#signingKeyId}' key found in keystore. Abort signing.`)
        }
        if(keys.length > 1) {
            throw new Error(`Multiple '${this.#signingKeyId}' keys found in keystore. Abort signing.`)
        }

        const key = keys[0]
        //console.log(key)
        return key
    }

    async createNewKeys() {
        const key = await secp256k1DIDKey.Secp256k1KeyPair.generate({
            // @ts-ignore
            secureRandom: () => crypto.randomBytes(32)
        });

        if (!key.privateKeyBuffer) {
            throw Error("Cannot create keys without privateKeyBuffer")
        };

        this.#did = key.controller
        this.#didDocument = JSON.stringify(await resolver.resolve(this.#did))
        this.#agent = new Agent(this.#did)
        this.#signingKeyId = key.id

        const keys = [{
            type: 'assymetric',
            encoding: "hex",
            publicKey: key.publicKeyBuffer.toString('hex'),
            privateKey: key.privateKeyBuffer.toString('hex'),
            tags: [key.type, key.id]
        }]

        this.#wallet = didWallet.create({keys})

        console.debug(key)
        console.debug(JSON.stringify(key))
    }

    async initialize(did: string, didDocument: string, keystore: string, password: string) {
        this.#did = did
        this.#didDocument = didDocument
        this.#agent = new Agent(did)
        this.#signingKeyId = did+'#primary'

        console.debug("Creating wallet...")
        this.#wallet = didWallet.create(keystore)
        console.debug("done.")

        console.debug("Unlocking wallet...")
        try {
            // @ts-ignore
            this.#wallet.unlock(password)
        } catch(e) {
            console.error(e)
            return
        }

        console.debug("done.")

        console.debug("Saving wallet...")
        this.save(password)
        console.debug("done.")

        console.debug("Registering new DID with agent language...")
        this.storeAgentProfile()
        this.#pubsub.publish(PubSubInstance.AGENT_UPDATED, this.#agent)
        this.#readyPromiseResolve!()
    }

    isInitialized() {
        return fs.existsSync(this.#file)
    }

    isUnlocked() {
        // @ts-ignore
        const keys = this.#wallet.keys ? true : false
        return keys
    }

    async unlock(password: string) {
        // @ts-ignore
        this.#wallet.unlock(password)
        await this.storeAgentProfile()
        this.#pubsub.publish(PubSubInstance.AGENT_STATUS_CHANGED, this.dump())
        this.#readyPromiseResolve!()
    }

    lock(password: string) {
        // @ts-ignore
        this.#wallet.lock(password)
        this.#pubsub.publish(PubSubInstance.AGENT_STATUS_CHANGED, this.dump())
    }

    async save(password: string) {
        // @ts-ignore
        this.#wallet.lock(password)

        const dump = {
            did: this.#did,
            didDocument: this.#didDocument,
            signingKeyId: this.#signingKeyId,
            // @ts-ignore
            keystore: this.#wallet.export(),
            agent: this.#agent
        }

        fs.writeFileSync(this.#file, JSON.stringify(dump))

        // @ts-ignore
        await this.#wallet.unlock(password)
        this.#readyPromiseResolve!()
    }

    load() {
        if(!this.isInitialized()) return

        const dump = JSON.parse(fs.readFileSync(this.#file).toString())

        this.#did = dump.did
        this.#didDocument = dump.didDocument
        this.#signingKeyId = dump.signingKeyId
        this.#wallet = didWallet.create(dump.keystore)
        if(fs.existsSync(this.#fileProfile))
            this.#agent = JSON.parse(fs.readFileSync(this.#fileProfile).toString())
        else {
            this.#agent = new Agent(this.#did!)
        }
    }

    dump() {
        const isInitialized = this.isInitialized()
        let isUnlocked = false
        if(isInitialized) {
            // @ts-ignore
            isUnlocked= this.#wallet.keys ? true : false
        }

        const dump = {
            agent: this.#agent,
            isInitialized,
            isUnlocked,
            did: this.#did,
            didDocument: this.#didDocument
        }
        return dump
    }

    async getCapabilities(token: string) {
        if (token == this.#adminCredential) {
            return [ALL_CAPABILITY]
        }
        
        if (token === '') {
            return [AGENT_AUTH_CAPABILITY]
        }

        const key = this.getSigningKey()
        // @ts-ignore
        let keyEncoder = new KeyEncoder.default('secp256k1')
        const pemPublicKey = keyEncoder.encodePublic(key.publicKey, 'raw', 'pem')
        const pubKeyObj = crypto.createPublicKey(pemPublicKey)

        const { payload } = await jose.jwtVerify(token, pubKeyObj)

        return payload.capabilities
    }
    
    requestCapability(appName: string, appDesc: string, appUrl: string, capabilities: string) {
        let requestId = uuidv4()
        let authExtended = {
            requestId,
            auth: {
                appName,
                appDesc,
                appUrl,
                capabilities: JSON.parse(capabilities),
            } as AuthInfo,
        } as AuthInfoExtended
        
        this.#pubsub.publish(
            PubSubInstance.EXCEPTION_OCCURRED_TOPIC,
            {
                title: "Request to authenticate application",
                message: `${appName} is waiting for authentication, go to ad4min for more information.`,
                type: ExceptionType.CapabilityRequested,
                addon: JSON.stringify(authExtended),
            } as ExceptionInfo
        )

        return requestId
    }

    // TODO, we may want to change the capability request workflow.
    // https://github.com/perspect3vism/ad4m-executor/issues/73
    permitCapability(authExt: string, capabilities: Capability[]) {
        console.log("admin user capabilities: ", capabilities)
        console.log("auth info: ", authExt)

        let { requestId, auth }: AuthInfoExtended = JSON.parse(authExt)
        let rand = genRandomDigits()
        this.#requests.set(genRequestKey(requestId, rand), auth)
        
        this.#requestingAuthInfo = JSON.parse(authExt);

        return rand
    }

    async generateJwt(requestId: string, rand: string) {
        const authKey = genRequestKey(requestId, rand)
        console.log("rand number with requestId: ", authKey)
        const auth = this.#requests.get(authKey)

        if (!auth) {
            throw new Error("Can't find permitted request")
        }
        
        const key = this.getSigningKey()
        // @ts-ignore
        let keyEncoder = new KeyEncoder.default('secp256k1')
        const pemPrivateKey = keyEncoder.encodePrivate(key.privateKey, 'raw', 'pem')
        const keyObj = crypto.createPrivateKey(pemPrivateKey)

        const jwt = await new jose.SignJWT({...auth})
            .setProtectedHeader({ alg: "ES256K" })
            .setIssuedAt()
            .setIssuer(this.did || "")
            .setAudience(`${auth.appName}:${this.did || ""}`)
            .setExpirationTime(`${this.#tokenValidPeriod}s`)
            .sign(keyObj)

        this.#requests.delete(authKey)

        if (requestId === this.#requestingAuthInfo?.requestId) {
            let apps
            try {
                apps = JSON.parse(fs.readFileSync(this.#appsFile).toString())
            } catch(e) {
                apps = []
            }
            
            fs.writeFileSync(this.#appsFile, JSON.stringify([...apps, this.#requestingAuthInfo]))
        }

        return jwt
    }

    getApps(): AuthInfoExtended[] {
        try {
            return JSON.parse(fs.readFileSync(this.#appsFile).toString())
        } catch (e) {
            fs.writeFileSync(this.#appsFile, '[]')
            return []
        }
    }

    async signMessage(msg: string) {
        const key = this.getSigningKey()
        const msgHash = await secp.utils.sha256(new TextEncoder().encode(msg));
        const signature = await secp.sign(msgHash, key.privateKey)
        const sigHex = Buffer.from(signature).toString('hex')
        return new AgentSignature(sigHex, key.publicKey)
    }
}

export function init(rootConfigPath: string, reqCredential?: string): AgentService {
    const agent = new AgentService(rootConfigPath, reqCredential)
    agent.load()
    return agent
}
