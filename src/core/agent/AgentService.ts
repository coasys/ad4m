import * as path from 'path';
import * as fs from 'fs';
import didWallet from '@transmute/did-wallet'
import Expression, { ExpressionProof } from '../../acai/Expression';
import secp256k1 from 'secp256k1'
import * as ed25519 from '@transmute/did-key-ed25519';
import * as secp256k1DIDKey from '@transmute/did-key-secp256k1';
import Signatures from './Signatures';
import Agent from '../../acai/Agent';
import type Language from '../../acai/Language';
import * as PubSubInstance from '../PubSub'
import type { PubSub } from 'apollo-server';
import crypto from 'crypto'
import { resolver } from '@transmute/did-key.js';

export default class AgentService {
    #did: string
    #didDocument: string
    #signingKeyId: string
    #wallet: object
    #file: string
    #fileProfile: string
    #agent: Agent
    #agentLanguage: Language
    #pubsub: PubSub

    #readyPromise: Promise<void>
    #readyPromiseResolve

    constructor(rootConfigPath: string) {
        this.#file = path.join(rootConfigPath, "agent.json")
        this.#fileProfile = path.join(rootConfigPath, "agentProfile.json")
        this.#pubsub = PubSubInstance.get()
        this.#readyPromise = new Promise(resolve => {
            this.#readyPromiseResolve = resolve
        })
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

        const timestamp = new Date().toString()
        const payloadBytes = Signatures.buildMessage(data, timestamp)

        const key = this.getSigningKey()
        const privKey = Uint8Array.from(Buffer.from(key.privateKey, key.encoding))

        const sigObj = secp256k1.ecdsaSign(payloadBytes, privKey)
        const sigBuffer = Buffer.from(sigObj.signature)
        const sigHex = sigBuffer.toString('hex')

        const signedExpresssion = {
            author: { did: this.#did },
            timestamp,
            data,
            proof: new ExpressionProof(sigHex, this.#signingKeyId)
        } as Expression

        console.debug("Signed Expression:", signedExpresssion)
        return signedExpresssion
    }

    updateAgent(a: Agent) {
        this.#agent = a
        this.storeAgentProfile()
        this.#pubsub.publish(PubSubInstance.AGENT_UPDATED, a)
    }

    setAgentLanguage(lang: Language) {
        if(!lang?.agentAdapter) {
            console.error("AgentService ERROR: Not an AgentLanguage:", lang)
            return
        }

        this.#agentLanguage = lang
    }

    storeAgentProfile() {
        fs.writeFileSync(this.#fileProfile, JSON.stringify(this.#agent))

        if(!this.#agentLanguage) {
            console.error("AgentService ERROR: Can't store Agent profile. No AgentLanguage installed.")
            return
        }

        if(this.#agent?.did)
            this.#agentLanguage.agentAdapter.setProfile(this.#agent)
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
        console.log(key)
        return key
    }

    async createNewKeys() {
        const key = await secp256k1DIDKey.Secp256k1KeyPair.generate({
            // @ts-ignore
            secureRandom: () => crypto.randomBytes(32)
        });

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

    initialize(did, didDocument, keystore, password) {
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
        this.#readyPromiseResolve()
    }

    isInitialized() {
        return fs.existsSync(this.#file)
    }

    isUnlocked() {
        // @ts-ignore
        const keys = this.#wallet.keys ? true : false
        return keys
    }

    unlock(password) {
        // @ts-ignore
        this.#wallet.unlock(password)
        this.storeAgentProfile()
        this.#readyPromiseResolve()
    }

    save(password) {
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
        this.#wallet.unlock(password)
        this.#readyPromiseResolve()
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
            this.#agent = new Agent(this.#did)
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
}

export function init(rootConfigPath: string): AgentService {
    const agent = new AgentService(rootConfigPath)
    agent.load()
    return agent
}