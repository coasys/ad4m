import { DirectMessageAdapter, HolochainLanguageDelegate, LanguageContext, MessageCallback, Perspective, PerspectiveExpression } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import { BUNDLE, DNA_ROLE, ZOME_NAME } from "./build/happ.js";

//!@ad4m-template-variable
const recipient_did = "<not templated yet>"

//@ts-ignore
export const sleep = ms => new Promise(r => setTimeout(r, ms))

export default class DMAdapter implements DirectMessageAdapter {
  #context: LanguageContext
  #holochain: HolochainLanguageDelegate;
  #messageCallbacks: MessageCallback[];


  constructor(context: LanguageContext) {
    this.#context = context
    this.#holochain = context.Holochain as HolochainLanguageDelegate;
    this.#messageCallbacks = []
  }

  async init() {
    const that = this
    //@ts-ignore
    await this.#holochain.registerDNAs(
      [
        { 
          file: BUNDLE, 
          nick: DNA_ROLE, 
          //@ts-ignore
          zomeCalls: [
            [ZOME_NAME, "send_p2p"],
            [ZOME_NAME, "send_inbox"],
            [ZOME_NAME, "set_status"],
            [ZOME_NAME, "get_status"],
            [ZOME_NAME, "fetch_inbox"],
            [ZOME_NAME, "inbox"],
          ] 
        }
      ], async (signal) => {
        console.debug("DM Language got HC signal:", signal)
        //@ts-ignore
        let payload = signal.payload
        try {
          //@ts-ignore
          let string = signal.payload.toString()
          let cropped = string.substring(string.indexOf("{"))
          let parsed = JSON.parse(cropped)
          payload = parsed
        } catch(e) {
          console.error(e)
        }
        for (const cb of that.#messageCallbacks) {
          await cb(payload)
        }
      });
  }

  recipient(): string{
    return recipient_did
  }

  async status(): Promise<PerspectiveExpression | void> {
    let status = null
    try {
      //@ts-ignore
      status = await this.#holochain.call(DNA_ROLE, ZOME_NAME, "get_status", null)  
    } catch(e) {
      console.debug("DirectMessage Language couldn't get status:", e)
    }
    return status
  }

  async sendP2P(message: Perspective): Promise<PerspectiveExpression|void> {
    try {
      const messageExpression = this.#context.agent.createSignedExpression(message)
      await this.#holochain.call(DNA_ROLE, ZOME_NAME, "send_p2p", messageExpression)
      return messageExpression
    } catch(e) {
      console.error("Direct Message Language: Error sending p2p to", recipient_did)
    }
  }

  async sendInbox(message: Perspective): Promise<PerspectiveExpression|void> {
    try {
      const messageExpression = this.#context.agent.createSignedExpression(message)
      await this.#holochain.call(DNA_ROLE, ZOME_NAME, "send_inbox", messageExpression)
      return messageExpression
    } catch(e) {
      console.error("Direct Message Language: Error sending to inbox of", recipient_did)
    }
  }

  onlyRecipient() {
    console.log(recipient_did, this.#context.agent.did);
    if(recipient_did !== this.#context.agent.did) throw new Error("Only recipient can call this function!")
  }

  async setStatus(status: PerspectiveExpression) {
    this.onlyRecipient()
    const statusExpression = this.#context.agent.createSignedExpression(status)
    await this.#holochain.call(DNA_ROLE, ZOME_NAME, "set_status", statusExpression)
  }

  async inbox(filter?: string): Promise<PerspectiveExpression[]> {
    this.onlyRecipient()
    //@ts-ignore
    await this.#holochain.call(DNA_ROLE, ZOME_NAME, "fetch_inbox", null)
    //@ts-ignore
    return await this.#holochain.call(DNA_ROLE, ZOME_NAME, "inbox", filter)
  }

  addMessageCallback(callback: MessageCallback) {
    console.log("adding callback on dm language");
    this.onlyRecipient()
    this.#messageCallbacks.push(callback)
  }
}