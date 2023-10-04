import * as path from "https://deno.land/std@0.203.0/path/mod.ts";
import * as fs from "https://deno.land/std@0.203.0/fs/mod.ts";
import { PerspectiveExpression } from '@perspect3vism/ad4m';
import { MainConfig } from './Config';

const TRUSTED_AGENTS_FILE = "trustedAgents.json"
const KNOW_LINK_LANGUAGES_FILE = "knownLinkLanguages.json"
const FRIENDS_FILE = "friends.json"
const OUTBOX_FILE = "outbox.json"

function _add(items: string[], file: string): void {
    let all: string[];
    if (fs.existsSync(file)) {
        const decoder = new TextDecoder("utf-8");
        const data = decoder.decode(Deno.readFileSync(file));
        all = Array.from(JSON.parse(data));
        all = all.concat(items);
        all = Array.from(new Set(all));
    } else {
        all = items
    }
    const encoder = new TextEncoder();
    const data = encoder.encode(JSON.stringify(all));
    Deno.writeFileSync(file, data)
}

function _addObject(item: object, file: string): void {
    let all: object[];
    if (fs.existsSync(file)) {
        const decoder = new TextDecoder("utf-8");
        const data = decoder.decode(Deno.readFileSync(file));
        all = Array.from(JSON.parse(data));
        all.push(item)
    } else {
        all = [item]
    }

    const encoder = new TextEncoder();
    const data = encoder.encode(JSON.stringify(all));
    Deno.writeFileSync(file, data)
}

function _delete(items: string[], file: string): void {
    if (fs.existsSync(file)) {
        const decoder = new TextDecoder("utf-8");
        const data = decoder.decode(Deno.readFileSync(file));
        let all= Array.from(JSON.parse(data));
        for (const item of items) {
            all.splice(all.findIndex((value) => value == item), 1);
        }

        const encoder = new TextEncoder();
        const allEncoded = encoder.encode(JSON.stringify(all));
        Deno.writeFileSync(file, allEncoded)
    }
}

function _get(file: string): string[] {
    let all: string[] = []
    if (fs.existsSync(file)) {
        const decoder = new TextDecoder("utf-8");
        const data = decoder.decode(Deno.readFileSync(file));
        all.push(...Array.from<string>(JSON.parse(data)));
    }
    return all
}

function _getObjects(file: string): object[] {
    if (fs.existsSync(file)) {
        const decoder = new TextDecoder("utf-8");
        const data = decoder.decode(Deno.readFileSync(file));
        return JSON.parse(data)
    } else {
        return []
    }
}

export interface Message {
    recipient: string;
    message: PerspectiveExpression;
}

export default class RuntimeService {
    #config: MainConfig
    #did: string


    constructor(config: MainConfig) {
        this.#config = config
        this.#did = ""
    }

    set did(did: string) {
        this.#did = did
    }

    trustedAgentsPath(): string {
        return path.join(this.#config.rootConfigPath, TRUSTED_AGENTS_FILE)
    }

    knowLinkLanguagesPath(): string {
        return path.join(this.#config.rootConfigPath, KNOW_LINK_LANGUAGES_FILE)
    }

    friendsPath(): string {
        return path.join(this.#config.rootConfigPath, FRIENDS_FILE)
    }

    outboxPath(): string {
        return path.join(this.#config.rootConfigPath, OUTBOX_FILE)
    }

    addTrustedAgents(agents: string[]): void {
        _add(agents, this.trustedAgentsPath())
    }

    deleteTrustedAgents(agents: string[]): void {
        _delete(agents, this.trustedAgentsPath())
    }

    getTrustedAgents(): string[] {
        let agents = [this.#did!, ...this.#config.trustedAgents, ..._get(this.trustedAgentsPath())]
        let dedupAgents = [...new Set(agents)]
        return dedupAgents
    }

    addKnowLinkLanguageTemplates(addresses: string[]): void {
        _add(addresses, this.knowLinkLanguagesPath())
    }

    removeKnownLinkLanguageTemplates(addresses: string[]): void {
        _delete(addresses, this.knowLinkLanguagesPath())
    }

    knowLinkLanguageTemplates(): string[] {
        return [...this.#config.knownLinkLanguages, ..._get(this.knowLinkLanguagesPath())]
    }

    addFriends(addresses: string[]): void {
        _add(addresses, this.friendsPath())
    }

    removeFriends(addresses: string[]): void {
        _delete(addresses, this.friendsPath())
    }

    friends(): string[] {
        return _get(this.friendsPath())
    }

    addMessageOutbox(recipient: string, message: PerspectiveExpression) {
        _addObject({recipient, message}, this.outboxPath())
    }

    getMessagesOutbox(filter?: string): Message[] {
        let messages = _getObjects(this.outboxPath()) as Message[]
        // console.log("OUTBOX:", messages)
        if(filter) {
            messages = messages.filter(m => m.recipient === filter)
        }
        return messages
    }

}