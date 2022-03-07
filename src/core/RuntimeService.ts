import * as path from 'path';
import * as fs from 'fs';
import { PerspectiveExpression } from '@perspect3vism/ad4m';

const TRUSTED_AGENTS_FILE = "trustedAgents.json"
const KNOW_LINK_LANGUAGES_FILE = "knownLinkLanguages.json"
const FRIENDS_FILE = "friends.json"
const OUTBOX_FILE = "outbox.json"

const PERSPECT3VISM_AGENT = "did:key:zQ3shkkuZLvqeFgHdgZgFMUx8VGkgVWsLA83w2oekhZxoCW2n"
const SOCIAL_CONTEXT_OFFICIAL = "QmfDoeJgiG5Hs4DJcwPqDWbwU2Ks8zLSJjv7bR8is84Qt5"

function _add(items: string[], file: string): void {
    let all: string[];
    if (fs.existsSync(file)) {
        all = Array.from(JSON.parse(fs.readFileSync(file).toString()));
        all = all.concat(items);
        all = Array.from(new Set(all));
    } else {
        all = items 
    }

    fs.writeFileSync(file, JSON.stringify(all))
}

function _addObject(item: object, file: string): void {
    let all: object[];
    if (fs.existsSync(file)) {
        all = Array.from(JSON.parse(fs.readFileSync(file).toString()));
        all.push(item)
    } else {
        all = [item] 
    }

    fs.writeFileSync(file, JSON.stringify(all))
}

function _delete(items: string[], file: string): void {
    if (fs.existsSync(file)) {
        let all= Array.from(JSON.parse(fs.readFileSync(file).toString()));
        for (const item of items) {
            all.splice(all.findIndex((value) => value == item), 1);
        }
        fs.writeFileSync(file, JSON.stringify(all))
    }
}

function _get(file: string): string[] {
    let all: string[] = []
    if (fs.existsSync(file)) {
        all.push(...Array.from<string>(JSON.parse(fs.readFileSync(file).toString())));   
    }
    return all
}

function _getObjects(file: string): object[] {
    if (fs.existsSync(file)) {
        return JSON.parse(fs.readFileSync(file).toString())
    } else {
        return []
    }
}

export interface Message {
    recipient: string;
    message: PerspectiveExpression;
}

export default class RuntimeService {
    #rootConfigPath: string
    #did: string

    constructor(rootConfigPath: string) {
        this.#rootConfigPath = rootConfigPath
        this.#did = ""
    }

    set did(did: string) {
        this.#did = did
    }

    trustedAgentsPath(): string {
        return path.join(this.#rootConfigPath, TRUSTED_AGENTS_FILE)
    }

    knowLinkLanguagesPath(): string {
        return path.join(this.#rootConfigPath, KNOW_LINK_LANGUAGES_FILE)
    }

    friendsPath(): string {
        return path.join(this.#rootConfigPath, FRIENDS_FILE)
    }

    outboxPath(): string {
        return path.join(this.#rootConfigPath, OUTBOX_FILE)
    }

    addTrustedAgents(agents: string[]): void {
        _add(agents, this.trustedAgentsPath())
    }

    deleteTrustedAgents(agents: string[]): void {
        _delete(agents, this.trustedAgentsPath())
    }
    
    getTrustedAgents(): string[] {
        return [this.#did!, PERSPECT3VISM_AGENT, ..._get(this.trustedAgentsPath())]
    }

    addKnowLinkLanguageTemplates(addresses: string[]): void {
        _add(addresses, this.knowLinkLanguagesPath())
    }

    removeKnownLinkLanguageTemplates(addresses: string[]): void {
        _delete(addresses, this.knowLinkLanguagesPath())
    }
    
    knowLinkLanguageTemplates(): string[] {
        return [SOCIAL_CONTEXT_OFFICIAL, ..._get(this.knowLinkLanguagesPath())]
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
        console.log("OUTBOX:", messages)
        if(filter) {
            messages = messages.filter(m => m.recipient === filter)
        }
        return messages
    }

}