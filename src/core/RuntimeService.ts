import * as path from 'path';
import * as fs from 'fs';

const TRUSTED_AGENTS_FILE = "trustedAgents.json"
const KNOW_LINK_LANGUAGES_FILE = "knownLinkLanguages.json"
const FRIENDS_FILE = "friends.json"

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

    addTrustedAgents(agents: string[]): void {
        _add(agents, this.trustedAgentsPath())
    }

    deleteTrustedAgents(agents: string[]): void {
        _delete(agents, this.trustedAgentsPath())
    }
    
    getTrustedAgents(): string[] {
        return [this.#did!, ..._get(this.trustedAgentsPath())]
    }

    addKnowLinkLanguageTemplates(addresses: string[]): void {
        _add(addresses, this.knowLinkLanguagesPath())
    }

    removeKnownLinkLanguageTemplates(addresses: string[]): void {
        _delete(addresses, this.knowLinkLanguagesPath())
    }
    
    knowLinkLanguageTemplates(): string[] {
        return _get(this.knowLinkLanguagesPath())
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


}