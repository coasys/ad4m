import * as path from 'node:path';
import * as fs from 'node:fs';
import { PerspectiveExpression } from '@coasys/ad4m';
import { MainConfig } from './Config';

const TRUSTED_AGENTS_FILE = "trustedAgents.json"
const KNOW_LINK_LANGUAGES_FILE = "knownLinkLanguages.json"
const FRIENDS_FILE = "friends.json"
const OUTBOX_FILE = "outbox.json"

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
    #config: MainConfig
    #did: string


    constructor(config: MainConfig) {
        this.#config = config
        this.#did = ""
    }

    set did(did: string) {
        this.#did = did
    }

    friendsPath(): string {
        return path.join(this.#config.rootConfigPath, FRIENDS_FILE)
    }

    outboxPath(): string {
        return path.join(this.#config.rootConfigPath, OUTBOX_FILE)
    }

    friends(): string[] {
        return _get(this.friendsPath())
    }

    addMessageOutbox(recipient: string, message: PerspectiveExpression) {
        _addObject({recipient, message}, this.outboxPath())
    }
}