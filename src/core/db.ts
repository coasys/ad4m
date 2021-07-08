import low from 'lowdb'
import FileSync from 'lowdb/adapters/FileSync'
import path from 'path'
import type { LinkExpression } from "@perspect3vism/ad4m";  

export class PerspectivismDb {
    #db: any

    constructor(adapter: typeof FileSync) {
        const db = low(adapter)
        this.#db = db
    }

    linkKey(pUUID: string, linkName: string) {
        return `${pUUID}-link-${linkName}`
    }

    allLinksKey(pUUID: string) {
        return `${pUUID}-all_links`
    }

    sourceKey(pUUID: string, source: string) {
        return `${pUUID}-from_source-${source}`
    }

    targetKey(pUUID: string, target: string) {
        return `${pUUID}-to_target-${target}`
    }

    storeLink(pUUID: string, link: object, linkName: string) {
        this.#db.set(this.linkKey(pUUID, linkName), [link]).write()

        const key = this.allLinksKey(pUUID)
        if(!this.#db.has(key).value()) {
            this.#db.set(key, []).write()
        }

        this.#db.get(key)
            .push(linkName)
            .write()
    }

    updateLink(pUUID: string, link: object, linkName: string) {
        const key = this.linkKey(pUUID, linkName)

        if(!this.#db.has(key).value()) {
            this.storeLink(pUUID, link, linkName)
            return
        }

        this.#db.get(key)
            .push(link)
            .write()
    }

    getLink(pUUID: string, linkName: string): LinkExpression|void {
        const key = this.linkKey(pUUID, linkName)
        const versions = this.#db.get(key).value()
        return versions[versions.length-1]
    }

    getAllLinks(pUUID: string): any[] {
        return this.getLinksByKey(pUUID, this.allLinksKey(pUUID))
    }

    getLinksBySource(pUUID: string, source: string): any[] {
        const key = this.sourceKey(pUUID, source)
        return this.getLinksByKey(pUUID, key)
    }

    getLinksByTarget(pUUID: string, target: string): any[] {
        const key = this.targetKey(pUUID, target)
        return this.getLinksByKey(pUUID, key)
    }

    getLinksByKey(pUUID: string, key: string): any[] {
        let allLinkNames = this.#db.get(key).value()
        if(!allLinkNames) {
            allLinkNames = []
        }

        const allLinks = []
        for(const linkName of allLinkNames) {
            allLinks.push({
                name: linkName,
                link: this.getLink(pUUID, linkName)
            })
        }
        return allLinks
    }

    attachSource(pUUID: string, source: string, linkName: string) {
        const key = this.sourceKey(pUUID, source)
        this.attach(key, linkName)
    }

    attachTarget(pUUID: string, target: string, linkName: string) {
        const key = this.targetKey(pUUID, target)
        this.attach(key, linkName)
    }

    attach(key: string, linkName: string) {
        if(!this.#db.has(key).value()) {
            this.#db.set(key, []).write()
        }

        if(!this.#db.get(key).includes(linkName).value()) {
            this.#db.get(key)
                .push(linkName)
                .write()
        }
    }

    removeSource(pUUID: string, source: string, linkName: string) {
        const key = this.sourceKey(pUUID, source)
        this.remove(key, linkName)
    }

    removeTarget(pUUID: string, target: string, linkName: string) {
        const key = this.targetKey(pUUID, target)
        this.remove(key, linkName)
    }


    remove(key: string, linkName: string) {
        //@ts-ignore
        this.#db.get(key).remove(l => l===linkName).write()
    }

}

export function init(dbFilePath: string): PerspectivismDb {
    const adapter = new FileSync(path.join(dbFilePath, 'db.json'))
    return new PerspectivismDb(adapter)
}

