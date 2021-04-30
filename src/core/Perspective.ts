import type Agent from "@perspect3vism/ad4m/Agent"
import Link, { hashLinkExpression, linkEqual, LinkQuery } from "@perspect3vism/ad4m/Links";
import { SHA3 } from "sha3";
import type Expression from "@perspect3vism/ad4m/Expression";
import type AgentService from "./agent/AgentService";
import type LanguageController from "./LanguageController";
import * as PubSub from './graphQL-interface/PubSub'
import type PerspectiveID from "./PerspectiveID"
import type SharedPerspective from "@perspect3vism/ad4m/SharedPerspective";
import type PerspectiveContext from "./PerspectiveContext"

export default class Perspective {
    name: string;
    uuid: string;
    author: Agent;
    timestamp: string;
    sharedPerspective: SharedPerspective;
    sharedURL: string;

    #db: any;
    #agent: AgentService;
    #languageController: LanguageController
    #pubsub: any

    constructor(id: PerspectiveID, context: PerspectiveContext) {
        this.updateFromId(id)

        this.#db = context.db
        this.#agent = context.agentService
        this.#languageController = context.languageController

        this.#pubsub = PubSub.get()
    }

    plain(): PerspectiveID {
        const { name, uuid, author, timestamp, sharedPerspective, sharedURL } = this
        return JSON.parse(JSON.stringify({
            name, uuid, author, timestamp
        }))
    }

    updateFromId(id: PerspectiveID) {
        if(id.name) this.name = id.name
        if(id.uuid) this.uuid = id.uuid
        if(id.author) this.author = id.author
        if(id.timestamp) this.timestamp = id.timestamp
        if(id.sharedPerspective) this.sharedPerspective = id.sharedPerspective
        if(id.sharedURL) this.sharedURL = id.sharedURL
    }

    linkToExpression(link: Link): Expression {
        return this.#agent.createSignedExpression(link)
    }

    ensureLinkExpression(maybeLink: any): Expression {
        if(maybeLink.author && maybeLink.timestamp && maybeLink.data) {
            return maybeLink as Expression
        }

        if(maybeLink.target) {
            return this.linkToExpression(maybeLink)
        }

        throw new Error("Object is neither Link nor Expression: " + JSON.stringify(maybeLink))
    }

    private callLinksAdapter(functionName: string, ...args): Promise<any> {
        if(!this.sharedPerspective || !this.sharedPerspective.linkLanguages || this.sharedPerspective.linkLanguages.length === 0) {
            return Promise.resolve([])
        }

        return new Promise(async (resolve, reject) => {
            setTimeout(() => resolve([]), 2000)
            try {
                const langRef = this.sharedPerspective.linkLanguages[0]
                const linksAdapter = this.#languageController.getLinksAdapter(langRef)
                if(linksAdapter) {
                    //console.debug(`Calling linksAdapter.${functionName}(${JSON.stringify(args)})`)
                    const result = await linksAdapter[functionName](...args)
                    //console.debug("Got result:", result)
                    resolve(result)
                } else {
                    console.error("LinksSharingLanguage", langRef.address, "set in perspective '"+this.name+"' not installed!")
                    // TODO: request install
                    resolve([])
                }
            } catch(e) {
                console.error("Error while trying to call links adapter:", e)
                reject(e)
            }
        })
    }

    async syncWithSharingAdapter() {
        const localLinks = this.#db.getAllLinks(this.uuid).map(l => l.link)
        const remoteLinks = await this.callLinksAdapter('getLinks')
        const includes = (link, list) => {
            return undefined !== list.find(e =>
                JSON.stringify(e.author) === JSON.stringify(link.author) &&
                e.timestamp === link.timestamp &&
                e.source === link.data.source &&
                e.target === link.data.target &&
                e.predicate === link.data.predicate
                )
        }
        for(const l of localLinks) {
            if(!includes(l, remoteLinks)) {
                await this.callLinksAdapter("addLink", l)
            }
        }

    }

    addLink(link: Link | Expression): Expression {
        const linkExpression = this.ensureLinkExpression(link)
        this.callLinksAdapter('addLink', linkExpression)
        const hash = new SHA3(256);
        hash.update(JSON.stringify(linkExpression));
        const addr = hash.digest('hex');

        link = linkExpression.data as Link

        this.#db.storeLink(this.uuid, linkExpression, addr)
        this.#db.attachSource(this.uuid, link.source, addr)
        this.#db.attachTarget(this.uuid, link.target, addr)

        this.#pubsub.publish(PubSub.LINK_ADDED_TOPIC, {
            perspective: this.plain(),
            linkAdded: linkExpression
        })

        return linkExpression
    }



    private findLink(linkToFind: Expression): string {
        const allLinks = this.#db.getAllLinks(this.uuid)
        for(const {name, link} of allLinks) {
            if(linkEqual(linkToFind, link)) {
                return name
            }
        }
        throw new Error(`Link not found in perspective "${this.plain()}": ${JSON.stringify(linkToFind)}`)
    }

    async updateLink(oldLink: Expression, newLink: Expression) {
        //console.debug("LINK REPO: updating link:", oldLink, newLink)
        const addr = this.findLink(oldLink)
        //console.debug("hash:", addr)

        const _old = oldLink.data as Link
        const _new = newLink.data as Link

        this.#db.updateLink(this.uuid, newLink, addr)
        if(_old.source !== _new.source){
            this.#db.removeSource(this.uuid, _old.source, addr)
            this.#db.attachSource(this.uuid, _new.source, addr)
        }
        if(_old.target !== _new.target){
            this.#db.removeTarget(this.uuid, _old.target, addr)
            this.#db.attachTarget(this.uuid, _new.target, addr)
        }

        this.callLinksAdapter('updateLink', oldLink, newLink)
        this.#pubsub.publish(PubSub.LINK_ADDED_TOPIC, {
            perspective: this.plain(),
            link: newLink
        })
        this.#pubsub.publish(PubSub.LINK_REMOVED_TOPIC, {
            perspective: this.plain(),
            link: oldLink
        })
    }

    async removeLink(linkExpression: Expression) {
        const addr = this.findLink(linkExpression)
        const link = linkExpression.data as Link

        this.#db.removeSource(this.uuid, link.source, addr)
        this.#db.removeTarget(this.uuid, link.target, addr)

        this.callLinksAdapter('removeLink', linkExpression)
        this.#pubsub.publish(PubSub.LINK_REMOVED_TOPIC, {
            perspective: this.plain(),
            link
        })
    }

    private getLinksLocal(query: LinkQuery): Expression[] {
        // console.debug("getLinks 1")
        if(!query || !query.source && !query.predicate && !query.target) {
            return this.#db.getAllLinks(this.uuid).map(e => e.link)
        }

        // console.debug("getLinks 2")

        if(query.source) {
            // console.debug("query.source", query.source)
            let result = this.#db.getLinksBySource(this.uuid, query.source).map(e => e.link)
            // @ts-ignore
            if(query.target) result = result.filter(l => l.data.target === query.target)
            // @ts-ignore


            if(query.predicate) result = result.filter(l => l.data.predicate === query.predicate)
            // console.debug("result", result)
            return result
        }

        // console.debug("getLinks 3")

        if(query.target) {
            let result = this.#db.getLinksByTarget(this.uuid, query.target).map(e => e.link)
            // @ts-ignore
            if(query.predicate) result = result.filter(l => l.data.predicate === query.predicate)
            return result
        }

        // console.debug("getLinks 4")

        return this.#db.getAllLinks(this.uuid).map(e => e.link).filter(link => link.data.predicate === query.predicate)
    }

    async getLinks(query: LinkQuery): Promise<Expression[]> {
        // console.debug("getLinks local...")
        const localLinks = await this.getLinksLocal(query)
        // console.debug("getLinks local", localLinks)
        // console.debug("getLinks remote...")
        const remoteLinks = await this.callLinksAdapter('getLinks', query)
        // console.debug("getLinks remote", remoteLinks)
        const mergedLinks = {}
        localLinks.forEach(l => mergedLinks[hashLinkExpression(l)] = l)
        remoteLinks.forEach(l => mergedLinks[hashLinkExpression(l)] = l)

        return Object.values(mergedLinks)
    }
}