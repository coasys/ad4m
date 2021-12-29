import { Agent, Expression, Neighbourhood, LinkExpression, LinkExpressionInput, LinkInput, LanguageRef, PerspectiveHandle, Literal, ExpressionRef, parseExprUrl } from "@perspect3vism/ad4m"
import { Link, hashLinkExpression, linkEqual, LinkQuery } from "@perspect3vism/ad4m";
import { SHA3 } from "sha3";
import type AgentService from "./agent/AgentService";
import type LanguageController from "./LanguageController";
import * as PubSub from './graphQL-interface/PubSub'
import type PerspectiveContext from "./PerspectiveContext"
import PrologInstance from "./PrologInstance";
import { link } from "fs";

export default class Perspective {
    name?: string;
    uuid?: string;
    author?: Agent;
    timestamp?: string;
    neighbourhood?: Neighbourhood;
    sharedUrl?: string;

    #db: any;
    #agent: AgentService;
    #languageController?: LanguageController
    #pubsub: any

    #prologEngine: PrologInstance|null
    #prologNeedsRebuild: boolean

    constructor(id: PerspectiveHandle, context: PerspectiveContext, neighbourhood?: Neighbourhood) {
        this.updateFromId(id)
        if (neighbourhood) this.neighbourhood = neighbourhood;

        this.#db = context.db
        this.#agent = context.agentService!
        this.#languageController = context.languageController!

        this.#pubsub = PubSub.get()
        this.#prologEngine = null
        this.#prologNeedsRebuild = true

        this.#pubsub.subscribe(PubSub.LINK_ADDED_TOPIC, () => {
            this.#prologNeedsRebuild = true
        })

        this.#pubsub.subscribe(PubSub.LINK_REMOVED_TOPIC, () => {
            this.#prologNeedsRebuild = true
        })

        const that = this

        process.on("SIGINT", function () {
            that.#prologEngine?.close()
        });
    }

    plain(): PerspectiveHandle {
        const { name, uuid, author, timestamp, sharedUrl, neighbourhood } = this
        return JSON.parse(JSON.stringify({
            name, uuid, author, timestamp, sharedUrl, neighbourhood
        }))
    }

    updateFromId(id: PerspectiveHandle) {
        if(id.name) this.name = id.name
        if(id.uuid) this.uuid = id.uuid
        if(id.sharedUrl) this.sharedUrl = id.sharedUrl
        if(id.neighbourhood) this.neighbourhood = id.neighbourhood
    }

    linkToExpression(link: Link): Expression {
        return this.#agent.createSignedExpression(link)
    }

    ensureLinkExpression(maybeLink: any): Expression {
        if(maybeLink.author && maybeLink.timestamp && maybeLink.data) {
            return maybeLink as LinkExpression
        }

        if(maybeLink.target) {
            return this.linkToExpression(maybeLink)
        }

        throw new Error("Object is neither Link nor Expression: " + JSON.stringify(maybeLink))
    }

    //@ts-ignore
    private callLinksAdapter(functionName: string, ...args): Promise<LinkExpression[]> {
        if(!this.neighbourhood || !this.neighbourhood.linkLanguage) {
            //console.warn("Perspective.callLinksAdapter: Did not find neighbourhood or linkLanguage for neighbourhood on perspective, returning empty array")
            return Promise.resolve([])
        }

        return new Promise(async (resolve, reject) => {
            setTimeout(() => resolve([]), 20000)
            try {
                const address = this.neighbourhood!.linkLanguage;
                const linksAdapter = await this.#languageController!.getLinksAdapter({address} as LanguageRef);
                if(linksAdapter) {
                    //console.debug(`Calling linksAdapter.${functionName}(${JSON.stringify(args)})`)
                    //@ts-ignore
                    const result = await linksAdapter[functionName](...args)
                    //console.debug("Got result:", result)
                    resolve(result)
                } else {
                    console.error("LinksSharingLanguage", address, "set in perspective '"+this.name+"' not installed!")
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
        //@ts-ignore
        const localLinks = this.#db.getAllLinks(this.uuid).map(l => l.link)
        const remoteLinks = await this.callLinksAdapter('getLinks')
        const includes = (link: LinkExpression, list: LinkExpression[]) => {
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

    addLink(link: LinkInput | LinkExpressionInput): LinkExpression {
        const linkExpression = this.ensureLinkExpression(link)
        this.callLinksAdapter('addLink', linkExpression)
        const hash = new SHA3(256);
        hash.update(JSON.stringify(linkExpression));
        const addr = hash.digest('hex');

        link = linkExpression.data as Link

        this.#db.storeLink(this.uuid, linkExpression, addr)
        this.#db.attachSource(this.uuid, link.source, addr)
        this.#db.attachTarget(this.uuid, link.target, addr)

        this.#prologNeedsRebuild = true
        this.#pubsub.publish(PubSub.LINK_ADDED_TOPIC, {
            perspective: this.plain(),
            link: linkExpression
        })

        return linkExpression
    }

    private findLink(linkToFind: LinkExpressionInput): string {
        const allLinks = this.#db.getAllLinks(this.uuid)
        for(const {name, link} of allLinks) {
            if(linkEqual(linkToFind, link)) {
                return name
            }
        }
        throw new Error(`Link not found in perspective "${this.plain()}": ${JSON.stringify(linkToFind)}`)
    }

    async updateLink(oldLink: LinkExpressionInput, newLink: LinkInput) {
        //console.debug("LINK REPO: updating link:", oldLink, newLink)
        const addr = this.findLink(oldLink)
        //console.debug("hash:", addr)

        const newLinkExpression = this.ensureLinkExpression(newLink)

        const _old = oldLink.data as Link

        this.#db.updateLink(this.uuid, newLinkExpression, addr)
        if(_old.source !== newLink.source){
            this.#db.removeSource(this.uuid, _old.source, addr)
            this.#db.attachSource(this.uuid, newLink.source, addr)
        }
        if(_old.target !== newLink.target){
            this.#db.removeTarget(this.uuid, _old.target, addr)
            this.#db.attachTarget(this.uuid, newLink.target, addr)
        }

        this.#prologNeedsRebuild = true
        this.callLinksAdapter('updateLink', oldLink, newLinkExpression)
        this.#pubsub.publish(PubSub.LINK_ADDED_TOPIC, {
            perspective: this.plain(),
            link: newLinkExpression
        })
        this.#pubsub.publish(PubSub.LINK_REMOVED_TOPIC, {
            perspective: this.plain(),
            link: oldLink
        })

        return newLinkExpression
    }

    async removeLink(linkExpression: LinkExpressionInput) {
        const addr = this.findLink(linkExpression)
        const link = linkExpression.data as Link

        this.#db.removeSource(this.uuid, link.source, addr)
        this.#db.removeTarget(this.uuid, link.target, addr)
        this.#db.remove(this.#db.allLinksKey(this.uuid), addr)

        this.#prologNeedsRebuild = true
        this.callLinksAdapter('removeLink', linkExpression)
        this.#pubsub.publish(PubSub.LINK_REMOVED_TOPIC, {
            perspective: this.plain(),
            link: linkExpression
        })
    }

    private getLinksLocal(query: LinkQuery): LinkExpression[] {
        // console.debug("getLinks 1")
        if(!query || !query.source && !query.predicate && !query.target) {
            //@ts-ignore
            return this.#db.getAllLinks(this.uuid).map(e => e.link)
        }

        // console.debug("getLinks 2")

        if(query.source) {
            // console.debug("query.source", query.source)
            //@ts-ignore
            let result = this.#db.getLinksBySource(this.uuid, query.source).map(e => e.link)
            // @ts-ignore
            if(query.target) result = result.filter(l => l.data.target === query.target)
            // @ts-ignore
            if(query.predicate) result = result.filter(l => l.data.predicate === query.predicate)
            //@ts-ignore
            if (query.fromDate) result = result.filter(l => new Date(l.timestamp) >= query.fromDate!)
            //@ts-ignore
            if (query.untilDate) result = result.filter(l => new Date(l.timestamp) <= query.untilDate!)
            // console.debug("result", result)
            if (query.limit) result = result.slice(0, query.limit);
            return result
        }

        // console.debug("getLinks 3")

        if(query.target) {
            //@ts-ignore
            let result = this.#db.getLinksByTarget(this.uuid, query.target).map(e => e.link)
            // @ts-ignore
            if(query.predicate) result = result.filter(l => l.data.predicate === query.predicate)
            //@ts-ignore
            if (query.fromDate) result = result.filter(l => new Date(l.timestamp) >= query.fromDate!)
            //@ts-ignore
            if (query.untilDate) result = result.filter(l => new Date(l.timestamp) <= query.untilDate!)
            if (query.limit) result = result.slice(0, query.limit);
            return result
        }

        // console.debug("getLinks 4")

        //@ts-ignore
        let result = this.#db.getAllLinks(this.uuid).map(e => e.link).filter(link => link.data.predicate === query.predicate)
        if (query.limit) result = result.slice(0, query.limit)
        return result
    }

    async getLinks(query: LinkQuery): Promise<LinkExpression[]> {
        // console.debug("getLinks local...")
        const localLinks = await this.getLinksLocal(query)
        // console.debug("getLinks local", localLinks)
        // console.debug("getLinks remote...")
        const remoteLinks = await this.callLinksAdapter('getLinks', query)
        // console.debug("getLinks remote", remoteLinks)
        const mergedLinks: {[key: number]: LinkExpression} = {};
        localLinks.forEach(l => mergedLinks[hashLinkExpression(l)] = l)
        remoteLinks.forEach(l => mergedLinks[hashLinkExpression(l)] = l)

        return Object.values(mergedLinks)
    }

    async createPrologFacts(): Promise<string> {
        let lines = []

        const allLinks = await this.getLinks(new LinkQuery({}))
        //-------------------
        // triple/3
        //-------------------
        lines = allLinks.map(l => `triple("${l.data.source}", "${l.data.predicate}", "${l.data.target}").`)

        //-------------------
        // reachable/2
        //-------------------
        lines.push("reachable(A,B) :- triple(A,_,B).")
        lines.push("reachable(A,B) :- triple(A,_,X), reachable(X,B).")

        //-------------------
        // hiddenExpression/1
        //-------------------
        lines.push(":- discontiguous hiddenExpression/1.")            

        //-------------------
        // languageAddress/2
        // languageName/2
        // expressionAddress/2
        //-------------------
        let langAddrs = []
        let langNames = []
        let exprAddrs = []

        let nodes = new Set<string>()
        for(let link of allLinks) {
            nodes.add(link.data.source)
            nodes.add(link.data.predicate)
            nodes.add(link.data.target)
        }

        for(let node of nodes) {
            //node.replace('\n', '\n\c')
            try {
                let ref = parseExprUrl(node)
                let lang
                if(!ref.language.name)
                    lang = await this.#languageController?.languageByRef(ref.language)
                else 
                    lang = ref.language

                langAddrs.push(`languageAddress("${node}", "${ref.language.address}").`)
                langNames.push(`languageName("${node}", "${lang!.name}").`)
                exprAddrs.push(`expressionAddress("${node}", "${ref.expression}").`)
            } catch(e) {
                console.debug("While creating expressionLanguageFacts:", e)
            }
        }

        lines = [...lines, ...langAddrs, ...langNames, ...exprAddrs]

        //-------------------
        // Social DNA zomes
        //-------------------

        for(let linkExpression of allLinks) {
            let link = linkExpression.data
            if(link.source == 'self' && link.predicate == 'ad4m://has_zome') {
                let code = Literal.fromUrl(link.target).get()
                lines.push(code)
            }
        }

        const factsCode = lines.join('\n')
        console.debug(factsCode)
        return factsCode
    }

    async spawnPrologEngine(): Promise<any> {
        if(this.#prologEngine) {
            await this.#prologEngine.close()
            this.#prologEngine = null
        }

        let error
        const prolog = new PrologInstance()

        try {
            const facts = await this.createPrologFacts()
            await prolog.consult(facts)
        } catch(e) {
            error = e
            prolog.close()
        } 

        if(error) throw error
        
        this.#prologNeedsRebuild = false
        this.#prologEngine = prolog
    }

    async prologQuery(query: string): Promise<any> {
        if(!this.#prologEngine || this.#prologNeedsRebuild)
            await this.spawnPrologEngine()
        
        return await this.#prologEngine!.query(query)
    }
}