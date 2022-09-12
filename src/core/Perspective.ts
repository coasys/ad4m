import { Agent, Expression, Neighbourhood, LinkExpression, LinkExpressionInput, LinkInput, LanguageRef, PerspectiveHandle, Literal, PerspectiveDiff, parseExprUrl, Perspective as Ad4mPerspective } from "@perspect3vism/ad4m"
import { Link, linkEqual, LinkQuery } from "@perspect3vism/ad4m";
import { SHA3 } from "sha3";
import type AgentService from "./agent/AgentService";
import type LanguageController from "./LanguageController";
import * as PubSub from './graphQL-interface/PubSub'
import type PerspectiveContext from "./PerspectiveContext"
import PrologInstance from "./PrologInstance";
import { MainConfig } from "./Config";
import { Mutex } from 'async-mutex'

type PerspectiveSubscription = {
    perspective: PerspectiveHandle,
    link: LinkExpression
}

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
    #config?: MainConfig;

    #prologEngine: PrologInstance|null
    #prologNeedsRebuild: boolean
    #pollingInterval: any;
    #prologMutex: Mutex

    constructor(id: PerspectiveHandle, context: PerspectiveContext, neighbourhood?: Neighbourhood) {
        this.updateFromId(id)
        if (neighbourhood) this.neighbourhood = neighbourhood;

        this.#db = context.db
        this.#agent = context.agentService!
        this.#languageController = context.languageController!
        this.#config = context.config;

        this.#pubsub = PubSub.get()
        this.#prologEngine = null
        this.#prologNeedsRebuild = true

        this.#pubsub.subscribe(PubSub.LINK_ADDED_TOPIC, ({ perspective }: PerspectiveSubscription) => {
            if (perspective.uuid === this.uuid) {
                this.#prologNeedsRebuild = true
            }
        })

        this.#pubsub.subscribe(PubSub.LINK_REMOVED_TOPIC, ({ perspective }: PerspectiveSubscription) => {
            if (perspective.uuid === this.uuid) {
                this.#prologNeedsRebuild = true
            }
        })

        const that = this

        process.on("SIGINT", () => {
            that.#prologEngine?.close()
            clearInterval(this.#pollingInterval);
        });

        this.callLinksAdapter("pull").then((remoteLinks) => {
            if (remoteLinks.additions && remoteLinks.removals) {
                this.populateLocalLinks(remoteLinks.additions, remoteLinks.removals);
                this.#prologNeedsRebuild = true
                if (this.neighbourhood) {
                    this.#languageController?.callLinkObservers(remoteLinks, {address: this.neighbourhood!.linkLanguage, name: ""});
                }
            }
        });

        // setup polling loop for Perspectives with a linkLanguage
        this.#pollingInterval = setInterval(
            async () => {
                let links = await this.callLinksAdapter("pull");
                if (links.additions && links.removals) {
                    this.populateLocalLinks(links.additions, links.removals);
                    this.#prologNeedsRebuild = true
                    if (this.neighbourhood) {
                        this.#languageController?.callLinkObservers(links, {address: this.neighbourhood!.linkLanguage, name: ""});
                    }
                }
            },
            20000
        );
        this.#prologMutex = new Mutex()
    }


    plain(): PerspectiveHandle {
        const { name, uuid, author, timestamp, sharedUrl, neighbourhood } = this
        return JSON.parse(JSON.stringify({
            name, uuid, author, timestamp, sharedUrl, neighbourhood
        }))
    }

    updateFromId(id: PerspectiveHandle) {
        this.name = id.name
        this.uuid = id.uuid
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

    private renderLinksAdapter(): Promise<Ad4mPerspective> {
        if(!this.neighbourhood || !this.neighbourhood.linkLanguage) {
            //console.warn("Perspective.callLinksAdapter: Did not find neighbourhood or linkLanguage for neighbourhood on perspective, returning empty array")
            return Promise.resolve(new Ad4mPerspective([]))
        }
        return new Promise(async (resolve, reject) => {
            setTimeout(() => reject(Error("LinkLanguage took to long to respond, timeout at 20000ms")), 20000)
            try {
                const address = this.neighbourhood!.linkLanguage;
                const linksAdapter = await this.#languageController!.getLinksAdapter({address} as LanguageRef);
                if(linksAdapter) {
                    //console.debug(`Calling linksAdapter.${functionName}(${JSON.stringify(args)})`)
                    const result = await linksAdapter.render();
                    //console.debug("Got result:", result)
                    resolve(result)
                } else {
                    console.error("LinksSharingLanguage", address, "set in perspective '"+this.name+"' not installed!")
                    // TODO: request install
                    resolve(new Ad4mPerspective([]))
                }
            } catch(e) {
                console.error("Error while trying to call links adapter:", e)
                reject(e)
            }
        })
    }

    //@ts-ignore
    private callLinksAdapter(functionName: string, ...args): Promise<PerspectiveDiff> {
        if(!this.neighbourhood || !this.neighbourhood.linkLanguage) {
            //console.warn("Perspective.callLinksAdapter: Did not find neighbourhood or linkLanguage for neighbourhood on perspective, returning empty array")
            return Promise.resolve({
                additions: [],
                removals: []
            })
        }

        return new Promise(async (resolve, reject) => {
            setTimeout(() => reject(Error("LinkLanguage took to long to respond, timeout at 20000ms")), 20000)
            try {
                const address = this.neighbourhood!.linkLanguage;
                const linksAdapter = await this.#languageController?.getLinksAdapter({address} as LanguageRef);
                if(linksAdapter) {
                    // console.debug(`Calling linksAdapter.${functionName}(${JSON.stringify(args)})`)
                    //@ts-ignore
                    const result = await linksAdapter[functionName](...args)
                    //console.debug("Got result:", result)
                    resolve(result)
                } else {
                    console.error("LinksSharingLanguage", address, "set in perspective '"+this.name+"' not installed!")
                    // TODO: request install
                    resolve({
                        additions: [],
                        removals: []
                    })
                }
            } catch(e) {
                console.error("Error while trying to call links adapter:", e)
                reject(e)
            }
        })
    }

    async syncWithSharingAdapter() {
        //@ts-ignore
        const localLinks = this.#db.getAllLinks(this.uuid).map(l => l.link);
        const remoteLinks = await this.renderLinksAdapter()
        const includes = (link: LinkExpression, list: LinkExpression[]) => {
            return undefined !== list.find(e =>
                JSON.stringify(e.author) === JSON.stringify(link.author) &&
                e.timestamp === link.timestamp &&
                e.source === link.data.source &&
                e.target === link.data.target &&
                e.predicate === link.data.predicate
                )
        }
        let linksToCommit = [];
        for(const l of localLinks) {
            if(!includes(l, remoteLinks.links)) {
                linksToCommit.push(l);
            }
        }
        if (linksToCommit.length > 0) {
            await this.callLinksAdapter("commit", {additions: linksToCommit, removals: []})
        }

        for(const l of remoteLinks.links) {
            if(!includes(l, localLinks)) {
                this.addLocalLink(l);
            }
        }
    }

    addLocalLink(linkExpression: LinkExpression) {
        let foundLink = this.findLink(linkExpression);
        if (!foundLink) {
            const hash = new SHA3(256);
            hash.update(JSON.stringify(linkExpression));
            const addr = hash.digest('hex');
    
            let link = linkExpression.data as Link
    
            this.#db.storeLink(this.uuid, linkExpression, addr)
            this.#db.attachSource(this.uuid, link.source, addr)
            this.#db.attachTarget(this.uuid, link.target, addr)
        }
    }

    removeLocalLink(linkExpression: LinkExpression) {
        let foundLink = this.findLink(linkExpression);
        if (foundLink) {
            let link = linkExpression.data as Link
            this.#db.removeSource(this.uuid, link.source, foundLink!)
            this.#db.removeTarget(this.uuid, link.target, foundLink!)
            this.#db.remove(this.#db.allLinksKey(this.uuid), foundLink!)
        }
    }

    async addLink(link: LinkInput | LinkExpressionInput): Promise<LinkExpression> {
        const linkExpression = this.ensureLinkExpression(link)
        this.callLinksAdapter('commit', {
            additions: [linkExpression],
            removals: []
        } as PerspectiveDiff)

        this.addLocalLink(linkExpression)
        this.#prologNeedsRebuild = true;
        this.#pubsub.publish(PubSub.LINK_ADDED_TOPIC, {
            perspective: this.plain(),
            link: linkExpression
        })

        return linkExpression
    }

    private findLink(linkToFind: LinkExpressionInput): string | undefined {
        const allLinks = this.#db.getAllLinks(this.uuid)
        for(const {name, link} of allLinks) {
            if(linkEqual(linkToFind, link)) {
                return name
            }
        }
    }

    async updateLink(oldLink: LinkExpressionInput, newLink: LinkInput) {
        //console.debug("LINK REPO: updating link:", oldLink, newLink)
        const addr = this.findLink(oldLink)
        if (!addr) {
            throw new Error(`Link not found in perspective "${this.plain()}": ${JSON.stringify(oldLink)}`)
        }
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

        this.callLinksAdapter('commit', {
            additions: [newLinkExpression],
            removals: [oldLink]
        } as PerspectiveDiff)
        const perspective = this.plain();
        this.#prologNeedsRebuild = true;
        this.#pubsub.publish(PubSub.LINK_REMOVED_TOPIC, {
            perspective: perspective,
            link: oldLink
        })
        this.#pubsub.publish(PubSub.LINK_ADDED_TOPIC, {
            perspective: perspective,
            link: newLinkExpression
        })

        return newLinkExpression
    }

    async removeLink(linkExpression: LinkExpressionInput) {
        this.removeLocalLink(linkExpression);
        this.callLinksAdapter('commit',  {
            additions: [],
            removals: [linkExpression]
        } as PerspectiveDiff);
        this.#prologNeedsRebuild = true;
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

        const reverse = query.fromDate! >= query.untilDate!;

        function fromDateFilter(link: LinkExpression) {
            if (reverse) {
                return new Date(link.timestamp) <= query.fromDate!
            } else {
                return new Date(link.timestamp) >= query.fromDate!
            }
        }

        function untilDateFilter(link: LinkExpression) {
            if (reverse) {
                return new Date(link.timestamp) >= query.untilDate!
            } else {
                return new Date(link.timestamp) <= query.untilDate!
            }
        }

        function limitFilter(results: LinkExpression[]) {
            if (query.limit) {
                const startLimit = reverse ? results.length - query.limit : 0;
                const endLimit = reverse ? (results.length - query.limit) + query.limit : query.limit;
                return results.slice(startLimit, endLimit)
            }

            return results;
        }

        if(query.source) {
            // console.debug("query.source", query.source)
            //@ts-ignore
            let result = this.#db.getLinksBySource(this.uuid, query.source).map(e => e.link)
            // @ts-ignore
            if(query.target) result = result.filter(l => l.data.target === query.target)
            // @ts-ignore
            if(query.predicate) result = result.filter(l => l.data.predicate === query.predicate)
            //@ts-ignore
            if (query.fromDate) result = result.filter(fromDateFilter)
            // @ts-ignore
            if (query.untilDate) result = result.filter(untilDateFilter)
            // console.debug("result", result)
            result = limitFilter(result);
            return result
        }

        // console.debug("getLinks 3")

        if(query.target) {
            //@ts-ignore
            let result = this.#db.getLinksByTarget(this.uuid, query.target).map(e => e.link)
            // @ts-ignore
            if(query.predicate) result = result.filter(l => l.data.predicate === query.predicate)
            //@ts-ignore
            if (query.fromDate) result = result.filter(fromDateFilter)
            //@ts-ignore
            if (query.untilDate) result = result.filter(untilDateFilter)
            result = limitFilter(result);
            return result
        }

        // console.debug("getLinks 4")

        //@ts-ignore
        let result = this.#db.getAllLinks(this.uuid).map(e => e.link).filter(link => link.data.predicate === query.predicate)
        if (query.limit) result = result.slice(0, query.limit)
        return result
    }

    async populateLocalLinks(additions: LinkExpression[], removals: LinkExpression[]) {
        if (additions) {
            additions.forEach((link) => {
                this.addLocalLink(link)
            })
        }

        if (removals) {
            removals.forEach((link) => {
                this.removeLocalLink(link);
            })
        }
    }

    async getLinks(query: LinkQuery): Promise<LinkExpression[]> {
        const remoteLinks = await this.callLinksAdapter('pull')
        if (remoteLinks.additions && remoteLinks.removals) {
            this.#prologNeedsRebuild = true;
            this.populateLocalLinks(remoteLinks.additions, remoteLinks.removals);   
        }

        // console.debug("getLinks local...")
        const links = await this.getLinksLocal(query)

        const reverse = query.fromDate! >= query.untilDate!;

        let values = Object.values(links).sort((a, b) => {
            return new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime();
        });


        if (query.limit) {
            const startLimit = reverse ? values.length - query.limit : 0;
            const endLimit = reverse ? (values.length - query.limit) + query.limit : query.limit;
            values = values.slice(startLimit, endLimit)
        }

        return values;
    }

    tripleFact(l: LinkExpression): string {
        return `triple("${l.data.source}", "${l.data.predicate}", "${l.data.target}").`
    }

    linkFact(l: LinkExpression): string {
        return `link("${l.data.source}", "${l.data.predicate}", "${l.data.target}", ${new Date(l.timestamp).getTime()}, "${l.author}").`
    }

    async nodeFacts(allLinks: LinkExpression[]): Promise<string[]> {
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
            if(link.data.source) nodes.add(link.data.source)
            if(link.data.predicate) nodes.add(link.data.predicate)
            if(link.data.target) nodes.add(link.data.target)
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

        return [...langAddrs, ...langNames, ...exprAddrs]
    }

    async addLinkToProlog(link: LinkExpression) {
        this.#prologNeedsRebuild = true
        return

        // Leaving this dead code here to potentially get reactivated in the future.
        // This doesn't work because consult/1 is more intelligent than I had expected,
        // it first removes all old facts&rules to any predicate that it finds again when
        // consulting. So we can just use consult/1 below in `prologQuery` to update the
        // state with no problem.
        // This is fine for now, but if our Prolog program becomes very large because of a
        // large perspective, we might not want to always recreate the whole program file
        // and instead use a different Prolog command to load our changes. Then we might
        // want to surgically only alter the affected links like attempted here...
        //
        //
        //if(this.isSDNALink(link)) {
        //    this.#prologNeedsRebuild = true
        //} else {
        //    let lines = [this.tripleFact(link), ...await this.nodeFacts([link])]
        //    await this.#prologEngine?.consult(lines.join('\n'))
        //}
    }

    async removeLinkFromProlog(link: LinkExpression) {
        this.#prologNeedsRebuild = true
        return

        // See above.
        //
        //if(this.isSDNALink(link)) {
        //    this.#prologNeedsRebuild = true
        //} else {
        //    const fact = this.tripleFact(link)
        //    const factWithoutDot = fact.substring(0, fact.length-1)
        //    await this.#prologEngine?.consult(`retract(${factWithoutDot}).`)
        //}
    }

    isSDNALink(link: LinkExpression): boolean {
        return link.source == 'ad4m://self' && link.predicate == 'ad4m://has_zome'
    }

    async initEngineFacts(): Promise<string> {
        let lines = []

        const allLinks = await this.getLinks(new LinkQuery({}))
        //-------------------
        // triple/3
        //-------------------
        lines.push(":- discontiguous triple/3.")            

        for (const link of allLinks) {
            lines.push(this.tripleFact(link));
            lines.push(this.linkFact(link));
        };

        //-------------------
        // reachable/2
        //-------------------
        lines.push("reachable(A,B) :- triple(A,_,B).")
        lines.push("reachable(A,B) :- triple(A,_,X), reachable(X,B).")

        //-------------------
        // hiddenExpression/1
        //-------------------
        lines.push(":- discontiguous hiddenExpression/1.")            



        lines = [...lines, ...await this.nodeFacts(allLinks)]

        //-------------------
        // Social DNA zomes
        //-------------------

        for(let linkExpression of allLinks) {
            let link = linkExpression.data
            if(this.isSDNALink(link)) {
                try {
                    let code = Literal.fromUrl(link.target).get()
                    lines.push(code)
                } catch {
                    console.error("Perspective.initEngineFacts: Error loading SocialDNA link target as literal... Ignoring SocialDNA link.");
                }
            }
        }

        const factsCode = lines.join('\n')
        return factsCode
    }

    async spawnPrologEngine(): Promise<any> {
        if(this.#prologEngine) {
            await this.#prologEngine.close()
            this.#prologEngine = null
        }

        let error
        const prolog = new PrologInstance(this.#config!)

        try {
            const facts = await this.initEngineFacts()
            await prolog.consult(facts)
        } catch(e) {
            error = e
            prolog.close()
        }

        if(error) throw error
        this.#prologEngine = prolog
    }

    async prologQuery(query: string): Promise<any> {
        await this.#prologMutex.runExclusive(async () => {
            if(!this.#prologEngine) {
                await this.spawnPrologEngine()
                this.#prologNeedsRebuild = false
            }
            if(this.#prologNeedsRebuild) {
                console.log("Perspective.prologQuery: Making prolog query but first rebuilding facts");
                this.#prologNeedsRebuild = false
                const facts = await this.initEngineFacts()
                await this.#prologEngine!.consult(facts)
            }
        })
        
        return await this.#prologEngine!.query(query)
    }

    clearPolling() {
        clearInterval(this.#pollingInterval);
    }

    closePrologEngine() {
        if(this.#prologEngine)
            this.#prologEngine.close()
    }
}