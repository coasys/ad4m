import { Agent, Expression, Neighbourhood, LinkExpression, LinkExpressionInput, LinkInput, LanguageRef, PerspectiveHandle, Literal, PerspectiveDiff, parseExprUrl, Perspective as Ad4mPerspective, LinkMutations, LinkExpressionMutations } from "@perspect3vism/ad4m"
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
    createdFromJoin: boolean;
    isFastPolling: boolean;

    #db: any;
    #agent: AgentService;
    #languageController?: LanguageController
    #pubsub: any
    #config?: MainConfig;

    #prologEngine: PrologInstance|null
    #prologNeedsRebuild: boolean
    #pollingInterval: any;
    #prologMutex: Mutex

    constructor(id: PerspectiveHandle, context: PerspectiveContext, neighbourhood?: Neighbourhood, createdFromJoin?: boolean) {
        this.updateFromId(id)
        this.createdFromJoin = false;
        this.isFastPolling = false;

        if (neighbourhood) {
            this.neighbourhood = neighbourhood
            if (createdFromJoin) {
                this.createdFromJoin = createdFromJoin;
            };
        };

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

        // setup polling loop for Perspectives with a linkLanguage
        try {
            this.getCurrentRevision().then(revision => {
                // if revision is null, then we are not connected to the network yet, so we need to poll fast
                if(!revision && this.createdFromJoin) {
                    this.isFastPolling = true;
                    this.#pollingInterval = this.setupPolling(3000);
                } else {
                    this.#pollingInterval = this.setupPolling(30000);
                }
            })
        } catch (e) {
            console.error("Perspective.constructor(): Got error when trying to get current revision and setting polling loop", e);
        }

        this.#prologMutex = new Mutex()
    }

    setupPolling(intervalMs: number) {
        return setInterval(
            async () => {
                try {
                    const currentRevision = await this.getCurrentRevision();

                    //If we are fast polling (since we have not seen any changes) and we see changes, we can slow down the polling
                    if (this.isFastPolling && currentRevision) {
                        this.isFastPolling = false;
                        clearInterval(this.#pollingInterval);
                        this.#pollingInterval = this.setupPolling(30000);

                        //Lets also publish the links we have commited but not pushed since we were not connected
                        const mutations = this.#db.getPendingDiffs(this.uuid);
                        const batchedMutations = {
                            additions: [],
                            removals: []
                        } as PerspectiveDiff
                        for (const addition of mutations.additions) {
                            batchedMutations.additions.push(addition);
                        }
                        for (const removal of mutations.removals) {
                            batchedMutations.removals.push(removal);
                        }
                        await this.callLinksAdapter('commit', batchedMutations);
                    }

                    let links = await this.callLinksAdapter("pull");
                    if (links.additions && links.removals) {
                        if (links.additions.length > 0 || links.removals.length > 0) {
                            this.populateLocalLinks(links.additions, links.removals);
                            this.#prologNeedsRebuild = true
                            if (this.neighbourhood) {
                                this.#languageController?.callLinkObservers(links, {address: this.neighbourhood!.linkLanguage, name: ""});
                            }
                        }
                    }
                } catch (e) {
                    console.warn("Perspective.constructor(): Got error when trying to pull on linksAdapter", e);
                }
            },
            intervalMs
        );
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
                    console.debug(`NH [${this.sharedUrl}] (${this.name}): Calling linksAdapter.${functionName}(${JSON.stringify(args)})`)
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

    private getCurrentRevision(): Promise<string | null> {
        if(!this.neighbourhood || !this.neighbourhood.linkLanguage) {
            return Promise.resolve(null)
        }

        return new Promise(async (resolve, reject) => {
            setTimeout(() => reject(Error("LinkLanguage took to long to respond, timeout at 20000ms")), 20000);
            try {
                const address = this.neighbourhood!.linkLanguage;
                const linksAdapter = await this.#languageController?.getLinksAdapter({address} as LanguageRef);
                if(linksAdapter) {
                    let currentRevisionString = await linksAdapter.currentRevision();

                    if(!currentRevisionString) {
                        resolve(null)
                        return
                    }

                    if(typeof(currentRevisionString) != 'string') {
                        //@ts-ignore
                        currentRevisionString = currentRevisionString.toString()
                    }

                    currentRevisionString = currentRevisionString.trim()
                    if(currentRevisionString.length == 0) {
                        resolve(null)
                    } else {
                        resolve(currentRevisionString)
                    }

                } else {
                    console.error("LinksSharingLanguage", address, "set in perspective '"+this.name+"' not installed!")
                    // TODO: request install
                    resolve(null)
                }
            } catch(e) {
                console.error("Error while trying to call links adapter:", e)
                reject(e)
            }
        })
    }

    private async commit(diff: PerspectiveDiff): Promise<PerspectiveDiff | null> {
        if(!this.neighbourhood || !this.neighbourhood.linkLanguage) {
            return null;
        }

        let canCommit = false;
        if (!this.createdFromJoin){
            //If current agent created neighbourhood, then we should always be allowed to commit
            canCommit = true;
        } else {
            //We did not create the neighbourhood, so we should check if we already have some data sync'd before making a commit
            if (await this.getCurrentRevision()) {
                canCommit = true;
            }
        }

        if (canCommit) {
            //Call the links adapter to commit the diff
            const returnedDiff = await this.callLinksAdapter('commit', diff);
            return returnedDiff;
        } else {
            return null;
        }
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
        const linkExpression = this.ensureLinkExpression(link);
        const diff = {
            additions: [linkExpression],
            removals: []
        } as PerspectiveDiff
        const addLink = await this.commit(diff);

        if (!addLink) {
            this.#db.addPendingDiff(this.uuid, diff);
        }

        this.addLocalLink(linkExpression)
        this.#prologNeedsRebuild = true;
        this.#pubsub.publish(PubSub.LINK_ADDED_TOPIC, {
            perspective: this.plain(),
            link: linkExpression
        })

        return linkExpression
    }

    async addLinks(links: (LinkInput | LinkExpressionInput)[]): Promise<LinkExpression[]> {
        const linkExpressions = links.map(l => this.ensureLinkExpression(l));
        const diff = {
            additions: linkExpressions,
            removals: []
        } as PerspectiveDiff
        const addLinks = await this.commit(diff);

        if (!addLinks) {
            this.#db.addPendingDiff(this.uuid, diff);
        }

        linkExpressions.forEach(l => this.addLocalLink(l))
        this.#prologNeedsRebuild = true;
        for (const link of linkExpressions) {
            this.#pubsub.publish(PubSub.LINK_ADDED_TOPIC, {
                perspective: this.plain(),
                link: link
            })
        };

        return linkExpressions
    }

    async removeLinks(links: LinkInput[]): Promise<LinkExpression[]> {
        const linkExpressions = links.map(l => this.ensureLinkExpression(l));
        const diff = {
            additions: [],
            removals: linkExpressions
        } as PerspectiveDiff
        const removeLinks = await this.commit(diff);

        if (!removeLinks) {
            this.#db.addPendingDiff(this.uuid, diff);
        }

        linkExpressions.forEach(l => this.removeLocalLink(l))
        this.#prologNeedsRebuild = true;
        for (const link of linkExpressions) {
            this.#pubsub.publish(PubSub.LINK_ADDED_TOPIC, {
                perspective: this.plain(),
                link: link
            })
        };

        return linkExpressions
    }

    async linkMutations(mutations: LinkMutations): Promise<LinkExpressionMutations> {
        const diff = {
            additions: mutations.additions.map(l => this.ensureLinkExpression(l)),
            removals: mutations.removals.map(l => this.ensureLinkExpression(l))
        };
        const mutation = await this.commit(diff);

        if (!mutation) {
            this.#db.addPendingDiff(this.uuid, diff);
        };

        diff.additions.forEach(l => this.addLocalLink(l))
        diff.removals.forEach(l => this.removeLocalLink(l))
        this.#prologNeedsRebuild = true;
        for (const link of diff.additions) {
            this.#pubsub.publish(PubSub.LINK_ADDED_TOPIC, {
                perspective: this.plain(),
                link: link
            });
        };
        for (const link of diff.removals) {
            this.#pubsub.publish(PubSub.LINK_REMOVED_TOPIC, {
                perspective: this.plain(),
                link: link
            });
        };

        return diff;
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
        const addr = this.findLink(oldLink)
        if (!addr) {
            throw new Error(`Link not found in perspective "${this.plain()}": ${JSON.stringify(oldLink)}`)
        }

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

        const diff = {
            additions: [newLinkExpression],
            removals: [oldLink]
        } as PerspectiveDiff
        const mutation = await this.commit(diff);

        if (!mutation) {
            this.#db.addPendingDiff(this.uuid, diff);
        }

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

        const diff = {
            additions: [],
            removals: [linkExpression]
        } as PerspectiveDiff
        const mutation = await this.commit(diff);
        
        if (!mutation) {
            this.#db.addPendingDiff(this.uuid, diff);
        }

        this.#prologNeedsRebuild = true;
        this.#pubsub.publish(PubSub.LINK_REMOVED_TOPIC, {
            perspective: this.plain(),
            link: linkExpression
        })
    }

    private getLinksLocal(query: LinkQuery): LinkExpression[] {
        if(!query || !query.source && !query.predicate && !query.target) {
            //@ts-ignore
            return this.#db.getAllLinks(this.uuid).map(e => e.link)
        }

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
            result = limitFilter(result);
            return result
        }

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

        langAddrs.push(":- dynamic languageAddress/2.")
        langAddrs.push(":- discontiguous languageAddress/2.")
        langNames.push(":- dynamic languageName/2.")  
        langNames.push(":- discontiguous languageName/2.")  
        exprAddrs.push(":- dynamic expressionAddress/2.")  
        exprAddrs.push(":- discontiguous expressionAddress/2.")  

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
                //@ts-ignore
                if (!e.message.includes("Language not found by reference")) {
                    console.debug("While creating expressionLanguageFacts:", e)
                }
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
        // link/5
        //-------------------
        lines.push(":- dynamic triple/3.")
        lines.push(":- discontiguous triple/3.")
        lines.push(":- dynamic link/5.")
        lines.push(":- discontiguous link/5.")  

        for (const link of allLinks) {
            lines.push(this.tripleFact(link));
        }
        for (const link of allLinks) {
            lines.push(this.linkFact(link));
        };

        //-------------------
        // reachable/2
        //-------------------
        lines.push(":- dynamic reachable/2.")
        lines.push(":- discontiguous reachable/2.")  
        lines.push("reachable(A,B) :- triple(A,_,B).")
        lines.push("reachable(A,B) :- triple(A,_,X), reachable(X,B).")

        //-------------------
        // hiddenExpression/1
        //-------------------
        lines.push(":- dynamic hiddenExpression/1.")
        lines.push(":- discontiguous hiddenExpression/1.")



        lines = [...lines, ...await this.nodeFacts(allLinks)]

        //-------------------
        // Social DNA zomes
        //-------------------

        lines.push(":- dynamic register_sdna_flow/2.")
        lines.push(":- dynamic flowable/2.")
        lines.push(":- dynamic flow_state/3.")
        lines.push(":- dynamic start_action/2.")
        lines.push(":- dynamic action/4.")

        lines.push(":- discontiguous register_sdna_flow/2.")
        lines.push(":- discontiguous flowable/2.")
        lines.push(":- discontiguous flow_state/3.")
        lines.push(":- discontiguous start_action/2.")
        lines.push(":- discontiguous action/4.")

        lines.push(":- dynamic subject_class/2.")
        lines.push(":- dynamic constructor/2.")
        lines.push(":- dynamic instance/2.")
        lines.push(":- dynamic property/2.")
        lines.push(":- dynamic property_getter/4.")
        lines.push(":- dynamic property_setter/3.")
        lines.push(":- dynamic collection_getter/4.")
        lines.push(":- dynamic collection_setter/3.")

        lines.push(":- discontiguous subject_class/2.")
        lines.push(":- discontiguous constructor/2.")
        lines.push(":- discontiguous instance/2.")
        lines.push(":- discontiguous property/2.")
        lines.push(":- discontiguous property_getter/4.")
        lines.push(":- discontiguous property_setter/3.")
        lines.push(":- discontiguous collection_getter/4.")
        lines.push(":- discontiguous collection_setter/3.")

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