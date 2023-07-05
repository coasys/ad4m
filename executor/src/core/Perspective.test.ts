import { v4 as uuidv4 } from 'uuid';
import { Neighbourhood, LinkQuery, PerspectiveHandle, PerspectiveState } from '@perspect3vism/ad4m'
import { Perspective as Ad4mPerspective, LinkExpression } from '@perspect3vism/ad4m'
import path from "path";
import { fileURLToPath } from 'url';
import { expect } from "chai";
import * as sinon from "sinon";

import Perspective from './Perspective'
import type PerspectiveContext from './PerspectiveContext'
import { PerspectivismDb } from './db'
import { createLink } from '../testutils/links'
import { createMockExpression } from '../testutils/expression'
import { MainConfig } from './Config'

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const did = 'did:local-test-agent'
const agentService = {
    did,
    createSignedExpression: sinon.fake(createMockExpression.bind(null, did)),
    agent: { did }
}

const sharingLanguage = "sharing-language-address";

function LinksAdapter() {
    //@ts-ignore
    this.pull = sinon.fake(args=>{return []})
    //@ts-ignore
    this.commit = sinon.fake(args=>{return "string"})
    //@ts-ignore
    this.render = sinon.fake(args=>{return {links: []}})
}

//@ts-ignore
let linksAdapter = new LinksAdapter()

const languageController = {
    getLinksAdapter: sinon.fake(langRef => {
        if(langRef.address === sharingLanguage)
            return linksAdapter
        else
            return null
    }),
    languageByRef: sinon.fake(ref => ref)
}


describe('Perspective', () => {
    let perspective: Perspective | undefined
    let allLinks: LinkExpression[] | undefined

    beforeEach(() => {
        const TEST_DIR = `${__dirname}/../tst-tmp`
        const appDataPath = path.join(TEST_DIR, 'agents', 'alice')
        const db = new PerspectivismDb();
        perspective = new Perspective(
            {
                uuid: uuidv4(),
                name: "Test Perspective",
                sharedUrl: undefined,
                state: PerspectiveState.Private
            } as PerspectiveHandle,
            //@ts-ignore
            {
                agentService,
                db,
                languageController,
                config: new MainConfig(TEST_DIR, appDataPath)
            } as PerspectiveContext)
        allLinks = []
    })

    afterEach(() => {
        perspective?.clearPolling();
    })

    it('wraps links in expressions on addLink', async () => {
        const link = createLink()
        const expression = await perspective!.addLink(link)
        expect(expression.author).to.be.equal(agentService.agent.did)
        expect(expression.data).to.be.equal(link)
        expect(agentService.createSignedExpression.calledOnce).to.be.true;
        expect(agentService.createSignedExpression.getCall(0).args[0]).to.deep.equal(link)
    })

    describe('after adding 5 links', () => {
        beforeEach(async () => {
            for(let i=0; i<5; i++) {
                const link = createLink()
                if(i%2 === 0) {
                    link.source = 'ad4m://self'
                }
                //@ts-ignore
                allLinks!.push(await perspective!.addLink(link))
                
            }
        })

        it('has asked agent service for 6 signatures', () => {
            expect(agentService.createSignedExpression.callCount).to.be.equal(6)
        })

        it('can get all links', async () => {
            const result = await perspective!.getLinks({} as LinkQuery)

            expect(result.length).to.be.equal(5)
            expect(result).to.have.deep.members(allLinks!);
        })

        it('can get links by source', async () => {
            const result = await perspective!.getLinks({source: 'ad4m://self'} as LinkQuery)
            expect(result.length).to.be.equal(3)
        })

        // it('Prolog queries return 5 triples', async () => {
        //     const result = await perspective!.prologQuery("triple(X,Y,Z).")
        //     expect(result.length).to.be.equal(5)
        // })

        // it('Prolog gets correctly updated when removing links', async () => {
        //     const link = allLinks![0]
        //     console.log("LINK TO REMOVE", link)
        //     await perspective!.removeLink(link)
        //     const result = await perspective!.prologQuery("triple(X,Y,Z)");
        //     expect(result.length).to.be.equal(4)
        // })
    })

    // describe('Prolog Engine', () => {
    //     it('answers correctly in a run with multiple link additions/removals', async () => {
    //         let result 
    //         let linkResult
    //         let l1 = await perspective!.addLink({source: 'ad4m://self', target: 'ad4m://test1'})

    //         result = await perspective!.prologQuery("triple(Source,Pred,Target)")
    //         expect(result.length).to.be.equal(1)
    //         expect(result[0].Source).to.be.equal('ad4m://self')
    //         expect(result[0].Target).to.be.equal('ad4m://test1')

    //         linkResult = await perspective!.prologQuery("link(Source,Pred,Target,Timestamp,Author)")
    //         expect(linkResult.length).to.be.equal(1)
    //         expect(linkResult[0].Source).to.be.equal('ad4m://self')
    //         expect(linkResult[0].Target).to.be.equal('ad4m://test1')
    //         expect(linkResult[0].Author).to.be.equal("did:local-test-agent")
    //         expect(linkResult[0].Timestamp).not.to.be.NaN;

    //         let l2 = await perspective!.addLink({source: 'ad4m://self', target: 'ad4m://test2'})
    //         result = await perspective!.prologQuery("triple(Source,Pred,Target)")
    //         expect(result.length).to.be.equal(2)
    //         linkResult = await perspective!.prologQuery("link(Source,Pred,Target,Timestamp,Author)")
    //         expect(linkResult.length).to.be.equal(2)

    //         let targetSet = new Set<string>()
    //         targetSet.add(result[0].Target)
    //         targetSet.add(result[1].Target)

    //         expect(result[1].Source).to.be.equal('ad4m://self')
    //         expect(targetSet.has('ad4m://test1')).to.be.true;
    //         expect(targetSet.has('ad4m://test2')).to.be.true;

    //         //...TBC
    //     })
    // })

    describe('with link sharing language', () => {
        beforeEach(() => {
            perspective!.neighbourhood = {
                linkLanguage: sharingLanguage,
                perspective: new Ad4mPerspective([]),
                meta: new Ad4mPerspective()
            } as Neighbourhood
            //@ts-ignore
            linksAdapter = new LinksAdapter()
        })

        it('calls commit on link language on addLink() with link expression once', async () => {
            const link = createLink()
            const linkExpression = await perspective!.addLink(link)

            expect(linksAdapter.commit.calledOnce).to.be.true;
            expect(linksAdapter.commit.getCall(0).args[0]).to.be.eql({
                additions: [linkExpression],
                removals: []
            })

            expect(linksAdapter.pull.callCount).to.be.equal(0)
            expect(linksAdapter.render.callCount).to.be.equal(0)
        })

        it('calls commit on link language on updateLink() with link expression once', async () => {
            const link1 = createLink()
            const link2 = createLink()

            const link1Expression = await perspective!.addLink(link1)
            const link2Expression = perspective!.ensureLinkExpression(link2)
            //@ts-ignore
            await perspective!.updateLink(link1Expression, link2Expression)

            expect(linksAdapter.commit.calledTwice).to.be.true;
            expect(linksAdapter.commit.getCall(0).args[0]).to.be.eql({
                additions: [link1Expression],
                removals: []
            })
            expect(linksAdapter.commit.getCall(1).args[0]).to.be.eql({
                additions: [link2Expression],
                removals: [link1Expression]
            })

            expect(linksAdapter.pull.callCount).to.be.equal(0)
        })

        it('calls commit on link language on removeLink() with link expression once', async () => {
            const link = createLink()

            const linkExpression = await perspective!.addLink(link)
            await perspective!.removeLink(linkExpression)

            expect(linksAdapter.commit.calledTwice).to.be.true;
            expect(linksAdapter.commit.getCall(0).args[0]).to.be.eql({
                additions: [linkExpression],
                removals: []
            })
            expect(linksAdapter.commit.getCall(1).args[0]).to.be.eql({
                additions: [],
                removals: [linkExpression]
            })

            expect(linksAdapter.pull.callCount).to.be.equal(0)
        })

        describe('syncWithSharingAdpater', () => {
            it('commits all missing links from local DB to linksAdapter', async () => {
                perspective!.neighbourhood = undefined
    
                const link = createLink()
                const linkExpression = await perspective!.addLink(link)
    
                perspective!.neighbourhood = {
                    linkLanguage: sharingLanguage,
                    perspective: new Ad4mPerspective([]),
                    meta: new Ad4mPerspective()
                } as Neighbourhood

                await perspective!.syncWithSharingAdapter()

                expect(linksAdapter.commit.calledOnce).to.be.true;
                expect(linksAdapter.commit.getCall(0).args[0]).to.be.eql({
                    additions: [linkExpression],
                    removals: []
                })
                expect(linksAdapter.render.calledOnce).to.be.true;
            })
        })
        
    })

})








