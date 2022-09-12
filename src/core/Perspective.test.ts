import Perspective from './Perspective'
import type PerspectiveContext from './PerspectiveContext'
import { PerspectivismDb } from './db'
import { v4 as uuidv4 } from 'uuid'
import { Neighbourhood, LinkQuery, PerspectiveHandle, LinkInput } from '@perspect3vism/ad4m'
import { Perspective as Ad4mPerspective, LinkExpression } from '@perspect3vism/ad4m'
import Memory from 'lowdb/adapters/Memory'
import { createLink } from '../testutils/links'
import { createMockExpression } from '../testutils/expression'
import { MainConfig } from './Config'
import path from "path";
import sleep from '../tests/sleep'


const did = 'did:local-test-agent'
const agentService = {
    did,
    createSignedExpression: jest.fn(createMockExpression.bind(null, did)),
    agent: { did }
}

const sharingLanguage = "sharing-language-address";

function LinksAdapter() {
    //@ts-ignore
    this.pull = jest.fn(args=>{return []})
    //@ts-ignore
    this.commit = jest.fn(args=>{return "string"})
    //@ts-ignore
    this.render = jest.fn(args=>{return {links: []}})
}

//@ts-ignore
let linksAdapter = new LinksAdapter()

const languageController = {
    getLinksAdapter: jest.fn(langRef => {
        if(langRef.address === sharingLanguage)
            return linksAdapter
        else
            return null
    }),
    languageByRef: jest.fn(ref => ref)
}


describe('Perspective', () => {
    let perspective: Perspective | undefined
    let allLinks: LinkExpression[] | undefined

    beforeEach(() => {
        const TEST_DIR = `${__dirname}/../test-temp`
        const appDataPath = path.join(TEST_DIR, 'agents', 'alice')
        const db = new PerspectivismDb(new Memory(""))
        perspective = new Perspective(
            {
                uuid: uuidv4(),
                name: "Test Perspective",
                sharedUrl: undefined
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
        expect(expression.author).toEqual(agentService.agent.did)
        expect(expression.data).toEqual(link)
        expect(agentService.createSignedExpression.mock.calls.length).toBe(1)
        expect(agentService.createSignedExpression.mock.calls[0][0]).toEqual(link)
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

        it('has asked agent service for 5 signatures', () => {
            expect(agentService.createSignedExpression.mock.calls.length).toBe(6)
        })

        it('can get all links', async () => {
            const result = await perspective!.getLinks({} as LinkQuery)

            expect(result.length).toEqual(5)

            for(let i=0; i<5; i++) {
                expect(result).toEqual(
                    expect.arrayContaining(
                        [expect.objectContaining(allLinks![i])]
                    )
                )
            }
        })

        it('can get links by source', async () => {
            const result = await perspective!.getLinks({source: 'ad4m://self'} as LinkQuery)
            expect(result.length).toEqual(3)
        })

        it('Prolog queries return 5 triples', async () => {
            const result = await perspective!.prologQuery("triple(X,Y,Z).")
            expect(result.length).toEqual(5)
        })

        it('Prolog gets correctly updated when removing links', async () => {
            const link = allLinks![0]
            console.log("LINK TO REMOVE", link)
            await perspective!.removeLink(link)
            const result = await perspective!.prologQuery("triple(X,Y,Z)");
            expect(result.length).toEqual(4)
        })
    })

    describe('Prolog Engine', () => {
        it('answers correctly in a run with multiple link additions/removals', async () => {
            let result 
            let linkResult
            let l1 = await perspective!.addLink({source: 'ad4m://self', target: 'ad4m://test1'})

            result = await perspective!.prologQuery("triple(Source,Pred,Target)")
            expect(result.length).toEqual(1)
            expect(result[0].Source).toBe('ad4m://self')
            expect(result[0].Target).toBe('ad4m://test1')

            linkResult = await perspective!.prologQuery("link(Source,Pred,Target,Timestamp,Author)")
            expect(linkResult.length).toEqual(1)
            expect(linkResult[0].Source).toBe('ad4m://self')
            expect(linkResult[0].Target).toBe('ad4m://test1')
            expect(linkResult[0].Author).toBe("did:local-test-agent")
            expect(linkResult[0].Timestamp).not.toBeNaN();

            let l2 = await perspective!.addLink({source: 'ad4m://self', target: 'ad4m://test2'})
            result = await perspective!.prologQuery("triple(Source,Pred,Target)")
            expect(result.length).toEqual(2)
            linkResult = await perspective!.prologQuery("link(Source,Pred,Target,Timestamp,Author)")
            expect(linkResult.length).toEqual(2)

            let targetSet = new Set<string>()
            targetSet.add(result[0].Target)
            targetSet.add(result[1].Target)

            expect(result[1].Source).toBe('ad4m://self')
            expect(targetSet.has('ad4m://test1')).toBeTruthy()
            expect(targetSet.has('ad4m://test2')).toBeTruthy()

            //...TBC
        })
    })

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

        it('calls pull on link language on getLinks() with the query once', async () => {
            const query = {source: 'root'} as LinkQuery;
            await perspective!.getLinks(query)

            expect(linksAdapter.pull.mock.calls.length).toBe(1)
            expect(linksAdapter.commit.mock.calls.length).toBe(0)
        })

        it('calls commit on link language on addLink() with link expression once', async () => {
            const link = createLink()
            const linkExpression = await perspective!.addLink(link)

            expect(linksAdapter.commit.mock.calls.length).toBe(1)
            expect(linksAdapter.commit.mock.calls[0][0]).toEqual({
                additions: [linkExpression],
                removals: []
            })

            expect(linksAdapter.pull.mock.calls.length).toBe(0)
            expect(linksAdapter.render.mock.calls.length).toBe(0)
        })

        it('calls commit on link language on updateLink() with link expression once', async () => {
            const link1 = createLink()
            const link2 = createLink()

            const link1Expression = await perspective!.addLink(link1)
            const link2Expression = perspective!.ensureLinkExpression(link2)
            //@ts-ignore
            await perspective!.updateLink(link1Expression, link2Expression)

            expect(linksAdapter.commit.mock.calls.length).toBe(2)
            expect(linksAdapter.commit.mock.calls[0][0]).toEqual({
                additions: [link1Expression],
                removals: []
            })
            expect(linksAdapter.commit.mock.calls[1][0]).toEqual({
                additions: [link2Expression],
                removals: [link1Expression]
            })

            expect(linksAdapter.pull.mock.calls.length).toBe(0)
        })

        it('calls commit on link language on removeLink() with link expression once', async () => {
            const link = createLink()

            const linkExpression = await perspective!.addLink(link)
            await perspective!.removeLink(linkExpression)

            expect(linksAdapter.commit.mock.calls.length).toBe(2)
            expect(linksAdapter.commit.mock.calls[0][0]).toEqual({
                additions: [linkExpression],
                removals: []
            })
            expect(linksAdapter.commit.mock.calls[1][0]).toEqual({
                additions: [],
                removals: [linkExpression]
            })

            expect(linksAdapter.pull.mock.calls.length).toBe(0)
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

                expect(linksAdapter.commit.mock.calls.length).toBe(1)
                expect(linksAdapter.commit.mock.calls[0][0]).toEqual({
                    additions: [linkExpression],
                    removals: []
                })
                expect(linksAdapter.render.mock.calls.length).toBe(1)
            })
        })
        
    })

})








