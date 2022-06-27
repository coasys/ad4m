import Perspective from './Perspective'
import type PerspectiveContext from './PerspectiveContext'
import { PerspectivismDb } from './db'
import { v4 as uuidv4 } from 'uuid'
import { Neighbourhood, LinkQuery, PerspectiveHandle, LinkInput } from '@perspect3vism/ad4m'
import { Perspective as Ad4mPerspective, LinkExpression } from '@perspect3vism/ad4m'
import Memory from 'lowdb/adapters/Memory'
import { createLink } from '../testutils/links'
import { createMockExpression } from '../testutils/expression'


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
    })
}


describe('Perspective', () => {
    let perspective: Perspective | undefined
    let allLinks: LinkExpression[] | undefined

    beforeEach(() => {
        const db = new PerspectivismDb(new Memory(""))
        perspective = new Perspective(
            {
                uuid: uuidv4(),
                name: "Test Perspective",
                sharedUrl: undefined
            } as PerspectiveHandle,
            // @ts-ignore
            {
                agentService,
                db,
                languageController
            } as PerspectiveContext)
        allLinks = []
    })

    it('wraps links in expressions on addLink', () => {
        const link = createLink()
        const expression = perspective!.addLink(link)
        expect(expression.author).toEqual(agentService.agent.did)
        expect(expression.data).toEqual(link)
        expect(agentService.createSignedExpression.mock.calls.length).toBe(1)
        expect(agentService.createSignedExpression.mock.calls[0][0]).toEqual(link)
    })

    describe('after adding 5 links', () => {
        beforeEach(() => {
            for(let i=0; i<5; i++) {
                const link = createLink()
                if(i%2 === 0) {
                    link.source = 'root'
                }
                //@ts-ignore
                allLinks!.push(link)
                perspective!.addLink(link)
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
                        [expect.objectContaining({data: allLinks![i]})]
                    )
                )
            }
        })

        it('can get links by source', async () => {
            const result = await perspective!.getLinks({source: 'root'} as LinkQuery)
            expect(result.length).toEqual(3)
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

        it('calls link language on getLinks() with the query once', async () => {
            const query = {source: 'root'} as LinkQuery;
            await perspective!.getLinks(query)

            expect(linksAdapter.pull.mock.calls.length).toBe(1)
            expect(linksAdapter.commit.mock.calls.length).toBe(0)
        })

        it('calls link language on addLink() with link expression once', async () => {
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

        it('calls link language on updateLink() with link expression once', async () => {
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

        it('calls link language on removeLink() with link expression once', async () => {
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
            it('adds all missing links from local DB to linksAdapter', async () => {
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








