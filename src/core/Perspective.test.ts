import Perspective from './Perspective'
import type PerspectiveContext from './PerspectiveContext'
import { PerspectivismDb } from './db'
import { v4 as uuidv4 } from 'uuid'
import type { LanguageRef, Link, LinkQuery, PerspectiveHandle } from '@perspect3vism/ad4m'
import Memory from 'lowdb/adapters/Memory'
import { createLink } from '../testutils/links'
import { createMockExpression } from '../testutils/expression'


const did = 'did:local-test-agent'
const agentService = {
    did,
    createSignedExpression: jest.fn(createMockExpression.bind(null, did)),
    agent: { did }
}

const sharingLanguage = { 
    address: "sharing-language-address",
    name: "sharing-lanugage Name"
} as LanguageRef

function LinksAdapter() {
    this.getLinks = jest.fn(args=>{return []})
    this.addLink = jest.fn(args=>{return []})
    this.updateLink = jest.fn(args=>{return []})
    this.removeLink = jest.fn(args=>{return []})
}

let linksAdapter = new LinksAdapter()

const languageController = {
    getLinksAdapter: jest.fn(langRef => {
        if(langRef.address === sharingLanguage.address)
            return linksAdapter
        else
            return null
    })
}


describe('Perspective', () => {
    let perspective
    let allLinks

    beforeEach(() => {
        const db = new PerspectivismDb(new Memory())
        perspective = new Perspective(
            {
                uuid: uuidv4(),
                name: "Test Perspective"
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
        const expression = perspective.addLink(link)
        expect(expression.author).toEqual(agentService.agent)
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
                allLinks.push(link)
                perspective.addLink(link)
            }
        })

        it('has asked agent service for 5 signatures', () => {
            expect(agentService.createSignedExpression.mock.calls.length).toBe(6)
        })

        it('can get all links', async () => {
            const result = await perspective.getLinks({} as LinkQuery)

            expect(result.length).toEqual(5)

            for(let i=0; i<5; i++) {
                expect(result).toEqual(
                    expect.arrayContaining(
                        [expect.objectContaining({data: allLinks[i]})]
                    )
                )
            }
        })

        it('can get links by source', async () => {
            const result = await perspective.getLinks({source: 'root'} as LinkQuery)
            expect(result.length).toEqual(3)
        })
    })

    describe('with link sharing language', () => {
        beforeEach(() => {
            perspective.sharedPerspective = {
                linkLanguages: [sharingLanguage]
            }
            linksAdapter = new LinksAdapter()
        })

        it('calls link language on getLinks() with the query once', async () => {
            const query = {source: 'root'}
            await perspective.getLinks(query)

            expect(linksAdapter.getLinks.mock.calls.length).toBe(1)
            expect(linksAdapter.getLinks.mock.calls[0][0]).toEqual(query)

            expect(linksAdapter.addLink.mock.calls.length).toBe(0)
            expect(linksAdapter.updateLink.mock.calls.length).toBe(0)
            expect(linksAdapter.removeLink.mock.calls.length).toBe(0)
        })

        it('calls link language on addLink() with link expression once', async () => {
            const link = createLink()
            const linkExpression = await perspective.addLink(link)

            expect(linksAdapter.addLink.mock.calls.length).toBe(1)
            expect(linksAdapter.addLink.mock.calls[0][0]).toEqual(linkExpression)

            expect(linksAdapter.getLinks.mock.calls.length).toBe(0)
            expect(linksAdapter.updateLink.mock.calls.length).toBe(0)
            expect(linksAdapter.removeLink.mock.calls.length).toBe(0)
        })

        it('calls link language on updateLink() with link expression once', async () => {
            const link1 = createLink()
            const link2 = createLink()

            const link1Expression = await perspective.addLink(link1)
            const link2Expression = perspective.ensureLinkExpression(link2)
            await perspective.updateLink(link1Expression, link2Expression)

            expect(linksAdapter.updateLink.mock.calls.length).toBe(1)
            expect(linksAdapter.updateLink.mock.calls[0][0]).toEqual(link1Expression)
            expect(linksAdapter.updateLink.mock.calls[0][1]).toEqual(link2Expression)

            expect(linksAdapter.getLinks.mock.calls.length).toBe(0)
            expect(linksAdapter.addLink.mock.calls.length).toBe(1)
            expect(linksAdapter.removeLink.mock.calls.length).toBe(0)
        })

        it('calls link language on removeLink() with link expression once', async () => {
            const link = createLink()

            const linkExpression = await perspective.addLink(link)
            await perspective.removeLink(linkExpression)

            expect(linksAdapter.removeLink.mock.calls.length).toBe(1)
            expect(linksAdapter.removeLink.mock.calls[0][0]).toEqual(linkExpression)

            expect(linksAdapter.getLinks.mock.calls.length).toBe(0)
            expect(linksAdapter.updateLink.mock.calls.length).toBe(0)
            expect(linksAdapter.addLink.mock.calls.length).toBe(1)
        })

        describe('syncWithSharingAdpater', () => {
            it('adds all missing links from local DB to linksAdapter', async () => {
                perspective.sharedPerspective = null
    
                const link = createLink()
                const linkExpression = await perspective.addLink(link)
    
                perspective.sharedPerspective = {
                    linkLanguages: [sharingLanguage]
                }

                await perspective.syncWithSharingAdapter()

                expect(linksAdapter.addLink.mock.calls.length).toBe(1)
                expect(linksAdapter.addLink.mock.calls[0][0]).toEqual(linkExpression)
            })
        })
        
    })

})








