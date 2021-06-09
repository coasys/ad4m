import { v4 as uuidv4 } from 'uuid'
import faker from 'faker'
import type Link from '@perspect3vism/ad4m/Links'
import type { LinkQuery } from '@perspect3vism/ad4m/Links'
import Memory from 'lowdb/adapters/Memory'
import type LanguageRef from '@perspect3vism/ad4m/LanguageRef'
import { createLink } from './testutils/links'
import create from "./core/PerspectivismCore";

const DATA_RESOURCE_PATH = `${__dirname}/test-temp`
const LANG_TO_TEST = "social-context-channel"

jest.setTimeout(15000)

const core = new create({
    appDataPath: DATA_RESOURCE_PATH,
    appResourcePath: DATA_RESOURCE_PATH,
    builtInLangPath: DATA_RESOURCE_PATH,
    builtInLangs: [LANG_TO_TEST]
})

const ready = new Promise<void>(async (resolve)=>{
    
    resolve()
})


describe(LANG_TO_TEST, () => {

    beforeAll(async () => {
        await core.initServices()
        await core.agentService.createNewKeys()
        core.agentService.save('')
        core.initControllers()
        await core.initLanguages()
    })

    afterAll(async () => {
        console.log("afterAll 1")
        await core.exit()
        console.log("afterAll 2")
        await new Promise((resolve)=>setTimeout(resolve, 1000))
        console.log("afterAll 3")
    })

    it('has a linksAdapter', (done) => {
        try {
            throw "test throw"
        } catch(e) {
            console.log(e)
        }
        const langs = core.languageController.getLanguagesWithLinksAdapter()
        expect(langs.length).toEqual(1)
        expect(langs[0].name).toEqual(LANG_TO_TEST)
        done()
    })
    /*

    it('wraps links in expressions on addLink', () => {
        const link = createLink()
        core.languageController.getLanguagesWithLinksAdapter()
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
    */

})








