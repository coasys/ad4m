import { v4 as uuidv4 } from 'uuid'
import faker from 'faker'
import type Link from '@perspect3vism/ad4m/Links'
import type { LinkQuery } from '@perspect3vism/ad4m/Links'
import Memory from 'lowdb/adapters/Memory'
import type LanguageRef from '@perspect3vism/ad4m/LanguageRef'
import { createLink } from './testutils/links'
import create from "./core/PerspectivismCore";
import { LinksAdapter } from '@perspect3vism/ad4m/Language'
import { createMockExpression } from './testutils/expression'

const DATA_RESOURCE_PATH = `${__dirname}/test-temp`
const LANG_TO_TEST = "social-context-channel"

jest.setTimeout(15000)

const core = new create({
    appDataPath: DATA_RESOURCE_PATH,
    appResourcePath: DATA_RESOURCE_PATH,
    builtInLangPath: DATA_RESOURCE_PATH,
    builtInLangs: [LANG_TO_TEST]
})

describe(LANG_TO_TEST, () => {

    beforeAll(async () => {
        await core.initServices()
        await core.agentService.createNewKeys()
        core.agentService.save('')
        core.initControllers()
        await core.initLanguages(true)
    })

    afterAll(async () => {
        await core.exit()
        await new Promise((resolve)=>setTimeout(resolve, 1000))
    })

    it('has a linksAdapter', (done) => {
        const langs = core.languageController.getLanguagesWithLinksAdapter()
        expect(langs.length).toEqual(1)
        expect(langs[0].name).toEqual(LANG_TO_TEST)
        done()
    })

    describe('after adding 5 links', () => {
        let allLinks = []
        let linkLanguage: LinksAdapter

        beforeAll(async () => {
            const langs = core.languageController.getLanguagesWithLinksAdapter()
            const maybeLang = core.languageController.getLinksAdapter(langs[0])
            if(!maybeLang) throw "Couldn't get linksAdapter of test language"
            else linkLanguage = maybeLang

            const did = "did:test:link-author"
            for(let i=0; i<5; i++) {
                const link = createLink()
                if(i%2 === 0) {
                    link.source = 'root'
                }

                const linkExpression = createMockExpression(did, link)
                allLinks.push(linkExpression)
                linkLanguage.addLink(linkExpression)
            }
        })

        it('can get all links', async () => {
            const result = await linkLanguage.getLinks({} as LinkQuery)

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
            const result = await linkLanguage.getLinks({source: 'root'} as LinkQuery)
            expect(result.length).toEqual(3)
        })
    })
})








