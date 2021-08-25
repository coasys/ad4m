import type { LinkQuery } from '@perspect3vism/ad4m'
import { Expression, LinksAdapter } from '@perspect3vism/ad4m'
import { createLink } from '../testutils/links'
import create from "../core/PerspectivismCore";
import { createMockExpression } from '../testutils/expression'
import PerspectivismCore from '../core/PerspectivismCore'
import fs from 'fs-extra'
import path from 'path'

// Patch Reflect to have missing getOwnPropertyDescriptor()
// which should be there in any ES6 runtime but for some reason
// is missing on some machines...
import getOwnPropertyDescriptor from '../shims/getOwnPropertyDescriptor'
Reflect.getOwnPropertyDescriptor = getOwnPropertyDescriptor

const TEST_DIR = `${__dirname}/../test-temp`
const LANG_TO_TEST = "social-context"

jest.setTimeout(35000)
let core: PerspectivismCore|null = null

describe(LANG_TO_TEST, () => {

    beforeAll(async () => {
        if(!fs.existsSync(TEST_DIR)) {
            throw Error("Please ensure that prepare-test is run before running tests!");
        }
        const appDataPath = path.join(TEST_DIR, 'agents', 'linksLangTest')
        if(!fs.existsSync(path.join(TEST_DIR, 'agents')))
            fs.mkdirSync(path.join(TEST_DIR, 'agents'))
        if(!fs.existsSync(appDataPath))
            fs.mkdirSync(appDataPath)
        const ipfsRepoPath = path.join(appDataPath, '.jsipfs')
        core = new create({
            appDataPath,
            appResourcePath: TEST_DIR,
            builtInLangPath: path.join(TEST_DIR, 'languages'),
            builtInLangs: [LANG_TO_TEST],
        })

        await core.initServices({
            portHCAdmin: 22000,
            portHCApp: 21337,
            ipfsSwarmPort: 24002,
            ipfsRepoPath
        })
        await core.agentService.createNewKeys()
        await core.agentService.save('')
        await core.initControllers()
        await core.initLanguages()
    })

    afterAll(async () => {
        await core!.exit()
        await new Promise((resolve)=>setTimeout(resolve, 1000))
    })

    it('has a linksAdapter', (done) => {
        const langs = core!.languageController.getLanguagesWithLinksAdapter()
        expect(langs.length).toEqual(1)
        expect(langs[0].name).toEqual(LANG_TO_TEST)
        done()
    })

    describe('after adding 5 links', () => {
        let allLinks = [] as Expression[]
        let linkLanguage: LinksAdapter

        beforeAll(async () => {
            const langs = core!.languageController.getLanguagesWithLinksAdapter()
            const maybeLang = await core!.languageController.getLinksAdapter(langs[0])
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
                await linkLanguage.addLink(linkExpression)
            }
        })


        it('can get a link by source', async () => {
            const addedLink = allLinks[1]
            const source = addedLink.data['source']
            const result = await linkLanguage.getLinks({source} as LinkQuery)
            expect(result.length).toEqual(1)
            expect(result[0]).toEqual(addedLink)
        })


        it('can get a link by predicate', async () => {
            const addedLink = allLinks[0]
            const predicate = addedLink.data['predicate']
            const result = await linkLanguage.getLinks({predicate} as LinkQuery)
            expect(result.length).toEqual(1)
            expect(result[0]).toEqual(addedLink)
        })

        it('can get a link by target', async () => {
            const addedLink = allLinks[0]
            const target = addedLink.data['target']
            const result = await linkLanguage.getLinks({target} as LinkQuery)
            expect(result.length).toEqual(1)
            expect(result[0]).toEqual(addedLink)
        })

        it('can get multiple links by source', async () => {
            const result = await linkLanguage.getLinks({source: 'root'} as LinkQuery)
            expect(result.length).toEqual(3)
        })

        it('can get all links', async () => {
            const result = await linkLanguage.getLinks({} as LinkQuery)
            expect(result.length).toEqual(5)

            for(let i=0; i<5; i++) {
                expect(result).toEqual(
                    expect.arrayContaining(
                        [expect.objectContaining(allLinks[i])]
                    )
                )
            }
        })
    })
})








