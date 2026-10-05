import { TestContext } from './test-context'
import path from "path";
import { Ad4mClient, InteractionCall, LanguageMetaInput } from '@coasys/ad4m';
import { expect } from "chai";
import { fileURLToPath } from 'url';
import { sleep } from '../utils/utils';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

/**
 * Reads served from the expression cache and writes that succeed while the
 * language cannot publish, against `languages/local-first-store`: its
 * "remote" is an in-memory map the `setOnline` interaction cuts off.
 */
export default function localFirstExpressionTests(testContext: TestContext) {
    return () => {
        describe('Local-first expressions', () => {
            let ad4mClient: Ad4mClient
            let lang = ""

            const control = () => `${lang}://control`
            const setOnline = (online: boolean) =>
                ad4mClient.expression.interact(control(), new InteractionCall('setOnline', { online }))
            const stats = async (): Promise<{ getCalls: number, remote: string[] }> =>
                JSON.parse(await ad4mClient.expression.interact(control(), new InteractionCall('stats', {})) as string)

            before(async () => {
                ad4mClient = testContext.ad4mClient
                const bundlePath = path.join(__dirname, "../languages/local-first-store/build/bundle.js").replace(/\\/g, "/")
                const meta = new LanguageMetaInput("local-first-store", "Test language with prepare/publish and an offline switch")
                lang = (await ad4mClient.languages.publish(bundlePath, meta)).address
                await ad4mClient.languages.byAddress(lang)
            })

            afterEach(async () => {
                await setOnline(true)
            })

            it('create succeeds offline, reads back without the language, and publishes once online', async () => {
                await setOnline(false)
                const url = await ad4mClient.expression.create({ note: "written offline" }, lang)
                const address = url.split("://")[1]

                const expr = await ad4mClient.expression.get(url)
                expect(JSON.parse(expr.data)).to.deep.equal({ note: "written offline" })
                expect(expr.proof.valid).to.be.true

                let s = await stats()
                expect(s.getCalls).to.equal(0)
                expect(s.remote).not.to.include(address)

                await setOnline(true)
                // The queue's first retry is due 10 s after the failure and
                // the worker polls every 10 s.
                for (let i = 0; i < 40 && !s.remote.includes(address); i++) {
                    await sleep(1000)
                    s = await stats()
                }
                expect(s.remote).to.include(address)
            })

            it('getMany fetches misses in one batch and serves repeats from the cache', async () => {
                const contents = [{ n: 1 }, { n: 2 }, { n: 3 }]
                const addresses: string[] = JSON.parse(await ad4mClient.expression.interact(
                    control(), new InteractionCall('seedRemote', { contents })) as string)
                const urls = addresses.map(a => `${lang}://${a}`)
                const before = (await stats()).getCalls

                const first = await ad4mClient.expression.getMany(urls)
                expect(first.map(e => JSON.parse(e.data))).to.deep.equal(contents)
                expect((await stats()).getCalls).to.equal(before + 3)

                // Offline, so a read that reached the language would fail.
                await setOnline(false)
                const second = await ad4mClient.expression.getMany(urls)
                expect(second.map(e => JSON.parse(e.data))).to.deep.equal(contents)
                expect((await stats()).getCalls).to.equal(before + 3)
            })

            it('an expression never fetched is not served while offline', async () => {
                const [address] = JSON.parse(await ad4mClient.expression.interact(
                    control(), new InteractionCall('seedRemote', { contents: [{ unseen: true }] })) as string)
                await setOnline(false)
                // This language throws when offline, which `get` passes on;
                // the centralized file store returns null instead.
                const result = await ad4mClient.expression.get(`${lang}://${address}`).catch(() => null)
                expect(result).to.be.null
            })
        })
    }
}
