import { callSafely } from './notifyListeners'

describe('callSafely', () => {
    it('logs a throw or a rejection with the label and raises neither', async () => {
        const errorSpy = jest.spyOn(console, 'error').mockImplementation(() => {})
        const unhandled = jest.fn()
        process.on('unhandledRejection', unhandled)

        callSafely(() => { throw new Error('sync boom') }, 'label:')
        callSafely(async () => { throw new Error('async boom') }, 'label:')
        await new Promise((r) => setTimeout(r, 0))

        expect(errorSpy.mock.calls.map(([label, e]) => [label, (e as Error).message]))
            .toEqual([['label:', 'sync boom'], ['label:', 'async boom']])
        expect(unhandled).not.toHaveBeenCalled()
        process.off('unhandledRejection', unhandled)
        errorSpy.mockRestore()
    })
})
