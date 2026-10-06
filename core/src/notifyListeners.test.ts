import { notifyListeners } from './notifyListeners'

describe('notifyListeners', () => {
    it('logs a throwing or rejecting listener and still calls the rest', async () => {
        const errorSpy = jest.spyOn(console, 'error').mockImplementation(() => {})
        const unhandled = jest.fn()
        process.on('unhandledRejection', unhandled)
        const received: number[] = []

        notifyListeners<[number]>([
            () => { throw new Error('sync boom') },
            async () => { throw new Error('async boom') },
            (n) => { received.push(n) },
        ], 7)
        await new Promise((r) => setTimeout(r, 0))

        expect(received).toEqual([7])
        const messages = errorSpy.mock.calls.map(([, e]) => (e as Error).message)
        expect(messages).toEqual(expect.arrayContaining(['sync boom', 'async boom']))
        expect(unhandled).not.toHaveBeenCalled()
        process.off('unhandledRejection', unhandled)
        errorSpy.mockRestore()
    })
})
