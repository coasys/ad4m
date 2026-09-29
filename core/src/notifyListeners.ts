/** Call each listener. One that throws is logged and does not stop the rest. */
export function notifyListeners<A extends unknown[]>(listeners: ReadonlyArray<(...args: A) => unknown>, ...args: A): void {
    for (const listener of listeners) {
        try {
            listener(...args)
        } catch (e) {
            console.error('Error in event listener:', e)
        }
    }
}
