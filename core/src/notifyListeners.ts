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

/** Add `listener` to `listeners`. Returns a function that removes it. */
export function addListener<T>(listeners: T[], listener: T): () => void {
    listeners.push(listener)
    return () => {
        const index = listeners.indexOf(listener)
        if (index >= 0) listeners.splice(index, 1)
    }
}
