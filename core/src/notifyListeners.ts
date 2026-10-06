/** Call `callback`. A throw, or a rejected promise from an async callback, is logged, not raised. */
export function callSafely<A extends unknown[]>(callback: (...args: A) => unknown, label: string, ...args: A): void {
    const log = (e: unknown) => console.error(label, e)
    try {
        const result = callback(...args)
        if (result instanceof Promise) result.catch(log)
    } catch (e) {
        log(e)
    }
}

/** Call each listener. One that throws or rejects is logged and does not stop the rest. */
export function notifyListeners<A extends unknown[]>(listeners: ReadonlyArray<(...args: A) => unknown>, ...args: A): void {
    for (const listener of listeners) callSafely(listener, 'Error in event listener:', ...args)
}

/** Add `listener` to `listeners`. Returns a function that removes it. */
export function addListener<T>(listeners: T[], listener: T): () => void {
    listeners.push(listener)
    return () => {
        const index = listeners.indexOf(listener)
        if (index >= 0) listeners.splice(index, 1)
    }
}
