/**
 * Call each listener with `args`. A listener that throws does not stop the
 * others: the error goes to `console.error`, as for signal handlers and query
 * subscription callbacks. Only synchronous throws are caught.
 */
export function notifyListeners<A extends unknown[]>(
    listeners: ReadonlyArray<(...args: A) => unknown>,
    label: string,
    ...args: A
): void {
    listeners.forEach((listener) => {
        try {
            listener(...args)
        } catch (e) {
            console.error(`Error in ${label} listener:`, e)
        }
    })
}
