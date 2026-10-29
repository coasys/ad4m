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
