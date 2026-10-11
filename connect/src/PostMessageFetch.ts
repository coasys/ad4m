/**
 * Proxies an HTTP fetch request through window.parent via postMessage.
 *
 * Used in embedded proxy mode where the iframe cannot reach the AD4M executor
 * directly (e.g. when hosted on a different origin inside WE).  The parent
 * receives the AD4M_PROXY_HTTP_REQUEST message, performs the real fetch, and
 * replies with AD4M_PROXY_HTTP_RESPONSE / AD4M_PROXY_HTTP_ERROR.
 *
 * The request URL may use the placeholder origin 'http://proxy' — the parent
 * replaces it with the real executor base URL before fetching.
 */

let _idCounter = 0;

/** Timeout (ms) waiting for the parent to respond to a proxied HTTP request. */
const PROXY_FETCH_TIMEOUT_MS = 60_000;

/**
 * Serialise a HeadersInit value to a plain Record so it survives structured
 * cloning through postMessage.
 */
function serialiseHeaders(headers: HeadersInit | undefined): Record<string, string> {
    if (!headers) return {};
    if (headers instanceof Headers) {
        const out: Record<string, string> = {};
        headers.forEach((v, k) => { out[k] = v; });
        return out;
    }
    if (Array.isArray(headers)) {
        return Object.fromEntries(headers);
    }
    return { ...(headers as Record<string, string>) };
}

export function makePostMessageFetch(targetOrigin: string): typeof fetch {
    return async function postMessageFetch(input: RequestInfo | URL, init?: RequestInit): Promise<Response> {
        const url = typeof input === 'string' ? input : input instanceof URL ? input.href : input.url;
        const method = init?.method ?? 'GET';
        const headers = serialiseHeaders(init?.headers);
        const id = String(++_idCounter);

        // Extract body as a transferable ArrayBuffer when possible.
        let bodyBuffer: ArrayBuffer | null = null;
        if (init?.body instanceof ArrayBuffer) {
            // Slice to avoid transferring a larger shared buffer.
            const b = init.body as ArrayBuffer;
            bodyBuffer = b.slice(0);
        }

        return new Promise<Response>((resolve, reject) => {
            let timer: ReturnType<typeof setTimeout> | null = null;

            const cleanup = () => {
                window.removeEventListener('message', handler);
                if (timer) clearTimeout(timer);
            };

            const handler = (event: MessageEvent) => {
                if (event.source !== window.parent) return;
                if (event.origin !== targetOrigin) return;
                if (!event.data || event.data.id !== id) return;

                if (event.data.type === 'AD4M_PROXY_HTTP_RESPONSE') {
                    cleanup();
                    const { status, statusText, body } = event.data as {
                        status: number;
                        statusText: string;
                        body: ArrayBuffer | null;
                    };
                    resolve(new Response(body ?? null, { status, statusText }));
                    return;
                }

                if (event.data.type === 'AD4M_PROXY_HTTP_ERROR') {
                    cleanup();
                    reject(new TypeError((event.data as { message?: string }).message || 'Failed to fetch'));
                    return;
                }
            };

            window.addEventListener('message', handler);

            timer = setTimeout(() => {
                cleanup();
                reject(new TypeError(`AD4M proxy HTTP request timed out after ${PROXY_FETCH_TIMEOUT_MS}ms`));
            }, PROXY_FETCH_TIMEOUT_MS);

            const msg = { type: 'AD4M_PROXY_HTTP_REQUEST', id, url, method, headers };

            if (bodyBuffer) {
                // Transfer the buffer zero-copy; the parent receives it as an ArrayBuffer.
                window.parent.postMessage({ ...msg, body: bodyBuffer }, targetOrigin, [bodyBuffer]);
            } else {
                window.parent.postMessage({ ...msg, body: null }, targetOrigin);
            }
        });
    };
}
