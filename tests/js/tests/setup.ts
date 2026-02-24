/**
 * Mocha global setup — loaded via --require in all test scripts.
 *
 * Polyfills global.fetch with node-fetch so test files don't each need to
 * import and assign it individually.
 */
import fetch from "node-fetch";

// @ts-ignore — node-fetch v3 type is close enough for runtime use
(global as any).fetch = fetch;
