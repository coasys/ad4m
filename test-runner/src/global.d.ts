
declare module globalThis {
  var hideLogs: boolean;
  var languageAddress: string;
  var perspective: string;
  var neighbourhood: string;
  var relativePath: string;
  var ad4mHostVersion: string;
  var ad4mToken: string;
  var config: {
    relativePath: string;
    bundle: string;
    meta: string;
    defaultLangPath: string;
    ui: boolean;
  };
  var agents: {
    client: any;
    port: any;
    relativePath: string,
    languageAddress?: string,
    neighbourhood?: string,
    perspective?: string,
    clear: () => void
  }[];
  var tests: any[];
  var describe: (desc: string, fn: () => void) => void;
  var it: (desc: string, fn: () => void) => void;
  var expect: {};
  var beforeAll: (fn: () => void) => void;
  var afterAll: (fn: () => void) => void;
  var beforeEach: (fn: () => void) => void;
  var afterEach: (fn: () => void) => void;
}