Error.captureStackTrace = function (err, constructorOpt) {
    if (typeof Error.prepareStackTrace === 'function') {
      const fakeCallSite = {
        getThis() {
          return undefined;
        },
        getTypeName() {
          return undefined;
        },
        getFunction() {
          return undefined;
        },
        getFunctionName() {
          return undefined;
        },
        getMethodName() {
          return undefined;
        },
        getFileName() {
          return err.fileName;
        },
        getLineNumber() {
          return err.lineNumber;
        },
        getColumnNumber() {
          return err.columnNumber;
        },
        getEvalOrigin() {
          return undefined;
        },
        isToplevel() {
          return false;
        },
        isEval() {
          return false;
        },
        isNative() {
          return false;
        },
        isConstructor() {
          return false;
        },
        isAsync() {
          return false;
        },
        isPromiseAll() {
          return false;
        },
      };

      const structuredStackTrace = [fakeCallSite];
      if(err.code == "UNKNOWN", err.syscall == "accept") {
        err.stack = Error.prepareStackTrace(err, structuredStackTrace);
      }
    } else {
      err.stack = err.stack || err.toString();
    }
} 

import { init, path, os } from 'https://ad4m.runtime/executor'

const appDataPath = path.join(os.homedir(), 'ad4m', 'tests', 'ad4m1');
const binaryPath = path.join(appDataPath, 'binary');
const swiplHomePath = (process.platform == "win32" ? path.join(appDataPath, 'swipl/') : path.join(appDataPath, 'swipl/lib/swipl/'))
const swiplPath = path.join(appDataPath, 'swipl/bin/swipl');
const ipfsSwarmPort = undefined;
const gqlPort = 13000
const ipfsRepoPath = path.join(appDataPath, 'ipfs')
const networkBootstrapSeed = path.join(appDataPath, "mainnet_seed.seed")
const languageLanguageOnly = false
const mocks = false
const runDappServer = false
const hcPortAdmin = undefined
const hcPortApp = undefined
const appLangAliases = {}
const bootstrapFixtures = {
  languages: [],
  perspectives: [],
};
const connectHolochain = false;
const reqCredential = undefined;

async function initCore(config) {
    const core = await init(config)
    globalThis.core = core
    return core
}

globalThis.initCore = initCore

const n = 5
globalThis.n = n
