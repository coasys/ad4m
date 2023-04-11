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

console.log("Hello from main")

globalThis.appDataPath = path.join(os.homedir(), 'ad4m', 'tests', 'ad4m1');
globalThis.binaryPath = path.join(appDataPath, 'binary');
globalThis.swiplHomePath = (process.platform == "win32" ? path.join(appDataPath, 'swipl/') : path.join(appDataPath, 'swipl/lib/swipl/'))
globalThis.swiplPath = path.join(appDataPath, 'swipl/bin/swipl');
globalThis.ipfsSwarmPort = undefined;
globalThis.gqlPort = 13000
globalThis.ipfsRepoPath = path.join(appDataPath, 'ipfs')
globalThis.networkBootstrapSeed = path.join(appDataPath, "mainnet_seed.seed")
globalThis.languageLanugageOnly = false
globalThis.mocks = false
globalThis.runDappServer = false
globalThis.hcPortAdmin = undefined
globalThis.hcPortApp = undefined
globalThis.appLangAliases = {}
globalThis.bootstrapFixtures = {
  languages: [],
  perspectives: [],
};
globalThis.connectHolochain = false;
globalThis.reqCredential = undefined;

//if (!fs.existsSync(appDataPath)) {
//  fs.mkdirSync(appDataPath);
//}
//const bLanguage = bootstrapLanguage ? await import(path.isAbsolute(bootstrapLanguage) ? bootstrapLanguage: path.join(__dirname, bootstrapLanguage)) : [];
//const bPerspective = bootstrapPerspective ? await import(path.isAbsolute(bootstrapPerspective) ? bootstrapPerspective: path.join(__dirname, bootstrapPerspective)) : [];

const config = {
    appDataPath: globalThis.appDataPath,
    resourcePath: globalThis.binaryPath,
    networkBootstrapSeed: globalThis.networkBootstrapSeed,
    languageLanguageOnly: globalThis.languageLanugageOnly,
    bootstrapFixtures: globalThis.bootstrapFixtures,
    appLangAliases: globalThis.appLangAliases,
    mocks: globalThis.mocks,
    runDappServer: globalThis.runDappServer,
    gqlPort: globalThis.gqlPort,
    hcPortAdmin: globalThis.hcPortAdmin,
    hcPortApp: globalThis.hcPortApp,
    ipfsRepoPath: globalThis.ipfsRepoPath,
    ipfsSwarmPort: globalThis.ipfsSwarmPort,
    connectHolochain: globalThis.connectHolochain,
    reqCredential: globalThis.reqCredential,
    swiplPath: globalThis.swiplPath,
    swiplHomePath: globalThis.swiplHomePath,
};

async function initCore() {
    const core = await init(config)
    globalThis.core = core
    return core
}

globalThis.initCore = initCore
//await initCore()

console.log("main done")

const n = 5
globalThis.n = n
