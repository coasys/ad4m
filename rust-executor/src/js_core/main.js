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
      };
      const structuredStackTrace = [fakeCallSite];
      err.stack = Error.prepareStackTrace(err, structuredStackTrace);
    } else {
      err.stack = err.stack || err.toString();
    }
} 

import { init, path, os } from 'https://ad4m.runtime/executor'

console.log("Hello from main")

let appDataPath = path.join(os.homedir(), 'ad4m', 'tests', 'ad4m1');
const binaryPath = path.join(appDataPath, 'binary');
const swiplHomePath = (process.platform == "win32" ? path.join(appDataPath, 'swipl/') : path.join(appDataPath, 'swipl/lib/swipl/'))
const swiplPath = path.join(appDataPath, 'swipl/bin/swipl');
const gqlPort = 13000
const ipfsRepoPath = path.join(appDataPath, 'ipfs')

//if (!fs.existsSync(appDataPath)) {
//  fs.mkdirSync(appDataPath);
//}
//const bLanguage = bootstrapLanguage ? await import(path.isAbsolute(bootstrapLanguage) ? bootstrapLanguage: path.join(__dirname, bootstrapLanguage)) : [];
//const bPerspective = bootstrapPerspective ? await import(path.isAbsolute(bootstrapPerspective) ? bootstrapPerspective: path.join(__dirname, bootstrapPerspective)) : [];

const config = {
    appDataPath: appDataPath,
    resourcePath: binaryPath,
    networkBootstrapSeed: path.join(appDataPath, "mainnet_seed.seed"),
    languageLanguageOnly: true,
    bootstrapFixtures: {
        languages: [],
        perspectives: [],
    },
    appLangAliases: {},
    mocks: false,
    runDappServer: false,
    gqlPort,
    hcPortAdmin: undefined,
    hcPortApp: undefined,
    ipfsRepoPath,
    ipfsSwarmPort: undefined,
    connectHolochain: true,
    reqCredential: undefined,
    swiplPath,
    swiplHomePath
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
