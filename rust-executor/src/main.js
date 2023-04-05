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
networkBootstrapSeed: appDataPath,
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

const core = await init(config)
console.log(core)