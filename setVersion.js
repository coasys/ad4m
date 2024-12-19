const fs = require('fs')
const readline = require('readline-sync');

function replaceVersionLine(content, version, prefix = 'version = ', suffix = '') {
    const lines = content.split('\n')
    const versionLineIndex = lines.findIndex(line => line.startsWith(prefix))
    if (versionLineIndex === -1) throw new Error(`Could not find version line in ${content}`)
    const scope = lines[versionLineIndex].substring(prefix.length)
    const oldVersion = scope.split('"')[1]
    const newVersionLine = `${prefix}"${version}"${suffix}` 
    lines[versionLineIndex] = newVersionLine
    const newContent = lines.join('\n')
    return { oldVersion, newContent }
}

let isPreRelease = false;

//const answer = readline.question('Is this a pre-release version? (y/n) ');
//isPreRelease = answer === 'y' || answer === 'Y';
//if (isPreRelease) {
//    console.log('Setting versions for pre-release (will not add pre-release prefix to launcher version)');
//}

let VERSION = process.argv[2]
let RUST_VERSION = process.argv[2]

if (isPreRelease) {
    VERSION = VERSION + "-prerelease";
    RUST_VERSION = VERSION + ".0";
}

const RAW_VERSION = process.argv[2];

console.log("Setting all sub-project versions to: " + VERSION + " with tauri still being set to: " + RAW_VERSION);

const rootRepo = JSON.parse(fs.readFileSync('package.json', 'utf8'))
console.log("Root repo version: " + rootRepo.version + " -> " + VERSION)
rootRepo.version = VERSION
fs.writeFileSync('package.json', JSON.stringify(rootRepo, null, 2) + '\n')

let ad4mClient;
if (isPreRelease) {
    cli = replaceVersionLine(fs.readFileSync('cli/Cargo.toml', 'utf8'), RUST_VERSION)
    console.log("CLI version: " + cli.oldVersion + " -> " + RUST_VERSION)
    cli = replaceVersionLine(cli.newContent, RUST_VERSION, `ad4m-client = { path = "../rust-client", version=`, ` }`)
    console.log(`CLI ad4m-client dep: ${cli.oldVersion} -> ${RUST_VERSION}`)
    cli = replaceVersionLine(cli.newContent, RUST_VERSION, `ad4m-executor = { path = "../rust-client", version=`, ` }`)
    console.log(`CLI ad4m-client dep: ${cli.oldVersion} -> ${RUST_VERSION}`)
} else {
    cli = replaceVersionLine(fs.readFileSync('cli/Cargo.toml', 'utf8'), RAW_VERSION)
    console.log("CLI version: " + cli.oldVersion + " -> " + RAW_VERSION)
    cli = replaceVersionLine(cli.newContent, RAW_VERSION, `ad4m-client = { path = "../rust-client", version=`, ` }`)
    console.log(`CLI ad4m-client dep: ${cli.oldVersion} -> ${RAW_VERSION}`)
    cli = replaceVersionLine(cli.newContent, RAW_VERSION, `ad4m-executor = { path = "../rust-executor", version=`, ` }`)
    console.log(`CLI ad4m-executor dep: ${cli.oldVersion} -> ${RAW_VERSION}`)
}
fs.writeFileSync('cli/Cargo.toml', cli.newContent)

const connect = JSON.parse(fs.readFileSync('connect/package.json', 'utf8'))
console.log("Connect version: " + connect.version + " -> " + VERSION)
connect.version = VERSION
connect.devDependencies["@coasys/ad4m"] = "workspace:" + VERSION
fs.writeFileSync('connect/package.json', JSON.stringify(connect, null, 2) + '\n')

const core = JSON.parse(fs.readFileSync('core/package.json', 'utf8'))
console.log("Core version: " + core.version + " -> " + VERSION)
core.version = VERSION
fs.writeFileSync('core/package.json', JSON.stringify(core, null, 2) + '\n')

const hookHelpers = JSON.parse(fs.readFileSync('ad4m-hooks/helpers/package.json', 'utf8'))
console.log("Hook Helpers version: " + core.version + " -> " + VERSION)
hookHelpers.version = VERSION
fs.writeFileSync('ad4m-hooks/helpers/package.json', JSON.stringify(hookHelpers, null, 2) + '\n')

const reactHooks = JSON.parse(fs.readFileSync('ad4m-hooks/react/package.json', 'utf8'))
console.log("Ad4m React hook version: " + reactHooks.version + " -> " + VERSION)
reactHooks.version = VERSION
fs.writeFileSync('ad4m-hooks/react/package.json', JSON.stringify(reactHooks, null, 2) + '\n')

const vueHooks = JSON.parse(fs.readFileSync('ad4m-hooks/vue/package.json', 'utf8'))
console.log("Ad4m Vue hook version: " + vueHooks.version + " -> " + VERSION)
vueHooks.version = VERSION
fs.writeFileSync('ad4m-hooks/vue/package.json', JSON.stringify(vueHooks, null, 2) + '\n')

const executor = JSON.parse(fs.readFileSync('executor/package.json', 'utf8'))
console.log("Executor version: " + executor.version + " -> " + VERSION)
executor.version = VERSION
fs.writeFileSync('executor/package.json', JSON.stringify(executor, null, 2) + '\n')

const rustExecutor = JSON.parse(fs.readFileSync('rust-executor/package.json', 'utf8'))
console.log("Executor version: " + rustExecutor.version + " -> " + VERSION)
rustExecutor.version = VERSION
fs.writeFileSync('rust-executor/package.json', JSON.stringify(rustExecutor, null, 2) + '\n')

const tests = JSON.parse(fs.readFileSync('tests/js/package.json', 'utf8'))
console.log("Executor version: " + tests.version + " -> " + VERSION)
tests.version = VERSION
fs.writeFileSync('tests/js/package.json', JSON.stringify(tests, null, 2) + '\n')


const executorHardWired = replaceVersionLine(
    fs.readFileSync('executor/src/core/Config.ts', 'utf8'), 
    VERSION, 
    'export let ad4mExecutorVersion = ',
    ';')
console.log("Hard-wired version string in executor's Config.ts: " + executorHardWired.oldVersion + " -> " + VERSION)
fs.writeFileSync('executor/src/core/Config.ts', executorHardWired.newContent)

const rustClient = replaceVersionLine(fs.readFileSync('rust-client/Cargo.toml', 'utf8'), VERSION)
console.log("rust-client version: " + rustClient.oldVersion + " -> " + VERSION)
fs.writeFileSync('rust-client/Cargo.toml', rustClient.newContent)

let rustExecutorCargo = replaceVersionLine(fs.readFileSync('rust-executor/Cargo.toml', 'utf8'), VERSION)
console.log("rust-executor version: " + rustExecutorCargo.oldVersion + " -> " + VERSION)
rustExecutorCargo = replaceVersionLine(rustExecutorCargo.newContent, VERSION, `ad4m-client = { path = "../rust-client", version=`, ` }`)
console.log(`rust-executor ad4m-client dep: ${rustExecutorCargo.oldVersion} -> ${VERSION}`)
fs.writeFileSync('rust-executor/Cargo.toml', rustExecutorCargo.newContent)

const globalsRs = replaceVersionLine(
    fs.readFileSync('rust-executor/src/globals.rs', 'utf8'), 
    VERSION,
    `    pub static ref AD4M_VERSION: String = String::from(`,
    `);`
    )
console.log("globals.rs version: " + globalsRs.oldVersion + " -> " + VERSION)
fs.writeFileSync('rust-executor/src/globals.rs', globalsRs.newContent)

const uiPackage = JSON.parse(fs.readFileSync('ui/package.json', 'utf8'))
if (isPreRelease) {
    console.log("UI version: " + uiPackage.version + " -> " + VERSION)
    uiPackage.version = VERSION
} else {
    console.log("UI version: " + uiPackage.version + " -> " + RAW_VERSION)
    uiPackage.version = RAW_VERSION
}
fs.writeFileSync('ui/package.json', JSON.stringify(uiPackage, null, 2) + '\n')

const uiTauri = JSON.parse(fs.readFileSync('ui/src-tauri/tauri.conf.json', 'utf8'))
if (isPreRelease) {
    console.log("UI Tauri version: " + uiTauri.version + " -> " + VERSION)
    uiTauri.version = VERSION
} else {
    console.log("UI Tauri version: " + uiTauri.version + " -> " + RAW_VERSION)
    uiTauri.version = RAW_VERSION
}
fs.writeFileSync('ui/src-tauri/tauri.conf.json', JSON.stringify(uiTauri, null, 2) + '\n')

let uiTauriCargo;
if (isPreRelease) {
    uiTauriCargo = replaceVersionLine(fs.readFileSync('ui/src-tauri/Cargo.toml', 'utf8'), RUST_VERSION)
    console.log("UI Cargo version: " + uiTauriCargo.oldVersion + " -> " + RUST_VERSION)
} else {
    uiTauriCargo = replaceVersionLine(fs.readFileSync('ui/src-tauri/Cargo.toml', 'utf8'), RAW_VERSION)
    console.log("UI Cargo version: " + uiTauriCargo.oldVersion + " -> " + VERSION)
}
fs.writeFileSync('ui/src-tauri/Cargo.toml', uiTauriCargo.newContent)

const book = JSON.parse(fs.readFileSync('docs/package.json', 'utf8'))
console.log("Docs version: " + book.version + " -> " + VERSION)
book.version = VERSION
fs.writeFileSync('docs/package.json', JSON.stringify(book, null, 2) + '\n')

const testRunner = JSON.parse(fs.readFileSync('test-runner/package.json', 'utf8'))
console.log("Test runner version: " + testRunner.version + " -> " + VERSION)
testRunner.version = VERSION
fs.writeFileSync('test-runner/package.json', JSON.stringify(testRunner, null, 2) + '\n')

const perspectiveDiffSync = JSON.parse(fs.readFileSync('bootstrap-languages/p-diff-sync/package.json', 'utf8'))
console.log("Perspective diff sync: " + perspectiveDiffSync.version + " -> " + VERSION)
perspectiveDiffSync.version = VERSION
fs.writeFileSync('bootstrap-languages/p-diff-sync/package.json', JSON.stringify(perspectiveDiffSync, null, 2) + '\n')

const agentLanguage = JSON.parse(fs.readFileSync('bootstrap-languages/agent-language/package.json', 'utf8'))
console.log("Agent language: " + agentLanguage.version + " -> " + VERSION)
agentLanguage.version = VERSION
fs.writeFileSync('bootstrap-languages/agent-language/package.json', JSON.stringify(agentLanguage, null, 2) + '\n')

const directMessageLanguage = JSON.parse(fs.readFileSync('bootstrap-languages/direct-message-language/package.json', 'utf8'))
console.log("Direct message language: " + directMessageLanguage.version + " -> " + VERSION)
directMessageLanguage.version = VERSION
fs.writeFileSync('bootstrap-languages/direct-message-language/package.json', JSON.stringify(directMessageLanguage, null, 2) + '\n')

const neighbourhoodLanguage = JSON.parse(fs.readFileSync('bootstrap-languages/neighbourhood-language/package.json', 'utf8'))
console.log("Neighbourhood language: " + neighbourhoodLanguage.version + " -> " + VERSION)
neighbourhoodLanguage.version = VERSION
fs.writeFileSync('bootstrap-languages/neighbourhood-language/package.json', JSON.stringify(neighbourhoodLanguage, null, 2) + '\n')

const perspectiveLanguage = JSON.parse(fs.readFileSync('bootstrap-languages/perspective-language/package.json', 'utf8'))
console.log("Perspective language: " + perspectiveLanguage.version + " -> " + VERSION)
perspectiveLanguage.version = VERSION
fs.writeFileSync('bootstrap-languages/perspective-language/package.json', JSON.stringify(perspectiveLanguage, null, 2) + '\n')
fs.writeFileSync('bootstrap-languages/p-diff-sync/package.json', JSON.stringify(perspectiveDiffSync, null, 2) + '\n')
