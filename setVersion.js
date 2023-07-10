const fs = require('fs')
const readline = require('readline-sync');

function replaceVersionLine(content, version, prefix = 'version = ', suffix = '') {
    const lines = content.split('\n')
    const versionLineIndex = lines.findIndex(line => line.startsWith(prefix))
    const scope = lines[versionLineIndex].substring(prefix.length)
    const oldVersion = scope.split('"')[1]
    const newVersionLine = `${prefix}"${version}"${suffix}` 
    lines[versionLineIndex] = newVersionLine
    const newContent = lines.join('\n')
    return { oldVersion, newContent }
}

let isPreRelease = false;

const answer = readline.question('Is this a pre-release version? (y/n) ');
isPreRelease = answer === 'y' || answer === 'Y';
if (isPreRelease) {
    console.log('Setting versions for pre-release (will not add pre-release prefix to launcher version)');
}

let VERSION = process.argv[2]

if (isPreRelease) {
    VERSION = VERSION + ".prerelease";
}

const RAW_VERSION = process.argv[2];

console.log("Setting all sub-project versions to: " + VERSION + " with tauri still being set to: " + RAW_VERSION);

const rootRepo = JSON.parse(fs.readFileSync('package.json', 'utf8'))
console.log("Root repo version: " + rootRepo.version + " -> " + VERSION)
rootRepo.version = VERSION
fs.writeFileSync('package.json', JSON.stringify(rootRepo, null, 2) + '\n')

const cli = replaceVersionLine(fs.readFileSync('cli/Cargo.toml', 'utf8'), VERSION)
console.log("CLI version: " + cli.oldVersion + " -> " + VERSION)
const ad4mClient = replaceVersionLine(cli.newContent, VERSION, `ad4m-client = { path = "../rust-client", version = `, ` }`)
console.log(`CLI ad4m-client dep: ${ad4mClient.oldVersion} -> ${VERSION}`)
fs.writeFileSync('cli/Cargo.toml', ad4mClient.newContent)

const connect = JSON.parse(fs.readFileSync('connect/package.json', 'utf8'))
console.log("Connect version: " + connect.version + " -> " + VERSION)
connect.version = VERSION
fs.writeFileSync('connect/package.json', JSON.stringify(connect, null, 2) + '\n')

const core = JSON.parse(fs.readFileSync('core/package.json', 'utf8'))
console.log("Core version: " + core.version + " -> " + VERSION)
core.version = VERSION
fs.writeFileSync('core/package.json', JSON.stringify(core, null, 2) + '\n')

const executor = JSON.parse(fs.readFileSync('executor/package.json', 'utf8'))
console.log("Executor version: " + executor.version + " -> " + VERSION)
executor.version = VERSION
fs.writeFileSync('executor/package.json', JSON.stringify(executor, null, 2) + '\n')


const executorHardWired = replaceVersionLine(
    fs.readFileSync('executor/src/core/Config.ts', 'utf8'), 
    VERSION, 
    'export let ad4mExecutorVersion = ',
    ';')
console.log("Hard-wired version string in executor's Config.ts: " + executorHardWired.oldVersion + " -> " + VERSION)
fs.writeFileSync('executor/src/core/Config.ts', executorHardWired.newContent)

const host = JSON.parse(fs.readFileSync('host/package.json', 'utf8'))
console.log("Host version: " + host.version + " -> " + VERSION)
host.version = VERSION
fs.writeFileSync('host/package.json', JSON.stringify(host, null, 2) + '\n')

const rustClient = replaceVersionLine(fs.readFileSync('rust-client/Cargo.toml', 'utf8'), VERSION)
console.log("rust-client version: " + rustClient.oldVersion + " -> " + VERSION)
fs.writeFileSync('rust-client/Cargo.toml', rustClient.newContent)

const uiPackage = JSON.parse(fs.readFileSync('ui/package.json', 'utf8'))
if (isPreRelease) {
    console.log("UI version: " + uiPackage.version + " -> " + RAW_VERSION)
    uiPackage.version = RAW_VERSION
} else {
    console.log("UI version: " + uiPackage.version + " -> " + VERSION)
    uiPackage.version = VERSION
}
fs.writeFileSync('ui/package.json', JSON.stringify(uiPackage, null, 2) + '\n')

const uiTauri = JSON.parse(fs.readFileSync('ui/src-tauri/tauri.conf.json', 'utf8'))
if (isPreRelease) {
    console.log("UI Tauri version: " + uiTauri.package.version + " -> " + RAW_VERSION)
    uiTauri.package.version = RAW_VERSION
} else {
    console.log("UI Tauri version: " + uiTauri.package.version + " -> " + VERSION)
    uiTauri.package.version = VERSION
}
fs.writeFileSync('ui/src-tauri/tauri.conf.json', JSON.stringify(uiTauri, null, 2) + '\n')

let uiTauriCargo;
if (isPreRelease) {
    uiTauriCargo = replaceVersionLine(fs.readFileSync('ui/src-tauri/Cargo.toml', 'utf8'), RAW_VERSION)
    console.log("UI Cargo version: " + uiTauriCargo.oldVersion + " -> " + RAW_VERSION)
} else {
    uiTauriCargo = replaceVersionLine(fs.readFileSync('ui/src-tauri/Cargo.toml', 'utf8'), VERSION)
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
