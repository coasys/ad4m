const fs = require('fs')

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

const VERSION = process.argv[2]
console.log("Setting all sub-project versions to: " + VERSION)

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
console.log("UI version: " + uiPackage.version + " -> " + VERSION)
uiPackage.version = VERSION
fs.writeFileSync('ui/package.json', JSON.stringify(uiPackage, null, 2) + '\n')

const uiTauri = JSON.parse(fs.readFileSync('ui/src-tauri/tauri.conf.json', 'utf8'))
console.log("UI Tauri version: " + uiTauri.package.version + " -> " + VERSION)
uiTauri.package.version = VERSION
fs.writeFileSync('ui/src-tauri/tauri.conf.json', JSON.stringify(uiTauri, null, 2) + '\n')

const uiCargo = replaceVersionLine(fs.readFileSync('ui/src-tauri/Cargo.toml', 'utf8'), VERSION)
console.log("UI Cargo version: " + uiCargo.oldVersion + " -> " + VERSION)
fs.writeFileSync('ui/src-tauri/Cargo.toml', uiCargo.newContent)

const book = JSON.parse(fs.readFileSync('docs-src/book.json', 'utf8'))
console.log("Docs version: " + book.variables.ad4mVersion + " -> " + VERSION)
book.variables.ad4mVersion = VERSION
fs.writeFileSync('docs-src/book.json', JSON.stringify(book, null, 2) + '\n')

const testRunner = JSON.parse(fs.readFileSync('test-runner/package.json', 'utf8'))
console.log("Test runner version: " + testRunner.version + " -> " + VERSION)
testRunner.version = VERSION
fs.writeFileSync('test-runner/package.json', JSON.stringify(testRunner, null, 2) + '\n')

const perspectiveDiffSync = JSON.parse(fs.readFileSync('bootstrap-languages/p-diff-sync/package.json', 'utf8'))
console.log("Perspective diff sync: " + perspectiveDiffSync.version + " -> " + VERSION)
perspectiveDiffSync.version = VERSION
fs.writeFileSync('bootstrap-languages/p-diff-sync/package.json', JSON.stringify(perspectiveDiffSync, null, 2) + '\n')