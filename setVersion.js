const fs = require('fs')

function replaceVersionLine(content, version) {
    const lines = content.split('\n')
    const versionLineIndex = lines.findIndex(line => line.startsWith('version = '))
    const oldVersion = lines[versionLineIndex].split('"')[1]
    const newVersionLine = 'version = "' + version + '"'
    lines[versionLineIndex] = newVersionLine
    const newContent = lines.join('\n')
    return { oldVersion, newContent }
}

const VERSION = process.argv[2]
console.log("Setting all sub-project versions to: " + VERSION)

const cli = replaceVersionLine(fs.readFileSync('cli/Cargo.toml', 'utf8'), VERSION)
console.log("CLI version: " + cli.oldVersion + " -> " + VERSION)
fs.writeFileSync('cli/Cargo.toml', cli.newContent)

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
