const IPFS = require('ipfs')
const { app, BrowserWindow, ipcMain } = require('electron')

const _appendBuffer = (buffer1, buffer2) => {
    const tmp = new Uint8Array(buffer1.byteLength + buffer2.byteLength);
    tmp.set(new Uint8Array(buffer1), 0);
    tmp.set(new Uint8Array(buffer2), buffer1.byteLength);
    return tmp.buffer;
};

const uint8ArrayConcat = (chunks) => {
    return chunks.reduce(_appendBuffer)
}

export async function init () {
    const node = await IPFS.create({
        EXPERIMENTAL: {
            pubsub: true
        }
    })
    const version = await node.version()

    console.log('Version:', version.version)

    ipcMain.handle('ipfs-add', async (event, data) => {
        const fileAdded = await node.add(data)
        console.debug('IPFS: Added file:', fileAdded.path, fileAdded.cid)
        return fileAdded
    })

    ipcMain.handle('ipfs-cat', async (event, cid) => {
        const chunks = []
        for await (const chunk of node.cat(cid)) {
            chunks.push(chunk)
        }

        const fileString = uint8ArrayConcat(chunks).toString();
        console.debug('IPFS: Read file contents:', fileString)
        return fileString
    })

    return node
}