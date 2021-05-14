import IPFS from "ipfs";

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
            ipnsPubsub: true
        }
    });
    const version = await node.version()

    console.log('IPFS Version:', version.version)

    return node
}