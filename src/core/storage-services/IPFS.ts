import IPFS from "ipfs";

const _appendBuffer = (buffer1: Uint8Array, buffer2: Uint8Array) => {
    const tmp = new Uint8Array(buffer1.byteLength + buffer2.byteLength);
    tmp.set(new Uint8Array(buffer1), 0);
    tmp.set(new Uint8Array(buffer2), buffer1.byteLength);
    return tmp.buffer;
};

const uint8ArrayConcat = (chunks: Uint8Array) => {
    //@ts-ignore
    return chunks.reduce(_appendBuffer)
}

export async function init (swarmPort?: number, repo?: string) {
    if(!swarmPort) swarmPort = 4002

    const node = await IPFS.create({
        EXPERIMENTAL: {
            ipnsPubsub: true
        },
        repo,
        config: {
            Addresses: {
                Swarm: [
                    `/ip4/0.0.0.0/tcp/${swarmPort}`,
                    `/ip4/127.0.0.1/tcp/${swarmPort + 1}/ws`
                ]
            },
            Discovery: {
                MDNS: {
                    Enabled: true,
                    Interval: 10,
                },
                webRTCStar: {
                    Enabled: true
                }
            }
        }
    });
    const version = await node.version()

    console.log('IPFS Version:', version.version)

    return node
}