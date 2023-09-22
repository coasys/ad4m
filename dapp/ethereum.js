import detectEthereumProvider from '@metamask/detect-provider';

export async function startEthereum(handleAccountsChanged) {
    const provider = await detectEthereumProvider();
    if(!provider) {
        console.log('Please install MetaMask!')
        setTimeout(startEthereum, 1000)
    } else {
        // If the provider returned by detectEthereumProvider is not the same as
        // window.ethereum, something is overwriting it, perhaps another wallet.
        if (provider !== window.ethereum) {
            console.error('Do you have multiple wallets installed?');
        }
        // Access the decentralized web!

        window.ethereum.on('chainChanged', window.location.reload)

        ethereum
            .request({ method: 'eth_accounts' })
            .then(handleAccountsChanged)
            .catch((err) => {
                // Some unexpected error.
                // For backwards compatibility reasons, if no accounts are available,
                // eth_accounts will return an empty array.
                console.error(err);
            });

        // Note that this event is emitted on page load.
        // If the array of accounts is non-empty, you're already
        // connected.
        ethereum.on('accountsChanged', handleAccountsChanged);
    }
}

const domain = [
    { name: "name", type: "string" },
    { name: "version", type: "string" },
    { name: "chainId", type: "uint256" },
];

const didEntangle = [
    { name: "did", type: "string" }
];

export async function sign(signer, did) {
    const domainData = {
        name: "Ad4m DID Authentification",
        version: "2",
        //TODO: infer env from process.env and then use correct chainId; ropstein or eth
        chainId: 1,
    };

    var message = {
        did: did,
    };

    const data = {
        types: {
            EIP712Domain: domain,
            DIDEntangle: didEntangle,
        },
        domain: domainData,
        primaryType: "DIDEntangle",
        message: message
    };

    let req = {
        method: "eth_signTypedData_v3",
        params: [signer, JSON.stringify(data)],
        from: signer
    }
    console.log('sign: sending request ' + JSON.stringify(req))
    const result = await ethereum.request(req)

    console.log('sign: response received ' + result)

    const signature = result.substring(2);
    const r = "0x" + signature.substring(0, 64);
    const s = "0x" + signature.substring(64, 128);
    const v = parseInt(signature.substring(128, 130), 16);

    return { data, signature, r, s, v }
}

export function connectWallet(handleAccountsChanged) {
    ethereum
        .request({ method: 'eth_requestAccounts' })
        .then(handleAccountsChanged)
        .catch((err) => {
            if (err.code === 4001) {
                // EIP-1193 userRejectedRequest error
                // If this happens, the user rejected the connection request.
                console.log('Please connect to MetaMask.');
            } else {
                console.error(err);
            }
        });
}