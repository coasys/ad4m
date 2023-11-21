import React from 'react';
import { WagmiConfig, createConfig, mainnet } from 'wagmi'
import { createPublicClient, http } from 'viem'
import { useAccount, useConnect, useDisconnect } from 'wagmi'
import { InjectedConnector } from 'wagmi/connectors/injected'
import { signMessage } from '@wagmi/core'
import Ad4mConnectUI from "@perspect3vism/ad4m-connect";
import { Ad4mClient } from '@perspect3vism/ad4m';

const config = createConfig({
  autoConnect: true,
  publicClient: createPublicClient({
    chain: mainnet,
    transport: http()
  }),
})

const ui = Ad4mConnectUI({
  appName: "ADAM 🔗 Web3",
  appDesc: "This is a builtin app to connect ADAM Agents with Web3 wallets.",
  appDomain: "ad4m.dev",
  appIconPath: "https://icons.getbootstrap.com/assets/icons/wallet.svg",
  capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
});

function App() {
  return (
    <div className="App">
      <header className="App-header">
        AD4M 🔗 Web3
      </header>
      <br/>
      <WagmiConfig config={config}><Profile/></WagmiConfig>
    </div>
  );
}
 
function Profile() {
  const { address, isConnected } = useAccount()
  const { connect } = useConnect({
    connector: new InjectedConnector(),
  })
  const { disconnect } = useDisconnect()
  const [ client, setClient] = React.useState<Ad4mClient | null>(null);

  const connectToAd4m = async () => {
    const client = await ui.connect();
    setClient(client);
  };

  const sign = async () => {
    if (client && address) {
      const me = (await client?.agent.me())!.did;
      const signature = await signMessage({
        message: me,
      });
      console.log("Got signature", signature);

      const entanglementPreFlight = await client.agent.entanglementProofPreFlight(address, "ethereum");
      //Add the signed did to the proof
      entanglementPreFlight.didSignedByDeviceKey = signature;
      //@ts-ignore
      delete entanglementPreFlight["__typename"];
      console.log("Sending", entanglementPreFlight);
      //Save proof into ad4m

      //@ts-ignore
      const addProof = await client.agent.addEntanglementProofs([entanglementPreFlight]);
      console.log("Added EP with result", addProof);
    }
  };

  if (client) {
    return (
      <div>
        <h3>Connected to AD4M</h3>
        <button className="button" onClick={() => sign()}>Sign</button>
      </div>
    )
  }
 
  if (isConnected)
    return (
      <div>
        Connected to {address}
        <button className="button" onClick={() => disconnect()}>Disconnect</button>
        <button className="button" onClick={() => connectToAd4m()}>Connect to AD4M</button>
      </div>
    )
  return <button className="button" onClick={() => connect()}>Connect</button>
}

export default App;
