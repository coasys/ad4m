import React, { useEffect, useState } from "react";
import { WagmiConfig, createConfig, configureChains, mainnet } from "wagmi";
import { useAccount, useConnect, useDisconnect } from "wagmi";
import { InjectedConnector } from "wagmi/connectors/injected";
import { signMessage } from "@wagmi/core";
import Ad4mConnectUI from "@perspect3vism/ad4m-connect";
import { Ad4mClient } from "@perspect3vism/ad4m";
import Logo from "./Logo";
import { publicProvider } from "wagmi/providers/public";
import ConnectAnimation from "./ConnectAnimation";

const { chains, publicClient, webSocketPublicClient } = configureChains(
  [mainnet],
  [publicProvider()]
);

const config = createConfig({
  autoConnect: true,
  publicClient,
  webSocketPublicClient,
});

const ui = Ad4mConnectUI({
  appName: "ADAM 🔗 Web3",
  appDesc: "This is a builtin app to connect ADAM Agents with Web3 wallets.",
  appDomain: "ad4m.dev",
  appIconPath: "https://icons.getbootstrap.com/assets/icons/wallet.svg",
  capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
});

function App() {
  return (
    <WagmiConfig config={config}>
      <Main />
    </WagmiConfig>
  );
}

function Main() {
  const [isSigning, setIsSigning] = useState(false);
  const [proofs, setProofs] = useState([]);
  const { address, isConnected, isConnecting } = useAccount();

  const hasProved = proofs.some(
    (p: any) => p.deviceKey.toLowerCase() === address?.toLowerCase()
  );

  const [client, setClient] = React.useState<Ad4mClient | null>(null);

  const { connect, isError: hasConnectionError } = useConnect({
    connector: new InjectedConnector(),
    onSuccess: () => {
      if (!hasProved) {
        sign();
      }
    },
  });

  useEffect(() => {
    ui.connect();
  }, []);

  useEffect(() => {
    async function cb(e: any) {
      if (e.detail === "authenticated") {
        const client = ui.getAd4mClient();
        setClient(client);

        const proofs: any = await client.agent.getEntanglementProofs();

        setProofs(proofs);

        connect();
      }
    }

    ui.addEventListener("authstatechange", cb);

    return () => {
      ui.removeEventListener("authstatechange", cb);
    };
  }, [address]);

  const sign = async () => {
    if (client && address) {
      setIsSigning(true);
      try {
        const { did } = await client.agent.me();
        const signature = await signMessage({
          message: did,
        });

        client.agent.getEntanglementProofs();

        const entanglementPreFlight =
          await client.agent.entanglementProofPreFlight(address, "ethereum");

        entanglementPreFlight.didSignedByDeviceKey = signature;

        const addProof = await client.agent.addEntanglementProofs([
          //@ts-ignore
          entanglementPreFlight,
        ]);

        setHasProved(true);
      } catch (e) {
        console.log(e);
      } finally {
        setIsSigning(false);
      }
    }
  };

  function ViewComp() {
    if (hasConnectionError) {
      return (
        <>
          <h1 className="title">Couldn't connect to wallet!</h1>
          <p className="paragraph">Are you sure you have a wallet installed?</p>
          <button className="button" onClick={() => connect()}>
            {isSigning ? "Connecting..." : "Try again"}
          </button>
        </>
      );
    }

    if (hasProved)
      return (
        <>
          <h1 className="title">Ownership proved!</h1>
          <p className="paragraph">
            Your wallet ownership is proved and connected with ADAM.
          </p>
        </>
      );

    if (isConnected)
      return (
        <>
          <h1 className="title">Sign this message to prove wallet ownership</h1>
          <button className="button" onClick={() => sign()}>
            {isSigning ? "Signing..." : "Prove ownership"}
          </button>
        </>
      );

    return (
      <>
        <h1 className="title">
          <span className="accent">ADAM</span> wants to connect to your Web3
          wallet
        </h1>
        <button className="button" onClick={() => connect()}>
          {isSigning ? "Connecting..." : "Connect wallet"}
        </button>
      </>
    );
  }

  let viewState = "ready";
  if (isConnected) viewState = "sign";
  if (hasProved) viewState = "done";

  return (
    <div className="App">
      <div className="connect">
        <div className="block">
          <Logo></Logo>
        </div>
        <ConnectAnimation
          done={hasProved}
          connecting={isSigning || isConnecting}
        ></ConnectAnimation>
        <div className="block">
          <WalletIcon></WalletIcon>
        </div>
      </div>
      <ViewComp></ViewComp>
    </div>
  );
}

function WalletIcon() {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      fill="currentColor"
      viewBox="0 0 16 16"
    >
      <path
        fill="url(#gradient)"
        d="M0 3a2 2 0 0 1 2-2h13.5a.5.5 0 0 1 0 1H15v2a1 1 0 0 1 1 1v8.5a1.5 1.5 0 0 1-1.5 1.5h-12A2.5 2.5 0 0 1 0 12.5zm1 1.732V12.5A1.5 1.5 0 0 0 2.5 14h12a.5.5 0 0 0 .5-.5V5H2a1.99 1.99 0 0 1-1-.268M1 3a1 1 0 0 0 1 1h12V2H2a1 1 0 0 0-1 1"
      />
    </svg>
  );
}

export default App;
