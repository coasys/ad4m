import React, { useEffect, useState } from "react";
import {
  WagmiConfig,
  createConfig,
  configureChains,
  mainnet,
  useConfig,
} from "wagmi";
import { useAccount, useConnect, useDisconnect } from "wagmi";
import { InjectedConnector } from "wagmi/connectors/injected";
import { signMessage } from "@wagmi/core";
// @ts-ignore
import { EntanglementProof } from "@coasys/ad4m";
import Logo from "./Logo";
import { publicProvider } from "wagmi/providers/public";
import ConnectAnimation from "./ConnectAnimation";
import {
  getProfileProofs,
  publishProofToProfile,
  removeProofFromProfile,
  shortenETHAddress,
} from "./helpers";
import { useAdamConnect } from "./helpers/useAd4mConnect";

const { chains, publicClient, webSocketPublicClient } = configureChains(
  [mainnet],
  [publicProvider()]
);

const config = createConfig({
  autoConnect: true,
  publicClient,
  connectors: [new InjectedConnector()],
  webSocketPublicClient,
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
  const [profileProofs, setProfileProofs] = useState<EntanglementProof[]>([]);
  const [proofs, setProofs] = useState<EntanglementProof[]>([]);
  const { connectors } = useConfig();
  const { address, isConnected, isConnecting } = useAccount();
  const {
    isConnected: isConnectedToADAM,
    connect: connectToADAM,
    hasConnectionError: hasADAMConnectionError,
    client,
    verificationRequired,
    verifyCode,
  } = useAdamConnect();

  const hasProved = proofs.some(
    (p: any) => p.deviceKey.toLowerCase() === address?.toLowerCase()
  );

  const { connect, isError: hasConnectionError } = useConnect({
    connector: connectors[0],
    onSuccess: () => {
      if (!hasProved) {
        sign();
      }
    },
  });

  function loadProfileProofs() {
    if (client) {
      getProfileProofs(client).then(setProfileProofs);
    }
  }

  useEffect(() => {
    if (client && proofs.length > 0) {
      loadProfileProofs();
    }
  }, [proofs.length, client]);

  useEffect(() => {
    if (client && isConnectedToADAM) {
      // @ts-ignore
      client.agent.getEntanglementProofs().then(setProofs);
    } else {
      setProofs([]);
    }
  }, [isConnectedToADAM]);

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
        // @ts-ignore
        setProofs(addProof);
      } catch (e) {
        console.log(e);
      } finally {
        setIsSigning(false);
      }
    }
  };

  function ViewComp() {
    if (hasADAMConnectionError) {
      return (
        <>
          <h1 className="title">Couldn't connect to ADAM</h1>
          <p className="paragraph">
            Make sure you have ADAM{" "}
            <a href="https://ad4m.dev/download" target="_blank">
              downloaded
            </a>{" "}
            and running.
          </p>
          <button className="button" onClick={() => connectToADAM()}>
            Try again
          </button>
        </>
      );
    }

    if (hasConnectionError) {
      return (
        <>
          <h1 className="title">Couldn't connect to wallet</h1>
          <p className="paragraph">
            Check if your Web3 wallet is installed and active.
          </p>
          <button className="button" onClick={() => connect()}>
            {isSigning ? "Connecting..." : "Try again"}
          </button>
        </>
      );
    }

    if (!isConnectedToADAM) {
      return (
        <>
          <h1 className="title">Prove your Web3 wallet ownership</h1>
          {verificationRequired ? (
            <>
              <input
                placeholder="Enter Verification Code"
                type="tel"
                className="input"
                onChange={(e) => {
                  if (e.target.value.length === 6) {
                    verifyCode(e.target.value);
                  }
                }}
              ></input>
              <button className="link" onClick={() => connectToADAM()}>
                Try again
              </button>
            </>
          ) : (
            <button className="button" onClick={() => connectToADAM()}>
              Connect to ADAM
            </button>
          )}
        </>
      );
    }

    if (isConnected && !hasProved)
      return (
        <>
          <h1 className="title">Prove wallet ownership</h1>
          <button className="button" onClick={() => sign()}>
            {isSigning ? "Signing..." : "Prove ownership"}
          </button>
        </>
      );

    // Map to dedupe array
    const seen = new Set<string>();

    return (
      <>
        <h1 className="title">
          {hasProved ? (
            <span>Successfully Verified!</span>
          ) : (
            <span>Almost There!</span>
          )}
        </h1>
        {!hasProved && (
          <button className="button" onClick={() => connect()}>
            {isSigning ? "Connecting..." : "Connect wallet"}
          </button>
        )}
        <div className="wallets">
          {proofs
            .filter((p: EntanglementProof) => {
              if (seen.has(p.deviceKey)) {
                return false;
              } else {
                seen.add(p.deviceKey);
                return true;
              }
            })
            .map((p: EntanglementProof) => {
              return (
                <div className="wallet-proof">
                  <span
                    style={{
                      width: "1.5rem",
                      height: "1.5rem",
                      display: "inline-block",
                    }}
                  >
                    <WalletIcon></WalletIcon>
                  </span>
                  <span className="wallet-address">
                    {shortenETHAddress(p.deviceKey)}
                  </span>
                  {profileProofs.some(
                    (profileProof) => profileProof.deviceKey === p.deviceKey
                  ) ? (
                    <button
                      onClick={() =>
                        client &&
                        removeProofFromProfile(client, p).then(
                          loadProfileProofs
                        )
                      }
                    >
                      Remove from profile
                    </button>
                  ) : (
                    <button
                      onClick={() =>
                        client &&
                        publishProofToProfile(client, p).then(loadProfileProofs)
                      }
                    >
                      Publish to profile
                    </button>
                  )}
                </div>
              );
            })}
        </div>
      </>
    );
  }

  const showConnecting = isSigning || isConnecting || verificationRequired;

  return (
    <div className="App">
      <div className="connect">
        <div className="block">
          {isConnectedToADAM && <span className="dot"></span>}
          <Logo></Logo>
        </div>
        <ConnectAnimation
          done={hasProved}
          connecting={showConnecting}
        ></ConnectAnimation>
        <div className="block">
          {isConnected && <span className="dot"></span>}
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
