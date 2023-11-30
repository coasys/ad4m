import React, { useEffect, useState } from "react";
import { WagmiConfig, createConfig, configureChains, mainnet } from "wagmi";
import { useAccount, useConnect, useDisconnect } from "wagmi";
import { InjectedConnector } from "wagmi/connectors/injected";
import { signMessage } from "@wagmi/core";
// @ts-ignore
import Ad4mConnect from "@perspect3vism/ad4m-connect/core";
import { Ad4mClient, EntanglementProof } from "@perspect3vism/ad4m";
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

const ad4mConnect = new Ad4mConnect({
  appName: "ADAM 🔗 Web3",
  appDesc: "This is a builtin app to connect ADAM Agents with Web3 wallets.",
  appDomain: "ad4m.dev",
  appIconPath: "https://icons.getbootstrap.com/assets/icons/wallet.svg",
  capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
  token: localStorage.getItem("token") || "",
});

type ConnectOptions = {
  autoConnect?: boolean;
};

function useAdamConnect(options?: ConnectOptions) {
  const [client, setClient] = useState<Ad4mClient | null>(null);
  const [isLocked, setIsLocked] = useState(false);
  const [isConnected, setIsConnected] = useState(false);
  const [verificationRequired, setVerificationRequired] = useState(false);

  useEffect(() => {
    ad4mConnect.on("authstatechange", (event: any) => {
      if (event === "authenticated") {
        setIsConnected(true);
        setIsLocked(false);
      }
      if (event === "locked") {
        setIsLocked(true);
      }
      if (event === "unauthenticated") {
        setIsConnected(false);
        setIsLocked(false);
      }
    });
  }, []);

  useEffect(() => {
    if (options?.autoConnect) {
      connect();
    }
  }, []);

  async function connect() {
    const client = await ad4mConnect.connect();
    setClient(client);
    if (ad4mConnect.authState !== "authenticated") {
      requestCapability();
    }
  }

  async function requestCapability() {
    await ad4mConnect.requestCapability(true);
    setVerificationRequired(true);
  }

  async function verifyCode(code: string) {
    const jwt = await ad4mConnect.verifyCode(code);
    localStorage.setItem("token", jwt);
    setVerificationRequired(false);
  }

  return {
    connect,
    client,
    verificationRequired,
    verifyCode,
    isConnected,
    isLocked,
  };
}

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
  const { disconnect } = useDisconnect();
  const { address, isConnected, isConnecting } = useAccount();
  const {
    isConnected: isConnectedToADAM,
    connect: connectToADAM,
    client,
    verificationRequired,
    verifyCode,
  } = useAdamConnect();

  const hasProved = proofs.some(
    (p: any) => p.deviceKey.toLowerCase() === address?.toLowerCase()
  );

  const { connect, isError: hasConnectionError } = useConnect({
    connector: new InjectedConnector(),
    onSuccess: () => {
      if (!hasProved) {
        sign();
      }
    },
  });

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
            <span>Successfully Verified</span>
          ) : (
            <span>Almost There!</span>
          )}
        </h1>
        {!hasProved && (
          <button className="button" onClick={() => connect()}>
            {isSigning ? "Connecting..." : "Connect wallet"}
          </button>
        )}
        {proofs.length > 0 && <p>Verified Wallets:</p>}
        <p>
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
                  {shortenETHAddress(p.deviceKey)}
                  {p.deviceKey === address && (
                    <span
                      className="dot"
                      style={{
                        right: "-4px",
                      }}
                    ></span>
                  )}
                </div>
              );
            })}
        </p>
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

function CheckIcon() {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      fill="currentColor"
      width="16"
      height="16"
      viewBox="0 0 16 16"
    >
      <path d="M12.736 3.97a.733.733 0 0 1 1.047 0c.286.289.29.756.01 1.05L7.88 12.01a.733.733 0 0 1-1.065.02L3.217 8.384a.757.757 0 0 1 0-1.06.733.733 0 0 1 1.047 0l3.052 3.093 5.4-6.425a.247.247 0 0 1 .02-.022" />
    </svg>
  );
}

function shortenETHAddress(address: string) {
  if (!address || address.length !== 42 || !address.startsWith("0x")) {
    return "Invalid ETH Address";
  }
  return `${address.substring(0, 8)}...${address.substring(
    address.length - 4
  )}`;
}

export default App;
