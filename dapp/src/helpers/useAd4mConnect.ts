import { Ad4mClient } from "@perspect3vism/ad4m";
import { useState, useEffect } from "react";
// @ts-ignore
import Ad4mConnect from "@perspect3vism/ad4m-connect/core";

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

export function useAdamConnect(options?: ConnectOptions) {
  const [client, setClient] = useState<Ad4mClient | null>(null);
  const [hasConnectionError, setConnectionError] = useState(false);
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
    try {
      setConnectionError(false);

      const client = await ad4mConnect.connect();

      if (!client) {
        setConnectionError(true);
      } else {
        setClient(client);
        if (ad4mConnect.authState !== "authenticated") {
          requestCapability();
        }
      }
    } catch (e: any) {
      setConnectionError(true);
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
    hasConnectionError,
    verificationRequired,
    verifyCode,
    isConnected,
    isLocked,
  };
}
