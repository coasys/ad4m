import { Link, Literal } from "@coasys/ad4m";
import { invoke } from "@tauri-apps/api/core";
import { createContext, useContext, useState } from "react";
import { useNavigate } from "react-router-dom";
import { PREDICATE_FIRSTNAME, PREDICATE_LASTNAME, PREDICATE_USERNAME } from "../constants/triples";
import { Ad4minContext } from "./Ad4minContext";

type State = {
  loading: boolean;
  hasLoginError: Boolean;
};

type ContextProps = {
  state: State;
  methods: {
    unlockAgent: (str: string, holochain: boolean) => void;
    lockAgent: (str: string) => void;
    generateAgent: (password: string) => void;
    mutateAgent: (username: string, firstName: string, lastName: string) => void;
  };
};

const initialState: ContextProps = {
  state: {
    loading: false,
    hasLoginError: false,
  },
  methods: {
    unlockAgent: () => null,
    lockAgent: () => null,
    generateAgent: () => null,
    mutateAgent: () => null,
  },
};

export const AgentContext = createContext(initialState);

export function AgentProvider({ children }: any) {
  const {
    state: { client },
    methods: { handleLogin },
  } = useContext(Ad4minContext);
  let navigate = useNavigate();

  const [state, setState] = useState(initialState.state);

  const setLoading = (loading: boolean) => {
    setState((prev) => ({
      ...prev,
      loading,
    }));
  };

  const generateAgent = async (password: string) => {
    setLoading(true);
    console.log("Generating agent");

    let agentStatus = await client!.agent.generate(password);

    console.log("generate done with: ", agentStatus);

    console.log("agent status in generate: ", agentStatus);

    setLoading(false);

    await invoke("login_proxy", { subdomain: agentStatus.did! });
  };

  const mutateAgent = async (username: string, firstName: string, lastName: string) => {
    const agentStatus = await client!.agent.status();

    handleLogin(client!, agentStatus.isUnlocked, agentStatus.did!);

    await invoke("close_main_window");
    await invoke("open_tray_message");

    const additions = [];

    if (username) {
      additions.push(
        new Link({
          source: agentStatus.did!,
          target: Literal.from(username).toUrl(),
          predicate: PREDICATE_USERNAME,
        })
      );
    }

    if (firstName) {
      additions.push(
        new Link({
          source: agentStatus.did!,
          target: Literal.from(firstName).toUrl(),
          predicate: PREDICATE_FIRSTNAME,
        })
      );
    }

    if (lastName) {
      additions.push(
        new Link({
          source: agentStatus.did!,
          target: Literal.from(lastName).toUrl(),
          predicate: PREDICATE_LASTNAME,
        })
      );
    }

    console.log("mutating public perspective: ", additions);
    await client?.agent.mutatePublicPerspective({
      additions,
      removals: [],
    });

    navigate("/apps");
  };

  const unlockAgent = async (password: string, holochain: boolean) => {
    console.log("Holochain config:", holochain);
    setLoading(true);

    let agentStatus: AgentStatus | null = null;
    try {
      agentStatus = await client?.agent.unlock(password, holochain);
    } catch (error) {
      console.error("Error unlocking agent:", error);
      setState((prev) => ({ ...prev, hasLoginError: true }));
    }

    setLoading(false);

    if (agentStatus?.isUnlocked) {
      handleLogin(client!, agentStatus!.isUnlocked, agentStatus!.did!);
      console.log("agent status in unlock: ", agentStatus);
      await invoke("close_main_window");
      await invoke("open_tray_message");
      await invoke("login_proxy", { subdomain: agentStatus!.did });
      navigate("/apps");
    } else {
      setState((prev) => ({ ...prev, hasLoginError: true }));
    }
  };

  const lockAgent = async (passphrase: string) => {
    setLoading(true);

    const status = await client!.agent.lock(passphrase);

    handleLogin(client!, status!.isUnlocked, status!.did!);

    setLoading(false);
  };

  return (
    <AgentContext.Provider
      value={{
        state,
        methods: {
          generateAgent,
          unlockAgent,
          lockAgent,
          mutateAgent,
        },
      }}
    >
      {children}
    </AgentContext.Provider>
  );
}
