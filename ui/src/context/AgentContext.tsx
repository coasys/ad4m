import { Link, Literal } from "@perspect3vism/ad4m";
import { invoke } from "@tauri-apps/api";
import { createContext, useContext, useState } from "react";
import { useNavigate } from "react-router-dom";
import { PREDICATE_FIRSTNAME, PREDICATE_LASTNAME, PREDICATE_USERNAME } from "../constants/triples";
import { Ad4minContext } from "./Ad4minContext";

type State = {
  loading: boolean;
  hasLoginError: Boolean;
}

type ContextProps = {
  state: State;
  methods: {
    unlockAgent: (str: string) => void,
    lockAgent: (str: string) => void,
    generateAgent: (username: string, firstName: string, lastName: string, password: string) => void,
  };
}

const initialState: ContextProps = {
  state: {
    loading: false,
    hasLoginError: false,
  },
  methods: {
    unlockAgent: () => null,
    lockAgent: () => null,
    generateAgent: () => null,
  }
}

export const AgentContext = createContext(initialState);


export function AgentProvider({ children }: any) {
  const {state: {
    client
  }, methods: {
    handleLogin
  }} = useContext(Ad4minContext);
  let navigate = useNavigate();

  const [state, setState] = useState(initialState.state);

  
  const setLoading = (loading: boolean) => {
    setState((prev) => ({
      ...prev,
      loading
    }))
  }

  const generateAgent = async (username: string, firstName: string, lastName: string, password: string) => {
    setLoading(true);

    let agentStatus = await client!.agent.generate(password);

    const additions = [];

    additions.push(
      new Link({
        source: agentStatus.did!,
        target: Literal.from(username).toUrl(),
        predicate: PREDICATE_USERNAME
      })
    );


    if (firstName) {
      additions.push(
        new Link({
          source: agentStatus.did!,
          target: Literal.from(firstName).toUrl(),
          predicate: PREDICATE_FIRSTNAME
        })
      );
    }

    if (lastName) {
      additions.push(
        new Link({
          source: agentStatus.did!,
          target: Literal.from(lastName).toUrl(),
          predicate: PREDICATE_LASTNAME
        })
      );
    }

    await client?.agent.mutatePublicPerspective({
      additions,
      removals: []
    })

    handleLogin(client!, agentStatus.isUnlocked, agentStatus.did!);

    console.log("agent status in generate: ", agentStatus);

    setLoading(false);

    await invoke('close_main_window');
    
    navigate('/profile');
  };

  const unlockAgent = async (password: string) => {
    setLoading(true)
    
    let agentStatus = await client?.agent.unlock(password);

    setLoading(false);

    if(agentStatus!.isUnlocked) {
      handleLogin(client!, agentStatus!.isUnlocked, agentStatus!.did!);
      console.log("agent status in unlock: ", agentStatus);
      await invoke('close_main_window');
      navigate('/settings');
    } else {
      setState((prev) => ({ ...prev, hasLoginError: true }));
    }

  }

  const lockAgent = async (passphrase: string) => {
    setLoading(true);

    const status = await client!.agent.lock(passphrase);

    handleLogin(client!, status!.isUnlocked, status!.did!);

    setLoading(false);
  } 

  return (
    <AgentContext.Provider 
      value={{
        state,
        methods: {
          generateAgent,
          unlockAgent,
          lockAgent,
        }
      }}
    >
      {children}
    </AgentContext.Provider>
  )
}
