import { useContext, useEffect } from "react";
import { Outlet, useLocation, useNavigate } from "react-router-dom";
import PackageInfo from "../../package.json";
import { Ad4minContext } from "../context/Ad4minContext";
import { AgentProvider } from "../context/AgentContext";
import Logo from "./Logo";
import Profile from "./Profile";

type Props = {
  did: String;
  opened: boolean;
  setOpened: (val: boolean) => void;
};

const Navigation = ({ did, opened, setOpened }: Props) => {
  const {
    state: { connected, isUnlocked, expertMode, connectedLoading },
  } = useContext(Ad4minContext);

  let navigate = useNavigate();
  let location = useLocation();

  useEffect(() => {
    if (!connected && !connectedLoading) {
      navigate("/connect");
    } else if (connected && !isUnlocked) {
      navigate("/login");
    }
  }, [connected, isUnlocked, navigate, connectedLoading]);

  return (
    <>
      <j-box px="500" py="400">
        <j-flex a="center" j="between">
          <j-flex a="center" gap="400">
            <a href="https://ad4m.dev" target="_blank">
              <Logo style={{ height: 40, width: 40 }} />
            </a>
            <j-text variant="caption">v{PackageInfo.version}</j-text>
          </j-flex>
          <AgentProvider>
            <Profile />
          </AgentProvider>
        </j-flex>
      </j-box>

      <j-tabs
        value={location.pathname}
        onChange={(e) => navigate(e.target.value)}
        variant="tab"
      >
        <j-tab-item value="/apps">Apps</j-tab-item>
        <j-tab-item value="/perspective">Perspectives</j-tab-item>
        <j-tab-item value="/ai">AI</j-tab-item>
        <j-tab-item value="/settings">Settings</j-tab-item>
        {expertMode && <j-tab-item value="/language">Languages</j-tab-item>}
      </j-tabs>

      <Outlet />
    </>
  );
};

export default Navigation;
