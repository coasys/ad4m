import { invoke } from "@tauri-apps/api/core";
import { relaunch } from "@tauri-apps/plugin-process";
import { checkUpdate, installUpdate } from "@tauri-apps/plugin-updater";
import { useCallback, useContext, useEffect, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import { AgentContext } from "../context/AgentContext";
import { buildAd4mClient } from "../util";
import { fetchProfile } from "./Settings";

function Profile() {
  const {
    state: { loading },
    methods: { lockAgent },
  } = useContext(AgentContext);

  const {
    state: { url, did, isInitialized },
  } = useContext(Ad4minContext);

  const [password, setPassword] = useState("");
  const [showProfileInfo, setShowProfileInfo] = useState(false);
  const [lockAgentModalOpen, setLockAgentModalOpen] = useState(false);
  const [installUpdateModelOpen, setInstallUpdateModelOpen] = useState(false);
  const [showPassword, setShowPassword] = useState(false);
  const [profile, setProfile] = useState({
    firstName: "",
    lastName: "",
    username: "",
  });

  const fetchCurrentAgentProfile = useCallback(async () => {
    if (url) {
      const client = await buildAd4mClient(url, false);
      const agent = await client!.agent.me();

      const profile = await fetchProfile(agent);

      setProfile(profile);
    }
  }, [url]);

  // @ts-ignore
  const onKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === "Enter") {
      if (isInitialized) {
        lockAgent(password);
      }
    }
  };

  useEffect(() => {
    fetchCurrentAgentProfile();
  }, [fetchCurrentAgentProfile]);

  const onPasswordChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    event.preventDefault();
    let { value } = event.target;
    setPassword(value);
  };

  const onCheckUpdate = async () => {
    try {
      const { shouldUpdate, manifest } = await checkUpdate();
      if (shouldUpdate) {
        setInstallUpdateModelOpen(true);
      }
    } catch (error) {
      console.log(error);
    }
  };

  const onInstallUpdate = async () => {
    try {
      await installUpdate();

      await relaunch();
    } catch (error) {
      console.log(error);
    }
  };

  const onOpenDApp = async () => {
    try {
      await invoke("open_dapp");
    } catch (error) {
      console.log(error);
    }
  };

  return (
    <div>
      <j-popover placement="bottom">
        <j-button slot="trigger" variant="ghost" size="sm">
          <j-flex a="center">
            {profile.username}
            <j-box p="200"></j-box>
            <j-icon size="xs" name="chevron-down"></j-icon>
          </j-flex>
        </j-button>
        <j-menu slot="content">
          <j-menu-item onClick={() => setLockAgentModalOpen(true)}>
            Lock Agent
            <j-icon size="xs" slot="start" name="lock"></j-icon>
          </j-menu-item>
          <j-menu-item onClick={() => onCheckUpdate()}>
            Check updates
            <j-icon size="xs" slot="start" name="arrow-repeat"></j-icon>
          </j-menu-item>
          <j-menu-item onClick={() => onOpenDApp()}>
            Connect Web3
            <j-icon size="xs" slot="start" name="link"></j-icon>
          </j-menu-item>
          <j-menu-item onClick={() => setShowProfileInfo(true)}>
            Profile details
            <j-icon size="xs" slot="start" name="info-circle"></j-icon>
          </j-menu-item>
          <j-menu-item onClick={() => invoke("close_application")}>
            Poweroff Agent
            <j-icon size="xs" slot="start" name="x-circle"></j-icon>
          </j-menu-item>
        </j-menu>
      </j-popover>
      {lockAgentModalOpen && (
        <j-modal
          size="fullscreen"
          open={lockAgentModalOpen}
          onToggle={(e: any) => setLockAgentModalOpen(e.target.open)}
        >
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Lock Agent
              </j-text>
            </j-box>
            <j-box p="200"></j-box>
            <j-input
              placeholder="Password"
              label="Input your passphrase"
              type={showPassword ? "text" : "password"}
              size="lg"
              required
              onInput={onPasswordChange}
              onKeyDown={onKeyDown}
              autovalidate
            >
              <j-button onClick={() => setShowPassword(!showPassword)} slot="end" variant="link" square>
                <j-icon name={showPassword ? "eye-slash" : "eye"} size="sm"></j-icon>
              </j-button>
            </j-input>
            <j-box p="200"></j-box>
            <j-button variant="primary" onClick={() => lockAgent(password)} loading={loading}>
              Lock agent
            </j-button>
          </j-box>
        </j-modal>
      )}
      {showProfileInfo && (
        <j-modal size="fullscreen" open={showProfileInfo} onToggle={(e: any) => setShowProfileInfo(e.target.open)}>
          <j-box px="400" py="600">
            <j-box pb="900">
              <j-text nomargin color="black" size="600" weight="600">
                Profile details
              </j-text>
            </j-box>

            <j-box>
              <j-text variant="label">Agent DID</j-text>
              <j-text wrap>{did}</j-text>
            </j-box>

            <j-box>
              <j-text variant="label">Username</j-text>
              <j-text>{profile?.username || "No username"}</j-text>
            </j-box>

            <j-box>
              <j-text variant="label">Name</j-text>
              <j-text>{`${profile.firstName} ${profile.lastName}`}</j-text>
            </j-box>

            <j-button variant="secondary" onClick={() => setShowProfileInfo(false)}>
              Close
            </j-button>
          </j-box>
        </j-modal>
      )}
      {installUpdateModelOpen && (
        <j-modal
          size="fullscreen"
          open={installUpdateModelOpen}
          onToggle={(e: any) => setInstallUpdateModelOpen(e.target.open)}
        >
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Install Update
              </j-text>
              <j-text>Warning: App will restart once the update is installed.</j-text>
            </j-box>
            <j-box p="200"></j-box>
            <j-button variant="primary" onClick={() => onInstallUpdate()} loading={loading}>
              Install
            </j-button>
          </j-box>
        </j-modal>
      )}
    </div>
  );
}

export default Profile;
