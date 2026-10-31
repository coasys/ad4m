import { invoke } from "@tauri-apps/api/core";
import { useContext, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Ad4minContext } from "../context/Ad4minContext";
import { AgentContext } from "../context/AgentContext";
import "../index.css";
import Logo from "./Logo";
import AISetup from "./AISetup";

const Login = () => {
  const {
    state: { loading, hasLoginError },
    methods: { generateAgent, unlockAgent, mutateAgent },
  } = useContext(AgentContext);

  const {
    state: { isInitialized, isUnlocked, connected, connectedLoading, client },
    methods: { resetEndpoint },
  } = useContext(Ad4minContext);

  let navigate = useNavigate();

  const [currentIndex, setCurrentIndex] = useState(0);
  const [password, setPassword] = useState("");
  const [confirmPassword, setConfirmPassword] = useState("");
  const [showPassword, setShowPassword] = useState(false);
  const [showConfirmPassword, setShowConfirmPassword] = useState(false);
  const [passwordError, setPasswordError] = useState<string | null>(null);
  const [failedAttempts, setFailedAttempts] = useState(0);
  const [username, setUsername] = useState("");
  const [firstName, setFirstName] = useState("");
  const [lastName, setLastName] = useState("");
  const [clearAgentModalOpen, setClearAgentModalOpen] = useState(false);
  const [showAgentPicker, setShowAgentPicker] = useState(false);
  const [pendingSwitchAgent, setPendingSwitchAgent] = useState<any>(null);
  const [appState, setAppState] = useState<any>(null);
  const [holochain, setHolochain] = useState(true);
  const [aiReady, setAIReady] = useState(false);

  if (hasLoginError) setPasswordError("Invalid password");

  async function loadAppState() {
    try {
      const result = await invoke("get_app_agent_list");
      if (result) {
        setAppState(JSON.parse(result as string));
      }
    } catch (e) {
      console.error("Failed to load app state:", e);
    }
  }

  async function switchAgent(agent: any) {
    await invoke("set_selected_agent", { agent });
    await invoke("close_application");
  }

  async function clearAgent() {
    let agentStatus = await client?.agent.status();
    if (!agentStatus?.isUnlocked) await invoke("clear_state");
  }

  function passwordValid(): boolean {
    if (password.length === 0) {
      setPasswordError("Password is required");
      return false;
    }
    if (password !== confirmPassword) {
      setPasswordError("Passwords do not match");
      return false;
    }
    setPasswordError(null);
    return true;
  }

  useEffect(() => {
    loadAppState();
  }, []);

  useEffect(() => {
    if (!connected && !connectedLoading) {
      navigate("/connect");
    } else if (connected && isUnlocked) {
      navigate("/apps");
    } else if (isInitialized && currentIndex === 0) {
      setCurrentIndex(7);
    }
  }, [connected, isUnlocked, navigate, isInitialized, connectedLoading, currentIndex]);

  const handlePasswordSubmit = () => {
    if (passwordValid()) {
      generateAgent(password);
      setCurrentIndex(3);
    }
  };

  return (
    <div className="wrapper">
      {currentIndex === 0 && (
        <div className="page">
          <Logo style={{ marginBottom: 50 }} />

          <j-flex
            direction="column"
            gap="300"
            style={{ textAlign: "center", marginBottom: 50 }}
          >
            <j-text size="900" weight="600" nomargin color="ui-0">
              ADAM Layer
            </j-text>
            <j-text
              size="800"
              nomargin
              color="ui-900"
              style={{ textWrap: "wrap" }}
            >
              A social layer for the <i>Wise Web</i>
            </j-text>
          </j-flex>

          <j-button
            size="xl"
            onClick={() => setCurrentIndex(1)}
            variant="primary"
          >
            Get Started
          </j-button>

        </div>
      )}

      {currentIndex === 1 && (
        <div className="page">
          <Logo style={{ marginBottom: 50 }} />

          <j-flex
            direction="column"
            a="center"
            gap="400"
            style={{ marginBottom: 30 }}
          >
            <j-icon name="house-lock" size="xl" color="ui-700" />
            <j-text size="800" nomargin color="ui-0">
              Privacy and Security
            </j-text>
          </j-flex>

          <j-flex
            direction="column"
            a="center"
            gap="400"
            style={{ textAlign: "center", width: "100%", maxWidth: 500 }}
          >
            <j-text size="600" nomargin color="ui-900">
              ADAM generates keys on your device, so only you have access to
              your account and data.
            </j-text>
            <j-text size="600" nomargin color="ui-900">
              We will ask for a password used to encrypt your local keys. Don't
              forget it! There is no way to recover it.
            </j-text>
          </j-flex>

          <j-flex gap="500" j="center" wrap style={{ marginTop: 70 }}>
            <j-button size="xl" onClick={() => setCurrentIndex(0)}>
              Previous
            </j-button>
            <j-button
              variant="primary"
              size="xl"
              onClick={() => setCurrentIndex(2)}
            >
              Next
            </j-button>
          </j-flex>
        </div>
      )}

      {currentIndex === 2 && (
        <div className="page">
          <Logo style={{ marginBottom: 40 }} />

          <j-flex
            direction="column"
            a="center"
            gap="300"
            style={{ marginBottom: 40 }}
          >
            <j-icon name="key" size="xl" color="ui-700" />
            <j-text size="800" nomargin color="ui-0">
              Create your encryption passphrase
            </j-text>
          </j-flex>

          <j-flex
            direction="column"
            a="center"
            gap="400"
            style={{ textAlign: "center", width: "100%", maxWidth: 500, marginBottom: 30 }}
          >
            <j-text size="600" nomargin color="ui-900">
              This passphrase will be used to encrypt your local keys. If you lose it, you will lose access to your agent and will need to create a new one.
            </j-text>
            <j-text size="600" nomargin color="ui-900">
              Please choose a strong passphrase and store it securely.
            </j-text>
          </j-flex>

          <j-flex direction="column" gap="400" style={{ width: "100%" }}>
            <j-input
              size="lg"
              placeholder="Enter passphrase..."
              minlength={10}
              maxlength={30}
              autovalidate
              required
              type={showPassword ? "text" : "password"}
              onInput={(e: any) => setPassword(e.target.value)}
              onKeyDown={(e: any) => {
                if (e.key === "Enter") {
                  handlePasswordSubmit();
                }
              }}
              style={{ width: "100%" }}
            >
              <j-button
                onClick={() => setShowPassword(!showPassword)}
                slot="end"
                variant="subtle"
                square
                style={{ marginRight: -13 }}
                tabIndex={-1}
              >
                <j-icon name={showPassword ? "eye-slash" : "eye"} size="sm" />
              </j-button>
            </j-input>

            <j-input
              size="lg"
              placeholder="Confirm passphrase..."
              minlength={10}
              maxlength={30}
              autovalidate
              required
              type={showConfirmPassword ? "text" : "password"}
              onInput={(e: any) => setConfirmPassword(e.target.value)}
              onKeyDown={(e: any) => {
                if (e.key === "Enter") {
                  handlePasswordSubmit();
                }
              }}
              style={{ width: "100%" }}
            >
              <j-button
                onClick={() => setShowConfirmPassword(!showConfirmPassword)}
                slot="end"
                variant="subtle"
                square
                style={{ marginRight: -13 }}
                tabIndex={-1}
              >
                <j-icon name={showConfirmPassword ? "eye-slash" : "eye"} size="sm" />
              </j-button>
            </j-input>

            {passwordError && (
              <j-text color="danger-500" nomargin>
                {passwordError}
              </j-text>
            )}
          </j-flex>

          <j-flex gap="500" j="center" wrap style={{ marginTop: 70 }}>
            <j-button size="xl" onClick={() => setCurrentIndex(1)}>
              Previous
            </j-button>
            <j-button
              variant="primary"
              size="xl"
              onClick={handlePasswordSubmit}
              loading={loading}
              disabled={password.length === 0 || confirmPassword.length === 0}
            >
              Next
            </j-button>
          </j-flex>
        </div>
      )}

      {currentIndex === 3 && (
        <div className="page">
          <Logo style={{ marginBottom: 50 }} />

          <j-flex
            direction="column"
            a="center"
            gap="300"
            style={{ marginBottom: 30 }}
          >
            <j-icon name="volume-up" size="xl" color="ui-700" />
            <j-text size="800" nomargin color="ui-0">
              Censorship free
            </j-text>
          </j-flex>

          <j-flex
            direction="column"
            a="center"
            gap="400"
            style={{ textAlign: "center", width: "100%", maxWidth: 500 }}
          >
            <j-text size="600" nomargin color="ui-900">
              ADAM allows you to express yourself without fear of censorship or
              suppression. You can share your thoughts and opinions without
              depending on a central authority or a particular app.
            </j-text>
            <j-text size="600" nomargin color="ui-900">
              That includes and starts with your personal profile. In the next
              step you can add optional information about yourself that ADAM
              will make available publicly to other users through any ADAM app.
            </j-text>
          </j-flex>

          <j-flex gap="500" j="center" wrap style={{ marginTop: 60 }}>
            {/* <j-button size="xl" onClick={() => setCurrentIndex(3)}>
              Previous
            </j-button> */}
            <j-button
              variant="primary"
              size="xl"
              onClick={() => setCurrentIndex(4)}
            >
              Next
            </j-button>
          </j-flex>
        </div>
      )}

      {currentIndex === 4 && (
        <div className="page" style={{ width: "100%", maxWidth: 350 }}>
          <Logo style={{ marginBottom: 40 }} />

          <j-flex
            direction="column"
            a="center"
            gap="300"
            style={{ marginBottom: 30 }}
          >
            <j-icon name="person" size="xl" color="ui-700" />
            <j-text size="800" nomargin color="ui-0">
              Personal profile
            </j-text>
          </j-flex>

          <j-flex direction="column" gap="500" style={{ width: "100%" }}>
            <j-input
              full
              autofocus
              size="lg"
              label="Username (optional but recommended)"
              minlength={10}
              maxlength={30}
              autovalidate
              type="text"
              onInput={(e: any) => setUsername(e.target.value)}
            />
            <j-input
              full
              autofocus
              size="lg"
              label="First name (optional)"
              minlength={10}
              maxlength={30}
              autovalidate
              type="text"
              onInput={(e: any) => setFirstName(e.target.value)}
            />
            <j-input
              full
              size="lg"
              label="Last name (optional)"
              minlength={10}
              maxlength={30}
              autovalidate
              type="text"
              onInput={(e: any) => setLastName(e.target.value)}
            />
          </j-flex>

          <j-flex gap="500" j="center" wrap style={{ marginTop: 70 }}>
            <j-button size="xl" onClick={() => setCurrentIndex(3)}>
              Previous
            </j-button>
            <j-button
              variant="primary"
              size="xl"
              onClick={() => setCurrentIndex(5)}
            >
              Next
            </j-button>
          </j-flex>
        </div>
      )}

      {currentIndex === 5 && (
        <div className="page">
          <Logo style={{ marginBottom: 40 }} />

          <j-flex direction="column" gap="400" style={{ marginBottom: 30 }}>
            <j-icon name="robot" size="xl" color="ui-700" />
            <j-text size="800" nomargin color="ui-0">
              AI Settings
            </j-text>
          </j-flex>

          <j-flex
            direction="column"
            a="center"
            gap="400"
            style={{
              textAlign: "center",
              width: "100%",
              maxWidth: 570,
              marginBottom: 40,
            }}
          >
            <j-text size="800" nomargin color="ui-900">
              Where should your AI models run?
            </j-text>
          </j-flex>

          {client && <AISetup client={client} onReady={setAIReady} />}

          <j-flex gap="500" j="center" wrap style={{ marginTop: 60 }}>
            <j-button size="xl" onClick={() => setCurrentIndex(4)}>
              Previous
            </j-button>
            <j-button
              variant="primary"
              size="xl"
              onClick={() => setCurrentIndex(6)}
              disabled={!aiReady}
            >
              Next
            </j-button>
          </j-flex>
        </div>
      )}

      {currentIndex === 6 && (
        <div className="page">
          <Logo style={{ marginBottom: 40 }} />

          <j-flex direction="column" gap="400" style={{ marginBottom: 30 }}>
            <j-icon name="person-check-fill" size="xl" color="ui-700" />
            <j-text size="800" nomargin color="ui-0">
              Agent centric
            </j-text>
          </j-flex>

          <j-flex
            direction="column"
            a="center"
            gap="400"
            style={{
              textAlign: "center",
              width: "100%",
              maxWidth: 500,
            }}
          >
            <j-text size="600" nomargin color="ui-900">
              With ADAM you own your data and decide what apps get to use it. No
              more app silos with you as the central authority.
            </j-text>
            <j-text size="600" nomargin color="ui-900">
              Once agent generation is done, ADAM will run on your device, in
              the background. Open an ADAM app, like{" "}
              <a href="https://fluxsocial.io" target="_blank" rel="noreferrer">
                Flux
              </a>
              , and connect it to your ADAM agent.
            </j-text>
            <j-text size="600" nomargin color="ui-900">
              To interact with ADAM, click the ADAM icon in your system tray
              (next to the clock).
            </j-text>
          </j-flex>

          <j-flex gap="500" j="center" wrap style={{ marginTop: 60 }}>
            {/* <j-button size="xl" onClick={() => setCurrentIndex(5)}>
              Previous
            </j-button> */}
            <j-button
              variant="primary"
              size="xl"
              onClick={() => mutateAgent(username, firstName, lastName)}
              loading={loading}
            >
              Finish ADAM setup
            </j-button>
          </j-flex>
        </div>
      )}

      {currentIndex === 7 && (
        <div className="page">
          <Logo style={{ marginBottom: 40 }} />

          <j-flex
            direction="column"
            a="center"
            gap="300"
            style={{ marginBottom: 40 }}
          >
            <j-icon name="key" size="xl" color="ui-700" />
            <j-text size="800" nomargin color="ui-0">
              Unlock your agent
            </j-text>
            {appState?.selected_agent && (
              <j-text size="600" nomargin color="ui-500">
                {appState.selected_agent.name}
              </j-text>
            )}
          </j-flex>

          <j-flex
            direction="column"
            a="center"
            gap="400"
            style={{ textAlign: "center", width: "100%", maxWidth: 500, marginBottom: 30 }}
          >
            <j-text size="600" nomargin color="ui-900">
              Enter your encryption passphrase to unlock your agent and access your data.
            </j-text>
          </j-flex>

          <j-flex direction="column" gap="400" style={{ width: "100%" }}>
            <j-input
              size="lg"
              placeholder="Enter passphrase..."
              minlength={10}
              maxlength={30}
              autovalidate
              required
              type={showPassword ? "text" : "password"}
              disabled={loading}
              onInput={(e: any) => setPassword(e.target.value)}
              onKeyDown={(e: any) => {
                if (e.key === "Enter") {
                  if (isInitialized) {
                    setFailedAttempts(prev => prev + 1);
                    unlockAgent(password, holochain);
                  }
                  else if (passwordValid()) generateAgent(password);
                }
              }}
              style={{ width: "100%" }}
            >
              <j-button
                onClick={() => setShowPassword(!showPassword)}
                slot="end"
                variant="subtle"
                square
                style={{ marginRight: -13 }}
                tabIndex={-1}
              >
                <j-icon name={showPassword ? "eye-slash" : "eye"} size="sm" />
              </j-button>
            </j-input>

            {hasLoginError && (
              <>
                <j-text color="danger-500" nomargin style={{ maxWidth: 500 }}>
                  Invalid passphrase. Please try again.
                </j-text>
                
                {failedAttempts >= 2 && 
                  <j-text color="danger-700" nomargin style={{ maxWidth: 500 }}>
                    If you've forgotten your passphrase, you might need to create a new agent 
                    with the "Reset agent" button below.
                    This will delete keys and data and you'll have to start over.
                  </j-text>
                }
              </>
            )}
          </j-flex>

          <j-flex
            direction="column"
            gap="500"
            a="center"
            wrap
            style={{ marginTop: 60 }}
          >
            <j-button
              size="xl"
              variant="primary"
              onClick={() => {
                setFailedAttempts(prev => prev + 1);
                unlockAgent(password, holochain)
              }}
              loading={loading}
            >
              Unlock Agent
            </j-button>
            
            <j-flex
              direction="row"
              a="center"
              gap="400"
              style={{ textAlign: "center", width: "100%", maxWidth: 500, marginBottom: 30 }}
            >
              {appState && appState.agent_list.length > 1 && (
                <j-button size="xl" onClick={() => { loadAppState(); setShowAgentPicker(true); }} disabled={loading}>
                  Switch agent
                </j-button>
              )}
              <j-button size="xl" onClick={() => setClearAgentModalOpen(true)} disabled={loading}>
                Reset agent
              </j-button>
              <j-button size="xl" onClick={() => invoke("close_application")} disabled={loading}>
                Quit
              </j-button>
            </j-flex>
          </j-flex>
        </div>
      )}

      {clearAgentModalOpen && (
        <j-modal
          open={clearAgentModalOpen}
          onToggle={(e: any) => setClearAgentModalOpen(e.target.open)}
        >
          <j-box px="400" py="600">
            <j-flex
              direction="column"
              a="center"
              gap="500"
              style={{ textAlign: "center" }}
            >
              <j-text nomargin size="800">
                Reset agent
              </j-text>
              <j-text nomargin size="600" color="danger-600">
                Warning: by clearing the agent you will loose all the data and
                will have to start with a fresh agent
              </j-text>
              <j-button
                variant="primary"
                size="xl"
                onClick={clearAgent}
                loading={loading}
                style={{ marginTop: 20 }}
              >
                Delete Agent
              </j-button>
            </j-flex>
          </j-box>
        </j-modal>
      )}

      {showAgentPicker && !pendingSwitchAgent && (
        <j-modal
          open={true}
          onToggle={(e: any) => { if (!e.target.open) setShowAgentPicker(false); }}
        >
          <j-box px="400" py="600">
            <j-flex direction="column" gap="400">
              <j-text nomargin size="600" weight="600">
                Switch agent
              </j-text>
              {appState?.agent_list.map((agent: any) => (
                <j-button
                  key={agent.path}
                  full
                  variant={agent.path === appState.selected_agent?.path ? "primary" : "secondary"}
                  onClick={() => {
                    if (agent.path !== appState.selected_agent?.path) {
                      setPendingSwitchAgent(agent);
                    } else {
                      setShowAgentPicker(false);
                    }
                  }}
                >
                  {agent.name}
                </j-button>
              ))}
            </j-flex>
          </j-box>
        </j-modal>
      )}

      {pendingSwitchAgent && (
        <j-modal
          open={true}
          onToggle={(e: any) => { if (!e.target.open) { setPendingSwitchAgent(null); setShowAgentPicker(false); } }}
        >
          <j-box px="400" py="600">
            <j-flex direction="column" a="center" gap="500" style={{ textAlign: "center" }}>
              <j-text nomargin size="800">
                Switch to "{pendingSwitchAgent.name}"?
              </j-text>
              <j-text nomargin size="600" color="ui-800">
                The app will close. Please reopen it to start with the new agent.
              </j-text>
              <j-flex gap="400">
                <j-button size="xl" onClick={() => { setPendingSwitchAgent(null); }}>
                  Cancel
                </j-button>
                <j-button size="xl" variant="primary" onClick={() => switchAgent(pendingSwitchAgent)}>
                  Switch and quit
                </j-button>
              </j-flex>
            </j-flex>
          </j-box>
        </j-modal>
      )}

      <img
        src="holochain-large.png"
        alt=""
        style={{
          width: "100%",
          padding: "20px",
          maxWidth: 500,
          position: "absolute",
          bottom: 0,
        }}
      />
    </div>
  );
};

export default Login;
