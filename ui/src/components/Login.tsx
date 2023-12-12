import { useContext, useEffect, useState } from "react";
import { AgentContext } from "../context/AgentContext";
import { Ad4minContext } from "../context/Ad4minContext";
import { useNavigate } from "react-router-dom";
import Logo from "./Logo";
import { invoke } from "@tauri-apps/api";

const Login = (props: any) => {
  const {
    state: { loading, hasLoginError },
    methods: { generateAgent, unlockAgent },
  } = useContext(AgentContext);

  const {
    state: { isInitialized, isUnlocked, connected, connectedLaoding, client },
    methods: { resetEndpoint },
  } = useContext(Ad4minContext);

  let navigate = useNavigate();

  const [currentIndex, setCurrentIndex] = useState(0);
  const [currentSignupIndex, setCurrentSignupIndex] = useState(0);

  const [password, setPassword] = useState("");
  const [showPassword, setShowPassword] = useState(false);
  const [username, setUsername] = useState("");
  const [firstName, setFirstName] = useState("");
  const [lastName, setLastName] = useState("");
  const [opened, setOpened] = useState(false);
  const [usernameError, setUsernameError] = useState<string | null>(null);
  let [passwordError, setPasswordError] = useState<string | null>(null);
  const [clearAgentModalOpen, setClearAgentModalOpen] = useState(false);
  const [holochain, setHolochain] = useState(false);

  if (hasLoginError) {
    passwordError = "Invalid password";
  }

  const clearAgent = async () => {
    let agentStatus = await client?.agent.status();
    
    if (!agentStatus?.isUnlocked) {
      await invoke("clear_state");
    }
  };

  const generate = () => {
    checkUsernamePassword();

    if (username.length > 0 && password.length > 0) {
      generateAgent(username, firstName, lastName, password);
    }
  };

  // @ts-ignore
  const onKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === "Enter") {
      if (isInitialized) {
        unlockAgent(password, holochain);
      } else {
        generate();
      }
    }
  };

  const onSignupStepOneKeyDown = (
    event: React.KeyboardEvent<HTMLInputElement>
  ) => {
    if (event.key === "Enter") {
      gotoNextSignUpStep();
    }
  };

  const onSignupStepTwoKeyDown = (
    event: React.KeyboardEvent<HTMLInputElement>
  ) => {
    if (event.key === "Enter") {
      generate();
    }
  };

  const checkUsernamePassword = () => {
    if (username.length === 0) {
      setUsernameError("Username is requied");
    } else {
      setUsernameError(null);
    }

    if (password.length === 0) {
      setPasswordError("Password is requied");
    } else {
      setPasswordError(null);
    }
  };

  const gotoNextSignUpStep = () => {
    checkUsernamePassword();

    if (username.length > 0 && password.length > 0) {
      setCurrentSignupIndex(1);
    }
  };

  useEffect(() => {
    if (!connected && !connectedLaoding) {
      navigate("/connect");
    } else if (connected && isUnlocked) {
      navigate("/apps");
    } else if (isInitialized) {
      setCurrentIndex(5);
    }
  }, [connected, isUnlocked, navigate, isInitialized, connectedLaoding]);

  return (
    <div className="slider">
      {currentIndex === 0 && (
        <div className="slider__slide">
          <div className="slider__slide-content center text-center">
            <Logo
              style={{
                width: "80px",
                height: "80px",
                margin: "0 auto",
                marginBottom: "var(--j-space-500)",
              }}
              gradient
            ></Logo>

            <j-text size="800" color="black">
              The First Social Network
            </j-text>

            <j-flex direction="column" gap="200">
              <j-button
                size="xl"
                onClick={() => setCurrentIndex(1)}
                variant="primary"
              >
                Get Started
              </j-button>
            </j-flex>
          </div>
        </div>
      )}
      {currentIndex === 1 && (
        <div className="slider__slide">
          <div className="slider__slide-content text-center">
            <Logo
              style={{
                width: "80px",
                height: "80px",
                margin: "0 auto",
                marginBottom: "var(--j-space-500)",
              }}
              gradient
            ></Logo>

            <div>
              <j-text variant="heading">Privacy and Security</j-text>
              <j-text variant="ingress" nomargin>
                AD4M generates keys on your device, so only you have access to
                your account and data. No third parties can snoop on your data
                without your consent.
              </j-text>
            </div>

            <j-flex j="center" a="center" gap="500">
              <j-button
                variant="link"
                size="xl"
                onClick={() => setCurrentIndex(0)}
              >
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
        </div>
      )}
      {currentIndex === 2 && (
        <div className="slider__slide">
          <div className="slider__slide-content text-center">
            <Logo
              style={{
                width: "80px",
                height: "80px",
                margin: "0 auto",
                marginBottom: "var(--j-space-500)",
              }}
              gradient
            ></Logo>

            <div>
              <j-text variant="heading">Agent centric</j-text>
              <j-text variant="ingress" nomargin>
                With AD4M you own your data and decide what apps get to use it.
                No more app silos with you as the central authority. Censorship
                free.
              </j-text>
            </div>

            <j-flex j="center" a="center" gap="500">
              <j-button
                variant="link"
                size="xl"
                onClick={() => setCurrentIndex(1)}
              >
                Previous
              </j-button>
              <j-button
                variant="primary"
                size="xl"
                onClick={() => setCurrentIndex(3)}
              >
                Next
              </j-button>
            </j-flex>
          </div>
        </div>
      )}
      {currentIndex === 3 && (
        <div className="slider__slide">
          <div className="slider__slide-content text-center">
            <Logo
              style={{
                width: "80px",
                height: "80px",
                margin: "0 auto",
                marginBottom: "var(--j-space-500)",
              }}
              gradient
            ></Logo>

            <div>
              <j-text variant="heading">Censorship free</j-text>
              <j-text variant="ingress" nomargin>
                AD4M allows you to express yourself without fear of censorship
                or suppression. You can share your thoughts and opinions without
                worrying about being silenced by a central authority.
              </j-text>
            </div>

            <j-flex j="center" a="center" gap="500">
              <j-button
                variant="link"
                size="xl"
                onClick={() => setCurrentIndex(2)}
              >
                Previous
              </j-button>
              {!isInitialized ? (
                <j-button
                  size="xl"
                  full
                  variant="primary"
                  onClick={() => setCurrentIndex(4)}
                >
                  Create account
                </j-button>
              ) : (
                <j-button
                  size="xl"
                  variant="primary"
                  full
                  onClick={() => {
                    setCurrentIndex(5);
                  }}
                >
                  Sign in
                </j-button>
              )}
            </j-flex>
          </div>
        </div>
      )}
      {currentIndex === 4 && (
        <div className="slider__slide">
          <div className="slider__slide-content center">
            <Logo
              style={{
                width: "80px",
                height: "80px",
                margin: "0 auto",
                marginBottom: "var(--j-space-500)",
              }}
              gradient
            ></Logo>
            {currentSignupIndex === 0 && (
              <j-flex direction="column" gap="500" style="width: 100%">
                <j-input
                  full
                  autofocus
                  size="lg"
                  label="Username"
                  minlength={10}
                  maxlength={30}
                  autovalidate
                  required
                  type="text"
                  onInput={(e: any) => setUsername(e.target.value)}
                ></j-input>
                <j-input
                  size="lg"
                  label="Password"
                  minlength={10}
                  maxlength={30}
                  autovalidate
                  required
                  type={showPassword ? "text" : "password"}
                  full
                  onInput={(e: any) => setPassword(e.target.value)}
                  onKeyDown={onSignupStepOneKeyDown}
                >
                  <j-button
                    onClick={() => setShowPassword(!showPassword)}
                    slot="end"
                    variant="link"
                    square
                  >
                    <j-icon
                      name={showPassword ? "eye-slash" : "eye"}
                      size="sm"
                    ></j-icon>
                  </j-button>
                </j-input>
                <j-button
                  full
                  class="full-button"
                  size="lg"
                  variant="primary"
                  style={{ alignSelf: "center" }}
                  onClick={() => gotoNextSignUpStep()}
                  loading={loading}
                >
                  Next
                </j-button>
              </j-flex>
            )}
            {currentSignupIndex === 1 && (
              <j-flex direction="column" gap="500" style="width: 100%">
                <j-input
                  full
                  autofocus
                  size="lg"
                  label="First name (optional)"
                  minlength={10}
                  maxlength={30}
                  autovalidate
                  type="text"
                  onKeyDown={onSignupStepTwoKeyDown}
                  onInput={(e: any) => setFirstName(e.target.value)}
                ></j-input>
                <j-input
                  full
                  size="lg"
                  label="Last name (optional)"
                  minlength={10}
                  maxlength={30}
                  autovalidate
                  type="text"
                  onKeyDown={onSignupStepTwoKeyDown}
                  onInput={(e: any) => setLastName(e.target.value)}
                ></j-input>
                <j-button
                  class="full-button"
                  full
                  size="lg"
                  variant="primary"
                  style={{ alignSelf: "center" }}
                  onClick={() => generate()}
                  loading={loading}
                >
                  Generate Agent
                </j-button>
              </j-flex>
            )}
          </div>
        </div>
      )}
      {currentIndex === 5 && (
        <div className="slider__slide" style={{ height: "100vh" }}>
          <div className="slider__slide-content center">
            <Logo
              style={{
                width: "80px",
                height: "80px",
                margin: "0 auto",
                marginBottom: "var(--j-space-500)",
              }}
              gradient
            ></Logo>
            <j-flex direction="column" gap="500" style="width: 100%">
              <j-input
                autofocus
                size="lg"
                label="Password"
                minlength={10}
                maxlength={30}
                autovalidate
                required
                type={showPassword ? "text" : "password"}
                full
                onInput={(e: any) => setPassword(e.target.value)}
                onKeyDown={onKeyDown}
                errortext={passwordError}
                error={passwordError}
              >
                <j-button
                  onClick={() => setShowPassword(!showPassword)}
                  slot="end"
                  variant="link"
                  square
                >
                  <j-icon
                    name={showPassword ? "eye-slash" : "eye"}
                    size="sm"
                  ></j-icon>
                </j-button>
              </j-input>
              <j-button
                size="sm"
                variant="link"
                onClick={() => setClearAgentModalOpen(true)}
              >
                Reset agent
              </j-button>
              <j-box px="100" >
              <j-popover event="mouseover" placement="bottom">          
                <j-toggle slot="trigger"
                  checked={holochain}
                  onChange={(e) => {
                    setHolochain(e.target.checked);
                  }}
                >
                  {`Enable Holochain`}
                </j-toggle>
                <j-box p="200" slot="content">
                  <j-text size="400" color="black" weight="600">
                    This ADAM release includes a new unstable Holochain release for testing.
                  </j-text>
                  <j-text size="400" color="black" weight="600">
                    Regular use of ADAM will work without Holochain using our centralized testing Languages.
                  </j-text>
                </j-box>
              </j-popover>
            </j-box>
              <j-button
                full
                size="lg"
                class="full-button"
                variant="primary"
                style={{ alignSelf: "center" }}
                onClick={() => unlockAgent(password, holochain)}
                loading={loading}
              >
                Unlock Agent
              </j-button>
            </j-flex>
          </div>
        </div>
      )}
      {clearAgentModalOpen && (
        <j-modal
          open={clearAgentModalOpen}
          onToggle={(e: any) => setClearAgentModalOpen(e.target.open)}
        >
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Reset agent
              </j-text>
            </j-box>
            <j-text>
              Warning: by clearing the agent you will loose all the data and
              will have to start with a fresh agent
            </j-text>
            <j-box p="200"></j-box>
            <j-flex>
              <j-button
                variant="primary"
                onClick={() => clearAgent(password)}
                loading={loading}
              >
                Delete Agent
              </j-button>
            </j-flex>
          </j-box>
        </j-modal>
      )}
    </div>
  );
};

export default Login;
