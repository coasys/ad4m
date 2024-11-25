import { ModelInput } from "@coasys/ad4m/lib/src/ai/AIResolver";
import { invoke } from "@tauri-apps/api/core";
import { useContext, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Ad4minContext } from "../context/Ad4minContext";
import { AgentContext } from "../context/AgentContext";
import "../index.css";
import Logo from "./Logo";

const llmModels = [
  // "tiny_llama_1_1b",
  "External API",
  "llama_7b",
  "llama_8b",
  "llama_13b",
  "llama_70b",
];
const transcriptionModels = ["whisper"];
const embeddingModels = ["bert"];

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
  const [showPassword, setShowPassword] = useState(false);
  const [passwordError, setPasswordError] = useState<string | null>(null);
  const [username, setUsername] = useState("");
  const [firstName, setFirstName] = useState("");
  const [lastName, setLastName] = useState("");
  const [clearAgentModalOpen, setClearAgentModalOpen] = useState(false);
  const [holochain, setHolochain] = useState(true);
  // LLM
  const [llmModel, setLlmModel] = useState(llmModels[0]);
  const [llmName, setLlmName] = useState("");
  const [llmNameError, setLlmNameError] = useState(false);
  // Transcriber
  const [transcriptionModel, setTranscriptionModel] = useState(
    transcriptionModels[0]
  );
  const [transcriberName, setTranscriberName] = useState("");
  const [transcriberNameError, setTranscriberNameError] = useState(false);
  // Embedder
  const [embeddingModel, setEmbeddingModel] = useState(embeddingModels[0]);
  const [embedderName, setEmbedderName] = useState("");
  const [embedderNameError, setEmbedderNameError] = useState(false);
  // LLM API options
  const [apiUrl, setApiUrl] = useState("https://api.openai.com/v1");
  const [apiKey, setApiKey] = useState("");
  const [apiUrlError, setApiUrlError] = useState(false);
  const [apiKeyError, setApiKeyError] = useState(false);

  if (hasLoginError) setPasswordError("Invalid password");

  async function clearAgent() {
    let agentStatus = await client?.agent.status();
    if (!agentStatus?.isUnlocked) await invoke("clear_state");
  }

  // todo:
  // + look into weird glitching when UI first loads
  // + distinguish 'ADAM Layer' title from subtitle on first screen (& use SVG for 'powered by holochain' text)
  // + add lock icon on Privacy & security screen
  // + close menus when items selected (if possible using 'open' prop)

  function passwordValid(): boolean {
    const valid = password.length > 0;
    setPasswordError(valid ? null : "Password is requied");
    return valid;
  }

  function closeMenu(menuId: string) {
    const menu = document.getElementById(menuId);
    const items = menu?.shadowRoot?.querySelector("details");
    if (items) items.open = false;
  }

  function saveModels() {
    // validate model settings
    const invalidNames = !(llmName && embedderName && transcriberName);
    const invalidApi = llmModel === "External API" && !(apiUrl && apiKey);
    if (invalidNames || invalidApi) {
      // display errors
      setLlmNameError(!llmName);
      setEmbedderNameError(!embedderName);
      setTranscriberNameError(!transcriberName);
      if (invalidApi) {
        setApiUrlError(!apiUrl);
        setApiKeyError(!apiKey);
      }
    } else {
      // add llm model
      const llm = { name: llmName, modelType: "LLM" } as ModelInput;
      if (llmModel === "External API")
        llm.api = { baseUrl: apiUrl, apiKey, apiType: "OPEN_AI" };
      else
        llm.local = {
          fileName: llmModel,
          tokenizerSource: "",
          modelParameters: "",
        };
      client!.ai.addModel(llm);
      // add embedding model
      client!.ai.addModel({
        name: embedderName,
        local: {
          fileName: embeddingModel,
          tokenizerSource: "",
          modelParameters: "",
        },
        modelType: "EMBEDDING",
      });
      // add transcription model
      client!.ai.addModel({
        name: transcriberName,
        local: {
          fileName: transcriptionModel,
          tokenizerSource: "",
          modelParameters: "",
        },
        modelType: "TRANSCRIPTION",
      });
      setCurrentIndex(6);
    }
  }

  useEffect(() => {
    if (!connected && !connectedLoading) navigate("/connect");
    else if (connected && isUnlocked) navigate("/apps");
    else if (isInitialized) setCurrentIndex(7);
  }, [connected, isUnlocked, navigate, isInitialized, connectedLoading]);

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
              ADAM Layer<br></br>A social layer for the{" "}
              <i style={{ fontFamily: "Noto Serif" }}>Wise Web</i>
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
          <img src="holochain-large.png" alt="" style={{ float: "left" }} />
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
            />

            <div>
              <j-text variant="heading">Privacy and Security</j-text>
              <j-text variant="ingress" nomargin>
                ADAM generates keys on your device, so only you have access to
                your account and data.
                <p />
                We will ask for a password used to encrypt your local keys.
                Don't forget it! There is no way to recover it.
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
          <div className="slider__slide-content center">
            <Logo
              style={{
                width: "80px",
                height: "80px",
                margin: "0 auto",
                marginBottom: "var(--j-space-500)",
              }}
              gradient
            />
            <j-flex direction="column" gap="500" style={{ width: "100%" }}>
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
                onKeyDown={(e: any) => {
                  if (e.key === "Enter" && passwordValid()) {
                    generateAgent(password);
                    setCurrentIndex(3);
                  }
                }}
              >
                <j-button
                  onClick={() => setShowPassword(!showPassword)}
                  slot="end"
                  variant="link"
                  square
                >
                  <j-icon name={showPassword ? "eye-slash" : "eye"} size="sm" />
                </j-button>
              </j-input>
              <j-button
                full
                className="full-button"
                size="lg"
                variant="primary"
                style={{ alignSelf: "center" }}
                onClick={() => {
                  if (passwordValid()) {
                    generateAgent(password);
                    setCurrentIndex(3);
                  }
                }}
                loading={loading}
                disabled={password.length === 0}
              >
                Generate Agent
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
                ADAM allows you to express yourself without fear of censorship
                or suppression. You can share your thoughts and opinions without
                depending on a central authority or a particular app.
                <p />
                That includes and starts with your personal profile. In the next
                step you can add optional information about yourself that ADAM
                will make available publicly to other users through any ADAM
                app.
              </j-text>
            </div>

            <j-flex j="center" a="center" gap="500">
              <j-button
                size="xl"
                full
                variant="primary"
                onClick={() => setCurrentIndex(4)}
              >
                Next
              </j-button>
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
            />
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
              <j-button
                className="full-button"
                full
                size="lg"
                variant="primary"
                style={{ alignSelf: "center" }}
                onClick={() => setCurrentIndex(5)}
              >
                Initialize public profile
              </j-button>
            </j-flex>
          </div>
        </div>
      )}

      {currentIndex === 5 && (
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
            />

            <div>
              <j-text variant="heading-lg">AI Model Selection</j-text>
              <j-text variant="ingress" nomargin>
                Choose between local or remote AI processing and the models you
                want to use for different tasks
              </j-text>
            </div>

            <j-flex direction="column" gap="500" style={{ width: "100%" }}>
              <j-box pb="500" className="box">
                <j-flex direction="column" gap="600">
                  <j-text variant="heading-sm" nomargin>
                    LLM Processing
                  </j-text>
                  <j-flex a="center" gap="400">
                    <j-text
                      nomargin
                      color="color-white"
                      style={{ flexShrink: 0 }}
                    >
                      Name:
                    </j-text>
                    <j-input
                      size="md"
                      type="text"
                      value={llmName}
                      error={llmNameError}
                      errortext="Required"
                      onInput={(e: any) => {
                        setLlmNameError(false);
                        setLlmName(e.target.value);
                      }}
                      style={{ width: "100%" }}
                    />
                  </j-flex>
                  <j-flex a="center" gap="400">
                    <j-text
                      nomargin
                      color="color-white"
                      style={{ flexShrink: 0 }}
                    >
                      Model:
                    </j-text>
                    <div style={{ height: 42, zIndex: 15 }}>
                      <j-menu>
                        <j-menu-group
                          collapsible
                          title={llmModel}
                          id="llm-models"
                        >
                          {llmModels.map((model) => (
                            <j-menu-item
                              selected={llmModel === model}
                              onClick={() => {
                                setLlmModel(model);
                                if (model === "External API") {
                                  setApiUrl("https://api.openai.com/v1");
                                } else {
                                  setApiUrl("");
                                  setApiKey("");
                                  setApiUrlError(false);
                                  setApiKeyError(false);
                                }
                                closeMenu("llm-models");
                              }}
                            >
                              {model}
                              {model === "External API"
                                ? " (Recommended for low to mid spec devices)"
                                : ""}
                            </j-menu-item>
                          ))}
                        </j-menu-group>
                      </j-menu>
                    </div>
                  </j-flex>
                  {llmModel === "External API" && (
                    <>
                      <j-flex a="center" gap="400">
                        <j-text
                          nomargin
                          color="color-white"
                          style={{ flexShrink: 0 }}
                        >
                          API URL:
                        </j-text>
                        <j-input
                          size="md"
                          type="text"
                          value={apiUrl}
                          error={apiUrlError}
                          errortext="Required"
                          onInput={(e: any) => {
                            setApiUrlError(false);
                            setApiUrl(e.target.value);
                          }}
                          style={{ width: "100%" }}
                        />
                      </j-flex>
                      <j-flex a="center" gap="400">
                        <j-text
                          nomargin
                          color="color-white"
                          style={{ flexShrink: 0 }}
                        >
                          API Key:
                        </j-text>
                        <j-input
                          size="md"
                          type="text"
                          value={apiKey}
                          error={apiKeyError}
                          errortext="Required"
                          onInput={(e: any) => {
                            setApiKeyError(false);
                            setApiKey(e.target.value);
                          }}
                          style={{ width: "100%" }}
                        />
                      </j-flex>
                    </>
                  )}
                </j-flex>
              </j-box>

              <j-box pb="500" className="box">
                <j-flex direction="column" gap="600" wrap>
                  <j-text variant="heading-sm" nomargin>
                    Audio Transcription
                  </j-text>
                  <j-flex a="center" gap="400">
                    <j-text
                      nomargin
                      color="color-white"
                      style={{ flexShrink: 0 }}
                    >
                      Name:
                    </j-text>
                    <j-input
                      size="md"
                      type="text"
                      value={transcriberName}
                      error={transcriberNameError}
                      errortext="Required"
                      onInput={(e: any) => {
                        setTranscriberNameError(false);
                        setTranscriberName(e.target.value);
                      }}
                      style={{ width: "100%" }}
                    />
                  </j-flex>
                  <j-flex a="center" gap="400">
                    <j-text
                      nomargin
                      color="color-white"
                      style={{ flexShrink: 0 }}
                    >
                      Model:
                    </j-text>
                    <j-menu>
                      <j-menu-group
                        collapsible
                        title={transcriptionModel}
                        id="transcription-models"
                      >
                        {transcriptionModels.map((model) => (
                          <j-menu-item
                            selected={transcriptionModel === model}
                            onClick={() => {
                              setTranscriptionModel(model);
                              closeMenu("transcription-models");
                            }}
                          >
                            {model}
                          </j-menu-item>
                        ))}
                      </j-menu-group>
                    </j-menu>
                  </j-flex>
                </j-flex>
              </j-box>

              <j-box pb="500" className="box">
                <j-flex direction="column" gap="600" wrap>
                  <j-text variant="heading-sm" nomargin>
                    Vector Embedding
                  </j-text>
                  <j-flex a="center" gap="400">
                    <j-text
                      nomargin
                      color="color-white"
                      style={{ flexShrink: 0 }}
                    >
                      Name:
                    </j-text>
                    <j-input
                      size="md"
                      type="text"
                      value={embedderName}
                      error={embedderNameError}
                      errortext="Required"
                      onInput={(e: any) => {
                        setEmbedderNameError(false);
                        setEmbedderName(e.target.value);
                      }}
                      style={{ width: "100%" }}
                    />
                  </j-flex>
                  <j-flex a="center" gap="400">
                    <j-text
                      nomargin
                      color="color-white"
                      style={{ flexShrink: 0 }}
                    >
                      Model:
                    </j-text>
                    <j-menu>
                      <j-menu-group
                        collapsible
                        title={embeddingModel}
                        id="embedding-models"
                      >
                        {embeddingModels.map((model) => (
                          <j-menu-item
                            selected={embeddingModel === model}
                            onClick={() => {
                              setEmbeddingModel(model);
                              closeMenu("embedding-models");
                            }}
                          >
                            {model}
                          </j-menu-item>
                        ))}
                      </j-menu-group>
                    </j-menu>
                  </j-flex>
                </j-flex>
              </j-box>

              <j-button
                className="full-button"
                full
                size="lg"
                variant="primary"
                style={{ alignSelf: "center" }}
                onClick={saveModels}
              >
                Save Models
              </j-button>
            </j-flex>
          </div>
        </div>
      )}

      {currentIndex === 6 && (
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
                With ADAM you own your data and decide what apps get to use it.
                No more app silos with you as the central authority.
                <p />
                Once agent generation is done, ADAM will run on your device, in
                the background. Open an ADAM app, like{" "}
                <a
                  href="https://fluxsocial.io"
                  target="_blank"
                  rel="noreferrer"
                  style={{ color: "var(--end-color)" }}
                >
                  Flux
                </a>
                , and connect it to your ADAM agent.
                <p />
                To interact with ADAM, click the ADAM icon in your system tray
                (next to the clock).
              </j-text>
            </div>
            <j-button
              className="full-button"
              full
              size="lg"
              variant="primary"
              style={{ alignSelf: "center" }}
              onClick={() => mutateAgent(username, firstName, lastName)}
              loading={loading}
            >
              Finish ADAM setup
            </j-button>
          </div>
        </div>
      )}

      {currentIndex === 7 && (
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
            <j-flex direction="column" gap="500" style={{ width: "100%" }}>
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
                onKeyDown={(e: any) => {
                  if (e.key === "Enter") {
                    if (isInitialized) unlockAgent(password, holochain);
                    else if (passwordValid()) generateAgent(password);
                  }
                }}
                errortext={passwordError || undefined}
                error={!!passwordError}
              >
                <j-button
                  onClick={() => setShowPassword(!showPassword)}
                  slot="end"
                  variant="link"
                  square
                >
                  <j-icon name={showPassword ? "eye-slash" : "eye"} size="sm" />
                </j-button>
              </j-input>
              <j-button
                size="sm"
                variant="link"
                onClick={() => setClearAgentModalOpen(true)}
              >
                Reset agent
              </j-button>
              <j-button
                full
                size="lg"
                className="full-button"
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
                onClick={clearAgent}
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
