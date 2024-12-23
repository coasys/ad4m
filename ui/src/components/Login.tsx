import { ModelInput } from "@coasys/ad4m/lib/src/ai/AIResolver";
import { invoke } from "@tauri-apps/api/core";
import { useContext, useEffect, useState, useRef } from "react";
import { useNavigate } from "react-router-dom";
import { Ad4minContext } from "../context/Ad4minContext";
import { AgentContext } from "../context/AgentContext";
import { open } from "@tauri-apps/plugin-shell";
import "../index.css";
import Logo from "./Logo";

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
  const [aiMode, setAIMode] = useState("Local");

  const [apiUrl, setApiUrl] = useState("https://api.openai.com/v1");
  const [apiUrlError, setApiUrlError] = useState("");
  const [apiKey, setApiKey] = useState("");
  const [apiKeyError, setApiKeyError] = useState("");
  const [apiValid, setApiValid] = useState(false);
  const [apiModelValid, setApiModelValid] = useState(false);
  const [apiModelError, setApiModelError] = useState("");
  const [apiLoading, setApiLoading] = useState(false);
  const [apiModel, setApiModel] = useState("");
  const [apiModels, setApiModels] = useState<string[]>([]);
  const apiUrlRef = useRef("https://api.openai.com/v1");
  const apiKeyRef = useRef("");

  if (hasLoginError) setPasswordError("Invalid password");

  async function clearAgent() {
    let agentStatus = await client?.agent.status();
    if (!agentStatus?.isUnlocked) await invoke("clear_state");
  }

  function closeMenu(menuId: string) {
    const menu = document.getElementById(menuId);
    const items = menu?.shadowRoot?.querySelector("details");
    if (items) items.open = false;
  }

  function passwordValid(): boolean {
    const valid = password.length > 0;
    setPasswordError(valid ? null : "Password is requied");
    return valid;
  }

  async function checkApi() {
    setApiLoading(true);
    setApiKeyError("");
    setApiUrlError("");
    let valid = false;
    try {
      const response = await fetch(`${apiUrlRef.current}/models`, {
        method: "GET",
        headers: {
          Authorization: `Bearer ${apiKeyRef.current}`,
          "Content-Type": "application/json",
        },
      });
      const { ok, status, statusText } = response;
      if (ok) valid = true;
      else {
        // key invalid
        setApiKeyError(status === 401 ? "Invalid key" : statusText);
      }
    } catch {
      // url invalid
      setApiUrlError("Error connecting to API");
    }
    setApiValid(valid);

    if (valid) {
      // get models
      setApiModels([]);
      setApiModel("");
      setApiModelValid(false);
      try {
        const response = await fetch(`${apiUrlRef.current}/models`, {
          method: "GET",
          headers: {
            Authorization: `Bearer ${apiKeyRef.current}`,
            "Content-Type": "application/json",
          },
        });
        if (response.ok) {
          const body = await response.json();
          const models = body.data.map((e: any) => e.id);
          setApiModels(models);
          if (apiUrlRef.current === "https://api.openai.com/v1")
            setApiModel("gpt-4o");
        }
      } catch {}
    } else {
      setApiModels([]);
      setApiModel("");
    }
    setApiLoading(false);
  }

  async function checkModel() {
    setApiLoading(true);
    if (apiModel) {
      try {
        const response = await fetch(`${apiUrl}/chat/completions`, {
          method: "POST",
          headers: {
            Authorization: `Bearer ${apiKey}`,
            "Content-Type": "application/json",
          },
          body: JSON.stringify({
            model: apiModel,
            messages: [{ role: "user", content: "Hi" }],
            max_tokens: 5,
          }),
        });
        setApiLoading(false);
        const { ok, status, statusText } = response;
        if (ok) {
          setApiModelValid(true);
        } else {
          if (status === 401) {
            setApiKeyError("Invalid Key");
          } else {
            setApiModelError(statusText);
          }
          return false;
        }
      } catch (e) {
        setApiModelError(`Error testing API completion: ${e}`);
      }
    } else {
      setApiModelError("Model required");
    }
    setApiLoading(false);
  }

  async function saveModels() {
    // add llm model
    if (aiMode !== "None") {
      const llm = { name: "LLM Model 1", modelType: "LLM" } as ModelInput;
      if (aiMode === "Local") {
        llm.local = {
          fileName: "solar_10_7b_instruct",
          tokenizerSource: "",
          modelParameters: "",
        };
      } else {
        llm.api = {
          baseUrl: apiUrl,
          apiKey,
          apiType: "OPEN_AI",
          model: apiModel,
        };
      }
      client!.ai
        .addModel(llm)
        .then((modelId) => client!.ai.setDefaultModel("LLM", modelId));
    }
    // add embedding model
    client!.ai.addModel({
      name: "bert",
      local: {
        fileName: "bert",
        tokenizerSource: "",
        modelParameters: "",
      },
      modelType: "EMBEDDING",
    });
    // add transcription model
    client!.ai.addModel({
      name: "Transcription Model 1",
      local: {
        fileName: "whisper",
        tokenizerSource: "",
        modelParameters: "",
      },
      modelType: "TRANSCRIPTION",
    });
    setCurrentIndex(6);
  }

  useEffect(() => {
    if (!connected && !connectedLoading) navigate("/connect");
    else if (connected && isUnlocked) navigate("/apps");
    else if (isInitialized) setCurrentIndex(7);
  }, [connected, isUnlocked, navigate, isInitialized, connectedLoading]);

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
              Create your password
            </j-text>
          </j-flex>

          <j-input
            size="lg"
            placeholder="Password..."
            minlength={10}
            maxlength={30}
            autovalidate
            required
            type={showPassword ? "text" : "password"}
            onInput={(e: any) => setPassword(e.target.value)}
            onKeyDown={(e: any) => {
              if (e.key === "Enter" && passwordValid()) {
                generateAgent(password);
                setCurrentIndex(3);
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
            >
              <j-icon name={showPassword ? "eye-slash" : "eye"} size="sm" />
            </j-button>
          </j-input>

          <j-flex gap="500" j="center" wrap style={{ marginTop: 70 }}>
            <j-button size="xl" onClick={() => setCurrentIndex(1)}>
              Previous
            </j-button>
            <j-button
              variant="primary"
              size="xl"
              onClick={() => {
                if (passwordValid()) {
                  generateAgent(password);
                  setCurrentIndex(3);
                }
              }}
              loading={loading}
              disabled={password.length === 0}
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
              Is your computer capabale of running Large Language Models
              locally?
            </j-text>
            <j-text>
              Regardless of your choice here, we will always download and use
              small AI models (such as{" "}
              <a
                onClick={() =>
                  open("https://huggingface.co/openai/whisper-small")
                }
                style={{ cursor: "pointer" }}
              >
                Whisper small
              </a>{" "}
              and an{" "}
              <a
                onClick={() =>
                  open(
                    "https://huggingface.co/Snowflake/snowflake-arctic-embed-xs"
                  )
                }
                style={{ cursor: "pointer" }}
              >
                Embedding model
              </a>
              ) to handle basic tasks on all devices.
              <br></br>
              <br></br>
              When it comes to LLMs, it depends on you having either an Apple
              Silicon mac (M1 or better) or an nVidia GPU (with enough vRAM).
              <br></br>
              <br></br>
              Alternatively, you can configure ADAM to out-source LLM tasks to a
              remote API. If you unsure, you can select "None" now and add,
              remove or change model settings later-on in the <b>AI tab</b>.
            </j-text>
          </j-flex>

          <j-flex gap="400" style={{ padding: "0 10px" }}>
            <button
              className={`card ${aiMode === "Local" && "selected"}`}
              onClick={() => setAIMode("Local")}
            >
              <j-icon name="house-fill" size="lg" color="ui-500" />
              <j-text
                size="600"
                color="ui-0"
                nomargin
                style={{ marginBottom: 5 }}
              >
                Local
              </j-text>
              <j-text size="500" nomargin color="ui-800">
                Select Local if you have an <b>M1 mac</b> (or better) or an{" "}
                <b>nVidia GPU</b> with 8Gb of RAM or more!
              </j-text>
            </button>

            <button
              className={`card ${aiMode === "Remote" && "selected"}`}
              onClick={() => setAIMode("Remote")}
            >
              <j-icon name="broadcast-pin" size="lg" color="ui-500" />
              <j-text
                size="600"
                color="ui-0"
                nomargin
                style={{ marginBottom: 5 }}
              >
                Remote
              </j-text>
              <j-text size="500" nomargin color="ui-800">
                Select to use an external API like <b>OpenAI</b> or your own{" "}
                <b>Ollama</b> server.
              </j-text>
            </button>

            <button
              className={`card ${aiMode === "None" && "selected"}`}
              onClick={() => setAIMode("None")}
            >
              <j-icon name="x-lg" size="lg" color="ui-500" />
              <j-text
                size="600"
                color="ui-0"
                nomargin
                style={{ marginBottom: 5 }}
              >
                None
              </j-text>
              <j-text size="500" nomargin color="ui-800">
                Select if you'd prefer <b>NOT to use LLMs</b> at all.
              </j-text>
            </button>
          </j-flex>

          {aiMode === "Local" && (
            <j-flex
              direction="column"
              a="center"
              gap="400"
              style={{ marginTop: 30, maxWidth: 350 }}
            >
              <j-text>
                This will download{" "}
                <a
                  onClick={() =>
                    open(
                      "https://huggingface.co/TheBloke/SOLAR-10.7B-Instruct-v1.0-GGUF"
                    )
                  }
                  style={{ cursor: "pointer" }}
                >
                  SOLAR 10.7b instruct
                </a>
              </j-text>
            </j-flex>
          )}

          {aiMode === "Remote" && (
            <j-box pt="500">
              <j-flex a="center" direction="column" gap="500">
                <j-flex a="center" gap="400">
                  <j-button
                    onClick={() => {
                      setApiUrl("https://api.openai.com/v1");
                      apiUrlRef.current = "https://api.openai.com/v1";
                      setApiValid(false);
                      setApiModelValid(false);
                      setApiKeyError("");
                      setApiUrlError("");
                    }}
                  >
                    OpenAI
                  </j-button>
                  <j-button
                    onClick={() => {
                      setApiUrl("http://localhost:11434/v1");
                      apiUrlRef.current = "http://localhost:11434/v1";
                      setApiValid(false);
                      setApiModelValid(false);
                      setApiKeyError("");
                      setApiUrlError("");
                    }}
                  >
                    Ollama
                  </j-button>
                </j-flex>

                <j-flex a="center" gap="400">
                  <j-text nomargin color="ui-800" style={{ flexShrink: 0 }}>
                    API URL:
                  </j-text>
                  <j-input
                    size="md"
                    type="text"
                    value={apiUrl}
                    error={!!apiUrlError}
                    errortext={apiUrlError}
                    onInput={(e: any) => {
                      setApiUrl(e.target.value);
                      apiUrlRef.current = e.target.value;
                      setApiValid(false);
                      setApiKeyError("");
                      setApiUrlError("");
                    }}
                    style={{ width: "100%" }}
                  />
                  {apiUrlError && <j-icon name="x-circle" color="danger-500" />}
                  {apiValid && (
                    <j-icon name="check-circle" color="success-500" />
                  )}
                </j-flex>

                <j-flex a="center" gap="400">
                  <j-text nomargin color="ui-800" style={{ flexShrink: 0 }}>
                    API Key:
                  </j-text>
                  <j-input
                    size="md"
                    type="text"
                    value={apiKey}
                    error={!!apiKeyError}
                    errortext={apiKeyError}
                    onInput={(e: any) => {
                      setApiKey(e.target.value);
                      apiKeyRef.current = e.target.value;
                      setApiValid(false);
                      setApiKeyError("");
                      setApiUrlError("");
                    }}
                    style={{ width: "100%" }}
                  />
                  {apiKeyError && <j-icon name="x-circle" color="danger-500" />}
                  {apiValid && (
                    <j-icon name="check-circle" color="success-500" />
                  )}
                </j-flex>

                {apiValid && (
                  <j-flex direction="column" a="center" gap="400">
                    <j-flex a="center" gap="400">
                      <j-text nomargin color="ui-800" style={{ flexShrink: 0 }}>
                        API Model:
                      </j-text>
                      <j-menu style={{ zIndex: 2 }}>
                        <j-menu-group
                          collapsible
                          title={apiModel || "None selected"}
                          id="api-models"
                        >
                          {apiModels.map((model) => (
                            <j-menu-item
                              selected={model === apiModel}
                              onClick={() => {
                                setApiModelError("");
                                setApiModel(model);
                                closeMenu("api-models");
                              }}
                            >
                              {model}
                            </j-menu-item>
                          ))}
                        </j-menu-group>
                      </j-menu>
                      {apiModelError && (
                        <j-icon name="x-circle" color="danger-500" />
                      )}
                      {apiModelValid && (
                        <j-icon name="check-circle" color="success-500" />
                      )}
                    </j-flex>

                    {apiModelError && (
                      <j-text color="danger-500" nomargin>
                        {apiModelError}
                      </j-text>
                    )}
                  </j-flex>
                )}

                {(!apiModelValid || !apiValid) && (
                  <>
                    {!apiValid ? (
                      <j-button
                        onClick={checkApi}
                        variant="primary"
                        loading={apiLoading}
                      >
                        Check API
                      </j-button>
                    ) : (
                      <j-button
                        onClick={checkModel}
                        variant="primary"
                        loading={apiLoading}
                      >
                        Check Model
                      </j-button>
                    )}
                  </>
                )}
              </j-flex>
            </j-box>
          )}

          {aiMode === "None" && (
            <j-flex
              direction="column"
              a="center"
              gap="400"
              style={{ marginTop: 30, maxWidth: 350 }}
            >
              <j-text>
                Selecting <b>None</b> here and not having any LLM configured
                might result in new Synergy features not working in Flux...
              </j-text>
            </j-flex>
          )}

          <j-flex gap="500" j="center" wrap style={{ marginTop: 60 }}>
            <j-button size="xl" onClick={() => setCurrentIndex(4)}>
              Previous
            </j-button>
            <j-button
              variant="primary"
              size="xl"
              onClick={saveModels}
              disabled={aiMode === "Remote" && (!apiValid || !apiModelValid)}
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
              Login
            </j-text>
          </j-flex>

          <j-input
            size="lg"
            placeholder="Password..."
            minlength={10}
            maxlength={30}
            autovalidate
            required
            type={showPassword ? "text" : "password"}
            onInput={(e: any) => setPassword(e.target.value)}
            onKeyDown={(e: any) => {
              if (e.key === "Enter") {
                if (isInitialized) unlockAgent(password, holochain);
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
            >
              <j-icon name={showPassword ? "eye-slash" : "eye"} size="sm" />
            </j-button>
          </j-input>

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
              onClick={() => unlockAgent(password, holochain)}
              loading={loading}
            >
              Unlock Agent
            </j-button>
            <j-button size="xl" onClick={() => setClearAgentModalOpen(true)}>
              Reset agent
            </j-button>
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
              <j-text nomargin size="600">
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
