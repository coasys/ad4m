import { getCurrentWebviewWindow } from "@tauri-apps/api/webviewWindow";
import { useContext, useEffect, useState } from "react";
import { Route, Routes } from "react-router-dom";
import "./App.css";
import AI from "./components/AI";
import Apps from "./components/Apps";
import Auth from "./components/Auth";
import { Connect } from "./components/Connect";
import Language from "./components/Language";
import Login from "./components/Login";
import Navigation from "./components/Navigation";
import Notification from "./components/Notification";
import Perspectives from "./components/Perspectives";
import Settings from "./components/Settings";
import Splashscreen from "./components/Splashscreen";
import TrayMessage from "./components/TrayMessage";
import TrustAgent from "./components/TrustAgent";
import { Ad4minContext } from "./context/Ad4minContext";
import { AgentProvider } from "./context/AgentContext";
const appWindow = getCurrentWebviewWindow();

const App = () => {
  const [opened, setOpened] = useState(false);
  const {
    state: { candidate, did, auth, notifications },
    methods: { handleTrustAgent },
  } = useContext(Ad4minContext);

  useEffect(() => {
    let unlisten: () => void;

    appWindow
      .listen("tauri://close-requested", ({ event, payload }) => {
        appWindow.hide();
      })
      .then((func) => {
        unlisten = func;
      })
      .catch((e) => console.error(e));

    return () => {
      if (unlisten) {
        unlisten();
      }
    };
  }, []);

  return (
    <div className="App">
      <Routes>
        <Route path="/splashscreen" element={<Splashscreen />} />
        <Route path="/tray_message" element={<TrayMessage />} />
        <Route
          path="/login"
          element={
            <AgentProvider>
              <Login />
            </AgentProvider>
          }
        />
        <Route
          path="/"
          element={
            <Navigation did={did} setOpened={setOpened} opened={opened} />
          }
        >
          <Route path="apps" element={<Apps />} />
          <Route
            path="language"
            element={<Language setOpened={setOpened} opened={opened} />}
          />
          <Route
            path="perspective"
            element={<Perspectives setOpened={setOpened} opened={opened} />}
          />
          <Route path="ai" element={<AI />} />
          <Route
            path="settings"
            element={
              <AgentProvider>
                <Settings did={did} setOpened={setOpened} opened={opened} />
              </AgentProvider>
            }
          />
        </Route>
        <Route path="/connect" element={<Connect />} />
      </Routes>
      {candidate && (
        <TrustAgent candidate={candidate} handleTrustAgent={handleTrustAgent} />
      )}
      {auth && <Auth />}
      {notifications.map((notification) => (
        <Notification key={notification.id} notification={notification} />
      ))}
    </div>
  );
};

export default App;
