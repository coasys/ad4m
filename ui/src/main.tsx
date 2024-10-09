import "@coasys/flux-ui";
import "@coasys/flux-ui/dist/main.css";
import { render } from "preact";
import { BrowserRouter as Router } from "react-router-dom";
import App from "./App";
import { Ad4minProvider } from "./context/Ad4minContext";
import "./index.css";
import reportWebVitals from "./reportWebVitals";

const Main = () => {
  return (
    <Ad4minProvider>
      <Router>
        <App />
      </Router>
    </Ad4minProvider>
  );
};

render(<Main />, document.getElementById("root")!);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
