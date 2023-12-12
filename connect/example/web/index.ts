import Ad4mConnectUI from "@coasys/ad4m-connect";

const ui = Ad4mConnectUI({
  appName: "ad4m-connect-example",
  appDesc: "hello",
  appDomain: "dev.ad4m.connect.example",
  appIconPath: "https://i.ibb.co/GnqjPJP/icon.png",
  capabilities: [
    {
      with: { domain: "*", pointers: ["*"] },
      can: ["*"],
    },
  ],
});

const authTitle = document.getElementById("auth");
const connectionTitle = document.getElementById("connection");
const button = document.getElementById("button");

connectionTitle!.innerText = ui.connectionState;
authTitle!.innerText = ui.authState;

button!.addEventListener("click", () => {
  ui.connect();
});

ui.addEventListener("connectionstatechange", (e) => {
  connectionTitle!.innerText = ui.connectionState;
});

ui.addEventListener("authstatechange", (e) => {
  authTitle!.innerText = ui.authState;
});
