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
const hostingCheckEmailButton = document.getElementById("button-hosting-check-email");
const hostingLoginButton = document.getElementById("button-hosting-login");
const requestTokenButton = document.getElementById("button-request-token");
const VerifyCodeButton = document.getElementById("button-verify-code");

connectionTitle!.innerText = ui.connectionState;
authTitle!.innerText = ui.authState;

button!.addEventListener("click", () => {
  ui.connect();
});

hostingCheckEmailButton!.addEventListener("click", () => {
  ui.checkEmail("jhon@example.com");
});

hostingLoginButton!.addEventListener("click", () => {
  ui.loginToHosting("jhon@example.com", "test123456");
});

requestTokenButton!.addEventListener("click", () => {
  ui.requestCapability();
});

VerifyCodeButton!.addEventListener("click", () => {
  ui.verifyCode("code here");
});

ui.addEventListener("connectionstatechange", (e) => {
  connectionTitle!.innerText = ui.connectionState;
});

ui.addEventListener("authstatechange", (e) => {
  authTitle!.innerText = ui.authState;
});
