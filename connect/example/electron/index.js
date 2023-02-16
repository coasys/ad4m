const { app } = require("electron");
const { ad4mConnect } = require("../../dist/electron");

app
  .whenReady()
  .then(async () => {
    await ad4mConnect({
      appName: "Ad4m",
      appIconPath: "https://avatars.githubusercontent.com/u/37603804?s=200&v=4",
      appDesc:
        "Ad4m is a simple and easy to use application for managing your files and folders.",
      capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
    });
  })
  .catch((e) => console.log(e));

app.on("window-all-closed", () => {
  if (process.platform !== "darwin") app.quit();
});
