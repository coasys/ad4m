const { app } = require("electron");
const { ad4mConnect } = require("@perspect3vism/ad4m-connect/electron");

console.log({ ad4mConnect });

app
  .whenReady()
  .then(async () => {
    console.log(ad4mConnect);

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
