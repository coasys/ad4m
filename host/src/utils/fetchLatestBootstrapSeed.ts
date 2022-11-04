import path from "path";
import wget from "wget-improved";
import fs from 'fs';
import fetch from 'node-fetch'

const BOOTSTRAP_LINK = "https://github.com/perspect3vism/ad4m-seeds/releases/download/0.0.3/mainnetSeed.json";

export const MAINNET_SEED = "mainnet_seed.json";

export async function fetchLatestBootstrapSeed(appDataPath: string) {
  return new Promise(async (resolve, reject) => {
    const dest = path.join(appDataPath, MAINNET_SEED);
    let download: any;

    download = wget.download(BOOTSTRAP_LINK, dest)
    download.on('end', async () => {
      await fs.chmodSync(dest, '777');
      console.log('Mainnet seed download succesfully')
      resolve(null);
    })

    download.on('error', async (err: any) => {
      console.log("Something went wrong downloading mainnet seed");
      reject(err);
    })
  });
}