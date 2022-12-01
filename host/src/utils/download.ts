import { DownloaderHelper } from "node-downloader-helper";
import path from "path";
import fs from "fs";

export function tryDownload(url: string, dest: string) {
  
  return new Promise(async (resolve, reject) => {
    const options = {
      fileName: path.basename(dest),
      retry: { maxRetries: 3, delay: 6000 },
    };
    const dl = new DownloaderHelper(url, path.dirname(dest), options);

    dl.on('end', async () => {
      console.log("Dep download succesfully: ", url);
      await fs.chmodSync(dest, '777');
      resolve(true);
    });
    dl.on('error', (err) => {
      console.log("Something went wrong downloading from destination: ", url);
      console.log(err);
      reject(err);
    });
    dl.start().catch(err => {
      console.log("Something went wrong downloading from destination: ", url);
      console.log(err);
      reject(err);
    });
  });
}
