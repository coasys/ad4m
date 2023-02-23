import chalk from "chalk";
import findProcess from "find-process";
import glob from "glob";
import { kill } from "process";
import wget from 'wget-improved';
import fetch from 'node-fetch';
import path from "path";
import fs from 'fs-extra';
import { homedir } from "os";

export const logger = {
  info: (...args: any[]) => !global.hideLogs && console.log(chalk.blue('[INFO]'),...args),
  error: (...args: any[]) => !global.hideLogs && console.error(chalk.red('[ERROR]'), ...args)
}

export function cleanOutput(data: string) {
  const lines = data.split('\n')
  lines.splice(0, 1);
  const cleaned = lines.join('\n')

  if (cleaned) {    
    const parsed = JSON.parse(cleaned.replace(/'/gm, "\"").replace(/(\w+)[:]\s/gm, "\"$1\": ").replace(/"{/g, "{").replace(/}"/g, "}"));

    return parsed;
  }

  throw Error('cannot be parsed');
}

export async function findAndKillProcess(processName: string) {
  try {
    const list = await findProcess('name', processName)

    for (const p of list) {      
      kill(p.pid, 'SIGKILL')
    }
  } catch (err) {
    logger.error(`No process found by name: ${processName}`)
  } 
}

export function getTestFiles() {
  const testFiles = glob.sync('**/*.test.js').filter(e => !e.includes('node_modules'))
  logger.info(testFiles)

  return testFiles;
}

function fileExist(binaryPath: string): Promise<string[]> {
  return new Promise((resolve, reject) => {
    glob(path.join(binaryPath, `ad4m-host*`), (err, file) => {
      if (err) {
        reject(err)
      }
      resolve(file)
    })
  })
}

export async function getAd4mHostBinary(relativePath: string) {
  return new Promise(async (resolve, reject) => {
    const response = await fetch("https://api.github.com/repos/perspect3vism/ad4m-host/releases/71753984");
    const data: any = await response.json();
    const version = data['name'].replace('v', '');
    global.ad4mHostVersion = version;

    const isWin = process.platform === "win32";
    const isMac = process.platform === 'darwin';
  
    const binaryPath = path.join(ad4mDataDirectory(relativePath), 'binary');

    const files = await fileExist(binaryPath)

    for (const file of files) {
      if (file === path.join(binaryPath, `ad4m-host-${version}`)) {
        logger.info('ad4m-host binary found')
        resolve(null);
        return;
      } else {
        fs.rmSync(file)
      }
    }

    logger.info('ad4m-host binary not found, downloading now...')

    const dest = path.join(binaryPath, `ad4m-host-${version}`);
    let download: any;
    await fs.ensureDir(binaryPath);
    
    if (isMac) {
      const link = data.assets.find((e: any) =>
        e.name.includes("macos")
      ).browser_download_url;
      download = wget.download(link, dest)
    } else if(isWin) {
      const link = data.assets.find((e: any) =>
        e.name.includes("windows")
      ).browser_download_url;
      download = wget.download(link, dest)
    } else {
      const link = data.assets.find((e: any) =>
        e.name.includes("linux")
      ).browser_download_url;
      download = wget.download(link, dest)
    }

    download.on('end', async () => {
      await fs.chmodSync(dest, '777');
      logger.info('ad4m-host binary downloaded sucessfully')
      resolve(null);
    })

    download.on('error', async (err: any) => {
      logger.error(`Something went wrong while downloading ad4m-host binary: ${err}`)
      reject(err);
    });
  });
}

export function ad4mDataDirectory(override?: string) {
  if (override)
    return path.join(homedir(), override)
  else
    return path.join(homedir(), '.ad4m')
}

export function deleteAllAd4mData(relativePath: string) {
  const dataPath = ad4mDataDirectory(relativePath)

  const files = fs.readdirSync(dataPath);

  
  for (const file of files) {
    const fileDir = path.join(ad4mDataDirectory(relativePath), './', file);
    
    if (file !== 'binary') {
      fs.removeSync(fileDir);
    }
  }
}