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
      if (p.name.includes(processName)) {
        kill(p.pid, 'SIGKILL')
      }   
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
    glob(path.join(binaryPath, `ad4m*`), (err, file) => {
      if (err) {
        reject(err)
      }
      resolve(file)
    })
  })
}

export async function getAd4mHostBinary(relativePath: string) {
  return new Promise(async (resolve, reject) => {
    const isWin = process.platform === "win32";
    const isMac = process.platform === 'darwin';
    const isLinux = process.platform === 'linux';
  
    const binaryPath = path.join(ad4mDataDirectory(relativePath), 'binary');

    // Check if binary already exists before hitting the API
    const files = await fileExist(binaryPath)
    
    for (const file of files) {
      if (path.normalize(file) === path.normalize(path.join(binaryPath, isWin ? 'ad4m.exe' : `ad4m`))) {
        logger.info('ad4m-host binary found')
        resolve(null);
        return;
      } else {
        fs.rmSync(file)
      }
    }

    logger.info('ad4m-host binary not found, downloading now...')

    let data: any;
    try {
      const response = await fetch("https://api.github.com/repos/coasys/ad4m/releases/latest");
      if (!response.ok) {
        const status = response.status;
        const message = status === 403 ? 'GitHub API rate limited' : `GitHub API returned ${status}`;
        logger.error(message);
        reject(new Error(message));
        return;
      }
      data = await response.json();
    } catch (err) {
      logger.error(`Failed to fetch release info from GitHub: ${err}`);
      reject(err);
      return;
    }

    const version = (data && data['name']) ? data['name'].replace(/^v/, '') : 'unknown';
    global.ad4mHostVersion = version;

    let dest = path.join(binaryPath, `ad4m`);
    let download: any;
    await fs.ensureDir(binaryPath);
    
    if (isMac) {
      const asset = data.assets?.find((e: any) => e.name.includes("-macos-"));
      if (!asset) { reject(new Error('No macOS binary found in release')); return; }
      download = wget.download(asset.browser_download_url, dest)
    } else if(isWin) {
      dest = path.join(binaryPath, `ad4m.exe`);
      const asset = data.assets?.find((e: any) => e.name.includes("-windows-"));
      if (!asset) { reject(new Error('No Windows binary found in release')); return; }
      download = wget.download(asset.browser_download_url, dest)
    } else if (isLinux){
      const asset = data.assets?.find((e: any) => e.name.includes("-linux"));
      if (!asset) { reject(new Error('No Linux binary found in release')); return; }
      download = wget.download(asset.browser_download_url, dest)
    } else {
      console.error("Unknown OS type, cannot fetch ad4m binary");
      throw new Error("Unknown OS type, cannot fetch ad4m binary");
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
  const dataPath = relativePath

  if (fs.existsSync(dataPath)) {
    const files = fs.readdirSync(dataPath);
  
    for (const file of files) {
      const fileDir = path.join(relativePath, './', file);
      
      if (file !== 'binary') {
        fs.removeSync(fileDir);
      }
    }
  }
}