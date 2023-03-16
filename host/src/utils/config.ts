import path from "path";
import fs from 'fs';
import { ad4mDataDirectory } from "../ad4mDataDirectory";

export const CONFIG = 'ad4m-host-config.json';

type GetConfigReturntype = {
  seedPath?: string;
  dataPath?: string;
}

export function getConfig(dataPath = ''): GetConfigReturntype {
  try {
    const ad4mHostConfig = path.join(ad4mDataDirectory(dataPath), CONFIG);

    const config = fs.readFileSync(ad4mHostConfig, { encoding: 'utf-8' })

    const parsed = JSON.parse(config);

    return parsed;  
  } catch (e) { 
    const dest = path.join(ad4mDataDirectory(dataPath), CONFIG);

    fs.writeFileSync(dest, JSON.stringify({}))

    return {}
  }
}

export function getAd4mHostVersion(): string {
  const packageJson = path.join(__dirname, `../package.json`);
  const packageJsonParsed = JSON.parse(fs.readFileSync(packageJson, { encoding: 'utf-8' }));
  const version = packageJsonParsed["version"];
  console.log("Got ad4mHost version: ", version);
  return version;
}

export function getOldestSupportedVersion(): {version: string, shouldClearState: boolean} {
  const migrationFile = JSON.parse(fs.readFileSync(path.join(__dirname, `../oldestSupportedVersion.json`), { encoding: 'utf-8' }));
  return {version: migrationFile.version, shouldClearState: migrationFile.shouldClearState};
}