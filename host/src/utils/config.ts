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