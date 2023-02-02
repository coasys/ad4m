/**
 * Copy binaries from pkg to os file system, https://github.com/vercel/pkg/issues/342
 */

import type { Arguments, Argv } from 'yargs';
import path from 'path';
import fs from 'fs-extra';
import { getAd4mHostVersion, getOldestSupportedVersion } from '../utils/config';
import { ad4mDataDirectory } from '../ad4mDataDirectory';
import semver from "semver";

type Options = {
    dataPath?: string;
};

export const command: string = 'prepare';
export const desc: string = 'Check the AD4M data directory and remove any old data from previous versions if required';

export const builder = (yargs: Argv) =>
  yargs
    .options({
        dataPath: { 
            type: 'string', 
            describe: 'Name of directory to store ad4m data in the systems app data path', 
            alias: 'rp'
        },
    });

function cleanAd4mData(dataPath: string) {
    const appDataPath = ad4mDataDirectory(dataPath);
    if (fs.existsSync(appDataPath)) {
        // Path to our binaries
        const binaryPath = path.join(appDataPath, 'binary');
        // Path to our config
        const configPath = path.join(appDataPath, 'ad4m-host-config.json');
        // Path to bootstrap seed
        const bootstrapSeedPath = path.join(appDataPath, 'mainnet_seed.json');
        // Path to holochain data
        const holochainDataPath = path.join(appDataPath, 'ad4m', 'h');
        // Path to languages
        const languagesPath = path.join(appDataPath, 'ad4m', 'languages');
        // path to perspectives
        const perspectivesPath = path.join(appDataPath, 'ad4m', 'perspectives.json');

        //Delete all the data which may conflict with the new version
        fs.removeSync(binaryPath);
        fs.removeSync(configPath);
        fs.removeSync(bootstrapSeedPath);
        fs.removeSync(holochainDataPath);
        fs.removeSync(languagesPath);
        fs.removeSync(perspectivesPath);
    }
}

export const handler = (argv: Arguments<Options>): void => {
    const {dataPath = ''} = argv;
    const appDataPath = ad4mDataDirectory(dataPath);

    if (!fs.existsSync(appDataPath)) {
        console.log("No ad4m data directory found, skipping prepare command");
        // The ad4mDataDirectory is not created yet, dont create here, but instead have it be created in init.ts
        return;
    }

    const lastSeenFile = path.join(appDataPath, 'last-seen-version');
    if (!fs.existsSync(lastSeenFile)) {
        // No last seen version file, lets clean their state. Note we are assuming the first time this added to a release
        // we wish to clear the stat eof an agent
        console.log("Not last seen version file, lets clean their state");
        cleanAd4mData(dataPath);
        fs.writeFileSync(lastSeenFile, getAd4mHostVersion());
        return;
    }

    const lastSeenVersion = fs.readFileSync(lastSeenFile, { encoding: 'utf-8' });
    console.log("Current last seen version is", lastSeenVersion);
    const oldestSupportedVersion = getOldestSupportedVersion();
    if (!semver.gte(lastSeenVersion, oldestSupportedVersion)) {
        // Agents old ad4m version is too old, lets clean their state
        console.log("Agents old ad4m version is too old, lets clean their state");
        cleanAd4mData(dataPath);
        return;
    }
};