import path from "path";
import fs from 'fs-extra';
import { ad4mDataDirectory, deleteAllAd4mData, findAndKillProcess, getAd4mHostBinary, logger } from "./utils";

/**
 * Prepare the test environment by setting up the bootstrap seed and initializing
 * the AD4M executor data directory.
 *
 * The bootstrap seed contains the language-language bundle inline (ESM) and
 * hashes for all system languages. At runtime, the language-language fetches
 * other languages by hash from the bootstrap store (Cloudflare).
 *
 * The seed is located at:
 * - In the AD4M monorepo: ../../tests/js/bootstrapSeed.json (relative to build/)
 * - When installed as a package: ../bootstrapSeed.json (placed by consumer's setup)
 *
 * This replaces the previous approach of downloading individual language bundles
 * from perspect3vism repos and converting them from CJS to ESM.
 */
export async function installSystemLanguages(relativePath = '') {
  deleteAllAd4mData(relativePath);

  let binaryPath = path.join(ad4mDataDirectory(relativePath), 'binary', `ad4m`);

  if (!fs.existsSync(binaryPath)) {
    await getAd4mHostBinary(relativePath);
    binaryPath = path.join(ad4mDataDirectory(relativePath), 'binary', `ad4m`);
  }

  await findAndKillProcess('holochain')
  await findAndKillProcess('lair-keystore')

  // Look for bootstrap seed in order of preference:
  // 1. Adjacent to package root (../bootstrapSeed.json from build/) — installed package
  // 2. In the monorepo (../../tests/js/bootstrapSeed.json from build/) — development
  const candidates = [
    path.join(__dirname, '../bootstrapSeed.json'),
    path.join(__dirname, '../../tests/js/bootstrapSeed.json'),
  ];

  let seedPath: string | null = null;
  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) {
      seedPath = candidate;
      break;
    }
  }

  if (!seedPath) {
    throw new Error(
      `Bootstrap seed not found. Looked in:\n` +
      candidates.map(c => `  - ${c}`).join('\n') +
      `\nEnsure bootstrapSeed.json exists (copy from tests/js/ or download from the AD4M repo).`
    );
  }

  // If the seed is in the monorepo, copy it to the package root for startServer to find
  const targetSeedPath = path.join(__dirname, '../bootstrapSeed.json');
  if (seedPath !== targetSeedPath) {
    fs.copySync(seedPath, targetSeedPath);
    logger.info(`Bootstrap seed copied from ${seedPath}`);
  } else {
    logger.info('Bootstrap seed found at package root');
  }
}

if (require.main === module) {
  installSystemLanguages().then(() => {
    process.exit(0);
  }).catch(e => {
    console.error(e);
    process.exit(1);
  });
}
