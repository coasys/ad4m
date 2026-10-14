#!/usr/bin/env node
/**
 * Standalone WE Electron client for Playwright verification.
 * Deployed to each machine; only requires playwright-core + electron.
 *
 * Args (via env):
 *   EXECUTOR_HOST     — remote executor hostname
 *   EXECUTOR_PORT     — remote executor port
 *   USER_TOKEN        — JWT from user.login
 *   CONNECT_VERSION   — ad4m-connect version prefix for localStorage
 *   WE_REPO_PATH      — path to WE repo root
 *   OUTPUT_DIR        — screenshot output directory
 *   MACHINE_ID        — identifier for this machine
 *   SETTLE_SECONDS    — seconds to wait before final screenshot
 */
'use strict';

const { _electron } = require('playwright-core');
const path = require('path');
const fs = require('fs');

const os = require('os');

function expandHome(p) {
  if (p && p.startsWith('~/')) return path.join(os.homedir(), p.slice(2));
  return p;
}

const MACHINE_ID = process.env.MACHINE_ID || 'unknown';
const EXECUTOR_HOST = process.env.EXECUTOR_HOST;
const EXECUTOR_PORT = process.env.EXECUTOR_PORT;
const USER_TOKEN = process.env.USER_TOKEN;
const CONNECT_VERSION = process.env.CONNECT_VERSION || '0.13.0-test-interpretation-2';
const WE_REPO_PATH = expandHome(process.env.WE_REPO_PATH);
const OUTPUT_DIR = process.env.OUTPUT_DIR || '/tmp/sfu-electron-verify';
const SETTLE_SECONDS = Number(process.env.SETTLE_SECONDS || '10');
const HOLD_CALL_SECONDS = Number(process.env.HOLD_CALL_SECONDS || '0');
const SPACE_NAME = process.env.SPACE_NAME || 'SFU Test';

function log(msg) {
  const ts = new Date().toISOString().slice(11, 23);
  console.error(`[${ts}] [${MACHINE_ID}] ${msg}`);
}

(async () => {
  const result = {
    machine: MACHINE_ID,
    startTime: Date.now(),
    screenshots: [],
  };

  let app = null;
  let seedBackup = null;
  let seedRuntimePath = null;
  try {
    if (!EXECUTOR_HOST || !EXECUTOR_PORT || !USER_TOKEN || !WE_REPO_PATH) {
      throw new Error('Missing required env vars: EXECUTOR_HOST, EXECUTOR_PORT, USER_TOKEN, WE_REPO_PATH');
    }

    const weElectronDir = path.resolve(WE_REPO_PATH, 'apps', 'we-electron');
    const electronBin = path.join(weElectronDir, 'node_modules', '.bin', 'electron');
    const mainJs = path.join(weElectronDir, 'electron', 'main.js');

    if (!fs.existsSync(electronBin)) {
      throw new Error(`Electron binary not found: ${electronBin}`);
    }
    if (!fs.existsSync(mainJs)) {
      throw new Error(`main.js not found: ${mainJs}`);
    }

    fs.mkdirSync(OUTPUT_DIR, { recursive: true });

    const dataDir = path.join(OUTPUT_DIR, `we-data-${MACHINE_ID}`);
    fs.mkdirSync(dataDir, { recursive: true });

    seedRuntimePath = path.join(weElectronDir, 'electron', 'seed-runtime.json');
    try {
      seedBackup = fs.readFileSync(seedRuntimePath, 'utf-8');
      const seedConfig = JSON.parse(seedBackup);
      seedConfig.ad4mDataPath = dataDir;
      fs.writeFileSync(seedRuntimePath, JSON.stringify(seedConfig, null, 2));
      log(`Overrode seed-runtime.json: ad4mDataPath → ${dataDir}`);
    } catch (e) {
      log(`Could not override seed-runtime.json: ${e.message}`);
    }

    log('Launching Electron...');
    app = await _electron.launch({
      executablePath: electronBin,
      args: [
        mainJs,
        '--no-sandbox',
        '--enable-unsafe-swiftshader',
        '--use-fake-device-for-media-stream',
        '--use-fake-ui-for-media-stream',
        `--user-data-dir=${dataDir}`,
      ],
      env: {
        ...process.env,
        WE_EXTERNAL_AD4M_PORT: EXECUTOR_PORT,
        WE_EXTERNAL_AD4M_TOKEN: USER_TOKEN,
        WE_AD4M_DATA_PATH: dataDir,
        ELECTRON_ENABLE_LOGGING: '1',
      },
      timeout: 60000,
    });
    log('Electron launched');

    const page = await app.firstWindow();
    log('Got first window');

    // Capture console messages from the very start
    const consoleMsgs = [];
    page.on('console', msg => {
      const entry = `[${msg.type()}] ${msg.text()}`;
      consoleMsgs.push(entry);
      if (msg.type() === 'error' || msg.text().includes('WebSocket') || msg.text().includes('connect')) {
        log(`CONSOLE: ${entry}`);
      }
    });
    page.on('pageerror', err => {
      log(`PAGE ERROR: ${err.message}`);
      consoleMsgs.push(`[pageerror] ${err.message}`);
    });

    await page.waitForLoadState('domcontentloaded');
    result.windowLoaded = true;

    // Take screenshot of initial state (before auth injection)
    const shotInitial = path.join(OUTPUT_DIR, `${MACHINE_ID}-01-initial.png`);
    await page.screenshot({ path: shotInitial, fullPage: true });
    result.screenshots.push(shotInitial);
    log('Initial screenshot captured');

    // Inject auth credentials into localStorage
    const executorUrl = `http://${EXECUTOR_HOST}:${EXECUTOR_PORT}`;
    await page.evaluate(({ ver, jwt, url, port }) => {
      localStorage.clear();
      localStorage.setItem(`${ver}/ad4m-token`, jwt);
      localStorage.setItem(`${ver}/ad4m-url`, url);
      localStorage.setItem(`${ver}/ad4m-port`, String(port));
    }, {
      ver: CONNECT_VERSION,
      jwt: USER_TOKEN,
      url: executorUrl,
      port: Number(EXECUTOR_PORT),
    });
    log('Auth injected into localStorage');

    // Reload to pick up credentials
    await page.reload({ waitUntil: 'domcontentloaded' });
    log('Page reloaded');

    // Wait for app to settle and connect
    await page.waitForTimeout(8000);

    // Take screenshot of connected state
    const shotConnected = path.join(OUTPUT_DIR, `${MACHINE_ID}-02-connected.png`);
    await page.screenshot({ path: shotConnected, fullPage: true });
    result.screenshots.push(shotConnected);
    log('Connected screenshot captured');

    // Dismiss the "What should we call you?" modal — retry during settle
    async function tryDismissModal() {
      try {
        // Try the "Not now" button first
        const notNowBtn = page.getByRole('button', { name: 'Not now' });
        if (await notNowBtn.isVisible({ timeout: 500 })) {
          await notNowBtn.click();
          log('Dismissed name modal via "Not now"');
          await page.waitForTimeout(1000);
          return true;
        }
      } catch {}
      try {
        // Fallback: close button (X) on the modal
        const closeBtn = page.locator('[class*="modal"] button[class*="close"], dialog button[aria-label="Close"], button:has-text("×")');
        if (await closeBtn.first().isVisible({ timeout: 500 })) {
          await closeBtn.first().click();
          log('Dismissed modal via close button');
          await page.waitForTimeout(1000);
          return true;
        }
      } catch {}
      try {
        // Last resort: click the X character visible in the modal
        const xBtn = page.locator('button').filter({ hasText: /^(×|✕|X|✖)$/ });
        if (await xBtn.first().isVisible({ timeout: 300 })) {
          await xBtn.first().click();
          log('Dismissed modal via X button');
          await page.waitForTimeout(1000);
          return true;
        }
      } catch {}
      return false;
    }

    await tryDismissModal();

    // Take screenshot of main UI after first dismiss attempt
    const shotMain = path.join(OUTPUT_DIR, `${MACHINE_ID}-03-main-ui.png`);
    await page.screenshot({ path: shotMain, fullPage: true });
    result.screenshots.push(shotMain);
    log('Main UI screenshot captured');

    // Wait for the settle period, retrying modal dismissal periodically
    log(`Waiting ${SETTLE_SECONDS}s (checking for modal)...`);
    const settleEnd = Date.now() + SETTLE_SECONDS * 1000;
    while (Date.now() < settleEnd) {
      if (await tryDismissModal()) break;
      await page.waitForTimeout(2000);
    }
    const remaining = settleEnd - Date.now();
    if (remaining > 0) await page.waitForTimeout(remaining);

    // Dump DOM structure — pierce Shadow DOM for WE custom elements
    try {
      const domInfo = await page.evaluate(() => {
        function walk(root, depth = 0) {
          const results = [];
          if (depth > 6) return results;
          for (const el of root.children || []) {
            const rect = el.getBoundingClientRect();
            if (rect.width === 0 || rect.height === 0) continue;
            const tag = el.tagName?.toLowerCase();
            if (tag?.startsWith('we-') || tag === 'button' || rect.x < 100) {
              results.push({
                tag,
                text: el.textContent?.trim()?.slice(0, 40),
                name: el.getAttribute('name'),
                aria: el.getAttribute('aria-label'),
                variant: el.getAttribute('variant'),
                x: Math.round(rect.x), y: Math.round(rect.y),
                w: Math.round(rect.width), h: Math.round(rect.height),
              });
            }
            // Recurse into shadow DOM
            if (el.shadowRoot) {
              results.push(...walk(el.shadowRoot, depth + 1));
            }
            results.push(...walk(el, depth + 1));
          }
          return results;
        }
        const all = walk(document.body);
        const sidebar = all.filter(el => el.x < 100);
        return { total: all.length, sidebar, sample: all.slice(0, 40) };
      });
      result.domInfo = domInfo;
      log(`DOM: ${domInfo.total} elements, ${domInfo.sidebar.length} in sidebar`);
      for (const el of domInfo.sidebar.slice(0, 20)) {
        log(`  Sidebar: <${el.tag}> name="${el.name}" variant="${el.variant}" "${el.text?.slice(0,20)}" @(${el.x},${el.y}) ${el.w}x${el.h}`);
      }
    } catch (e) {
      log(`DOM dump failed: ${e.message}`);
    }

    // --- SPACE CREATION + CALL FLOW ---
    const CREATE_SPACE = process.env.CREATE_SPACE === 'true';
    const SHARED_SPACE = process.env.SHARED_SPACE === 'true';
    const START_CALL = process.env.START_CALL === 'true';

    if (CREATE_SPACE) {
      log('Creating space via sidebar...');
      try {
        // Expand sidebar
        await page.mouse.move(40, 300);
        await page.waitForTimeout(1500);

        // Open create-space modal
        const plusIcon = page.locator('we-icon[name="plus"]');
        if (await plusIcon.count() === 0) throw new Error('No plus icon found in sidebar');
        await plusIcon.first().click();
        await page.waitForTimeout(3000);

        // Fill the name field — we-input wraps a native <input> in shadow DOM,
        // so click the custom element to focus, then type via keyboard
        let filled = false;
        try {
          const nameField = page.getByPlaceholder('Space name');
          if (await nameField.count() > 0) {
            await nameField.first().click();
            await page.waitForTimeout(300);
            await page.keyboard.type('SFU Test');
            filled = true;
            log('Filled name via getByPlaceholder + keyboard');
          }
        } catch (e) { log(`getByPlaceholder strategy failed: ${e.message}`); }
        if (!filled) {
          try {
            const weInput = page.locator('we-input').first();
            if (await weInput.isVisible({ timeout: 2000 })) {
              await weInput.click();
              await page.waitForTimeout(300);
              await page.keyboard.type('SFU Test');
              filled = true;
              log('Filled name via we-input + keyboard');
            }
          } catch (e) { log(`we-input strategy failed: ${e.message}`); }
        }
        if (!filled) throw new Error('Could not fill space name');

        // Toggle to shared mode if requested
        if (SHARED_SPACE) {
          const sharedSwitch = page.locator('we-switch');
          const switchCount = await sharedSwitch.count();
          log(`Found ${switchCount} we-switch elements`);
          if (switchCount > 0) {
            await sharedSwitch.first().click();
            await page.waitForTimeout(500);
            log('Toggled space to shared');
          }
        }

        const shotFilled = path.join(OUTPUT_DIR, `${MACHINE_ID}-06-filled.png`);
        await page.screenshot({ path: shotFilled, fullPage: true });
        result.screenshots.push(shotFilled);

        // Submit
        const createBtn = page.getByText('Create Space', { exact: true });
        if (await createBtn.count() === 0) throw new Error('Create Space button not found');
        await createBtn.first().click();
        log('Clicked Create Space');
        await page.waitForTimeout(10000);

        const shotCreated = path.join(OUTPUT_DIR, `${MACHINE_ID}-07-created.png`);
        await page.screenshot({ path: shotCreated, fullPage: true });
        result.screenshots.push(shotCreated);
        log('Space created');
      } catch (e) {
        log(`Space creation failed: ${e.message}`);
      }
    }

    // Navigate into the space
    if (CREATE_SPACE || START_CALL) {
      try {
        // Expand sidebar
        await page.mouse.move(40, 400);
        await page.waitForTimeout(1500);

        // Click the space name label in the expanded sidebar
        const spaceLabel = page.getByText(SPACE_NAME);
        const labelCount = await spaceLabel.count();
        log(`Found ${labelCount} "SFU Test" labels`);

        if (labelCount > 0) {
          await spaceLabel.first().click({ timeout: 5000 });
          log('Clicked SFU Test label');
        } else {
          // Fallback: find sidebar we-buttons below y=450 (space badge area)
          const sidebarBtns = page.locator('we-button');
          let clicked = false;
          for (let i = 0; i < await sidebarBtns.count(); i++) {
            const box = await sidebarBtns.nth(i).boundingBox().catch(() => null);
            if (box && box.x < 80 && box.y > 450) {
              await sidebarBtns.nth(i).click({ timeout: 5000 });
              log(`Clicked sidebar button[${i}] at (${Math.round(box.x)},${Math.round(box.y)})`);
              clicked = true;
              break;
            }
          }
          if (!clicked) {
            // Last resort: click at approximate badge coordinates
            await page.mouse.click(38, 534);
            log('Clicked at badge coordinates (38, 534)');
          }
        }

        await page.waitForTimeout(5000);
        const shotInSpace = path.join(OUTPUT_DIR, `${MACHINE_ID}-08-in-space.png`);
        await page.screenshot({ path: shotInSpace, fullPage: true });
        result.screenshots.push(shotInSpace);
        log('Space interior screenshot captured');

        // Start a call — try header button first, then rail icon
        if (START_CALL) {
          let callStarted = false;

          // Strategy 1: we-button with text attribute "Call"
          const headerCallBtn = page.locator('we-button[text="Call"]');
          const headerCount = await headerCallBtn.count();
          log(`Header call buttons (we-button[text="Call"]): ${headerCount}`);

          // Strategy 2: rail icon phone-call
          const railCallIcon = page.locator('we-icon[name="phone-call"]');
          const railCount = await railCallIcon.count();
          log(`Rail call icons (we-icon[name="phone-call"]): ${railCount}`);

          if (headerCount > 0) {
            await headerCallBtn.first().click({ timeout: 5000 });
            callStarted = true;
            log('Clicked header Call button');
          } else if (railCount > 0) {
            await railCallIcon.first().click({ timeout: 5000 });
            callStarted = true;
            log('Clicked rail call icon');
          } else {
            log('No call button found (canCall may be false)');
          }

          if (callStarted) {
            await page.waitForTimeout(5000);

            const shotCallPip = path.join(OUTPUT_DIR, `${MACHINE_ID}-09-call-pip.png`);
            await page.screenshot({ path: shotCallPip, fullPage: true });
            result.screenshots.push(shotCallPip);
            log('Call PiP screenshot captured');

            // Click "Go to the call" for full call view
            const goToCall = page.locator('we-button[text="Go to the call"]');
            const goToCallText = page.getByText('Go to the call');
            const goCount = await goToCall.count();
            const goTextCount = await goToCallText.count();
            log(`"Go to the call" buttons: ${goCount} by attr, ${goTextCount} by text`);

            if (goTextCount > 0) {
              await goToCallText.first().click({ timeout: 5000 });
              log('Clicked "Go to the call"');
            } else if (goCount > 0) {
              await goToCall.first().click({ timeout: 5000 });
              log('Clicked "Go to the call" via attr');
            }

            await page.waitForTimeout(5000);

            const shotCallFull = path.join(OUTPUT_DIR, `${MACHINE_ID}-10-call-full.png`);
            await page.screenshot({ path: shotCallFull, fullPage: true });
            result.screenshots.push(shotCallFull);
            log('Full call view screenshot captured');

            // Collect WebRTC stats
            try {
              const rtcInfo = await page.evaluate(() => {
                const pcs = [];
                const allPcs = performance.getEntriesByType?.('resource') || [];
                // Try to find RTCPeerConnection instances via window
                if (typeof RTCPeerConnection !== 'undefined') {
                  return { available: true, pcCount: pcs.length };
                }
                return { available: false };
              });
              log(`WebRTC info: ${JSON.stringify(rtcInfo)}`);
            } catch (e) { log(`WebRTC stats failed: ${e.message}`); }

            console.error('---CALL-ACTIVE---');

            if (HOLD_CALL_SECONDS > 0) {
              log(`Holding in call for ${HOLD_CALL_SECONDS}s...`);
              const holdEnd = Date.now() + HOLD_CALL_SECONDS * 1000;
              let holdN = 0;
              while (Date.now() < holdEnd) {
                await page.waitForTimeout(15000);
                holdN++;
                const holdShot = path.join(OUTPUT_DIR, `${MACHINE_ID}-call-hold-${holdN}.png`);
                await page.screenshot({ path: holdShot, fullPage: true });
                result.screenshots.push(holdShot);
                log(`Hold screenshot ${holdN} (${Math.round((holdEnd - Date.now()) / 1000)}s left)`);
              }
            }
          }
        }
      } catch (e) {
        log(`Space navigation/call failed: ${e.message}`);
      }
    }

    // Take final screenshot
    const shotFinal = path.join(OUTPUT_DIR, `${MACHINE_ID}-04-final.png`);
    await page.screenshot({ path: shotFinal, fullPage: true });
    result.screenshots.push(shotFinal);
    log('Final screenshot captured');

    // Capture page URL and title
    result.pageUrl = page.url();
    result.pageTitle = await page.title();

    // Include captured console messages in result
    result.consoleMsgs = consoleMsgs.slice(-100);

    // Capture WebRTC stats if available
    try {
      const rtcStats = await page.evaluate(() => {
        const pcs = window.__rtcPeerConnections || [];
        return Promise.all(pcs.map(async (pc, i) => {
          const stats = await pc.getStats();
          const entries = [];
          stats.forEach(s => entries.push(s));
          return { index: i, state: pc.connectionState, stats: entries.slice(0, 10) };
        }));
      });
      result.rtcStats = rtcStats;
    } catch {
      result.rtcStats = 'not available';
    }

    result.passed = true;
    log('Done');
  } catch (err) {
    result.error = err.message;
    result.stack = err.stack?.split('\n').slice(0, 5).join('\n');
    result.passed = false;
    log(`Error: ${err.message}`);
  }

  if (app) {
    try { await app.close(); } catch {}
  }

  if (seedBackup !== null && seedRuntimePath) {
    try {
      fs.writeFileSync(seedRuntimePath, seedBackup);
      log('Restored seed-runtime.json');
    } catch {}
  }

  result.endTime = Date.now();
  result.durationMs = result.endTime - result.startTime;
  console.log('---RESULT---');
  console.log(JSON.stringify(result, null, 2));
  process.exit(result.passed ? 0 : 1);
})();
