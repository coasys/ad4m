import "node:util";
// import "https://deno.land/std@0.177.0/node/global.ts";

import processModule from "node:process";
// import { Buffer as bufferModule } from "node:process";
import timers from "node:timers";
import {Buffer as BufferModule} from 'node:buffer';

Object.defineProperty(globalThis, "global", {
  value: globalThis,
  writable: false,
  enumerable: false,
  configurable: true,
});

Object.defineProperty(globalThis, "process", {
  value: processModule,
  enumerable: false,
  writable: true,
  configurable: true,
});

Object.defineProperty(globalThis, "Buffer", {
  value: BufferModule,
  enumerable: false,
  writable: true,
  configurable: true,
});

Object.defineProperty(globalThis, "setImmediate", {
  value: timers.setImmediate,
  enumerable: true,
  writable: true,
  configurable: true,
});

Object.defineProperty(globalThis, "clearImmediate", {
  value: timers.clearImmediate,
  enumerable: true,
  writable: true,
  configurable: true,
});


const process = globalThis.process;
const Buffer = globalThis.Buffer;
const setImmediate = globalThis.setImmediate;
const clearImmediate = globalThis.clearImmediate;
const __dirname = new URL('.', import.meta.url).pathname;
const __filename = new URL('', import.meta.url).pathname;
globalThis.__dirname = __dirname;
globalThis.__filename = __filename;

import { init as internalInit } from "./main.ts"
import * as internalPath from "node:path";
import * as internalOs from "node:os"

export const init = internalInit
export const path = internalPath
export const os = internalOs

console.log = (...args) => {
  UTILS.consoleLog(args.map(arg => typeof arg === 'string' ? arg : JSON.stringify(arg)).join(' '))
};

console.debug = (...args) => {
  UTILS.consoleDebug(args.map(arg => typeof arg === 'string' ? arg : JSON.stringify(arg)).join(' '))
};

console.dir = (...args) => {
  UTILS.consoleDebug(args.map(arg => JSON.stringify(arg)).join(' '))
};

console.error = (...args) => {
  UTILS.consoleError(args.map(arg => typeof arg === 'string' ? arg : JSON.stringify(arg)).join(' '))
};

console.warn = (...args) => {
  UTILS.consoleWarn(args.map(arg => typeof arg === 'string' ? arg : JSON.stringify(arg)).join(' '))
};

import "https://deno.land/x/xhr@0.3.0/mod.ts";

import { HTMLElement } from "linkedom"

// @ts-ignore
globalThis.HTMLElement = HTMLElement;