Error.captureStackTrace = function (err, constructorOpt) {
    if (typeof Error.prepareStackTrace === 'function') {
      const fakeCallSite = {
        getThis() {
          return undefined;
        },
        getTypeName() {
          return undefined;
        },
        getFunction() {
          return undefined;
        },
        getFunctionName() {
          return undefined;
        },
        getMethodName() {
          return undefined;
        },
        getFileName() {
          return err.fileName;
        },
        getLineNumber() {
          return err.lineNumber;
        },
        getColumnNumber() {
          return err.columnNumber;
        },
        getEvalOrigin() {
          return undefined;
        },
        isToplevel() {
          return false;
        },
        isEval() {
          return false;
        },
        isNative() {
          return false;
        },
        isConstructor() {
          return false;
        },
        isAsync() {
          return false;
        },
        isPromiseAll() {
          return false;
        },
      };

      const structuredStackTrace = [fakeCallSite];
      if(err.code == "UNKNOWN", err.syscall == "accept") {
        err.stack = Error.prepareStackTrace(err, structuredStackTrace);
      }
    } else {
      err.stack = err.stack || err.toString();
    }
} 

import { init } from 'https://ad4m.runtime/executor'

async function initCore(config) {
  console.log('lol 1')
    const core = await init(config)
    console.log('lol 2', JSON.stringify(core))
    globalThis.core = core
    console.log('lol 3')
    return core
}

globalThis.initCore = initCore
