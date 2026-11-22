import { core } from "ext:core/mod.js";
import { console_log, console_debug, console_info, console_error, console_warn, hash } from "ext:core/ops";

// Deno's Console hands its print function one formatted line and a level:
// 0 debug, 1 log/info/dir/table/count/time, 2 warn, 3 error, 4 trace.
// A trace is a stack for debugging, not a failure, so it logs at debug.
// `info` is the level for 1 in the console's own `info` method only.
function printTo(info) {
    return (msg, level) => {
        const { stripVTControlCharacters } = core.loadExtScript("ext:deno_web/01_console.js");
        const line = stripVTControlCharacters(String(msg)).replace(/\n+$/, "");
        if (line.trim() === "") return;
        if (level === 3) console_error(line);
        else if (level === 2) console_warn(line);
        else if (level === 1 && info) console_info(line);
        else console_debug(line);
    };
}

// A console whose output goes to the executor's log at the level of its
// method (log and debug at debug, info at info, warn, error), instead of
// straight to stdout, where it bypassed RUST_LOG and the log files.
function languageConsole() {
    const { Console } = core.loadExtScript("ext:deno_web/01_console.js");
    const console = new Console(printTo(false));
    console.info = new Console(printTo(true)).info;
    return console;
}

((globalThis) => {
    globalThis.UTILS = {
        hash: (data) => {
            return hash(data);
        },
        consoleLog: (args) => {
            return console_log(args);
        },
        consoleDebug: (args) => {
            return console_debug(args);
        },
        consoleInfo: (args) => {
            return console_info(args);
        },
        consoleError: (args) => {
            return console_error(args);
        },
        consoleWarn: (args) => {
            return console_warn(args);
        },
        languageConsole,
    };
  })(globalThis);
