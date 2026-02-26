import { console_log, console_debug, console_error, console_warn, hash } from "ext:core/ops";

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
        consoleError: (args) => {
            return console_error(args);
        },
        consoleWarn: (args) => {
            return console_warn(args);
        }
    };
  })(globalThis);
