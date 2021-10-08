import fs from "fs-extra";
//@ts-ignore
import _eval from 'eval'
//@ts-ignore
import { UTF8ArrayToString } from './utf8ArrayToString'
interface EmscriptenModule {
    cwrap(fn_name: string, param: string, params: string[]): void;
    allocate(a: number[], b: string, c: string): void;
    intArrayFromString(s: string): number[];
    _malloc(a: number): number;
    setValue(ptr: number, ptr2: void, value: string):void;

    ALLOC_NORMAL: string;
}

const createBindings = (module: EmscriptenModule) => {
    return {
        PL_initialise: module.cwrap('PL_initialise', 'number', ['number', 'number']),
        PL_new_term_ref: module.cwrap('PL_new_term_ref', 'number', []),
        PL_chars_to_term: module.cwrap('PL_chars_to_term', 'number', ['string', 'number']),
        PL_call: module.cwrap('PL_call', 'number', ['number', 'number'])
    };
};

export default class PrologInstance {
    #bindings: any
    #stdin = ''
    #stdinPosition = 0
    #stdout: number[] = []
    #FS: any
    #initialzed: Promise<void>

    constructor() {
        this.#bindings = {}
        //@ts-ignore
        let resolveInitialized
        this.#initialzed = new Promise(resolve=>{
            resolveInitialized = resolve
        })
        let that = this
        const readStdin = () => that.readStdin()
        //@ts-ignore
        const writeStdout = (char) => that.writeStdout(char)
        var Module = {
            ENVIRONMENT: "NODE",
            noInitialRun: true,
            locateFile: (url:string) => `./swipl-wasm/dist/${url}`,
            getPreloadedPackage: (file: string, size: number) => {
                //console.debug("getPreloadedPackage", file)
                const read = fs.readFileSync(file)
                //console.debug(read)
                return read
            },
            print: (x:any) => console.log("print:", x),
            printErr: (x:any) => console.log("printErr:", x),
            //@ts-ignore
            preRun: [() => Module.FS.init(readStdin, writeStdout)], // sets up stdin
            onRuntimeInitialized: () => {
                // Bind foreign functions to JavaScript.
                //@ts-ignore
                this.#bindings = createBindings(Module);
                // Initialise SWI-Prolog.
                //@ts-ignore
                this.initialise(Module);
                //@ts-ignore
                resolveInitialized()
            }
        }
        const swiplInitCode = fs.readFileSync('./swipl.js').toString()
        _eval(swiplInitCode, './swipl.js', { Module }, true)
        //@ts-ignore
        this.#FS = Module.FS
    }

    initialized(): Promise<void> {
        return this.#initialzed
    }

    initialise(module: EmscriptenModule) {
        const argvArray = [
            module.allocate(module.intArrayFromString('swipl'), 'i8', module.ALLOC_NORMAL),
            module.allocate(module.intArrayFromString('-x'), 'i8', module.ALLOC_NORMAL),
            module.allocate(module.intArrayFromString('wasm-preload/swipl.prc'), 'i8', module.ALLOC_NORMAL),
            module.allocate(module.intArrayFromString('--nosignals'), 'i8', module.ALLOC_NORMAL)
        ];
        const argvPtr = module._malloc(argvArray.length * 4);
        for (let i = 0; i < argvArray.length; i++) {
            module.setValue(argvPtr + i * 4, argvArray[i], '*');
        }
        if (!this.#bindings.PL_initialise(4, argvPtr)) {
            throw new Error('SWI-Prolog initialisation failed.');
        }
        // Set the path of the preloaded (from swipl-web.dat) standard library.
        // This makes it possible to call use_module(library(lists)) and so on.
        this.call("assert(user:file_search_path(library, 'wasm-preload/library')).");
    };

    setStdin(s: string) {
        this.#stdin = s;
        this.#stdinPosition = 0;
    }

    readStdin(): number|null {
        if (this.#stdinPosition >= this.#stdin.length) {
            return null;
        } else {
            const code = this.#stdin.charCodeAt(this.#stdinPosition);
            this.#stdinPosition++;
            return code;
        }
    };

    writeStdout(char: number) {
        this.#stdout.push(char)
    }

    getStdout(): string {
        const s = UTF8ArrayToString(this.#stdout, 0).trim()
        this.#stdout = []
        return s
    }

    query(input: string) {
        this.setStdin(input);
        // This will execute one iteration of toplevel.
        this.call('break'); // see call.js
    }

    call(query: string) {
        const ref = this.#bindings.PL_new_term_ref();
        if (!this.#bindings.PL_chars_to_term(query, ref)) {
            throw new Error('Query has a syntax error: ' + query);
        }
        const result = !!this.#bindings.PL_call(ref, 0);
        return result
    };

    consult(program: string) {
        //@ts-ignore
        this.#FS.writeFile('/file.pl', program);
        this.query("consult('/file.pl').");
    }

    writeFile(file: string, content: string) {
        this.#FS.writeFile(`/${file}`, content);
    }

    readFile(file: string): string {
        return this.#FS.readFile(file, {encoding: 'utf8'})
    }
}