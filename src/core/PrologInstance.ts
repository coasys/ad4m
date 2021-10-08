import fs from "fs-extra";
//@ts-ignore
import _eval from 'eval'
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
    #FS: any
    #initialzed: Promise<void>

    constructor() {
        this.#bindings = {}
        //@ts-ignore
        let resolveInitialized
        this.#initialzed = new Promise(resolve=>{
            resolveInitialized = resolve
        })
        const readStdin = this.readStdin
        var Module = {
            ENVIRONMENT: "NODE",
            noInitialRun: true,
            locateFile: (url:string) => `./swipl-wasm/dist/${url}`,
            getPreloadedPackage: (file: string, size: number) => {
                console.debug("getPreloadedPackage", file)
                const read = fs.readFileSync(file)
                console.debug(read)
                return read
            },
            print: (x:any) => console.log("print:", x),
            printErr: (x:any) => console.log("printErr:", x),
            //@ts-ignore
            preRun: [() => Module.FS.init(readStdin)], // sets up stdin
            onRuntimeInitialized: () => {
                console.log("onRuntimeInitialized 1")
                // Bind foreign functions to JavaScript.
                //@ts-ignore
                this.#bindings = createBindings(Module);
                // Initialise SWI-Prolog.
                //@ts-ignore
                this.initialise(Module);
                console.log("onRuntimeInitialized 2")
                //@ts-ignore
                resolveInitialized()
                console.log("onRuntimeInitialized 3")
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

    readStdin() {
        if (this.#stdinPosition >= this.#stdin.length) {
            return null;
        } else {
            const code = this.#stdin.charCodeAt(this.#stdinPosition);
            this.#stdinPosition++;
            return code;
        }
    };

    query(input: string) {
        this.setStdin(input);
        // This will execute one iteration of toplevel.
        this.call('break'); // see call.js
    }

    call(query: string) {
        console.log("call 1")
        const ref = this.#bindings.PL_new_term_ref();
        if (!this.#bindings.PL_chars_to_term(query, ref)) {
            throw new Error('Query has a syntax error: ' + query);
        }
        console.log("call 2")
        const result = !!this.#bindings.PL_call(ref, 0);
        console.log("result", result)
        console.log("call 3")
        return result
    };

    setProgram(program: string) {
        console.log("setProgram 1")
        //@ts-ignore
        this.#FS.writeFile('/file.pl', program);
        console.log("setProgram 2")
        this.query("consult('/file.pl').");
        console.log("setProgram 3")
    }
}