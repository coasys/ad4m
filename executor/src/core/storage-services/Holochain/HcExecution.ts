import child_process from "child_process";

function escapeShellArg (arg: string) {
    return arg.replace(" ", "\ ");
}

export function unpackDna(hcPath: string, dnaPath: string): string {
    return child_process.execFileSync(`${escapeShellArg(hcPath)}`, ["dna", "unpack", `${escapeShellArg(dnaPath)}`]).toString();
}

export function packDna(hcPath: string, workdirPath: string): string {
    return child_process.execFileSync(`${escapeShellArg(hcPath)}`, ["dna", "pack", `${escapeShellArg(workdirPath)}`]).toString();
}
