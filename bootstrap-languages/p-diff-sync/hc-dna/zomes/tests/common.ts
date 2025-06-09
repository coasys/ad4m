import path from "node:path";
import {Dna} from "@holochain/tryorama";

const dnas: Dna[] = [{ source: {path: path.join("../../workdir/perspective-diff-sync.dna")} }];
const happ_path = path.join("../../workdir/Perspective-Diff-Sync.happ");

export { dnas, happ_path };