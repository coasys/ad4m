import { ExpressionGeneric } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";

export type EntryHash = Uint8Array;

export class FileMetadata {
  name: string = "";
  size: number = 0;
  file_type: string = "";
  checksum: string = "";
  chunks_hashes: Array<EntryHash> = [];
}

export class FileExpression extends ExpressionGeneric(FileMetadata) {} ;
