import { ExpressionGeneric } from "@perspect3vism/ad4m";

export type EntryHash = Uint8Array;

export class FileMetadata {
  name: string;
  size: number;
  file_type: string;
  checksum: string;
  chunks_hashes: Array<EntryHash>;
}

export class FileExpression extends ExpressionGeneric(FileMetadata) {} ;
