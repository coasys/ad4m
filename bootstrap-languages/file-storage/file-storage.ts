import type { FileExpression, EntryHash } from "./types.ts";
//import { Blob } from "https://esm.sh/v135/buffer@6.0.3";

type ZomeCallFn = (fn: string, params: any) => Promise<any>;

export class FileStorage {
  #zomeCall: ZomeCallFn;

  constructor(zomeCall: ZomeCallFn) {
    this.#zomeCall = zomeCall;
  }

  async upload(
    file: Blob,
    onProgress:
      | undefined
      | ((percentatgeProgress: number, bytesSent: number) => void) = undefined,
    chunkSize: number = 256 * 1024
  ): Promise<EntryHash[]> {
    console.log("splitting file...")
    const blobs = this.splitBlob(file, chunkSize);
    console.log("done")
    const numberOfChunks = blobs.length;
    const bytesPerChunk = blobs[0].size;

    const chunksHashes: Array<EntryHash> = [];
    for (let i = 0; i < blobs.length; i++) {
      console.log("creating chunk", i)
      const chunkHash = await this.createChunk(blobs[i]);
      console.log("done")
      chunksHashes.push(chunkHash);
      if (onProgress) {
        onProgress(((i + 1) * 1.0) / numberOfChunks, bytesPerChunk * (i + 1));
      }
    }
    
    return chunksHashes;
  }

  async download(chunksHashes: Array<EntryHash>): Promise<Blob> {
    const chunks = [];
    for (const chunkHash of chunksHashes) {
      let chunk = await this.fetchChunk(chunkHash);
      chunks.push(chunk);
    }
    return this.mergeChunks(chunks);
  }

  async storeFileExpression(fileExpression: FileExpression): Promise<EntryHash> {
    return await this.#zomeCall("store_file_expression", fileExpression);
  }

  async getFileExpression(fileHash: EntryHash): Promise<FileExpression> {
    return await this.#zomeCall("get_file_expression", fileHash) as FileExpression;
  }

  async fetchChunk(fileChunkHash: EntryHash): Promise<Blob> {
    let bytes = null
    let tries = 0
    while (bytes === null && tries < 10) {
      tries++
      bytes = await this.#zomeCall("get_file_chunk", fileChunkHash);
      if (bytes === null) {
        await new Promise(resolve => setTimeout(resolve, 500))
      }
    }
    
    if(bytes === null) {
      throw new Error("Could not fetch chunk. Giving up after 10 attempts.")
    }

    // @ts-ignore
    return new Blob([new Uint8Array(bytes)]);
  }

  private splitBlob(file: Blob, chunkSize: number): Blob[] {
    let offset = 0;
    const chunks: Blob[] = [];

    while (file.size > offset) {
      const chunk = file.slice(offset, offset + chunkSize);
      offset += chunkSize;
      chunks.push(chunk);
    }

    return chunks;
  }

  private mergeChunks(chunks: Blob[]): Blob {
      const merged = new Blob(chunks);
      return merged;
  }

  private async createChunk(chunk: Blob): Promise<EntryHash> {
    const bytes = await chunk.arrayBuffer();
    // @ts-ignore
    return this.#zomeCall("store_chunk", new Uint8Array(bytes));
  }

}
  