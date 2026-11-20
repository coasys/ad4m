import { ApiClient } from "../apiClient";
import type { DataChunk } from "../generated/api/DataChunk";
import type { DataExportPrepared } from "../generated/api/DataExportPrepared";
import type { DataImportBegun } from "../generated/api/DataImportBegun";
import type { DataImportChunked } from "../generated/api/DataImportChunked";
import type { DataImported } from "../generated/api/DataImported";

/** The size of every chunk but the last, in bytes. */
export const DATA_CHUNK_BYTES = 1 << 20;

function toBase64(bytes: Uint8Array): string {
    let binary = "";
    for (let i = 0; i < bytes.length; i += 0x8000) {
        binary += String.fromCharCode(...bytes.subarray(i, i + 0x8000));
    }
    return btoa(binary);
}

function fromBase64(text: string): Uint8Array {
    const binary = atob(text);
    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
    return bytes;
}

async function sha256Hex(bytes: Uint8Array): Promise<string> {
    const digest = await crypto.subtle.digest("SHA-256", bytes as BufferSource);
    return Array.from(new Uint8Array(digest), (b) => b.toString(16).padStart(2, "0")).join("");
}

/**
 * A person's export and import of their own data: the file is gzip JSON of their perspectives
 * and links (signatures kept), moved in chunks of 1 MiB. Calls act for the session's own agent.
 * A held tenant may export but not import. An export lives 15 minutes past its last chunk; a
 * person has one export and one import open at a time and may prepare 4 exports an hour.
 */
export class DataClient {
    #apiClient: ApiClient;

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
    }

    /** Builds the export file of the caller's perspectives (or just `perspectives`, their uuids). */
    async exportPrepare(perspectives?: string[]): Promise<DataExportPrepared> {
        return this.#apiClient.call("data.exportPrepare", { perspectives: perspectives ?? null });
    }

    /** Chunk `index` of a prepared export, base64. Reading the last one ends the export. */
    async exportChunk(exportId: string, index: number): Promise<DataChunk> {
        return this.#apiClient.call("data.exportChunk", { exportId, index });
    }

    /** Opens an import of `size` bytes whose sha256 is `sha256` (hex). */
    async importBegin(size: number, sha256: string): Promise<DataImportBegun> {
        return this.#apiClient.call("data.importBegin", { size, sha256 });
    }

    /** Sends chunk `index` (in order from 0), base64, of at most 1 MiB decoded. */
    async importChunk(importId: string, index: number, data: string): Promise<DataImportChunked> {
        return this.#apiClient.call("data.importChunk", { importId, index, data });
    }

    /** Writes the file into new private perspectives of the caller. */
    async importCommit(importId: string): Promise<DataImported> {
        return this.#apiClient.call("data.importCommit", { importId });
    }

    /** The whole export file (gzip), read chunk by chunk and checked against its sha256. */
    async exportFile(perspectives?: string[]): Promise<Uint8Array> {
        const prepared = await this.exportPrepare(perspectives);
        const file = new Uint8Array(prepared.size);
        let at = 0;
        for (let index = 0; index < prepared.chunks; index++) {
            const part = fromBase64((await this.exportChunk(prepared.exportId, index)).data);
            file.set(part, at);
            at += part.length;
        }
        if (at !== prepared.size || (await sha256Hex(file)) !== prepared.sha256) {
            throw new Error("the export arrived damaged: its size or sha256 does not match");
        }
        return file;
    }

    /**
     * Imports an export file. Its spaces come back as `neighbourhoodUrls`, for the app to join
     * with `neighbourhood.join`.
     */
    async importFile(file: Uint8Array): Promise<DataImported> {
        const { importId } = await this.importBegin(file.length, await sha256Hex(file));
        for (let at = 0, index = 0; at < file.length; at += DATA_CHUNK_BYTES, index++) {
            await this.importChunk(importId, index, toBase64(file.subarray(at, at + DATA_CHUNK_BYTES)));
        }
        return this.importCommit(importId);
    }
}
