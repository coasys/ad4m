import type { NeighbourhoodExpression } from "@perspect3vism/ad4m";
import type { EntryHash } from "./types";

type ZomeCallFn = (fn: string, params: any) => Promise<any>;

export class NeighbourhoodStorage {
  #zomeCall: ZomeCallFn;

  constructor(zomeCall: ZomeCallFn) {
    this.#zomeCall = zomeCall;
  }

  async storeNeighbourhoodExpression(neighbourhoodExpression: NeighbourhoodExpression): Promise<EntryHash> {
    return await this.#zomeCall("store_neighbourhood_expression", neighbourhoodExpression);
  }

  async getNeighbourhoodExpression(fileHash: EntryHash): Promise<NeighbourhoodExpression> {
    return await this.#zomeCall("get_neighbourhood_expression", fileHash) as NeighbourhoodExpression;
  }
}

export default NeighbourhoodStorage