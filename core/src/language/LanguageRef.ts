import type { Address } from '../Address'
// Unique Language ID with option type
export class LanguageRef {
    address: Address;
    name: string;

    constructor(address?: Address, name?: string) {
        this.address = address
        this.name = name
    }
}
